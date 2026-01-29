# ==============================================================================
# 0. VOORBEREIDING EN SETUP
# ==============================================================================
library(terra)
library(tidyverse)
library(sf)
library(here)
library(exactextractr)
library(data.table)
library(lubridate)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::month)
conflicted::conflicts_prefer(dplyr::filter)
# Geheugen opruimen
gc()

message("Start script: Hydrologische Variabelen (Aangepast aan KMI Data)")

# ==============================================================================
# 1. DATA INLEZEN (AANGEPAST AAN JOUW FORMAT)
# ==============================================================================

# 1.1 Metadata van het grid
message("Inlezen metadata...")
# Let op: We dwingen pixel_id hier naar integer ipv factor voor de snelheid
meta_data <- read_delim(
  here("data", "ruw", "neerslag", "climategrid_pixel_metadata.csv"),
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE) %>%
  rename(latitude = PIXEL_LAT_CENTER, longitude = PIXEL_LON_CENTER) %>%
  mutate(pixel_id = as.integer(PIXEL_ID)) %>% # BELANGRIJK: Integer, geen factor!
  select(pixel_id, latitude, longitude)

# 1.2 Neerslagdata (Alle CSV's in de map)
message("Inlezen neerslag data (dit kan even duren)...")
path_kmi <- here("data", "ruw", "neerslag", "kmi")
bestandslijst <- list.files(path = path_kmi, pattern = "climategrid_.*\\.csv$", full.names = TRUE)

# We gebruiken fread met de juiste separators (punt decimaal, puntkomma scheiding)
dt_neerslag <- rbindlist(lapply(bestandslijst, fread, sep = ";", dec = "."))

# Zeker zijn dat alle kolomnamen kleine letters zijn (voor consistentie)
setnames(dt_neerslag, tolower(names(dt_neerslag)))

# Datatypes correct zetten (Snelheid optimalisatie)
# We nemen 'evapotrans_ref' meteen mee voor de SPEI berekening later!
dt_neerslag[, `:=`(
  pixel_id = as.integer(pixel_id),
  day = as.Date(day, format = "%Y/%m/%d"), # Specifiek formaat met slashes
  precip_quantity = as.numeric(precip_quantity),
  evapotrans_ref = as.numeric(evapotrans_ref)
)]

# Check of data goed geladen is
if (nrow(dt_neerslag) == 0) stop("Geen data ingelezen! Check je paden.")

# 1.3 Macroinvertebraten & Afstroomgebieden (zoals voorheen)
message("Inlezen spatial data...")
mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = TRUE) %>%
  filter(monsternamedatum > "2006-12-31") %>%
  mutate(monsternamedatum = as.Date(monsternamedatum))

afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_nested_all.gpkg"), quiet = TRUE) %>%
  select(meetplaats, geom) %>%
  mutate(meetplaats = as.character(meetplaats))

# Join samples aan afstroomgebieden
samples_geo <- mi_meetpunten %>%
  st_drop_geometry() %>%
  distinct(meetplaats, monsternamedatum, .keep_all = TRUE) %>%
  mutate(meetplaats = as.character(meetplaats)) %>%
  inner_join(afstroomgebieden, by = "meetplaats") %>%
  st_as_sf()

message(sprintf("Aantal samples om te verwerken: %d", nrow(samples_geo)))

# ==============================================================================
# 2. RUIMTELIJKE KOPPELING (WEIGHTS MATRIX)
# ==============================================================================
message("Stap 2: Ruimtelijke gewichten berekenen...")

# 2.1 Raster maken (ROBUUSTE METHODE: RASTERIZE)
message("   -> Raster opbouwen via 'Rasterize' methode...")

# A. Zet metadata om naar Lambert 72 (EPSG:31370)
# Dit is essentieel omdat je grids 5000x5000 meter zijn in dit stelsel
points_sf <- st_as_sf(meta_data, coords = c("longitude", "latitude"), crs = 4326)
points_lb <- st_transform(points_sf, 31370)

# B. Bepaal de grenzen (Extent) en lijn uit op het 5000m rooster
# We ronden de coördinaten af zodat ze perfect op het 5km grid vallen
bb <- st_bbox(points_lb)
xmin <- floor(bb["xmin"] / 5000) * 5000
ymin <- floor(bb["ymin"] / 5000) * 5000
xmax <- ceiling(bb["xmax"] / 5000) * 5000
ymax <- ceiling(bb["ymax"] / 5000) * 5000

# C. Maak een LEEG raster template
# We dwingen hier hard die 5000m resolutie af, net zoals jij deed.
r_empty <- rast(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                res = 5000, crs = "EPSG:31370")

# D. Vul het raster met de pixel_ids
# We 'branden' de ID's in het raster.
r_template <- rasterize(vect(points_lb), r_empty, field = "pixel_id")

# Controleer even visueel of het klopt (moet ca 1360 cells hebben die niet NA zijn)
print(r_template)
message("Raster succesvol aangemaakt met vaste resolutie.")

# 2.2 Watersheds transformeren naar raster CRS
# We transformeren de polygonen naar het coördinatenstelsel van het raster
watersheds_unique <- afstroomgebieden %>%
  distinct(meetplaats, .keep_all = TRUE) %>%
  st_transform(st_crs(r_template))

# 2.3 Exact Extract: Welke pixels liggen in welke watershed?
# Dit geeft een lijst terug met databrames
extract_list <- exact_extract(r_template, watersheds_unique, include_cell = FALSE)

# 2.4 Verwerk de lijst naar een tabel met gewichten
weights_df <- map2_df(extract_list, watersheds_unique$meetplaats, function(df, id) {
  # Veiligheidscheck voor lege resultaten (bvb watershed buiten grid)
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df %>%
    rename(pixel_id = value) %>%
    group_by(pixel_id) %>%
    summarise(coverage = sum(coverage_fraction), .groups = "drop") %>%
    mutate(meetplaats = id)
})

# 2.5 Normaliseer de gewichten (Weighted Mean Logic)
# Dit zorgt ervoor dat de som van gewichten per meetplaats altijd 1 is.
dt_weights <- as.data.table(weights_df)
dt_weights[, weight := coverage / sum(coverage), by = meetplaats]

# Filter verwaarloosbare pixels (bvb < 0.1% coverage) voor snelheid
dt_weights <- dt_weights[weight > 0.001]

message("Ruimtelijke koppeling voltooid.")

# ==============================================================================
# 3. TEMPORELE AGGREGATIE (GEOPTIMALISEERD VOOR GEHEUGEN)
# ==============================================================================
message("Stap 3: Dagelijkse P en E per afstroomgebied berekenen...")

# 3.0 Eerst grote schoonmaak: verwijder kolommen die we niet nodig hebben
# We hebben alleen pixel_id, day, precip en evap nodig.
# Temperatuur, humidity, etc. vreten geheugen -> weg ermee.
cols_to_keep <- c("pixel_id", "day", "precip_quantity", "evapotrans_ref")
dt_neerslag <- dt_neerslag[, ..cols_to_keep]
gc() # Forceer R om geheugen vrij te maken

# 3.1 De 'Chunking' Loop
# We verwerken het jaar per jaar om RAM te sparen.
jaren <- unique(year(dt_neerslag$day))
results_list <- list()

# Initialiseer progress bar
pb <- txtProgressBar(min = 0, max = length(jaren), style = 3)

for (i in seq_along(jaren)) {
  curr_year <- jaren[i]

  # A. Filter data voor 1 jaar
  dt_year <- dt_neerslag[year(day) == curr_year]

  # B. Join met gewichten (Nu is de tabel 35x kleiner, dus dit past in RAM)
  dt_merged <- merge(dt_year, dt_weights, by = "pixel_id", all.y = FALSE, allow.cartesian = TRUE)

  # C. Aggregeer direct naar catchment niveau
  dt_agg <- dt_merged[, .(
    P_mean = sum(precip_quantity * weight, na.rm = TRUE),
    E_mean = sum(evapotrans_ref * weight, na.rm = TRUE)
  ), by = .(meetplaats, day)]

  # D. Sla op in de lijst
  results_list[[i]] <- dt_agg

  # Update progress bar & Clean memory
  setTxtProgressBar(pb, i)
  rm(dt_year, dt_merged, dt_agg) # Verwijder tijdelijke objecten
  gc() # Garbage collection
}
close(pb)

# 3.2 Alles samenvoegen
message("   -> Jaren samenvoegen tot één tijdreeks...")
dt_catchment_daily <- rbindlist(results_list)

# Sorteer en zet key
setkey(dt_catchment_daily, meetplaats, day)

message("Tijdreeksen berekend zonder geheugencrash.")

# ==============================================================================
# 4. BEREKENEN VARIABELEN VOOR DE SAMPLES
# ==============================================================================
message("Stap 4: Variabelen berekenen...")

samples_list <- samples_geo %>%
  st_drop_geometry() %>%
  select(meetplaats, monsternamedatum) %>%
  distinct()

# Functie update: Nu met P en E beschikbaar
calculate_vars <- function(meetplaats_in, datum_in) {

  d_sub <- dt_catchment_daily[meetplaats == meetplaats_in]
  date_sample <- as.IDate(datum_in)

  # Periode grenzen
  date_start_7d  <- date_sample - 7
  date_start_3m  <- date_sample - 90  # Ca 3 maanden voor extremen

  # 1. P_Sum_7d (Acute run-off)
  p_7d <- d_sub[day >= date_start_7d & day < date_sample, sum(P_mean, na.rm = TRUE)]

  # Hier kunnen we later SPEI en Extremen aan toevoegen
  # Voor nu doen we even enkel de P_Sum_7d en P_Sum_1y als test

  return(list(p_sum_7d = p_7d))
}

# Uitvoeren
resultaten <- samples_list %>%
  mutate(vars = map2(meetplaats, monsternamedatum, calculate_vars, .progress = TRUE)) %>%
  unnest_wider(vars)

# ==============================================================================
# 5. OPSLAAN & CHECK
# ==============================================================================

# We slaan de dagelijkse reeksen ook op!
# Dit is handig voor de volgende stap (SPEI berekenen met een ander package)
saveRDS(dt_catchment_daily, here("data", "verwerkt", "hydrologisch", "dagelijkse_reeksen_per_catchment.rds"))

final_sf <- resultaten %>%
  left_join(afstroomgebieden, by = "meetplaats") %>%
  st_as_sf()

outfile <- here("data", "verwerkt", "hydrologisch", "neerslag_variabelen_watersheds.gpkg")
st_write(final_sf, outfile, delete_dsn = TRUE)

message("Klaar! Ook de tussenstap 'dagelijkse_reeksen_per_catchment.rds' is opgeslagen.")

final_sf <- read_sf(outfile)

# ==============================================================================
# STAP 6 & 7: SPEI EN EXTREME NEERSLAG BEREKENEN
# ==============================================================================
library(SPEI)
library(tidyverse)
library(data.table)
library(lubridate)
library(here)

# 1. DATA LADEN (Als ze niet meer in het geheugen zitten)
# ------------------------------------------------------------------------------
# Laad de dagelijkse reeksen die we in de vorige stap maakten
if (!exists("dt_catchment_daily")) {
  dt_catchment_daily <- readRDS(here("data", "verwerkt", "hydrologisch", "dagelijkse_reeksen_per_catchment.rds"))
}

# Laad de meetpunten (samples) opnieuw als nodig
if (!exists("samples_geo")) {
  # (Zorg dat je dit object hebt uit het vorige script of laad het opnieuw)
  # Hier een placeholder om te zorgen dat de code werkt:
  # samples_geo <- st_read(...)
  message("Zorg dat 'samples_geo' geladen is!")
}

# ==============================================================================
# DEEL A: SPEI-6 BEREKENING (DEFINITIEVE VERSIE)
# ==============================================================================
library(SPEI)
library(data.table)
library(tidyverse)

message("Start berekening SPEI-6...")

# 1. Aggregeren van Dag -> Maand
# We zorgen eerst dat we zeker weten dat we met jaartallen werken
dt_catchment_daily[, year := year(day)]
dt_catchment_daily[, month := month(day)]

dt_monthly <- dt_catchment_daily[, .(
  P_month = sum(P_mean, na.rm = TRUE), # gebiedsgemiddelde dagelijkse neerslag en ET
  E_month = sum(E_mean, na.rm = TRUE)
), by = .(meetplaats, year, month)]

# 2. HET SKELET MAKEN (Cruciaal tegen gaten)
# We dwingen de reeks om te starten in 1991 en door te lopen tot het einde
min_yr_global <- 1991
max_yr_global <- max(dt_monthly$year)

# Maak alle mogelijke combinaties (Meetplaats x Jaar x Maand)
all_combinations <- CJ(
  meetplaats = unique(dt_monthly$meetplaats),
  year = min_yr_global:max_yr_global,
  month = 1:12
)

# Merge data aan skelet
dt_monthly_full <- merge(all_combinations, dt_monthly,
                         by = c("meetplaats", "year", "month"),
                         all.x = TRUE)

# 3. NA's VULLEN
# Omdat we weten dat je dag-data geen NA's heeft, betekent een NA hier
# dat er gewoon die maand geen records waren in de daily set.
# We vullen dit met 0 (of een heel klein getal).
dt_monthly_full[is.na(P_month), P_month := 0]
dt_monthly_full[is.na(E_month), E_month := 0]

# 4. SORTEREN (Heel belangrijk voor TS objecten!)
setorder(dt_monthly_full, meetplaats, year, month)

# ------------------------------------------------------------------------------
# De Rekenfunctie (Met checks)
# ------------------------------------------------------------------------------
calc_spei_catchment <- function(df_sub) {

  # A. Check lengte
  # We verwachten (2025 - 1991 + 1) * 12 maanden = 420 maanden
  expected_rows <- (max_yr_global - min_yr_global + 1) * 12

  if(nrow(df_sub) < expected_rows) {
    # Als dit gebeurt, is de merge mislukt
    message(paste("LET OP: Te weinig data voor", unique(df_sub$meetplaats)))
    df_sub$spei6 <- NA
    return(df_sub)
  }

  # B. Waterbalans
  balance <- df_sub$P_month - df_sub$E_month

  # C. Maak Time Series
  # We weten zeker dat de data gesorteerd is en start bij min_yr_global, maand 1
  ts_balance <- ts(balance, start = c(min_yr_global, 1), frequency = 12)

  # D. SPEI Berekenen
  tryCatch({
    # We gebruiken de data van 1991-2020 als referentie voor de verdeling
    spei_obj <- spei(ts_balance, scale = 6,
                     ref.start = c(1991, 1), ref.end = c(2020, 12),
                     verbose = FALSE)

    # We halen de waarden eruit als numerieke vector
    vals <- as.numeric(spei_obj$fitted)
    df_sub$spei6 <- vals

  }, error = function(e) {
    message(paste("SPEI Error bij", unique(df_sub$meetplaats), ":", e$message))
    df_sub$spei6 <- NA
  })

  return(df_sub)
}

# 5. UITVOEREN (syntax data.table)
message("   -> SPEI berekenen (even geduld)...")
dt_monthly_spei <- dt_monthly_full[, calc_spei_catchment(.SD), by = meetplaats]

# 6. Datum kolom toevoegen
dt_monthly_spei[, month_date := as.Date(paste(year, month, "01", sep="-"))]

# 7. VALIDATIE RESULTAAT
valid_rows <- sum(!is.na(dt_monthly_spei$spei6))
total_rows <- nrow(dt_monthly_spei)
message(sprintf("Klaar! %d rijen berekend. %d rijen hebben een SPEI waarde (%.1f%%).",
                total_rows, valid_rows, (valid_rows/total_rows)*100))

# Toon de eerste paar niet-NA resultaten
print(head(dt_monthly_spei[!is.na(spei6)]))
message("SPEI-6 berekend.")

# ==============================================================================
# DEEL B: EXTREMEN (Thresholds & Telling)
# ==============================================================================
message("Start berekening extremen (P95)...")

# 1. Bepaal de Drempelwaarde (P95) per afstroomgebied
# DEFINITIE: 95e percentiel van "natte dagen" (>= 1mm) in de referentieperiode (1991-2020).
# Waarom natte dagen? Als we 0 meenemen, is P95 in droge gebieden soms maar 3mm (niet extreem).

# Filter referentieperiode
dt_ref <- dt_catchment_daily[year(day) >= 1991 & year(day) <= 2020]

# Bereken P95 per meetplaats (alleen dagen met >= 1mm regen)
thresholds <- dt_ref[P_mean >= 1, .(
  p95_threshold = quantile(P_mean, 0.95, na.rm = TRUE)
), by = meetplaats]

message("Drempelwaarden bepaald. Voorbeeld: Meetplaats ", thresholds$meetplaats[1],
        " heeft extreme grens > ", round(thresholds$p95_threshold[1], 1), " mm.")

# ==============================================================================
# DEEL C: KOPPELEN AAN SAMPLES (TURBO VERSIE - VECTORIZED)
# ==============================================================================
library(data.table)

samples_df <- samples_geo %>%
  st_drop_geometry() %>%
  distinct(meetplaats, monsternamedatum) %>%
  select(meetplaats, monsternamedatum)

message("Start razendsnelle koppeling...")

# 1. Voorbereiden data.tables
# Zorg dat alles data.tables zijn
setDT(samples_df)
setDT(dt_catchment_daily)
setDT(thresholds)
setDT(dt_monthly_spei)

# Zorg dat datums Date objecten zijn
samples_df[, monsternamedatum := as.Date(monsternamedatum)]
dt_catchment_daily[, day := as.Date(day)]

# ------------------------------------------------------------------------------
# STAP 1: BEREKEN SPEI (Gewone Join)
# ------------------------------------------------------------------------------
# We maken join-sleutels (jaar en maand)
samples_df[, `:=`(year = year(monsternamedatum), month = month(monsternamedatum))]

# Join SPEI aan de samples
# We koppelen samples aan de maand-tabel op meetplaats+jaar+maand
results_spei <- merge(
  samples_df,
  dt_monthly_spei[, .(meetplaats, year, month, spei6)], # Pak alleen wat nodig is
  by = c("meetplaats", "year", "month"),
  all.x = TRUE
)

# ------------------------------------------------------------------------------
# STAP 2: BEREKEN EXTREMEN (Non-Equi Join)
# ------------------------------------------------------------------------------
message("   -> Extremen tellen via Non-Equi Join...")

# A. Markeer eerst welke dagen extreem waren in de grote tabel
# We koppelen de threshold aan de dagdata
dt_daily_flagged <- merge(dt_catchment_daily, thresholds, by = "meetplaats")
# Maak een boolean kolom (1 = extreem, 0 = normaal)
dt_daily_flagged[, is_extreme := as.integer(P_mean > p95_threshold)]

# B. Definieer de zoekperiode per sample
# Startdatum is 90 dagen voor de monstername
samples_df[, start_window := monsternamedatum - 90]

# C. De Magische Non-Equi Join
# Dit zegt: "Voor elke sample, pak de rijen uit daily_flagged waar:
# 1. meetplaats gelijk is
# 2. de dag >= start_window
# 3. de dag < monsternamedatum"
# En tel vervolgens de 'is_extreme' kolom op.

extremes_counted <- dt_daily_flagged[samples_df,
                                     .(n_extreme_3m = sum(is_extreme, na.rm = TRUE)),
                                     on = .(meetplaats,
                                            day >= start_window,
                                            day < monsternamedatum),
                                     by = .EACHI] # .EACHI zorgt dat het per sample gebeurt

# ------------------------------------------------------------------------------
# STAP 3: ALLES SAMENVOEGEN
# ------------------------------------------------------------------------------

# De output van de non-equi join heeft dezelfde volgorde als samples_df
# We plakken het aan elkaar
results_final <- results_spei
results_final[, n_extreme_3m := extremes_counted$n_extreme_3m]

# Opruimen hulpkolommen
results_final[, c("year", "month", "start_window") := NULL]

message("Klaar! Check het resultaat:")
print(head(results_final))

# ==============================================================================
# DEEL D: OPSLAAN (Hetzelfde als voorheen)
# ==============================================================================
file_prev <- here("data", "verwerkt", "neerslag_variabelen_watersheds.gpkg")
prev_sf <- st_read(file_prev, quiet = TRUE) %>%
  mutate(meetplaats = as.character(meetplaats),
         monsternamedatum = as.Date(monsternamedatum))

# Zorg dat results_final ook character/date is voor de join
results_final[, meetplaats := as.character(meetplaats)]

final_dataset <- prev_sf %>%
  left_join(results_final, by = c("meetplaats", "monsternamedatum"))

outfile <- here("data", "verwerkt", "mi_metingen_met_klimaat_compleet.gpkg")
st_write(final_dataset, outfile, delete_dsn = TRUE)
message("✅ Opgeslagen: ", outfile)

hydro_data <- final_dataset %>%
  st_drop_geometry()
save(hydro_data, file = here("data", "verwerkt", "hydro_data.rdata"))

# ==============================================================================
# DEEL F: CHECK
# ==============================================================================

library(tidyverse)

# 1. Check op Oneindige waarden of NaN
n_inf <- sum(is.infinite(final_dataset$spei6))
n_nan <- sum(is.nan(final_dataset$spei6))
n_na  <- sum(is.na(final_dataset$spei6))

print(sprintf("Aantal Inf: %d | Aantal NaN: %d | Aantal NA: %d", n_inf, n_nan, n_na))

# 2. Bekijk de extremen
summary(final_dataset$spei6)

# 3. Hoeveel procent valt buiten het 'normale' bereik (-3 tot 3)?
buiten_bereik <- final_dataset %>%
  filter(spei6 < -3 | spei6 > 3) %>%
  nrow()

totaal <- nrow(final_dataset)
print(sprintf("Aantal extreme waarden (>3 of <-3): %d (%.2f%%)", buiten_bereik, (buiten_bereik/totaal)*100))

ggplot(final_dataset, aes(x = spei6)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  geom_vline(xintercept = c(-2, 0, 2), linetype = "dashed", color = "red") +
  labs(title = "Verdeling van berekende SPEI waarden",
       subtitle = "Zou eruit moeten zien als een 'Bell Curve' rond 0",
       x = "SPEI Waarde", y = "Aantal Samples") +
  theme_minimal()

# Bereken gemiddelde SPEI over alle meetpunten per maand
tijdreeks_check <- final_dataset %>%
  group_by(monsternamedatum) %>%
  summarise(gem_spei = mean(spei6, na.rm = TRUE)) %>%
  ungroup()

ggplot(tijdreeks_check, aes(x = monsternamedatum, y = gem_spei)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "black") +
  # Markeer 2018 (Droogte)
  annotate("rect", xmin = as.Date("2018-05-01"), xmax = as.Date("2018-10-01"),
           ymin = -3, ymax = 3, alpha = 0.2, fill = "red") +
  annotate("text", x = as.Date("2018-07-01"), y = -2.5, label = "Droogte 2018", color = "red") +
  labs(title = "Gemiddeld SPEI verloop in Vlaanderen (in jouw dataset)",
       y = "Gemiddelde SPEI") +
  theme_minimal()
