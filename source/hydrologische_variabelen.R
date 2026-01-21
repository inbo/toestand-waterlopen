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
# We transformeren de polygonen naar het coördinatenstelsel van het raster (WGS84)
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
# DEEL A: SPEI-6 BEREKENING (ROBUUSTE VERSIE)
# ==============================================================================
message("Start berekening SPEI-6 (Geoptimaliseerd)...")

# 1. Aggregeren van Dag -> Maand
dt_monthly <- dt_catchment_daily[, .(
  P_month = sum(P_mean, na.rm = TRUE),
  E_month = sum(E_mean, na.rm = TRUE)
), by = .(meetplaats, year = year(day), month = month(day))]

# 2. ROBUUSTHEIDS-CHECK: Vul ontbrekende maanden aan
# Als er ergens een maand data mist (bvb door een data-foutje),
# moet die wel bestaan als NA, anders schuift de hele tijdreeks op.

# Maak een "skelet" van alle maanden die er moeten zijn (1991-2025)
# We gaan ervan uit dat je data start in jan 1991.
min_yr <- 1991
max_yr <- max(dt_monthly$year)
all_months <- CJ(
  meetplaats = unique(dt_monthly$meetplaats),
  year = min_yr:max_yr,
  month = 1:12
)

# Koppel de data aan het skelet. Maanden zonder data worden NA (of 0).
# Voor P en E is 0 veiliger dan NA bij ontbrekende records,
# maar voor SPEI mag het NA blijven (dan wordt de output ook NA).
dt_monthly_full <- merge(all_months, dt_monthly,
                         by = c("meetplaats", "year", "month"),
                         all.x = TRUE)

# Sorteer cruciaal op tijd!
setorder(dt_monthly_full, meetplaats, year, month)

# 3. Functie om SPEI te berekenen per meetplaats
calc_spei_catchment <- function(df_sub) {

  # Check: als er te veel NA's zijn, return NA
  # (SPEI heeft een minimum aantal datapunten nodig)
  if (sum(!is.na(df_sub$P_month)) < 360) { # Minstens 30 jaar data nodig voor betrouwbare index
    df_sub$spei6 <- NA
    return(df_sub)
  }

  # Bereken balans
  balance <- df_sub$P_month - df_sub$E_month

  # --- HIER ZAT DE FOUT ---
  # We maken nu expliciet een Time Series object (ts)
  # We vertellen R: "Dit start in 1991, maand 1, en frequentie is 12"
  ts_balance <- ts(balance, start = c(1991, 1), frequency = 12)

  # Nu roepen we spei aan. Omdat ts_balance nu "weet" dat het 1991 is,
  # werkt de ref.start = c(1991, 1) perfect.
  tryCatch({
    spei_obj <- spei(ts_balance, scale = 6, ref.start = c(1991, 1), ref.end = c(2020, 12), verbose = FALSE)
    df_sub$spei6 <- as.numeric(spei_obj$fitted)
  }, error = function(e) {
    # Fallback als berekening faalt (bvb door reeks van 0-en)
    df_sub$spei6 <- NA
  })

  return(df_sub)
}

# 4. Pas toe op elk afstroomgebied
message("   -> SPEI berekenen per meetplaats (dit duurt even)...")
dt_monthly_spei <- dt_monthly_full[, calc_spei_catchment(.SD), by = meetplaats]

# Datum kolom toevoegen voor latere koppeling
dt_monthly_spei[, month_date := as.Date(paste(year, month, "01", sep = "-"))]

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
# DEEL C: KOPPELEN AAN DE SAMPLES
# ==============================================================================
message("Koppelen aan macroinvertebraten metingen...")

# We maken een lijst van samples
samples_df <- samples_geo %>%
  st_drop_geometry() %>%
  distinct(meetplaats, monsternamedatum) %>%
  select(meetplaats, monsternamedatum)

# Functie om alles op te halen voor 1 sample
get_climate_vars <- function(curr_mp, curr_date) {

  curr_date <- as.Date(curr_date)

  # --- 1. SPEI Ophalen ---
  # We pakken de SPEI van de maand waarin de monsterneming viel.
  # (SPEI-6 zegt op dat moment: hoe was de balans in de 6 mnd hiervoor?)
  spei_val <- dt_monthly_spei[
    meetplaats == curr_mp &
      year == year(curr_date) &
      month == month(curr_date)
  ]$spei6

  if (length(spei_val) == 0) spei_val <- NA

  # --- 2. Extremen Tellen ---
  # Periode: 3 maanden (90 dagen) voor de meting
  date_start_3m <- curr_date - 90

  # Haal de threshold op
  thresh <- thresholds[meetplaats == curr_mp]$p95_threshold
  if (length(thresh) == 0) return(list(spei6 = spei_val, n_extreme_3m = NA))

  # Filter dagdata
  # Let op: 'dt_catchment_daily' is een data.table met key, dus dit filtert snel
  subset_rain <- dt_catchment_daily[meetplaats == curr_mp & day >= date_start_3m & day < curr_date]

  # Tel aantal dagen boven de drempel
  n_ext <- sum(subset_rain$P_mean > thresh, na.rm = TRUE)

  return(list(spei6 = spei_val, n_extreme_3m = n_ext))
}

# Uitvoeren (met progress bar)
results_final <- samples_df %>%
  mutate(climate = map2(meetplaats, monsternamedatum, get_climate_vars, .progress = TRUE)) %>%
  unnest_wider(climate)

# ==============================================================================
# DEEL D: SAMENVOEGEN EN OPSLAAN
# ==============================================================================

# Voeg dit toe aan je vorige resultaat (P_sum_7d)
# We laden even het bestand uit de vorige stap
file_prev <- here("data", "verwerkt", "hydrologisch", "neerslag_variabelen_watersheds.gpkg")
if (file.exists(file_prev)) {
  prev_sf <- st_read(file_prev, quiet = TRUE)

  # Joinen
  # Let op: convert datums naar Date om zeker te zijn van match
  final_dataset <- prev_sf %>%
    mutate(monsternamedatum = as.Date(monsternamedatum)) %>%
    left_join(results_final, by = c("meetplaats", "monsternamedatum"))

  # Opslaan
  outfile <- here("data", "verwerkt", "hydrologisch", "mi_metingen_met_klimaat_compleet.gpkg")
  st_write(final_dataset, outfile, delete_dsn = TRUE)

  message("Klaar! Bestand opgeslagen: ", outfile)
  print(head(final_dataset))

} else {
  message("Let op: Het bestand uit de vorige stap is niet gevonden.")
}
