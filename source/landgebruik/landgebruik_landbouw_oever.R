if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

conflicted::conflicts_prefer(lubridate::year)
# --- 1. Instellingen en Data Inladen ---

# Parameters
straal <- 100 # Straal in meters
jaren <- 2008:2023

# Inlezen meetpunten
mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = TRUE) %>%
  filter(monsternamedatum > "2009-12-31") %>%
  mutate(jaar = lubridate::year(monsternamedatum)) # Jaar kolom toevoegen voor de koppeling

# Inlezen scores
lbg_intensiteit_scores <- read_xlsx(here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", "landbouwgebruikspercelen_intensiteitsscores.xlsx")) %>%
  mutate(combo = (gewasbescherming + nitraatresidues)/2)


# Zorg dat de meetpunten in het juiste CRS staan (Lambert 72) voor afstanden in meters
# We nemen aan dat de percelenkaarten ook in 31370 staan (standaard voor Vlaanderen)
crs_referentie <- 31370
mi_meetpunten_trans <- st_transform(mi_meetpunten, crs_referentie)


# We berekenen ook meteen de oppervlakte van de oeverzone voor de fractie-berekening later

oeverzones0 <- st_read(here("data", "ruw", "landgebruik", "oevers", "punten_buffer_mi.shp")) %>%
  select(meetplaats) %>%
  mutate(oppervlakte_oeverzone = st_area(st_geometry(.)))

oeverzones <- st_transform(oeverzones0, crs_referentie)
oeverzones_sf <- oeverzones %>%
  # Koppel de data van de meetpunten aan de oeverzones
  inner_join(mi_meetpunten_trans %>% st_drop_geometry(),
             by = "meetplaats")

# --- 3. Berekening per Jaar (Loop) ---

resultaten_lijst <- list()

for (j in jaren) {

  message(paste0("Bezig met verwerken jaar: ", j))

  # A. Filter oeverzones voor dit specifieke jaar
  # Dit versnelt de intersectie enorm omdat we niet zoeken naar punten uit andere jaren
  oeverzones_jaar <- oeverzones_sf %>%
    filter(jaar == j)

  if (nrow(oeverzones_jaar) == 0) {
    message(paste0("  -> Geen meetpunten gevonden voor jaar ", j, ". Sla over."))
    next
  }

  # B. Lees de percelenkaart voor dit jaar (Load on demand)
  pad_shp <- here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", paste0("Lbgebrperc", j, ".shp"))

  if (!file.exists(pad_shp)) {
    message(paste0("  -> Bestand niet gevonden: ", pad_shp))
    next
  }

  # Lees shapefile en transformeer direct naar CRS indien nodig
  # We selecteren alleen de geometrie en GEWASGROEP om geheugen te sparen
  percelen_sf <- st_read(pad_shp, quiet = TRUE) %>%
    st_transform(crs_referentie) %>%
    select(GEWASGROEP)

  # # oude code
  # # We houden alleen percelen over die daadwerkelijk een buffer raken
  # # Dit voorkomt dat we heel Vlaanderen intersecten met een paar puntjes
  # percelen_sf_cropped <- percelen_sf[st_intersects(percelen_sf, st_union(oeverzones_jaar), sparse = FALSE)[,1], ]

  # Bepaal welke percelen de oeverzones van dit jaar raken -> snellere code
  percelen_sf_cropped <- percelen_sf %>%
    st_filter(oeverzones_jaar, .predicate = st_intersects)


  # Koppel scores
  percelen_met_scores <- percelen_sf_cropped %>%
    inner_join(lbg_intensiteit_scores, by = "GEWASGROEP")

  # C. Ruimtelijke Intersectie
  # Bereken de overlap tussen oeverzones en percelen
  intersectie <- st_intersection(oeverzones_jaar, percelen_met_scores)

  if (nrow(intersectie) == 0) {
    message("  -> Wel oeverzones, maar geen landbouwpercelen gevonden binnen de zone.")
    next
  }

  # D. Berekeningen
  scores_jaar <- intersectie %>%
    mutate(
      oppervlakte_intersectie = st_area(st_geometry(.)),
      # Fractie t.o.v. de BUFFER (niet t.o.v. het perceel of afstroomgebied)
      fractie_oppervlakte = as.numeric(oppervlakte_intersectie) / as.numeric(oppervlakte_oeverzone)
    ) %>%
    st_drop_geometry() %>%
    group_by(meetplaats, monsternamedatum) %>%
    summarise(
      # Sommeer de scores gewogen naar oppervlakte binnen de buffer
      intensiteit_gewasbescherming = sum(fractie_oppervlakte * gewasbescherming, na.rm = TRUE),
      intensiteit_nitraatresidu = sum(fractie_oppervlakte * nitraatresidues, na.rm = TRUE),
      intensiteit_combo = sum(fractie_oppervlakte * combo, na.rm = TRUE),

      # Optioneel: hoeveel % van de buffer is daadwerkelijk landbouw?
      percentage_landbouw_in_buffer = sum(fractie_oppervlakte) * 100,
      .groups = 'drop'
    )

  resultaten_lijst[[as.character(j)]] <- scores_jaar
}

# --- 4. Resultaten Samenvoegen en Opslaan ---

intensiteit_landbouw_oeverzones <- bind_rows(resultaten_lijst)

# Omdat oeverzones die GEEN landbouw bevatten niet in de intersectie voorkomen,
# missen die nu in de resultaten. We voegen die terug toe met waarde 0.
intensiteit_landbouw_oeverzones_jaren <- oeverzones_sf %>%
  st_drop_geometry() %>%
  left_join(intensiteit_landbouw_oeverzones, by = c("meetplaats", "monsternamedatum")) %>%
  mutate(
    across(starts_with("intensiteit_"), ~replace_na(., 0)),
    percentage_landbouw_in_buffer = replace_na(percentage_landbouw_in_buffer, 0)
  )

cat("\n✅ Scores Landbouwintensiteit voor oeverzones succesvol berekend.\n")
print(head(intensiteit_landbouw_oeverzones_jaren))

# Opslaan met dynamische naam o.b.v. straal
bestandsnaam <- paste0("intensiteit_landbouw_scores_oeverzones.rdata")
save(intensiteit_landbouw_oeverzones_jaren, file = here("data", "verwerkt", "landgebruik", bestandsnaam))
