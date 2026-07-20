if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

conflicted::conflicts_prefer(lubridate::year)

# --- 1. Instellingen en Data Inladen ---

# Parameters
straal <- 100 # Straal in meters
jaren <- 2008:2023
crs_referentie <- 31370

# Inlezen scores
lbg_intensiteit_scores <- read_xlsx(here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", "landbouwgebruikspercelen_intensiteitsscores.xlsx")) %>%
  mutate(combo = (gewasbescherming + nitraatresidues)/2)

# --- 1A. Inlezen Macroinvertebraten (MI) ---
mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = TRUE) %>%
  filter(monsternamedatum > "2009-12-31") %>%
  mutate(
    jaar = lubridate::year(monsternamedatum),
    meetnet = "mi" # Label toevoegen
  )
mi_meetpunten_trans <- st_transform(mi_meetpunten, crs_referentie)

oeverzones_mi <- st_read(here("data", "ruw", "landgebruik", "oevers", "punten_buffer_mi.shp"), quiet = TRUE) %>%
  select(meetplaats) %>%
  mutate(
    oppervlakte_oeverzone = st_area(st_geometry(.)),
    meetnet = "mi"
  ) %>%
  st_transform(crs_referentie)

# --- 1B. Inlezen Macrofyten (MAFY) ---
mafy_meetpunten <- st_read(here("data", "ruw", "macrofyten", "mafy_meetpunten_datum.gpkg"), quiet = TRUE) %>%
  filter(monsternamedatum > "2009-12-31") %>%
  mutate(
    jaar = lubridate::year(monsternamedatum),
    meetnet = "mafy" # Label toevoegen
  )
mafy_meetpunten_trans <- st_transform(mafy_meetpunten, crs_referentie)

oeverzones_mafy <- st_read(here("data", "ruw", "landgebruik", "oevers", "buffers_mafi_ok.shp"), quiet = TRUE) %>%
  select(meetplaats) %>%
  mutate(
    oppervlakte_oeverzone = st_area(st_geometry(.)),
    meetnet = "mafy"
  ) %>%
  st_transform(crs_referentie)

# --- 2. Data Samenvoegen ---
# Koppel datums aan oevers per meetnet
oeverzones_mi_sf <- oeverzones_mi %>%
  inner_join(mi_meetpunten_trans %>% st_drop_geometry(), by = c("meetplaats", "meetnet"))

oeverzones_mafy_sf <- oeverzones_mafy %>%
  inner_join(mafy_meetpunten_trans %>% st_drop_geometry(), by = c("meetplaats", "meetnet"))

# Alles op één hoop gooien voor de loop (voor efficiëntie)
alle_oeverzones_sf <- bind_rows(oeverzones_mi_sf, oeverzones_mafy_sf)


# --- 3. Berekening per Jaar (Loop) ---

resultaten_lijst <- list()

for (j in jaren) {

  message(paste0("Bezig met verwerken jaar: ", j))

  # A. Filter oeverzones voor dit specifieke jaar
  oeverzones_jaar <- alle_oeverzones_sf %>%
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

  percelen_sf <- st_read(pad_shp, quiet = TRUE) %>%
    st_transform(crs_referentie) %>%
    select(GEWASGROEP)

  # Bepaal welke percelen de oeverzones van dit jaar raken
  percelen_sf_cropped <- percelen_sf %>%
    st_filter(oeverzones_jaar, .predicate = st_intersects)

  # Koppel scores
  percelen_met_scores <- percelen_sf_cropped %>%
    inner_join(lbg_intensiteit_scores, by = "GEWASGROEP")

  # C. Ruimtelijke Intersectie
  intersectie <- st_intersection(oeverzones_jaar, percelen_met_scores)

  if (nrow(intersectie) == 0) {
    message("  -> Wel oeverzones, maar geen landbouwpercelen gevonden binnen de zone.")
    next
  }

  # D. Berekeningen
  scores_jaar <- intersectie %>%
    mutate(
      oppervlakte_intersectie = st_area(st_geometry(.)),
      fractie_oppervlakte = as.numeric(oppervlakte_intersectie) / as.numeric(oppervlakte_oeverzone)
    ) %>%
    st_drop_geometry() %>%
    # Let op: 'meetnet' toegevoegd aan de group_by!
    group_by(meetnet, meetplaats, monsternamedatum) %>%
    summarise(
      intensiteit_gewasbescherming = sum(fractie_oppervlakte * gewasbescherming, na.rm = TRUE),
      intensiteit_nitraatresidu = sum(fractie_oppervlakte * nitraatresidues, na.rm = TRUE),
      intensiteit_combo = sum(fractie_oppervlakte * combo, na.rm = TRUE),
      percentage_landbouw_in_buffer = sum(fractie_oppervlakte) * 100,
      .groups = 'drop'
    )

  resultaten_lijst[[as.character(j)]] <- scores_jaar
}

# --- 4. Resultaten Samenvoegen en Opslaan ---

intensiteit_landbouw_oeverzones <- bind_rows(resultaten_lijst)

# Oeverzones zonder landbouw terug toevoegen met 0
intensiteit_landbouw_alle_jaren <- alle_oeverzones_sf %>%
  st_drop_geometry() %>%
  # Let op: we joinen nu ook op 'meetnet' om verwarring tussen meetplaatsen met dezelfde naam te voorkomen
  left_join(intensiteit_landbouw_oeverzones, by = c("meetnet", "meetplaats", "monsternamedatum")) %>%
  mutate(
    across(starts_with("intensiteit_"), ~replace_na(., 0)),
    percentage_landbouw_in_buffer = replace_na(percentage_landbouw_in_buffer, 0)
  )

cat("\n✅ Scores Landbouwintensiteit voor oeverzones (MI en MAFY) succesvol berekend.\n")

# Optioneel: Je kan ze nu weer splitsen als je ze liever als losse objecten wegschrijft:
intensiteit_landbouw_oeverzones_mi <- intensiteit_landbouw_alle_jaren %>% filter(meetnet == "mi")
intensiteit_landbouw_oeverzones_mafy <- intensiteit_landbouw_alle_jaren %>% filter(meetnet == "mafy")

# Opslaan
save(intensiteit_landbouw_alle_jaren, file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones_gecombineerd.rdata"))
save(intensiteit_landbouw_oeverzones_mi, file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones_mi.rdata"))
save(intensiteit_landbouw_oeverzones_mafy, file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones_mafy.rdata"))
