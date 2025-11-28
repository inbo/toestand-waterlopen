source(here::here("source", "inladen_packages.R"))

#inlezen data
mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg")) %>%
  filter(monsternamedatum > "2009-12-31")
afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_5000m.gpkg"))
crs_referentie <- st_crs(afstroomgebieden)
lbg_intensiteit_scores <- read_xlsx(here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", "landbouwgebruikspercelen_intensiteitsscores.xlsx")) %>%
  mutate(combo = (gewasbescherming + nitraatresidues)/2)

# inlezen percelenkaarten
jaren <- c(seq(from = 2008, to = 2023, by = 1))

bestanden <- paste0("Lbgebrperc", jaren, ".shp")
object_namen <- paste0("lbg_", jaren)

walk2(bestanden, object_namen, ~ {
  pad <- here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", .x)

  # 1. Lees het shapefile in
  data_sf <- st_read(pad, quiet = TRUE)

  # 2. Voer de CRS-transformatie uit
  # st_transform() projecteert de data naar de referentie CRS
  data_sf_geprojecteerd <- st_transform(data_sf, crs_referentie)

  # 3. Wijs het geprojecteerde object toe aan de globale omgeving
  assign(.y, data_sf_geprojecteerd, envir = .GlobalEnv)
})

#### unieke waarden GEWASGROEP ####

# Definieer de namen van de objecten die moeten worden gecontroleerd
object_namen <- paste0("lbg_", 2008:2023)

# Filter de namen om er zeker van te zijn dat alleen bestaande sf-objecten worden geselecteerd
bestaande_lbg_objecten <- object_namen[sapply(object_namen, exists)]

unieke_gewasgroepen <- map(bestaande_lbg_objecten, ~ {
  # Haal het object op
  data_sf <- get(.x)

  # Selecteer de GEWASGROEP kolom en retourneer de unieke waarden
  data_sf %>%
    st_drop_geometry() %>% # Optioneel, maar maakt de verwerking sneller
    select(GEWASGROEP, ) %>%
    unique()
})

names(unieke_gewasgroepen) <- bestaande_lbg_objecten

alle_unieke_gewasgroepen <- unlist(unieke_gewasgroepen) %>%
  unique() %>%
  sort()

print(alle_unieke_gewasgroepen)

#### berekening #####

# --- 2. Tijdsvoorbereiding ---

# Koppel de afstroomgebieden aan de meetpunten
# Dit dataframe zal de basis vormen voor de lus
data_basis <- mi_meetpunten %>%
  # Selecteer de benodigde kolommen om te koppelen
  st_drop_geometry() %>%
  select(meetplaats, monsternamedatum) %>%

  # Voeg het jaar toe voor latere matching met de perceelkaarten
  mutate(jaar = year(monsternamedatum)) %>%

  # Koppel de geometrie van het afstroomgebied toe (Meetplaats is de sleutel)
  left_join(afstroomgebieden %>% select(meetplaats, oppervlakte), by = "meetplaats") %>%
  # Zorg ervoor dat het een sf-object is voor de ruimtelijke bewerking
  st_as_sf()

# R-code om uit te voeren
# Definieer de kolomnamen voor de scores die we willen berekenen
score_kolommen <- c("intensiteit_gewasbescherming", "intensiteit_nitraatresidu")

# Maak een lege lijst om de jaarlijkse resultaten op te slaan
resultaten_lijst <- list()

# Start de lus over de jaren die we hebben ingelezen (2008 t/m 2023)
for (j in jaren) {

  lbg_naam <- paste0("lbg_", j)

  # Controleer of de perceelkaart voor dit jaar bestaat in de omgeving
  if (!exists(lbg_naam)) {
    cat(paste0("Let op: Percelenkaart ", lbg_naam, " niet gevonden. Jaar ", j, " overgeslagen.\n"))
    next
  }

  # Haal de percelenkaart voor dit jaar op
  percelen_sf <- get(lbg_naam) %>%
    # Koppel de intensiteitsscores voor dit jaar (alleen waar een score bestaat)
    inner_join(lbg_intensiteit_scores, by = "GEWASGROEP")

  # --- Filter de basisdata voor het huidige jaar ---
  data_jaar <- data_basis %>%
    filter(jaar == j)

  if (nrow(data_jaar) == 0) {
    cat(paste0("Geen meetpunten in afstroomgebieden gevonden voor jaar ", j, ". Overgeslagen.\n"))
    next
  }

  # --- 1. Ruimtelijke Intersectie ---

  # Bereken de doorsnede tussen de afstroomgebieden en de landbouwpercelen
  # Dit creëert nieuwe polygonen op de intersectiepunten
  intersectie_resultaat <- st_intersection(data_jaar, percelen_sf)

  # --- 2. Bepaal de Intersectie-Oppervlakte ---

  intersectie_resultaat <- intersectie_resultaat %>%
    mutate(
      oppervlakte_intersectie = st_area(geom),

      # Bereken het aandeel van elk perceel binnen het afstroomgebied
      # N.B. Gebruik de oorspronkelijke oppervlakte van het afstroomgebied (`oppervlakte`)
      fractie_oppervlakte = as.numeric(oppervlakte_intersectie) / as.numeric(oppervlakte)
    )

  # --- 3. Bereken de Oppervlakte-Gewogen Score ---

  scores_jaar <- intersectie_resultaat %>%
    st_drop_geometry() %>%
    group_by(meetplaats, monsternamedatum) %>%
    summarise(
      # Oppervlakte-gewogen score voor Gewasbescherming: som(fractie * score)
      intensiteit_gewasbescherming = sum(fractie_oppervlakte * gewasbescherming, na.rm = TRUE),

      # Oppervlakte-gewogen score voor Nitraatresidu: som(fractie * score)
      intensiteit_nitraatresidu = sum(fractie_oppervlakte * nitraatresidues, na.rm = TRUE),

      # Oppervlakte-gewogen score voor Nitraatresidu: som(fractie * score)
      intensiteit_combo = sum(fractie_oppervlakte * combo, na.rm = TRUE),
      .groups = 'drop'
    )

  # Voeg de resultaten toe aan de lijst
  resultaten_lijst[[as.character(j)]] <- scores_jaar
}

# --- 3. Combineer de Resultaten ---

# Combineer alle jaarlijkse dataframes tot één definitief dataframe
intensiteit_landbouw_scores <- bind_rows(resultaten_lijst)

cat("\n✅ Scores Landbouwintensiteit succesvol berekend en gecombineerd.\n")
cat("Structuur van de definitieve dataset (data_compleet_scores):\n")
print(head(data_compleet_scores))

save(intensiteit_landbouw_scores, file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores.rdata"))

