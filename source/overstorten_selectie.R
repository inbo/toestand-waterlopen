source(here::here("source", "inladen_packages.R"))

locations <- read_sf(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))
afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))
bedekking <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "bedekking", "ZVG_OS_status_2021.shp")) %>%
  mutate(bedekking = case_when(
    Model_stat %in% c("2", "3", "4") ~ 1,
    TRUE ~ 0
  ))
vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))
dtm_hydro_breached <- rast(here("data", "ruw", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"))

# eerst de afstroomgebieden die voor meer dan 20% buiten de bedekkinglaag vallen wegdoen

# 2. Bepaal de totale extent van de bedekkingslaag
# Combineer alle bedekkingspolygonen tot één groot polygoon
bedekking_totaal <- st_union(bedekking)

# 3. Bereken de intersectie tussen de afstroomgebieden en de totale bedekkingslaag
# Dit resulteert in een nieuw object met de overlappende delen
intersectie_totaal <- st_intersection(afstroomgebieden, bedekking_totaal)

# 4. Bereken de oppervlakte van de overlappende delen
intersectie_totaal <- intersectie_totaal %>%
  mutate(overlappende_opp = st_area(.))

# # 5. Voeg de overlappende oppervlakte toe aan de oorspronkelijke afstroomgebieden
# # Gebruik st_join() om de data te combineren
# afstroomgebieden_met_overlap <- st_join(afstroomgebieden, intersectie_totaal) %>%
#   # Zorg ervoor dat de geometrie van het oorspronkelijke afstroomgebied behouden blijft
#   select(meetplaats, oorspronkelijke_opp, overlappende_opp, geometry)

# 6. Bereken de overlapverhouding en filter de data
resultaat_Vlaanderen <- intersectie_totaal %>%
  # Verwijder eenheden van de oppervlakte voor de berekening
  mutate(
    oorspronkelijke_opp = oppervlakte,
    overlappende_opp = units::drop_units(overlappende_opp),
    verhouding_Vlaanderen = overlappende_opp / oorspronkelijke_opp
  ) %>%
  # Filter de meetplaatsen die voor meer dan 80% in Vlaanderen liggen
  filter(verhouding_Vlaanderen >= 0.8) %>%
  # Selecteer de gewenste kolommen
  select(meetplaats, verhouding_Vlaanderen, geom)

# Het object 'resultaat_Vlaanderen' bevat nu de afstroomgebieden die
# voldoen aan het criterium.

###################
# 2
# afstroomgebieden weglaten die meer dan 10% in ongemodelleerde zones liggen

# Bereken de oppervlakte van alle polygonen in de 'bedekking'-laag
bedekking_met_opp <- bedekking %>%
  mutate(opp_bedekking = st_area(.))

# Snijd de lagen met elkaar om overlappende polygonen te krijgen
intersectie <- st_intersection(afstroomgebieden, bedekking_met_opp)

# Bereken de oppervlakte van de overlappende delen
intersectie_met_opp <- intersectie %>%
  mutate(opp_intersectie = st_area(.))

# Groepeer de data per meetplaats en som de oppervlakte van de zones met bedekking == 0
resultaat <- intersectie_met_opp %>%
  group_by(meetplaats) %>%
  summarise(
    totale_opp_afstroom = sum(opp_intersectie),
    opp_bedekking_nul = sum(opp_intersectie[bedekking == 0])
  ) %>%
  ungroup()

# Bereken de verhouding van de oppervlakte met bedekking == 0
resultaat <- resultaat %>%
  mutate(verhouding_nul = opp_bedekking_nul / totale_opp_afstroom) %>%
  mutate(verhouding_nul = units::drop_units(verhouding_nul))

# Filter de meetplaatsen die voldoen aan de voorwaarde
gefilterde_meetplaatsen <- resultaat %>%
  filter(verhouding_nul <= 0.1) %>%
  st_drop_geometry()

gefilterde_meetplaatsen_vector <- gefilterde_meetplaatsen %>%
  pull(meetplaats)

gefilterde_meetplaatsen_afstroomgebieden <- afstroomgebieden %>%
  filter(meetplaats %in% gefilterde_meetplaatsen_vector)

# Enkel de meetpunten met afstroomgebieden 80% of meer binnen Vlaanderen en 90% of meer binnen gebieden waar overstortdata voor is.

gefilterde_meetplaatsen_overstorten <- inner_join(gefilterde_meetplaatsen_afstroomgebieden %>% select(meetplaats) %>% st_drop_geometry(),
           resultaat_Vlaanderen %>% select(meetplaats) %>% st_drop_geometry())

test <- afstroomgebieden %>%
  filter(meetplaats %in% gefilterde_meetplaatsen_overstorten$meetplaats)

mapview(test)

# Bekijk de ID's van de gefilterde meetplaatsen
print(gefilterde_meetplaatsen_afstroomgebieden)

mapview(gefilterde_meetplaatsen_afstroomgebieden) + mapview(bedekking, zcol = "bedekking") +
  mapview(dtm_hydro_breached) # wel nog afstroomgebieden die buiten vlaanderen vallen doordat dtm buffer rond vlaanderen heeft.

landuse_raster_2022 <- rast(here("data", "ruw", "landgebruik", "niveau1_vla_2022_v3.tif"))
bedekking_reproj <- st_transform(bedekking, crs = crs(landuse_raster_2022))

mapview(gefilterde_meetplaatsen_afstroomgebieden) + mapview(bedekking_reproj, zcol = "bedekking")  # wel nog afstroomgebieden die buiten vlaanderen vallen doordat dtm buffer rond vlaanderen heeft.

mapview(bedekking_reproj) + mapview(landuse_raster_2022)
