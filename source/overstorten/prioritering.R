
################################################################################
# Prioriteitsscore koppelen aan overstorten VHA uitlaat (blootstelling x waarschijnlijkheid)
################################################################################

prioritering <- read_excel("data/ruw/overstorten/prioritering/prioritering.xlsx")

overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp")) # overstorten die uitlaten op waterloop

a0_afstroom_prio_score <- st_read("data/ruw/waterlopen/Oppervlaktewaterlichamen_2022-2027_GewVLA_Shapefile/Shapefile/AfstrZonA0.shp", quiet = T) %>%
  left_join(prioritering %>%
              select(waterlichaam_code, Blootstellingsfactor, Waarschijnlijkheidsfactor),
            by = c("A0CODE" = "waterlichaam_code")) %>%
  st_transform(crs = st_crs(vuilvracht_overstorten))


crs(a0_afstroom_prio_score) == crs(overstorten_uitlaat_vha)

overstorten_uitlaat_vha_prio_score <- st_join(
  x = overstorten_uitlaat_vha,
  y = a0_afstroom_prio_score %>% select(Blootstellingsfactor, Waarschijnlijkheidsfactor), # Selecteer alleen de kolom die je nodig hebt
  join = st_intersects  # Dit is de standaardwaarde (punt in polygoon)
) %>%
  mutate(overstort_risk_score = Blootstellingsfactor * Waarschijnlijkheidsfactor)

st_write(overstorten_uitlaat_vha_prio_score, here("data", "verwerkt", "overstorten", "overstorten_uitlaat_vha_prio_score.gpkg"), delete_dsn = T)

#################################################
# Correlatie van vuilvrachten overstorten met prio_score
########################################################

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

sum_ie_a0 <- vuilvracht_overstorten %>%
  st_drop_geometry() %>%
  group_by(A0CODE) %>%
  summarise(totaal_IE_A0 = sum(IE))
relatie_factor_IE <- prioritering %>%
  inner_join(sum_ie_a0,
             by = c("waterlichaam_code" = "A0CODE")) %>%
  mutate(overstort_risk_score = Blootstellingsfactor * Waarschijnlijkheidsfactor)
relatie_factor_IE %>%
  ggplot(aes(Blootstellingsfactor, log10(totaal_IE_A0))) +
  geom_point() +
  geom_smooth(method = "lm")

relatie_factor_IE %>%
  ggplot(aes(overstort_risk_score, log10(totaal_IE_A0))) +
  geom_point() +
  geom_smooth(method = "lm")

relatie_factor_IE %>%
  ggplot(aes(log10(totaal_IE_A0), overstort_risk_score)) +
  geom_point() +
  geom_smooth(method = "lm")

m1 <- lm(data = relatie_factor_IE %>%
           filter(totaal_IE_A0 > 0),
         overstort_risk_score ~ log10(totaal_IE_A0))
summary(m1)
plot_model(m1, type = "pred", show.data = T)

relatie_factor_IE %>%
  filter(totaal_IE_A0 < 10000) %>%
  ggplot(aes(totaal_IE_A0, overstort_risk_score)) +
  geom_point() +
  geom_smooth(method = "lm")

m2 <- lm(data = relatie_factor_IE %>%
           filter(totaal_IE_A0 > 0) %>%
           filter(totaal_IE_A0 < 10000),
         overstort_risk_score ~ totaal_IE_A0)
summary(m2)
plot_model(m2, type = "pred", show.data = T)
