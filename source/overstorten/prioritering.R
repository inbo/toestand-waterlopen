prioritering <- read_excel("data/ruw/overstorten/prioritering/prioritering.xlsx") %>%
  mutate(
    owl = str_replace(waterlichaam_code, "^(A0_G_|A0_)", ""),
    a0_afstroom =
  )

test <- fc_lu_data_clean %>%
  left_join(prioritering,
            by = "owl") %>%
  filter(!is.na(Blootstellingsfactor))


model <- glmmTMB(
  mt_sw_prop ~ aantal_overstorten_500m * Blootstellingsfactor + (1 | meetplaats),
  family =  ordbeta,
  data = test)
summary(model)

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

sum_ie_a0 <- vuilvracht_overstorten %>% st_drop_geometry() %>% group_by(A0CODE) %>% summarise(totaal_IE_A0 = sum(IE))
relatie_factor_IE <- prioritering %>%
  inner_join(sum_ie_a0,
             by = c("waterlichaam_code" = "A0CODE"))
relatie_factor_IE %>%
  ggplot(aes(Blootstellingsfactor, totaal_IE_A0)) +
  geom_point() +
  geom_smooth(method = "lm")

# per meetpunt ipv grouping afstroom

individ_ie_a0 <- vuilvracht_overstorten %>% st_drop_geometry()
relatie_factor_IE <- prioritering %>%
  inner_join(individ_ie_a0,
             by = c("waterlichaam_code" = "A0CODE"))
relatie_factor_IE %>%
  ggplot(aes(Blootstellingsfactor, IE)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(data = relatie_factor_IE, IE ~ Blootstellingsfactor) %>%
  summary()

relatie_factor_IE %>%
  ggplot(aes(Blootstellingsfactor, totaal_IE_A0)) +
  geom_point() +
  geom_smooth(method = "lm")

relatie_factor_IE %>%
  filter(totaal_IE_A0 < 10000) %>%
  ggplot(aes(Blootstellingsfactor, totaal_IE_A0)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(data = relatie_factor_IE, totaal_IE_A0 ~ Blootstellingsfactor) %>%
  sjPlot::plot_model(type = "pred")

overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp")) # overstorten die uitlaten op waterloop

a0_afstroom_prio_score <- st_read("data/ruw/waterlopen/Oppervlaktewaterlichamen_2022-2027_GewVLA_Shapefile/Shapefile/AfstrZonA0.shp") %>%
  left_join(prioritering %>%
              select(waterlichaam_code, Blootstellingsfactor, Waarschijnlijkheidsfactor),
              by = c("A0CODE" = "waterlichaam_code")) %>%
  st_transform(crs = st_crs(vuilvracht_overstorten))


crs(a0_afstroom_blootstelling) == crs(overstorten_uitlaat_vha)

overstorten_uitlaat_vha_prio_score <- st_join(
  x = overstorten_uitlaat_vha,
  y = a0_afstroom_prio_score %>% select(Blootstellingsfactor, Waarschijnlijkheidsfactor), # Selecteer alleen de kolom die je nodig hebt
  join = st_intersects  # Dit is de standaardwaarde (punt in polygoon)
)

st_write(overstorten_uitlaat_vha_prio_score, here("data", "verwerkt", "overstorten", "overstorten_uitlaat_vha_prio_score.gpkg"))
