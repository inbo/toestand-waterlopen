prioritering <- read_excel("data/ruw/overstorten/prioritering/prioritering.xlsx") %>%
  mutate(
    owl = str_replace(waterlichaam_code, "^(A0_G_|A0_)", "")
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
