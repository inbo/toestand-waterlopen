mi_nat_sv %>% st_drop_geometry() %>%

  fc_lu_data_clean <- mi_nat_sv %>%
    left_join(overschrijdingen %>%
                group_by(meetplaats, jaar) %>%
                summarise(aantal_stoffen_met_overschrijding =
                            mean(aantal_stoffen_met_overschrijding),
                          aantal_pesticiden_met_overschrijding =
                            mean(aantal_pesticiden_met_overschrijding),
                          aantal_zware_metalen_met_overschrijding =
                            mean(aantal_zware_metalen_met_overschrijding)),
              by = c("meetplaats", "jaar")) %>%
    left_join(hm_data, by = "meetplaats") %>%
    left_join(finale_resultaten_sequentieel,
              by = c("meetplaats", "monsternamedatum")) %>%
    dplyr::select(groep, bekken, statuut, meetplaats, owl, ep_tw, ta_xw, ns_tw, sw_dw, mt_sw, mmif, mmif_20, n_t, p_h, t, ec_20, o2, p_t, landbouw_intens_afstr, hooggroen_afstr, hooggroen_oever, jaar, kjn, aantal_pesticiden_met_overschrijding, aantal_zware_metalen_met_overschrijding, Neerslag_som_10dagen, Neerslag_som_1jaar,
                  ekc2_waterlichaam) %>%
    left_join(overstort_tellingen_df %>%
                select(meetplaats, aantal_overstorten_500m),
              by = "meetplaats") %>%
    mutate(across(.cols = n_t:aantal_overstorten_500m, # Selects n_t and all columns to the end
                  .fns = ~as.numeric(scale(.x)),
                  .names = "{.col}_s")) %>%
    tidyr::drop_na() %>%
    filter(groep %in% c("beek", "kempen"))
openxlsx2::write_xlsx(test, "mi_nat_sv.xlsx")


# missing values

mi_nat_sv %>% filter(is.na(n_t)) %>% nrow()

mi_nat_sv %>% filter(is.na(kjn)) %>% nrow()

mi_nat_sv %>% filter(is.na(aantal_pesticiden_met_overschrijding)) %>% nrow()

mi_nat_sv %>% filter(is.na(ekc2_waterlichaam)) %>% nrow()

mi_nat_sv %>% filter(is.na(landbouw_intens_afstr)) %>% nrow()


test <- mi_nat_sv %>% select(n_t, aantal_pesticiden_met_overschrijding, ekc2_waterlichaam, owl.x, groep, bekken, monsternamedatum, meetplaats) %>%
  filter(groep %in% c("beek", "kempen"))
vis_miss(test)

gg_miss_fct(x = test, fct = bekken)
mcar_test(test)
test %>%
  bind_shadow() %>%
  mutate(maand = lubridate::month(monsternamedatum)) %>%
  ggplot(aes(x = maand, fill = n_t_NA)) +
  geom_density(alpha = 0.5)

test %>%
  bind_shadow() %>%
  mutate(maand = lubridate::month(monsternamedatum)) %>%
  ggplot(aes(x = maand, fill = aantal_pesticiden_met_overschrijding_NA)) +
  geom_density(alpha = 0.5)

# Maak een binaire kolom: 1 = missing, 0 = aanwezig
df <- test %>% mutate(is_missing = ifelse(is.na(n_t), 1, 0)) %>%
  mutate(maand = lubridate::month(monsternamedatum))


# Logistic regression
model <- glm(is_missing ~ bekken + maand,
             data = df,
             family = "binomial")

summary(model)
