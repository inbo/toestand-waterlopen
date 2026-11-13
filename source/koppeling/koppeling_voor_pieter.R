load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_mafy_2km_2jaar.rdata"))
load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_vis_2km_2jaar.rdata"))
load(here("data", "ruw", "vis", "traits_all_obs_no_marien_EQR.Rdata"))

koppeling_sleutel_mafy <-
  mi_met_mafy_2km_2jaar %>%
  select(meetplaats, monsternamedatum, mp_meetplaats, mp_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    mp_monsternamedatum = as.Date(mp_monsternamedatum)
  )

koppeling_sleutel_vis <-
  mi_met_vis_2km_2jaar %>%
  select(meetplaats, monsternamedatum, mp_meetplaats, mp_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    vis_monsternamedatum = as.Date(mp_monsternamedatum),
    vis_meetplaats = as.factor(mp_meetplaats)
  ) %>%
  select(-mp_meetplaats, -mp_monsternamedatum)

koppeling_mi_mafy_vis <- koppeling_sleutel_mafy %>%
  st_drop_geometry() %>%
  left_join(koppeling_sleutel_vis,
            by = c("monsternamedatum", "meetplaats")) %>%
  left_join(mi_data %>%
  select(meetplaats, monsternamedatum, mmif) %>%
    st_drop_geometry(),
  by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mafy_data %>%
              select(meetplaats, monsternamedatum, index_zonder_gep) %>%
              rename(mp_meetplaats = meetplaats,
                     mp_monsternamedatum = monsternamedatum,
                     eqr_mafy = index_zonder_gep) %>%
              st_drop_geometry(),
            by = c("mp_meetplaats", "mp_monsternamedatum")) %>%
  left_join(data_traits_EQR %>%
              mutate(vis_meetplaats = as.factor(WaarnemingVispuntID),
                     vis_monsternamedatum = as.Date(Datum),
                     eqr_vis = EQR) %>%
              select(vis_meetplaats, vis_monsternamedatum, eqr_vis),
            by = c("vis_meetplaats", "vis_monsternamedatum"))

save(koppeling_mi_mafy_vis, file = here::here("data", "verwerkt", "koppeling", "koppeling_mi_mafy_vis.rdata"))
