load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_mafy_2km_2jaar.rdata"))
load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_vis_2km_2jaar.rdata"))
source(here::here("source", "functies.R"))

load(here("data", "verwerkt", "mi_data.rdata")) # macroinvertebraten

load(here("data", "ruw", "vis", "traits_all_obs_no_marien_EQR.Rdata"))
vis_data <- data_traits_EQR %>%
  filter(MethodeGroepOmschrijving == "Elektrisch") %>%
  select(Datum, WaarnemingVispuntID, VispuntX, VispuntY, VHAS, VHAG,
         perc_inv_fresh, perc_exoot, EQR) %>%
  st_as_sf(coords = c("VispuntX", "VispuntY"), crs = st_crs(mi_data)) %>%
  rename(meetplaats = WaarnemingVispuntID,
         monsternamedatum = Datum) %>%
  mutate(monsternamedatum = as.Date(monsternamedatum),
         meetplaats = as.factor(meetplaats)) %>%
  filter(!is.na(VHAS))

mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

koppeling_sleutel_mafy <-
  mi_mafy_gekoppeld %>%
  select(meetplaats, monsternamedatum, match_meetplaats, match_monsternamedatum, match_index_nieuw, match_index_zonder_gep, match_ts_zonder_gep, match_gv_zonder_gep, match_vo_zonder_gep, match_v_zonder_gep) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    match_monsternamedatum = as.Date(match_monsternamedatum)
  )

koppeling_sleutel_vis <-
  mi_vis_gekoppeld %>%
  select(meetplaats, monsternamedatum, match_meetplaats, match_monsternamedatum,
         match_EQR, match_perc_inv_fresh, match_perc_exoot) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    vis_monsternamedatum = as.Date(match_monsternamedatum),
    vis_meetplaats = as.factor(match_meetplaats)
  ) %>%
  select(-match_meetplaats, -match_monsternamedatum)

load(here("data", "verwerkt", "mafy_data.rdata"))

gekoppelde_data_mi_mafy <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel_mafy, by = c("meetplaats", "monsternamedatum"))
# %>%
#
#   # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
#   left_join(mafy_data,
#             by = c("match_meetplaats" = "meetplaats",
#                    "match_monsternamedatum" = "monsternamedatum"),
#             suffix = c("", "_mafy"))

gekoppelde_data_mi_mafy %>%
  filter(is.na(match_index_zonder_gep)) %>%
  group_by(categorie) %>%
  summarise(n())

 test <- gekoppelde_data_mi_mafy %>%
  filter(groep == "beek") %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(!is.na(match_index_zonder_gep))

 ggplot(test, aes(match_index_nieuw, mmif)) +
   geom_point() +
   geom_smooth()


################################################################################
#  ultieme test -> mafy toevoegen aan sem data mi_nat_sv_beek
################################################################################
 source(here::here("source", "analyse", "mi_datasets_prep.R"))

 mi_mafy_data <-  data5 %>%
   st_drop_geometry() %>%
   filter(jaar > 2009) %>%
   # 1. Koppel de match-informatie aan je MI data
   # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
   left_join(koppeling_sleutel_mafy, by = c("meetplaats", "monsternamedatum")) %>%
  select(meetplaats, monsternamedatum, bekken, owl, jaar, vhag, mmif, ep_tw, sw_dw, ns_tw, mt_sw, ta_xw, o2, ec_20, zs, n_t, p_t, czv, intensiteit_combo_afstr, ekc2_waterlichaam, match_monsternamedatum, match_index_nieuw, match_index_zonder_gep, match_ts_zonder_gep, match_gv_zonder_gep, match_vo_zonder_gep, match_v_zonder_gep, overstorten_blootstelling_index) %>%
   mutate(across(.cols = c(jaar, o2:overstorten_blootstelling_index), # Selects n_t and all columns to the end
                 .fns = ~as.numeric(scale(.x)),
                 .names = "{.col}_s")) %>%
   dplyr::mutate(ep_tw = as.integer(ep_tw),
                 ta_xw = as.integer(ta_xw),
                 ns_tw = as.integer(ns_tw),
                 mt_sw_prop = mt_sw / 10,
                 bekken = as.factor(bekken),
                 maand = as.factor(lubridate::month(monsternamedatum)),
                 vhag = as.factor(vhag),
                 owl = as.factor(owl),
                 nst_prop = ns_tw / ta_xw,
                 stress_prop = (ep_tw + ns_tw)/ta_xw,
                 ept_prop = ep_tw / ta_xw,
                 n_t_log = log(n_t),
                 p_t_log = log(p_t),
                 # no2_log = log(no2),
                 # no3_log = log(no3),
                 czv_log = log(czv),
                 # nh4_log = log(nh4),
                 zs_log = log(zs),
                 ec_20_log = log(ec_20),
                 # overstorten_index_log = log(overstorten_index + 1),
                 overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1)) %>%

                 # lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                 # lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                 # lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                 # lozingen_riool_ie_log = log(lozingen_riool_ie + 1)) %>%
  na.omit()

mmif <- glmmTMB(
  mmif ~ match_index_nieuw + o2_s + n_t_log + p_t_log + zs_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats) + (1|owl),
  family = ordbeta,
  data = mi_mafy_data)
summary(mmif)

ept <- glmmTMB(
  ept_prop ~ index_zonder_gep + o2_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s  + (1 | bekken),
  family = binomial(link = "logit"),
  weights = mi_mafy_data$ta_xw,
  data = mi_mafy_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ intensiteit_combo_afstr_s + zs_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats)+ (1|owl),
  family = gaussian,
  data = mi_mafy_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ intensiteit_combo_afstr_s + zs_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats)+ (1|owl),
  family = gaussian,
  data = mi_mafy_data)
summary(ptot)

# czv <- glmmTMB(
#   czv_log ~ n_t_log + p_t_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1|owl),
#   family = gaussian,
#   data = mi_mafy_data)
# summary(czv)

zs <- glmmTMB(
  zs_log ~ overstorten_blootstelling_index_log + ekc2_waterlichaam_s + intensiteit_combo_afstr_s + jaar_s + (1|owl),
  family = gaussian,
  data = mi_mafy_data)
summary(zs)

o2 <- glmmTMB(
  o2_s ~ overstorten_blootstelling_index_log + ekc2_waterlichaam_s + zs_log + p_t_log + jaar_s + match_index_nieuw + (1|owl),
  family = gaussian,
  data = mi_mafy_data)
summary(o2)

mafy <- glmmTMB(
  match_index_nieuw ~ zs_log + n_t_log + p_t_log + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats) + (1|owl),
  family = ordbeta,
  data = mi_mafy_data)
summary(mafy)


sem_resultaat <- psem(mmif, mafy, o2, ptot, ntot, zs,
                      p_t_log %~~% n_t_log

                      )
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

coefs_filled <- standardize_psem(sem_resultaat)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_mafy_combo_mmif.html"), selfcontained = TRUE)
################################################################################
# VIS + MI
################################################################################

mi_vis_data <-  data5 %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel_vis, by = c("meetplaats", "monsternamedatum")) %>%
  select(meetplaats, monsternamedatum, bekken, owl, jaar, vhag, mmif, ep_tw, sw_dw, ns_tw, mt_sw, ta_xw, o2, ec_20, zs, n_t, p_t, czv, intensiteit_combo_afstr, ekc2_waterlichaam, match_EQR, match_perc_inv_fresh, match_perc_exoot, overstorten_blootstelling_index) %>%
  mutate(across(.cols = c(jaar, o2:overstorten_blootstelling_index), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                match_perc_exoot = match_perc_exoot/100,
                bekken = as.factor(bekken),
                maand = as.factor(lubridate::month(monsternamedatum)),
                vhag = as.factor(vhag),
                owl = as.factor(owl),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                # no2_log = log(no2),
                # no3_log = log(no3),
                czv_log = log(czv),
                # nh4_log = log(nh4),
                zs_log = log(zs),
                ec_20_log = log(ec_20),
                # overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1)) %>%

  # lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
  # lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
  # lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
  # lozingen_riool_ie_log = log(lozingen_riool_ie + 1)) %>%
  na.omit()

mmif <- glmmTMB(
  mmif ~ match_perc_exoot + o2_s + n_t_log + p_t_log + zs_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats) + (1|owl),
  family = ordbeta,
  data = mi_vis_data)
summary(mmif)

mmif_eqr <- glmmTMB(
  mmif ~ match_EQR + o2_s + n_t_log + p_t_log + zs_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats) + (1|owl),
  family = ordbeta,
  data = mi_vis_data)
summary(mmif_eqr)
ept <- glmmTMB(
  ept_prop ~ index_nieuw + o2_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s  + (1 | bekken),
  family = binomial(link = "logit"),
  weights = mi_mafy_data$ta_xw,
  data = mi_mafy_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ intensiteit_combo_afstr_s + zs_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s +  (1|owl),
  family = gaussian,
  data = mi_vis_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ intensiteit_combo_afstr_s + zs_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1|owl),
  family = gaussian,
  data = mi_vis_data)
summary(ptot)

# czv <- glmmTMB(
#   czv_log ~ n_t_log + p_t_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1|owl),
#   family = gaussian,
#   data = mi_vis_data)
# summary(czv)

zs <- glmmTMB(
  zs_log ~ overstorten_blootstelling_index_log + ekc2_waterlichaam_s + intensiteit_combo_afstr_s + jaar_s + (1|owl),
  family = gaussian,
  data = mi_vis_data)
summary(zs)

o2 <- glmmTMB(
  o2_s ~ overstorten_blootstelling_index_log + ekc2_waterlichaam_s + zs_log + p_t_log + jaar_s + (1|owl),
  family = gaussian,
  data = mi_vis_data)
summary(o2)

vis <- glmmTMB(
  match_EQR ~ o2_s + n_t_log + p_t_log  + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats) + (1|owl),
  family = ordbeta,
  data = mi_vis_data)
summary(vis)

perc_exoot <- glmmTMB(
  match_perc_exoot ~ o2_s + n_t_log + p_t_log  + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1|owl),
  family = ordbeta,
  data = mi_vis_data)
summary(perc_exoot)

sem_resultaat <- psem(mmif, perc_exoot, o2, ptot, ntot, zs,
                      n_t_log %~~% p_t_log)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

coefs_filled <- standardize_psem(sem_resultaat)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_vis_combo_mmif.html"), selfcontained = TRUE)
sem_resultaat <- psem(mmif_vis, vis, o2, ptot, ntot, zs,
                      n_t_log %~~% p_t_log)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

coefs_filled <- standardize_psem(sem_resultaat)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
# saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_vis_combo_mmif.html"), selfcontained = TRUE)
################################################################################
# VIS + MI + MAFY
################################################################################

mi_mafy_vis_data <-  data5 %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  left_join(koppeling_sleutel_vis, by = c("meetplaats", "monsternamedatum")) %>%
  left_join(koppeling_sleutel_mafy, by = c("meetplaats", "monsternamedatum")) %>%
  select(meetplaats, monsternamedatum, bekken, owl, jaar, vhag, mmif, ep_tw, sw_dw, ns_tw, mt_sw, ta_xw, o2, ec_20, zs, n_t, p_t, czv, intensiteit_combo_afstr, ekc2_waterlichaam, match_index_nieuw, match_index_zonder_gep, match_ts_zonder_gep, match_gv_zonder_gep, match_vo_zonder_gep, match_v_zonder_gep, match_EQR, match_perc_inv_fresh, match_perc_exoot, overstorten_blootstelling_index) %>%
  mutate(across(.cols = c(jaar, o2:overstorten_blootstelling_index), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                match_perc_exoot = match_perc_exoot/100,
                match_perc_inv_fresh = match_perc_inv_fresh/100,
                bekken = as.factor(bekken),
                maand = as.factor(lubridate::month(monsternamedatum)),
                vhag = as.factor(vhag),
                owl = as.factor(owl),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                # no2_log = log(no2),
                # no3_log = log(no3),
                czv_log = log(czv),
                # nh4_log = log(nh4),
                zs_log = log(zs),
                ec_20_log = log(ec_20),
                # overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1)) %>%

  # lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
  # lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
  # lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
  # lozingen_riool_ie_log = log(lozingen_riool_ie + 1)) %>%
  na.omit()
plot_groep_correlogram(mi_mafy_vis_data, c("match_index_zonder_gep", "match_v_zonder_gep", "match_ts_zonder_gep", "match_gv_zonder_gep", "match_vo_zonder_gep"))

mmif <- glmmTMB(
  mmif ~ zs_log+ match_perc_exoot + match_index_nieuw +ekc2_waterlichaam_s+ o2_s + n_t_log + p_t_log + overstorten_blootstelling_index_log + jaar_s + (1|owl),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(mmif)

mmif_vis <- glmmTMB(
  mmif ~ zs_log+ match_EQR+ match_index_nieuw +ekc2_waterlichaam_s+ o2_s + n_t_log + p_t_log + overstorten_blootstelling_index_log + jaar_s + (1|owl),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(mmif_vis)

ept <- glmmTMB(
  ept_prop ~ EQR + o2_s + n_t_log + p_t_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s  + (1 | owl),
  family = binomial(link = "logit"),
  weights = mi_mafy_vis_data$ta_xw,
  data = mi_mafy_vis_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ zs_log+ intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1|owl),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ zs_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1|owl),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(ptot)

zs <- glmmTMB(
  zs_log ~ intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(zs)

o2 <- glmmTMB(
  o2_s ~ zs_log + + n_t_log + p_t_log + match_index_nieuw + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(o2)

vis_exoot <- glmmTMB(
  match_perc_exoot ~ zs_log + o2_s + p_t_log + match_index_nieuw + ekc2_waterlichaam_s + jaar_s + (1|owl),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(vis_exoot)

vis_invoor <- glmmTMB(
  perc_inv_fresh ~ o2_s + p_t_log + index_zonder_gep + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(vis_invoor)

vis <- glmmTMB(
  match_EQR ~ o2_s + match_index_nieuw + ekc2_waterlichaam_s + jaar_s + (1 | owl) + (1|meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(vis)

mafy <- glmmTMB(
  match_index_nieuw ~ zs_log + p_t_log + intensiteit_combo_afstr_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(mafy)

sem_resultaat <- psem(mmif, vis_exoot, mafy, o2, ptot, ntot, zs, p_t_log %~~% n_t_log)
                      # EQR %~~% perc_exoot,
                      # EQR %~~% perc_inv_fresh)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)


coefs_filled <- standardize_psem(sem_resultaat)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten

sem_resultaat <- psem(mmif_vis, vis, mafy, o2, ptot, ntot, zs,  p_t_log %~~% n_t_log)
# EQR %~~% perc_exoot,
# EQR %~~% perc_inv_fresh)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

coefs_filled <- standardize_psem(sem_resultaat)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_mafy_vis_combo_mmif.html"), selfcontained = TRUE)
