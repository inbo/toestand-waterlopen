# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

load(here("data", "verwerkt", "mi_data.rdata")) # macroinvertebraten

load(here("data", "verwerkt", "fc_selectie.rdata")) # fyschem

load(here("data", "verwerkt", "hm_data.rdata")) # hydmo

load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata")) # LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))# LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer_100m_jaren.Rdata")) # LU cirkelbuffer
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_oever_jaren.rdata"))# LU

load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_afstroomgebieden.rdata"))# LU
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_buffer_100m.rdata"))# LU
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones.rdata"))# LU

load(file = here("data", "verwerkt", "hydro_data.rdata")) # hydrologie (neerslag)

load(file = here("data", "verwerkt", "lozingen_data.rdata")) # lozingen

load(file = "data/verwerkt/koppeling/koppeling_mi_nutrient.rdata") # koppeling fyschem

load(file = here("data", "verwerkt", "spear_data.rdata")) # spear pesticides
load(file = here("data", "verwerkt", "tu_resultaten.rdata")) # tu score pesticiden

# weglaten vijvers, meren, geisoleerd water en punten buiten Vlaanderen
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

# koppeling fyschem
koppeling_sleutel_nutrient <-
koppeling_mi_nutrient %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mi_nutrient <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel_nutrient, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(fc_selectie,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_fc"))

#### vergelijking met oude koppeling ####
#
# fc_mi_old <- mi_data_analyse %>%
#   st_drop_geometry() %>%
#   filter(jaar > 2009) %>%
#   select(meetplaats, monsternamedatum, groep, jaar) %>%
#   left_join(.,
#             fc_selectie,
#             by = c("meetplaats"), suffix = c("", "_fc")) %>%
#   filter(
#
#     {
#       days_before <- as.numeric(difftime(monsternamedatum, monsternamedatum_fc, units = "days"))
#       days_before < 90 &
#         days_before > -14
#
#     }
#   ) %>%
#   group_by(meetplaats, monsternamedatum) %>% #dubbele samples uitmiddelen
#   summarise(
#     across(
#       where(is.numeric), \(x) mean(x, na.rm = TRUE) # enkel numerische kolommen om de mean te pakken
#     ), # voor niet numerische waarden gewoon de eerste string nemen om te behouden
#     across(
#       where(is.factor) | where(is.character),
#       ~ dplyr::first(.)
#     ),
#     .groups = "drop" # Drop the grouping at the end
#   )
# fc_mi_old$n_t %>% is.na() %>% sum
gekoppelde_data_mi_nutrient %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "beek") %>%
  filter(jaar > 2009) %>%
  filter(!is.na(n_t))
#
#   pull(n_t) %>%
#   is.na() %>% sum
#
# fc_mi_old$czv %>% is.na() %>% sum
#
# fc_mi_old %>%
#   filter(groep == "beek") %>%
#   filter(jaar > 2009) %>%
#   filter(!is.na(n_t))
# gekoppelde_data_mi_nutrient$czv %>% is.na() %>% sum

#### koppeling overige data ####
data0 <- gekoppelde_data_mi_nutrient %>%
  left_join(hm_data,
            by = c("meetplaats"))

landgebruik_data <- landgebruik_afstroomgebied_jaren %>%
  select(-oppervlakte, -landgebruiksjaar) %>%
  filter(meetplaats %in% afstroomgebieden_binnen_vlaanderen$meetplaats) %>% # afstroomgebiedne moeten voor meer dan 80% binnen vlaanderen liggen
  full_join(.,
            landgebruik_oever_jaren %>%
              select(-landgebruiksjaar),
            by = c("meetplaats", "monsternamedatum"), suffix = c("_afstr", "_oever")) %>%
  left_join(.,
            landgebruik_buffer_100m_jaren %>%
              select(-landgebruiksjaar) %>%
              rename_with(~ paste0(., "_buffer"), !all_of(c("meetplaats", "monsternamedatum"))),
            by = c("meetplaats", "monsternamedatum"), suffix = c("", "_buffer")) %>%
  left_join(.,
            intensiteit_landbouw_afstroomgebieden_scores,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(.,
            intensiteit_landbouw_buffer_100m_jaren %>%
              select(-jaar, -oppervlakte_buffer),
            by = c("meetplaats", "monsternamedatum"),
            suffix = c("_afstr", "_buffer")) %>%
  left_join(.,
            intensiteit_landbouw_oeverzones_jaren %>%
              select(-jaar, -oppervlakte_oeverzone) %>%
              rename_with(~ paste0(., "_oeverzone"), !all_of(c("meetplaats", "monsternamedatum"))),
            by = c("meetplaats", "monsternamedatum"))


data1 <- data0 %>%
  left_join(landgebruik_data,
            by = c("meetplaats", "monsternamedatum"))

data2 <- data1 %>%
  left_join(hydro_data,
            by = c("meetplaats", "monsternamedatum"))

data3 <- data2 %>%
  left_join(lozingen_data,
            by = c("meetplaats", "monsternamedatum"))

################################################################################
# Data subsets maken ####
################################################################################
# 1. natuurlijk en sterk veranderd
# 1.1 beek
mi_nat_sv_beek <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "beek")

# 1.2 kempen
mi_nat_sv_kempen <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "kempen")

# 1.3 polder
mi_nat_sv_polder <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "polder")

# 1.4 rivier
mi_nat_sv_rivier <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "rivier")

# 2. kunstmatig
mi_kunstmatig <- data3 %>%
  filter(statuut %in% c("Kunstmatig"))

# 3. RtNt - bovenlopen -> NVT????
rtnt <- data3 %>%
  filter(type == "RtNt")
load(file = here("data", "verwerkt", "mi_data_analyse_rtnt_update.rdata"))

################################################################################
# Pesticiden
################################################################################

load(file = "data/verwerkt/koppeling/koppeling_mi_pesticides.rdata")

koppeling_sleutel_pesticides <-
  koppeling_mi_pesticides %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mi_pesticides <- data3 %>%
  select(-qual_meetplaats, -qual_monsternamedatum) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel_pesticides %>% st_drop_geometry() , by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(tu_specific_groups,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_pest")) %>%
  left_join(spear_data,
            by = c("meetplaats", "monsternamedatum"))


gekoppelde_data_mi_pesticides %>%
  select(meetplaats, monsternamedatum,
         groep, categorie,
         TU_sum) %>%
  group_by(groep) %>%
  mutate(totaal = n()) %>%
  na.omit() %>%
  group_by(groep, totaal) %>%
  summarise(n())

test <- gekoppelde_data_mi_pesticides %>%
  filter(groep %in% c("beek")) %>%
  filter(statuut %in% c("Sterk Veranderd", "Natuurlijk")) %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, groep,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs,
         TU_sum, TU_max, TU_insecticide, TU_neonicotinoids, spear_pesticides, concentratie_pesticiden_sum, concentratie_insecticide, TU_insecticide_max,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, intensiteit_gewasbescherming_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone,
         spei6, n_extreme_3m, p_sum_7d,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                TU_sum_log = log(TU_sum + 1))

test2 <- test %>%
  select(
    meetplaats, monsternamedatum, jaar_s,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse), intensiteit_gewasbescherming_afstr_s,
    all_of(clean_fysico),
    TU_sum_s, TU_max_s, TU_insecticide_s, TU_insecticide_max_s, TU_neonicotinoids_s, spear_pesticides_s, concentratie_pesticiden_sum_s, concentratie_insecticide_s, TU_sum_log, spear_pesticides,
    groep
  ) %>%
  na.omit %>%
  filter(TU_insecticide_max_s < 1)

plot_groep_correlogram(test2, c("TU_sum_s", "TU_max_s", "TU_insecticide_s", "TU_insecticide_max_s", "TU_neonicotinoids_s", "spear_pesticides_s", "concentratie_pesticiden_sum_s", "concentratie_insecticide_s", "TU_sum_log"))

model_mmif <- glmmTMB(data = test2, formula = mmif ~ TU_insecticide_max_s + ec_20_s + p_h_s + spei6_s + o2_s + p_t_log + n_extreme_3m_s + lozingen_riool_ie_log + verharding_afstr_s + jaar_s + intensiteit_combo_afstr_s + (1 | meetplaats),
                 REML = TRUE,
                 family = ordbeta)
summary(model_mmif)
plot_model(model_mmif, type = "pred", show.data = T)

model_mts <- glmmTMB(data = test2, formula = mt_sw_prop ~ TU_insecticide_max_s + jaar_s + (1|groep) + (1 | meetplaats),
                      REML = TRUE,
                      family = ordbeta)
summary(model_mts)

model_ept <- glmmTMB(data = test2, formula = ept_prop ~ TU_insecticide_max_s + jaar_s + (1 | meetplaats),
                      REML = TRUE,
                      family = binomial(link = "logit"),
                     weights = test2$ta_xw)
summary(model_ept)

model_stress <- glmmTMB(data = test2, formula = stress_prop ~ TU_insecticide_max_s + jaar_s + (1 | meetplaats),
                     REML = TRUE,
                     family = binomial(link = "logit"),
                     weights = test2$ta_xw)
summary(model_ept)

model_tax <- glmmTMB(data = test2, formula = ta_xw ~ TU_insecticide_max_s + jaar_s + (1 | meetplaats),
                        REML = TRUE,
                        family = poisson)
summary(model_tax)

model_swd <- glmmTMB(data = test2, formula = sw_dw ~ ec_20_s + TU_insecticide_max_s + p_t_log + n_t_log + intensiteit_combo_afstr_s + jaar_s + (1 | meetplaats),
                     REML = TRUE,
                     family = gaussian)
summary(model_swd)

model_TU <- glmmTMB(data = test2, formula = concentratie_pesticiden_sum_s ~ intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s +  n_t_log + lozingen_riool_ie_log + spear_pesticides_s + jaar_s + (1 | meetplaats),
                     REML = TRUE,
                     family = gaussian)
summary(model_TU)
plot_model(model_TU, type = "pred", show.data = T)

plot_model_vif(model_TU)

model_spear <- glmmTMB(data = test2, formula = spear_pesticides ~ TU_max_s + intensiteit_combo_afstr_s + natuur_oever_s + n_t_log + p_t_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                    REML = TRUE,
                    family = ordbeta)
summary(model_spear)
plot_model_vif(model_spear)


plot_groep_correlogram(test2, c("TU_max_s", "spear_pesticides_s", "n_t_log", "p_t_log", "ec_20_s"), "Responsen (Beken)")

