# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "fybe_datasets_prep.R"))

# ==============================================================================
# STAP 3: TOEPASSING OP JOUW DATA (Workflow)
# ==============================================================================

# 2. Kies je subset (bvb Natuurlijke Beken)
# -----------------------------------------------------------
data_subset <- fybe_nat_sv_beek %>%
  drop_na(meetplaats, jaar) %>%
  rename(n_t = qual_n_t,
         p_t = qual_p_t,
         czv = qual_czv,
         no2 = qual_no2,
         no3 = qual_no3,
         nh4 = qual_nh4,
         zs = qual_zs,
         cl = qual_cl,
         o2 = qual_o2,
         t = qual_t,
         ec_20 = qual_ec_20,
         clfyl_a = qual_clfyl_a) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl,
         fybe_index, ia, is,
         czv, n_t, no2, no3, nh4, p_t, zs, cl, clfyl_a, ec_20, o2, t,
         ekc2_waterlichaam, spei6, lozingen_riool_ie, overstorten_blootstelling_index,
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr,
         verharding_oever, natuur_oever, intensiteit_combo_oever,
         lozingen_rwzi_ie, lozingen_industrie_ie, n_extreme_3m, p_sum_7d, perc_schaduw) %>%
  mutate(across(.cols = c(jaar, czv:perc_schaduw), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(bekken = as.factor(bekken),
                owl = as.factor(owl),
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                ec_20_log = log(ec_20),
                zs_log = log(zs),
                cl_log = log(cl),
                # overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                # lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1)
                ) %>%
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry()

# 1. Definieer je ruwe lijsten met variabelen (nog niet gefilterd)
# -----------------------------------------------------------
raw_responses <- c("fybe_index", "ia", "is")

raw_fysico        <- c("t_s", "p_h_s", "ec_20_log", "zs_log") # o2 weglaten want niet relevant voor mafy
raw_nutrients     <- c("czv_log", "n_t_log", "no2_log", "no3_log", "nh4_log", "p_t_log")
raw_hydmo         <- c("breedte_diepte_ratio_s", "sinuositeit_s", "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s", "ekc2_traject_s", "stroomsnelheid_s")
raw_landuse <- c("verharding_afstr_s", "natuur_afstr_s", "intensiteit_combo_afstr_s", "perc_schaduw_s", "verharding_oever_s", "natuur_oever_s", "intensiteit_combo_oever_s")
# "verharding_oever_s", "natuur_oever_s", "intensiteit_combo_oeverzone_s")
raw_lozingen      <- c("lozingen_rwzi_ie_log", "lozingen_riool_ie_log", "lozingen_industrie_ie_log", "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log")
raw_klimaat       <- c("spei6_s", "n_extreme_3m_s", "p_sum_7d_s")

vis_miss(data_subset)

# ==============================================================================
# FUNCTIE: MAAK EEN MOOI CORRELOGRAM
# ==============================================================================

plot_groep_correlogram(data_subset, raw_responses, "Responsen (Beken)")

plot_groep_correlogram(data_subset, raw_fysico, "Fysico-chemie basisvariablen (Beken)")
plot_groep_correlogram(data_subset, raw_lozingen, "Lozingen & Overstorten (Beken)")
plot_groep_correlogram(data_subset, raw_nutrients, "Nutriënten (Beken)")
plot_groep_correlogram(data_subset, raw_hydmo, "Hydromorfologie (Beken)")
plot_groep_correlogram(data_subset, raw_landuse, "Landgebruik (Beken)")
plot_groep_correlogram(data_subset, raw_klimaat, "Klimaat (Beken)")

# 3. STAP A: Opschonen Collineariteit (binnen groepen)
# -----------------------------------------------------------
clean_fysico <- filter_collinear_vars(data_subset, raw_fysico)
clean_nutrients <- filter_collinear_vars(data_subset, raw_nutrients)
clean_landuse0 <- filter_collinear_vars(data_subset, raw_landuse)
clean_landuse <- c("intensiteit_combo_afstr_s", "verharding_afstr_s", "verharding_oever_s", "intensiteit_combo_oever_s") # geen schaduw perc -> niet in dredge drivers

clean_hydmo <- filter_collinear_vars(data_subset, raw_hydmo)
# Lozingen en Klimaat zijn vaak orthogonaal genoeg, maar je kan ze ook filteren:
clean_lozingen      <- filter_collinear_vars(data_subset, raw_lozingen)
clean_klimaat       <- filter_collinear_vars(data_subset, raw_klimaat)

raw_all <- c(clean_fysico, clean_nutrients, clean_landuse, clean_hydmo, clean_lozingen, clean_klimaat)
clean_all <- filter_collinear_vars(data_subset, raw_all)


data_subset2 <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, owl, x, y,
    index_zonder_gep, index_nieuw, vo_zonder_gep, ts_zonder_gep, v_zonder_gep, gv_zonder_gep,
    n_t_log, p_t_log,
    # czv_log,
    ekc2_waterlichaam_s,
    # stroomsnelheid_s,
    # profiel_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
    perc_schaduw_s
  ) %>%
  na.omit

dredge_data <- data_subset2

vis_miss(dredge_data)

plot_groep_correlogram(dredge_data, raw_nutrients, "Nutriënten (Beken)")
