# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mafy_datasets_prep.R"))

mafy_nat_sv_rivier %>% ggplot(aes(index_zonder_gep, ekc_mafy)) + geom_point() + geom_abline(a = 1, b = 0)
# ==============================================================================
# STAP 3: TOEPASSING OP JOUW DATA (Workflow)
# ==============================================================================

# 2. Kies je subset (bvb Natuurlijke rivieren)
# -----------------------------------------------------------
data_subset <- mafy_nat_sv_rivier %>%
  drop_na(meetplaats, jaar) %>%
  rename(n_t = qual_n_t,
         p_t = qual_p_t,
         czv = qual_czv,
         no2 = qual_no2,
         no3 = qual_no3,
         nh4 = qual_nh4,
         zs = qual_zs) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl,
         index_zonder_gep, ekc_mafy, index_nieuw, vo_zonder_gep, ts_zonder_gep, v_zonder_gep, gv_zonder_gep,
         t, p_h, o2, o2_verz, ec_20, secchi, relatieve_doorzichtbaarheid,
         czv, n_t, no2, no3, nh4, p_t, zs,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr,
         verharding_oever, natuur_oever, intensiteit_combo_oever,
         spei6, n_extreme_3m, p_sum_7d,
         perc_schaduw,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
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
                secchi_log = log(secchi),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1)) %>%
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry()

# 1. Definieer je ruwe lijsten met variabelen (nog niet gefilterd)
# -----------------------------------------------------------
raw_responses <- c("ekc_mafy", "ts_zonder_gep", "v_zonder_gep", "gv_zonder_gep")

raw_fysico        <- c("t_s", "p_h_s", "ec_20_log", "zs_log", "secchi_log") # o2 weglaten want niet relevant voor mafy
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

plot_groep_correlogram(data_subset, raw_responses, "Responsen (rivieren)")

plot_groep_correlogram(data_subset, raw_fysico, "Fysico-chemie basisvariablen (rivieren)")
plot_groep_correlogram(data_subset, raw_lozingen, "Lozingen & Overstorten (rivieren)")
plot_groep_correlogram(data_subset, raw_nutrients, "Nutriënten (rivieren)")
plot_groep_correlogram(data_subset, raw_hydmo, "Hydromorfologie (rivieren)")
plot_groep_correlogram(data_subset, raw_landuse, "Landgebruik (rivieren)")
plot_groep_correlogram(data_subset, raw_klimaat, "Klimaat (rivieren)")

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
    index_zonder_gep, ekc_mafy, index_nieuw, ts_zonder_gep, v_zonder_gep, gv_zonder_gep,
    n_t_log, p_t_log, czv_log,
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

plot_groep_correlogram(dredge_data, raw_nutrients, "Nutriënten (rivieren)")

################################################################################
# model fitten mafy index
################################################################################

y_var <- "ekc_mafy"
predictors <- c("spei6_s", "p_sum_7d_s", "t_s", "zs_log", "ec_20_log", "ekc2_waterlichaam_s", "perc_schaduw_s", "n_t_log", "p_t_log", "verharding_oever_s", "intensiteit_combo_oever_s", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1|meetplaats))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge_mafy.R"))

mafy_index_dredge <- dredge_model

best_model_ML <- get.models(mafy_index_dredge, subset = 1)[[1]]

mafy_index_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(mafy_index_best_model_rivier)
save(mafy_index_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "mafy_index_best_model_rivier.rdata"))

plot_model_vif(mafy_index_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

importance_mafy_index <- sw(mafy_index_dredge)

################################################################################
# model fitten verstoring
################################################################################
y_var <- "v_zonder_gep"
predictors <- c("spei6_s", "p_sum_7d_s", "t_s", "zs_log", "ec_20_log", "ekc2_waterlichaam_s", "perc_schaduw_s", "n_t_log", "p_t_log", "verharding_oever_s", "intensiteit_combo_oever_s", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)
performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge_mafy.R"))

verstoring_dredge <- dredge_model

best_model_ML <- get.models(verstoring_dredge, subset = 1)[[1]]

verstoring_best_model_rivier <- update(best_model_ML, REML = TRUE)
save(verstoring_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "verstoring_best_model_rivier.rdata"))

summary(verstoring_best_model_rivier)

plot_model_vif(verstoring_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten groeivorm
################################################################################
y_var <- "gv_zonder_gep"
predictors <- c("spei6_s", "p_sum_7d_s", "t_s", "zs_log", "ec_20_log", "ekc2_waterlichaam_s", "perc_schaduw_s", "n_t_log", "p_t_log", "verharding_oever_s", "intensiteit_combo_oever_s", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1|meetplaats) - (1|owl))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge_mafy.R"))

groeivorm_dredge <- dredge_model

best_model_ML <- get.models(groeivorm_dredge, subset = 1)[[1]]

groeivorm_best_model_rivier <- update(best_model_ML, REML = TRUE)
save(groeivorm_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "groeivorm_best_model_rivier.rdata"))

summary(groeivorm_best_model_rivier)

plot_model_vif(groeivorm_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten typespecificiteit
################################################################################
y_var <- "ts_zonder_gep"
predictors <- c("spei6_s", "p_sum_7d_s", "t_s", "zs_log", "ec_20_log", "ekc2_waterlichaam_s", "perc_schaduw_s", "n_t_log", "p_t_log", "verharding_oever_s", "intensiteit_combo_oever_s", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1|owl))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge_mafy.R"))

typspec_dredge <- dredge_model

best_model_ML <- get.models(typspec_dredge, subset = 1)[[1]]

typspec_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(typspec_best_model_rivier)
save(typspec_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "typspec_best_model_rivier.rdata"))

plot_model_vif(typspec_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten


################################################################################
# model fitten stikstof
################################################################################

y_var <- "n_t_log"
predictors <- c("spei6_s", "p_sum_7d_s", "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1|meetplaats))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

ntot_dredge <- dredge_model

best_model_ML <- get.models(ntot_dredge, subset = 1)[[1]]

ntot_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ntot_best_model_rivier)
save(ntot_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "ntot_best_model_rivier.rdata"))

plot_model_vif(ntot_best_model_rivier, "VIF Check")

################################################################################
# model fitten fosfor
################################################################################
y_var <- "p_t_log"
predictors <- c("spei6_s", "p_sum_7d_s", "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1|meetplaats))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

ptot_dredge <- dredge_model

best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

ptot_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_rivier)
save(ptot_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "ptot_best_model_rivier.rdata"))

plot_model_vif(ptot_best_model_rivier, "VIF Check")

################################################################################
# model fitten zs
################################################################################
y_var <- "zs_log"
predictors <- c("spei6_s", "p_sum_7d_s", clean_landuse, clean_lozingen, "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1 | bekken))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

zs_dredge <- dredge_model

best_model_ML <- get.models(zs_dredge, subset = 1)[[1]]

zs_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(zs_best_model_rivier)
save(zs_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "zs_best_model_rivier.rdata"))

plot_model_vif(zs_best_model_rivier, "VIF Check")

################################################################################
# model fitten czv
################################################################################

y_var <- "czv_log"
predictors <- c("spei6_s", "p_sum_7d_s", "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1 | bekken) - (1|meetplaats))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_rivier)
save(czv_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "czv_best_model_rivier.rdata"))

plot_model_vif(czv_best_model_rivier, "VIF Check")

################################################################################
# model fitten ec20
################################################################################

y_var <- "ec_20_log"
predictors <- c("spei6_s", "p_sum_7d_s", "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)
performance::check_singularity(model)
formula_obj <- update(formula_obj, . ~ . - (1|bekken) - (1|meetplaats))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_rivier)
save(ec20_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mafy", "ec20_best_model_rivier.rdata"))

plot_model_vif(ec20_best_model_rivier, "VIF Check")

################################################################################
# model fitten secchi
################################################################################
#
# y_var <- "secchi_log"
# predictors <- c("spei6_s", "p_sum_7d_s", "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "perc_schaduw_s")
#
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = gaussian)
# performance::check_singularity(model)
#
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# secchi_dredge <- dredge_model
#
# best_model_ML <- get.models(secchi_dredge, subset = 1)[[1]]
#
# secchi_best_model_rivier <- update(best_model_ML, REML = TRUE)
#
# summary(secchi_best_model_rivier)
#
# plot_model_vif(secchi_best_model_rivier, "VIF Check")

################################################################################
# model fitten relatieve doorzichtbaarheid
################################################################################
#
# y_var <- "relatieve_doorzichtbaarheid"
# predictors <- c("spei6_s", "p_sum_7d_s", "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "perc_schaduw_s", "zs_log")
#
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = ordbeta)
# performance::check_singularity(model)
#
# formula_obj <- update(formula_obj, . ~ . - (1 | meetplaats) - (1 | bekken))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = ordbeta)
#
# performance::check_singularity(model)
#
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# doorzicht_dredge <- dredge_model
#
# best_model_ML <- get.models(doorzicht_dredge, subset = 1)[[1]]
#
# doorzicht_best_model_rivier <- update(best_model_ML, REML = TRUE)
#
# summary(doorzicht_best_model_rivier)
#
# plot_model_vif(doorzicht_best_model_rivier, "VIF Check")

################################################################################
# sem models fitten
################################################################################

load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "mafy_index_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "typspec_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "groeivorm_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "verstoring_best_model_rivier.rdata"))

load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "ntot_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "ptot_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "czv_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "zs_best_model_rivier.rdata"))
# load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "cl_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mafy", "ec20_best_model_rivier.rdata"))


# mafy index

sem_mafy_index_rivier <- psem(
  mafy_index_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  zs_best_model_rivier,
  ec20_best_model_rivier
  # secchi_best_model_rivier
)

summary(sem_mafy_index_rivier)

# model updaten op basis van dSepS
mafy_index_best_model_updated <- update(mafy_index_best_model_rivier, . ~ .)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
zs_best_model_updated <- update(zs_best_model_rivier, . ~ .)
# secchi_best_model_updated <- update(secchi_best_model_rivier, . ~ . )


# het optimale model clean maken!
f_mafy_index  <- formula(mafy_index_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv   <- formula(czv_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
# f_secchi     <- formula(secchi_best_model_updated)

mafy_index_clean <- glmmTMB(
  formula     = f_mafy_index,
  data        = dredge_data,
  family      = ordbeta()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
# o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
# secchi_clean     <- glmmTMB(f_secchi,     data = dredge_data, family = gaussian())

mafy_index_sem_nat_sv_rivier_clean <- psem(
  mafy_index_clean,
  ntot_clean,
  ptot_clean,
  ec20_clean,
  czv_clean,
  zs_clean,
  # secchi_clean,
  n_t_log %~~% p_t_log,
  czv_log %~~% p_t_log,
  n_t_log %~~% czv_log
)

summary(mafy_index_sem_nat_sv_rivier_clean)

save(mafy_index_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "mafy_index_sem_nat_sv_rivier_clean.rdata"))
bewaar_r2(mafy_index_sem_nat_sv_rivier_clean, "mafy", "rivier", "index")


load(file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "mafy_index_sem_nat_sv_rivier_clean.rdata"))

# SAC test
model <- mafy_index_clean
# 1. Set glmmTMB to simulate conditionally on fitted REs
glmmTMB::set_simcodes(model$obj, val = "fix")
# 2. Run DHARMa as usual
res <- simulateResiduals(fittedModel = model)
glmmTMB::set_simcodes(model$obj, val = "random")

locaties_match <- dredge_data %>%
  group_by(meetplaats) %>%
  summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
  arrange(meetplaats) # DHARMa sorteert groepen standaard op naam/factor level

# 3. Aggregeer residuen
res_grouped <- recalculateResiduals(res, group = dredge_data$meetplaats)

# 4. De test
testSpatialAutocorrelation(res_grouped,
                           x = locaties_match$x,
                           y = locaties_match$y)

model_list <- list(
  mafy_index = mafy_index_clean,
  ntot = ntot_clean,
  ptot = ptot_clean,
  ec20 = ec20_clean,
  zs = zs_clean,
  czv = czv_clean
)

# Loop voor validatie
for (name in names(model_list)) {
  cat("\n--- Diagnostiek voor model:", name, "---\n")

  # 1. Bereken residuen (simulatie-gebaseerd)
  res <- simulateResiduals(fittedModel = model_list[[name]], n = 1000)

  # 2. Plot residuen (QQ-plot en Residual vs Predicted)
  plot(res)
  title(main = paste("DHARMa residuals:", name), line = 2)

  # # 3. Test specifiek op overdispersie en outliers
  # print(testDispersion(res))
  # print(testOutliers(res))
}

coefs_filled <- standardize_psem(mafy_index_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mafy_rivier_index.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mafy_nat_sv_mafy_index_rivier.html"), selfcontained = TRUE)

####################
## verstoring
#####################

sem_verstoring_rivier <- psem(
  verstoring_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  zs_best_model_rivier,
  ec20_best_model_rivier
  # secchi_best_model_rivier
)

summary(sem_verstoring_rivier, conserve = TRUE)



# updaten

verstoring_best_model_updated <- update(verstoring_best_model_rivier, . ~ . )
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )
# secchi_best_model_updated <- update(secchi_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_verstoring  <- formula(verstoring_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv   <- formula(czv_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
# f_secchi     <- formula(secchi_best_model_updated)

verstoring_clean <- glmmTMB(
  formula     = f_verstoring,
  data        = dredge_data,
  family      = ordbeta()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
# o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
# secchi_clean     <- glmmTMB(f_secchi,     data = dredge_data, family = gaussian())

verstoring_sem_nat_sv_rivier_clean <- psem(
  verstoring_clean,
  ntot_clean,
  ptot_clean,
  ec20_clean,
  zs_clean,
  czv_clean,
  # secchi_clean,
  n_t_log %~~% p_t_log,
  czv_log %~~% p_t_log,
  n_t_log %~~% czv_log
)

summary(verstoring_sem_nat_sv_rivier_clean)

save(verstoring_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "verstoring_sem_nat_sv_rivier_clean.rdata"))
bewaar_r2(verstoring_sem_nat_sv_rivier_clean, "mafy", "rivier", "verstoring")

load(file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "verstoring_sem_nat_sv_rivier_clean.rdata"))

# SAC test
model <- verstoring_clean
# 1. Set glmmTMB to simulate conditionally on fitted REs
glmmTMB::set_simcodes(model$obj, val = "fix")
# 2. Run DHARMa as usual
res <- simulateResiduals(fittedModel = model)
glmmTMB::set_simcodes(model$obj, val = "random")

locaties_match <- dredge_data %>%
  group_by(meetplaats) %>%
  summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
  arrange(meetplaats) # DHARMa sorteert groepen standaard op naam/factor level

# 3. Aggregeer residuen
res_grouped <- recalculateResiduals(res, group = dredge_data$meetplaats)

# 4. De test
testSpatialAutocorrelation(res_grouped,
                           x = locaties_match$x,
                           y = locaties_match$y)

model_list <- list(
  verstoring_index = verstoring_clean
)

# Loop voor validatie
for (name in names(model_list)) {
  cat("\n--- Diagnostiek voor model:", name, "---\n")

  # 1. Bereken residuen (simulatie-gebaseerd)
  res <- simulateResiduals(fittedModel = model_list[[name]], n = 1000)

  # 2. Plot residuen (QQ-plot en Residual vs Predicted)
  plot(res)
  title(main = paste("DHARMa residuals:", name), line = 2)

  # # 3. Test specifiek op overdispersie en outliers
  # print(testDispersion(res))
  # print(testOutliers(res))
}

coefs_filled <- standardize_psem(verstoring_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mafy_rivier_v.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mafy_nat_sv_verstoring_rivier.html"), selfcontained = TRUE)


# #################################
# # Typespecificiteit -> geen variatie!!!!
# ##################################
#
# sem_typspec_rivier <- psem(
#   typspec_best_model_rivier,
#   ntot_best_model_rivier,
#   ptot_best_model_rivier,
#   czv_best_model_rivier,
#   zs_best_model_rivier,
#   ec20_best_model_rivier
# )
#
# summary(sem_typspec_rivier)
#
# # model updaten op basis van dSepS
# typspec_best_model_updated <- update(typspec_best_model_rivier, . ~ . )
# ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
# ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . )
# czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )
# ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
# zs_best_model_updated <- update(zs_best_model_rivier, . ~ .)
# # secchi_best_model_updated <- update(secchi_best_model_rivier, . ~ . )
#
# # het optimale model clean maken!
# f_typspec  <- formula(typspec_best_model_updated)
# f_ntot  <- formula(ntot_best_model_updated)
# f_ptot  <- formula(ptot_best_model_updated)
# f_czv   <- formula(czv_best_model_updated)
# f_ec20  <- formula(ec20_best_model_updated)
# f_zs    <- formula(zs_best_model_updated)
# # f_secchi     <- formula(secchi_best_model_updated)
#
# typspec_clean <- glmmTMB(
#   formula     = f_typspec,
#   data        = dredge_data,
#   family      = ordbeta()
# )
#
# ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
# ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
# czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
# # o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
# ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
# # secchi_clean     <- glmmTMB(f_secchi,     data = dredge_data, family = gaussian())
#
# typspec_sem_nat_sv_rivier_clean <- psem(
#   typspec_clean,
#   ntot_clean,
#   ptot_clean,
#   czv_clean,
#   # o2_clean,
#   ec20_clean,
#   zs_clean,
#   n_t_log %~~% p_t_log,
#   czv_log %~~% p_t_log,
#   n_t_log %~~% czv_log,
#   ec_20_log %~~% czv_log)
#
# summary(typspec_sem_nat_sv_rivier_clean)
#
# save(typspec_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "typspec_sem_nat_sv_rivier_clean.rdata"))
#
#
# # SAC test
# model <- typspec_clean
# # 1. Set glmmTMB to simulate conditionally on fitted REs
# glmmTMB::set_simcodes(model$obj, val = "fix")
# # 2. Run DHARMa as usual
# res <- simulateResiduals(fittedModel = model)
# glmmTMB::set_simcodes(model$obj, val = "random")
#
# locaties_match <- dredge_data %>%
#   group_by(meetplaats) %>%
#   summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
#   arrange(meetplaats) # DHARMa sorteert groepen standaard op naam/factor level
#
# # 3. Aggregeer residuen
# res_grouped <- recalculateResiduals(res, group = dredge_data$meetplaats)
#
# # 4. De test
# testSpatialAutocorrelation(res_grouped,
#                            x = locaties_match$x,
#                            y = locaties_match$y)
#
# model_list <- list(
#   typspec = typspec_clean
# )
#
# # Loop voor validatie
# for (name in names(model_list)) {
#   cat("\n--- Diagnostiek voor model:", name, "---\n")
#
#   # 1. Bereken residuen (simulatie-gebaseerd)
#   res <- simulateResiduals(fittedModel = model_list[[name]], n = 1000)
#
#   # 2. Plot residuen (QQ-plot en Residual vs Predicted)
#   plot(res)
#   title(main = paste("DHARMa residuals:", name), line = 2)
#
#   # # 3. Test specifiek op overdispersie en outliers
#   # print(testDispersion(res))
#   # print(testOutliers(res))
# }
#
# coefs_filled <- standardize_psem(typspec_sem_nat_sv_rivier_clean)[,-9]
#
# source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
# saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mafy_nat_sv_typspec.html"), selfcontained = TRUE)

###########################
# Groeivorm
###########################

sem_groeivorm_rivier <- psem(
  groeivorm_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  zs_best_model_rivier,
  ec20_best_model_rivier
)

summary(sem_groeivorm_rivier)

# model updaten op basis van dSepS
groeivorm_best_model_updated <- update(groeivorm_best_model_rivier, . ~ . + ekc2_waterlichaam_s + intensiteit_combo_oever_s)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ .  )
zs_best_model_updated <- update(zs_best_model_rivier, . ~ .)
# secchi_best_model_updated <- update(secchi_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_groeivorm  <- formula(groeivorm_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv   <- formula(czv_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_zs    <- formula(zs_best_model_updated)
# f_secchi     <- formula(secchi_best_model_updated)

groeivorm_clean <- glmmTMB(
  formula     = f_groeivorm,
  data        = dredge_data,
  family      = ordbeta()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
# o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
# secchi_clean     <- glmmTMB(f_secchi,     data = dredge_data, family = gaussian())

groeivorm_sem_nat_sv_rivier_clean <- psem(
  groeivorm_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  # o2_clean,
  ec20_clean,
  zs_clean,
  n_t_log %~~% p_t_log,
  czv_log %~~% p_t_log,
  n_t_log %~~% czv_log)

summary(groeivorm_sem_nat_sv_rivier_clean)

save(groeivorm_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "groeivorm_sem_nat_sv_rivier_clean.rdata"))
bewaar_r2(groeivorm_sem_nat_sv_rivier_clean,  "mafy", "rivier", "groeivorm")

load(file = here("source", "analyse", "sem", "mafy_nat_sv_rivier", "groeivorm_sem_nat_sv_rivier_clean.rdata"))
# SAC test
model <- groeivorm_clean
# 1. Set glmmTMB to simulate conditionally on fitted REs
glmmTMB::set_simcodes(model$obj, val = "fix")
# 2. Run DHARMa as usual
res <- simulateResiduals(fittedModel = model)
glmmTMB::set_simcodes(model$obj, val = "random")

locaties_match <- dredge_data %>%
  group_by(meetplaats) %>%
  summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
  arrange(meetplaats) # DHARMa sorteert groepen standaard op naam/factor level

# 3. Aggregeer residuen
res_grouped <- recalculateResiduals(res, group = dredge_data$meetplaats)

# 4. De test
testSpatialAutocorrelation(res_grouped,
                           x = locaties_match$x,
                           y = locaties_match$y)

model_list <- list(
  groeivorm = groeivorm_clean
)

# Loop voor validatie
for (name in names(model_list)) {
  cat("\n--- Diagnostiek voor model:", name, "---\n")

  # 1. Bereken residuen (simulatie-gebaseerd)
  res <- simulateResiduals(fittedModel = model_list[[name]], n = 1000)

  # 2. Plot residuen (QQ-plot en Residual vs Predicted)
  plot(res)
  title(main = paste("DHARMa residuals:", name), line = 2)

  # # 3. Test specifiek op overdispersie en outliers
  # print(testDispersion(res))
  # print(testOutliers(res))
}

coefs_filled <- standardize_psem(groeivorm_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mafy_rivier_gv.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mafy_nat_sv_groeivorm_rivier.html"), selfcontained = TRUE)


########
# SAC: spatial autocorrelation
#######

psems_mafy_rivier <- list(ekc_mafy = mafy_index_sem_nat_sv_rivier_clean, v = verstoring_sem_nat_sv_rivier_clean,
                          gv = groeivorm_sem_nat_sv_rivier_clean)
sac_tab <- sac_psem_set(psems_mafy_rivier, data = dredge_data)
plot_sac_overview(sac_tab)                      # tegelplot: I per model en test, * = BH-significant

pl <- sac_plan(sac_tab)                         # eerst bekijken wat er gedraaid wordt
pl                                              # run_site / run_owl zijn aan te passen

fu <- sac_followup(sac_tab, psems_mafy_rivier, dredge_data,
                   plan = pl, out_dir = "output/figuren/sac/sac_mafy_rivier")
fu$resultaten    # één rij per model en schaal, met oordeel
fu$coef          # alle coëfficiënten, iid versus ruimtelijk
fu$zin           # concepttekst voor de methodesectie

# bekijk welke estimates van significantie veranderden
fu$coef %>%
  filter(sig_change | sign_change) %>%
  select(model, term, est_iid, est_sp, se_iid, se_sp, p_iid, p_sp, shift_se)

fu$coef %>% filter(sig_change | sign_change) %>%
  select(model, term, est_iid, est_sp, p_iid, p_sp)





