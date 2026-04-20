# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))

# ==============================================================================
# STAP 3: TOEPASSING OP JOUW DATA (Workflow)
# ==============================================================================

# 2. Kies je subset (bvb Natuurlijke Beken)
# -----------------------------------------------------------
data_subset <- mi_nat_sv_beek %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw, fdisp_full, fdisp_habitat, fdisp_waterkwaliteit,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone,
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
                fdisp_2 = fdisp_full^2,
                fdisp_s = fdisp_full / max(fdisp_full, na.rm = TRUE),
                fdisp_waterkwaliteit_s = fdisp_waterkwaliteit / (max(fdisp_waterkwaliteit, na.rm = TRUE)+ 0.01),
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
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1))

# 1. Definieer je ruwe lijsten met variabelen (nog niet gefilterd)
# -----------------------------------------------------------
raw_responses <- c("mmif", "ta_xw", "ns_tw", "mt_sw", "ep_tw", "sw_dw", "stress_prop")

raw_fysico        <- c("t_s", "p_h_s", "o2_s", "ec_20_s", "zs_s") # o2_verz_s weg en keuze o2
raw_nutrients     <- c("czv_log", "n_t_log", "no2_log", "no3_log", "nh4_log", "p_t_log")
raw_hydmo         <- c("breedte_diepte_ratio_s", "sinuositeit_s", "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s", "ekc2_traject_s", "stroomsnelheid_s")
raw_landuse <- c("verharding_afstr_s", "natuur_afstr_s", "intensiteit_combo_afstr_s", "verharding_oever_s", "natuur_oever_s", "intensiteit_combo_oeverzone_s")
raw_lozingen      <- c("lozingen_rwzi_ie_log", "lozingen_riool_ie_log", "lozingen_industrie_ie_log", "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log")
raw_klimaat       <- c("spei6_s", "n_extreme_3m_s", "p_sum_7d_s")

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
clean_landuse <- filter_collinear_vars(data_subset, raw_landuse)
clean_hydmo <- filter_collinear_vars(data_subset, raw_hydmo)
# Lozingen en Klimaat zijn vaak orthogonaal genoeg, maar je kan ze ook filteren:
clean_lozingen      <- filter_collinear_vars(data_subset, raw_lozingen)
clean_klimaat       <- filter_collinear_vars(data_subset, raw_klimaat)

raw_all <- c(clean_fysico, clean_nutrients, clean_landuse, clean_hydmo, clean_lozingen, clean_klimaat)
clean_all <- filter_collinear_vars(data_subset, raw_all)

# cat("\n--- Screening voor Biologie (MMIF) ---\n")
# # Stel dat N_T en O2 de belangrijkste chemische variabelen bleken:
# vars_voor_bio <- c(clean_nutrients, clean_fysico, clean_lozingen, clean_hydmo, clean_landuse, clean_klimaat)
#
# res_mmif <- screen_predictors(
#   data = data_subset %>% drop_na,
#   response_var = "mmif",
#   candidate_vars = vars_voor_bio,
#   family = glmmTMB::ordbeta # Of gaussian voor snelle check
# )
# print(head(res_mmif, 10))
#
# data_subset2 <- data_subset %>%
#   na.omit
# res_mmif_no_na <- screen_predictors(
#   data = data_subset2,
#   response_var = "mmif",
#   candidate_vars = clean_all,
#   family = glmmTMB::ordbeta # Of gaussian voor snelle check
# )
# print(head(res_mmif_no_na, 10))
#
#
# hydmo_test <- glmmTMB(mmif ~ breedte_diepte_ratio_s + sinuositeit_s + bodemsub_s + doodhout_s + profiel_s + ekc2_waterlichaam_s + ekc2_traject_s + stroomsnelheid_s, data = data_subset2)

dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit

dredge_data_fd <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken,
    fdisp_waterkwaliteit, fdisp_habitat, fdisp_full, fdisp_2, fdisp_s, fdisp_waterkwaliteit_s,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit

################################################################################
# model fitten MMIF
################################################################################

y_var <- "mmif"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge.R"))

mmif_dredge <- dredge_model

best_model_ML <- get.models(mmif_dredge, subset = 1)[[1]]

mmif_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(mmif_best_model_beek)

plot_model_vif(mmif_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

importance_mmif <- sw(mmif_dredge)

################################################################################
# model fitten EPT
################################################################################
y_var <- "ept_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

source(here("source" , "analyse", "sem", "dredge.R"))

ept_dredge <- dredge_model

best_model_ML <- get.models(ept_dredge, subset = 1)[[1]]

ept_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ept_best_model_beek)

plot_model_vif(ept_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten tax
################################################################################
y_var <- "ta_xw"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = poisson)

source(here("source" , "analyse", "sem", "dredge.R"))

tax_dredge <- dredge_model

best_model_ML <- get.models(tax_dredge, subset = 1)[[1]]

tax_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(tax_best_model_beek)

plot_model_vif(tax_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten nst
################################################################################

y_var <- "nst_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

source(here("source" , "analyse", "sem", "dredge.R"))

nst_dredge <- dredge_model

best_model_ML <- get.models(nst_dredge, subset = 1)[[1]]

nst_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(nst_best_model_beek)

plot_model_vif(nst_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten stress
################################################################################
y_var <- "stress_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

source(here("source" , "analyse", "sem", "dredge.R"))

stress_dredge <- dredge_model

best_model_ML <- get.models(stress_dredge, subset = 1)[[1]]

stress_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(stress_best_model_beek)

plot_model_vif(stress_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten mts
################################################################################
y_var <- "mt_sw_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge.R"))

mts_dredge <- dredge_model

best_model_ML <- get.models(mts_dredge, subset = 1)[[1]]

mts_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(mts_best_model_beek)

plot_model_vif(mts_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten swd
################################################################################
y_var <- "sw_dw"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

swd_dredge <- dredge_model

best_model_ML <- get.models(swd_dredge, subset = 1)[[1]]

swd_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(swd_best_model_beek)

plot_model_vif(swd_best_model_beek, "VIF Check")

################################################################################
# model fitten stikstof
################################################################################

y_var <- "n_t_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

ntot_dredge <- dredge_model

best_model_ML <- get.models(ntot_dredge, subset = 1)[[1]]

ntot_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ntot_best_model_beek)

plot_model_vif(ntot_best_model_beek, "VIF Check")

################################################################################
# model fitten fosfor
################################################################################
y_var <- "p_t_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

ptot_dredge <- dredge_model

best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

ptot_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_beek)

plot_model_vif(ptot_best_model_beek, "VIF Check")

################################################################################
# model fitten o2
################################################################################
y_var <- "o2_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "czv_log", "t_s", "ec_20_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

o2_dredge <- dredge_model

best_model_ML <- get.models(o2_dredge, subset = 1)[[1]]

o2_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(o2_best_model_beek)

plot_model_vif(o2_best_model_beek, "VIF Check")

################################################################################
# model fitten czv
################################################################################

y_var <- "czv_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_beek)

plot_model_vif(czv_best_model_beek, "VIF Check")

################################################################################
# model fitten ec20
################################################################################

y_var <- "ec_20_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_beek)

plot_model_vif(ec20_best_model_beek, "VIF Check")

################################################################################
# sem models fitten
################################################################################

# MMIF

sem_mmif_beek <- psem(
  mmif_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_mmif_beek)

# model updaten op basis van dSepS
mmif_best_model_updated <- update(mmif_best_model_beek, . ~ . + intensiteit_combo_afstr_s + t_s + n_t_log + p_t_log + verharding_afstr_s)
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . + t_s + verharding_afstr_s)
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . + lozingen_riool_ie_log + t_s + lozingen_rwzi_ie_log)
czv_best_model_updated <- update(czv_best_model_beek, . ~ . + t_s + intensiteit_combo_afstr_s - p_t_log)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + intensiteit_combo_afstr_s + t_s + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_beek, . ~ . + ec_20_s + p_sum_7d_s)

mmif_sem_nat_sv_beek <- psem(mmif_best_model_updated,
                 ntot_best_model_updated,
                 ptot_best_model_updated,
                 czv_best_model_updated,
                 o2_best_model_updated,
                 ec20_best_model_updated,
                 n_t_log %~~% p_t_log,
                 p_t_log %~~% czv_log,
                 n_t_log %~~% czv_log)
summary(mmif_sem_nat_sv_beek)

save(mmif_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "mmif_sem_nat_sv_beek.rdata"))

sem_resultaat <- mmif_sem_nat_sv_beek
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

# EPT

sem_ept_beek <- psem(
  ept_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_ept_beek)
# updaten

ept_best_model_updated <- update(ept_best_model_beek, . ~ . + czv_log + intensiteit_combo_afstr_s + p_t_log + n_t_log + ec_20_s + verharding_afstr_s)
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . + t_s + verharding_afstr_s)
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . + t_s + lozingen_riool_ie_log + lozingen_rwzi_ie_log)
czv_best_model_updated <- update(czv_best_model_beek, . ~ . + t_s + intensiteit_combo_afstr_s)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + t_s + intensiteit_combo_afstr_s + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_beek, . ~ . + p_sum_7d_s + ec_20_s)

ept_sem_nat_sv_beek <- psem(ept_best_model_updated,
                             ntot_best_model_updated,
                             ptot_best_model_updated,
                             czv_best_model_updated,
                             o2_best_model_updated,
                             ec20_best_model_updated,
                             n_t_log %~~% p_t_log,
                            n_t_log %~~% czv_log)
summary(ept_sem_nat_sv_beek)
r.squaredGLMM(ept_best_model_updated)
save(ept_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "ept_sem_nat_sv_beek.rdata"))

sem_resultaat <- ept_sem_nat_sv_beek
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

## swd

sem_swd_beek <- psem(
  swd_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_swd_beek)
r.squaredGLMM(swd_best_model_beek)
# updaten

swd_best_model_updated <- update(swd_best_model_beek, . ~ . + intensiteit_combo_afstr_s + spei6_s + n_t_log + p_t_log + verharding_afstr_s)
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . + t_s + verharding_afstr_s)
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . + t_s + lozingen_riool_ie_log + lozingen_rwzi_ie_log)
czv_best_model_updated <- update(czv_best_model_beek, . ~ . + t_s - p_t_log + intensiteit_combo_afstr_s)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + t_s + intensiteit_combo_afstr_s + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_beek, . ~ . + ec_20_s + p_sum_7d_s)

swd_sem_nat_sv_beek <- psem(swd_best_model_updated,
                            ntot_best_model_updated,
                            ptot_best_model_updated,
                            czv_best_model_updated,
                            o2_best_model_updated,
                            ec20_best_model_updated,
                            n_t_log %~~% p_t_log,
                            czv_log %~~% n_t_log,
                            czv_log %~~% p_t_log)
summary(swd_sem_nat_sv_beek)

save(swd_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "swd_sem_nat_sv_beek.rdata"))

sem_resultaat <- swd_sem_nat_sv_beek
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

## tax

sem_tax <- psem(
  tax_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_tax)

## nst

sem_nst <- psem(
  nst_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_nst)

## mts

sem_mts <- psem(
  mts_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_mts)


## stress

sem_stress <- psem(
  stress_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)

summary(sem_stress)


################################################################################
# model full dredge MMIF
################################################################################

y_var <- "mmif"
predictors <- c(clean_klimaat, clean_fysico, "ekc2_waterlichaam_s", clean_lozingen, clean_landuse)

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)



# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("dredge_data", "model"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = model,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(2, 8),
  fixed = c("cond(jaar_s)", "cond(o2_s)"),
  trace = 2
)

# STAP 5: Netjes opruimen
stopCluster(mijn_cluster)
options(na.action = "na.omit")

# --- STOP TIMER ---
eind_tijd <- Sys.time()
cat("\n--- KLAAR! ---\n")
cat("Totale rekentijd: ")
print(eind_tijd - start_tijd)
cat("----------------\n\n")

print(head(dredge_model, 10))


mmif_dredge <- dredge_model

best_model_ML <- get.models(mmif_dredge, subset = 1)[[1]]

mmif_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(mmif_best_model_beek)

plot_model_vif(mmif_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

importance_mmif <- sw(mmif_dredge)

################################################################################
# model fitten functionele dispersie
################################################################################
y_var <- "fdisp_waterkwaliteit"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data_fd, formula = formula_obj,
                 REML = FALSE,
                 family = tweedie)

source(here("source" , "analyse", "sem", "dredge_fd.R"))

fdisp_dredge <- dredge_model

best_model_ML <- get.models(fdisp_dredge, subset = 1)[[1]]

fdisp_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(fdisp_best_model_beek)

plot_model_vif(fdisp_best_model_beek, "VIF Check")

simulationOutput <- simulateResiduals(fdisp_best_model_beek, plot = TRUE)
testOutliers(simulationOutput)
testDispersion(simulationOutput)
# testDispersion(simulationOutput) # geen overdispersie
# testZeroInflation(simulationOutput) # geen zero_inflation
# testUniformity(simulationOutput)

################################################################################
# model fitten stikstof
################################################################################

y_var <- "n_t_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

ntot_dredge <- dredge_model

best_model_ML <- get.models(ntot_dredge, subset = 1)[[1]]

ntot_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ntot_best_model_beek)

plot_model_vif(ntot_best_model_beek, "VIF Check")

################################################################################
# model fitten fosfor
################################################################################
y_var <- "p_t_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

ptot_dredge <- dredge_model

best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

ptot_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_beek)

plot_model_vif(ptot_best_model_beek, "VIF Check")

################################################################################
# model fitten o2
################################################################################
y_var <- "o2_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "czv_log", "t_s", "ec_20_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

o2_dredge <- dredge_model

best_model_ML <- get.models(o2_dredge, subset = 1)[[1]]

o2_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(o2_best_model_beek)

plot_model_vif(o2_best_model_beek, "VIF Check")

################################################################################
# model fitten czv
################################################################################

y_var <- "czv_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_beek)

plot_model_vif(czv_best_model_beek, "VIF Check")

################################################################################
# model fitten ec20
################################################################################

y_var <- "ec_20_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_beek)

plot_model_vif(ec20_best_model_beek, "VIF Check")
