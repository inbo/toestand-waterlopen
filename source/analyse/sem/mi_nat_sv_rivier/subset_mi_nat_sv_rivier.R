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
data_subset <- mi_nat_sv_rivier %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw,
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
raw_fysico        <- c("t_s", "p_h_s", "o2_s", "ec_20_s", "zs_s") # o2_verz_s weg en keuze o2
raw_nutrients     <- c("czv_log", "n_t_log", "no2_log", "no3_log", "nh4_log", "p_t_log")
raw_hydmo         <- c("breedte_diepte_ratio_s", "sinuositeit_s", "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s", "ekc2_traject_s", "stroomsnelheid_s")
raw_landuse <- c("verharding_afstr_s", "natuur_afstr_s", "intensiteit_combo_afstr_s", "verharding_oever_s", "natuur_oever_s", "intensiteit_combo_oeverzone_s")
raw_lozingen      <- c("lozingen_rwzi_ie_log", "lozingen_riool_ie_log", "lozingen_industrie_ie_log", "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log")
raw_klimaat       <- c("spei6_s", "n_extreme_3m_s", "p_sum_7d_s")

# ==============================================================================
# FUNCTIE: MAAK EEN MOOI CORRELOGRAM
# ==============================================================================

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

data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  vis_miss()

data_subset2 <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit

dredge_data <- data_subset2

################################################################################
# model fitten MMIF
################################################################################

y_var <- "mmif"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

mmif_dredge <- dredge_model

best_model_ML <- get.models(mmif_dredge, subset = 1)[[1]]

mmif_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(mmif_best_model_rivier)

plot_model_vif(mmif_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten EPT
################################################################################
y_var <- "ept_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

ept_dredge <- dredge_model

best_model_ML <- get.models(ept_dredge, subset = 1)[[1]]

ept_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ept_best_model_rivier)

plot_model_vif(ept_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten tax
################################################################################
y_var <- "ta_xw"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = poisson)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

tax_dredge <- dredge_model

best_model_ML <- get.models(tax_dredge, subset = 1)[[1]]

tax_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(tax_best_model_rivier)

plot_model_vif(tax_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten nst
################################################################################

y_var <- "nst_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

nst_dredge <- dredge_model

best_model_ML <- get.models(nst_dredge, subset = 1)[[1]]

nst_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(nst_best_model_rivier)

plot_model_vif(nst_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten stress
################################################################################
y_var <- "stress_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

stress_dredge <- dredge_model

best_model_ML <- get.models(stress_dredge, subset = 1)[[1]]

stress_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(stress_best_model_rivier)

plot_model_vif(stress_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten mts
################################################################################
y_var <- "mt_sw_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

mts_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(mts_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
mts_best_model_rivier <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(mts_best_model_rivier)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_rivier, plot = TRUE)

plot_model_vif(mts_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten swd
################################################################################
y_var <- "sw_dw"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

swd_dredge <- dredge_model

best_model_ML <- get.models(swd_dredge, subset = 1)[[1]]

swd_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(swd_best_model_rivier)

plot_model_vif(swd_best_model_rivier, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

ntot_dredge <- dredge_model

best_model_ML <- get.models(ntot_dredge, subset = 1)[[1]]

ntot_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ntot_best_model_rivier)

plot_model_vif(ntot_best_model_rivier, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

ptot_dredge <- dredge_model

best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

ptot_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_rivier)

plot_model_vif(ptot_best_model_rivier, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

o2_dredge <- dredge_model

best_model_ML <- get.models(o2_dredge, subset = 1)[[1]]

o2_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(o2_best_model_rivier)

plot_model_vif(o2_best_model_rivier, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_rivier)

plot_model_vif(czv_best_model_rivier, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge_rivier.R"))

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_rivier)

plot_model_vif(ec20_best_model_rivier, "VIF Check")

################################################################################
# sem models fitten
################################################################################

# MMIF

sem_mmif_rivier <- psem(
  mmif_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier
)

summary(sem_mmif_rivier)

# model updaten op basis van dSepS
mmif_best_model_updated <- update(mmif_best_model_rivier, . ~ . + spei6_s + p_t_log + o2_s + n_extreme_3m_s)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . + intensiteit_combo_afstr_s + n_extreme_3m_s)
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + intensiteit_combo_afstr_s)
czv_best_model_updated <- czv_best_model_rivier
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + n_extreme_3m_s + spei6_s)

mmif_sem_nat_sv_rivier <- psem(mmif_best_model_updated,
                               ntot_best_model_updated,
                               ptot_best_model_updated,
                               czv_best_model_updated,
                               o2_best_model_updated,
                               ec20_best_model_updated,
                               n_t_log %~~% p_t_log,
                               n_t_log %~~% ec_20_s)
summary(mmif_sem_nat_sv_rivier)

save(mmif_sem_nat_sv_rivier, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "mmif_sem_nat_sv_rivier.rdata"))

sem_resultaat <- mmif_sem_nat_sv_rivier
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

# EPT

sem_ept_rivier <- psem(
  ept_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier
)

summary(sem_ept_rivier)
# updaten

ept_best_model_updated <- update(ept_best_model_rivier, . ~ . + czv_log)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . + intensiteit_combo_afstr_s + n_extreme_3m_s)
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + intensiteit_combo_afstr_s)
czv_best_model_updated <- czv_best_model_rivier
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + n_extreme_3m_s + spei6_s)

ept_sem_nat_sv_rivier <- psem(ept_best_model_updated,
                              ntot_best_model_updated,
                              ptot_best_model_updated,
                              czv_best_model_updated,
                              o2_best_model_updated,
                              ec20_best_model_updated,
                              n_t_log %~~% p_t_log,
                              n_t_log %~~% ec_20_s)
summary(ept_sem_nat_sv_rivier)

save(ept_sem_nat_sv_rivier, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "ept_sem_nat_sv_rivier.rdata"))

sem_resultaat <- ept_sem_nat_sv_rivier
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

## swd

sem_swd_rivier <- psem(
  swd_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier
)

summary(sem_swd_rivier)

# updaten

swd_best_model_updated <- update(swd_best_model_rivier, . ~ . + o2_s + p_t_log)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . +  ekc2_waterlichaam_s + n_extreme_3m_s + intensiteit_combo_afstr_s +  verharding_afstr_s)
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + lozingen_riool_ie_log)
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . + spei6_s + t_s)
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . + spei6_s)
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . +  spei6_s + p_t_log + czv_log + n_extreme_3m_s + overstorten_blootstelling_index_log)

swd_sem_nat_sv_rivier <- psem(swd_best_model_updated,
                              ntot_best_model_updated,
                              ptot_best_model_updated,
                              czv_best_model_updated,
                              o2_best_model_updated,
                              ec_20_best_model_updated,
                              n_t_log %~~% p_t_log,
                              czv_log %~~% n_t_log)
summary(swd_sem_nat_sv_rivier)

save(swd_sem_nat_sv_rivier, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "swd_sem_nat_sv_rivier.rdata"))

sem_resultaat <- swd_sem_nat_sv_rivier
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

