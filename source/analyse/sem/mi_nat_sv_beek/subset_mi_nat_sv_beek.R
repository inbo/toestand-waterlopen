# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# ==============================================================================
# STAP 3: TOEPASSING OP JOUW DATA (Workflow)
# ==============================================================================

# 2. Kies je subset (bvb Natuurlijke Beken)
# -----------------------------------------------------------
data_subset <- mi_nat_sv_beek %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl, geom, vhag,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw, fdisp_full, fdisp_habitat, fdisp_waterkwaliteit,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs, cl, spear_pesticides, spear_tu_estimated,
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
                fdisp_waterkwaliteit_s = fdisp_waterkwaliteit / (max(fdisp_waterkwaliteit, na.rm = TRUE) + 0.01),
                bekken = as.factor(bekken),
                maand = as.factor(lubridate::month(monsternamedatum)),
                vhag = as.factor(vhag),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                zs_log = log(zs),
                ec_20_log = log(ec_20),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1))

# 1. Definieer je ruwe lijsten met variabelen (nog niet gefilterd)
# -----------------------------------------------------------
raw_responses <- c("mmif", "ta_xw", "ns_tw", "mt_sw", "ep_tw", "sw_dw", "stress_prop")

raw_fysico        <- c("t_s",  "o2_s", "ec_20_log", "zs_log") # o2_verz_s weg en keuze o2 "p_h_s",
raw_nutrients     <- c("czv_log", "n_t_log", "no2_log", "no3_log", "nh4_log", "p_t_log")
raw_hydmo         <- c("breedte_diepte_ratio_s", "sinuositeit_s", "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s", "ekc2_traject_s", "stroomsnelheid_s")
raw_landuse <- c("verharding_afstr_s", "natuur_afstr_s", "intensiteit_combo_afstr_s", "verharding_oever_s", "natuur_oever_s", "intensiteit_combo_oeverzone_s")
raw_lozingen      <- c("lozingen_rwzi_ie_log", "lozingen_riool_ie_log", "lozingen_industrie_ie_log", "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log")
raw_klimaat       <- c("spei6_s", "n_extreme_3m_s", "p_sum_7d_s")

# ==============================================================================
# FUNCTIE: MAAK EEN MOOI CORRELOGRAM
# ==============================================================================

plot_groep_correlogram(data_subset, raw_responses, "Responsen (Beken)")

plot_groep_correlogram(data_subset, raw_fysico, "Fysico-chemie basisvariabelen (Beken)")
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
# plot_groep_correlogram(data_subset, raw_all, "all (Beken)")

clean_all <- filter_collinear_vars(data_subset, raw_all)


dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, vhag,
    mmif, ept_prop, ep_tw, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, ec_20, o2_verz_s, cl_s, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  )
dredge_data_beek <- dredge_data

save(dredge_data_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "dredge_data_beek.rdata"))

dredge_data_fd <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, owl, jaar,
    fdisp_waterkwaliteit, fdisp_habitat, fdisp_full, fdisp_2, fdisp_s, fdisp_waterkwaliteit_s,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit

vis_miss(data_subset %>%
           select(
             meetplaats, monsternamedatum, jaar_s, bekken,
             mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
             n_t_log, p_t_log, czv_log,
             ekc2_waterlichaam_s,
             all_of(clean_klimaat),
             all_of(clean_lozingen),
             all_of(clean_landuse),
             all_of(clean_fysico),
           ))

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

save(mmif_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "mmif_best_model_beek.rdata"))

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

save(ept_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "ept_best_model_beek.rdata"))

plot_model_vif(ept_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten EPT count
################################################################################
# y_var <- "ep_tw"
# predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")
#
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = poisson)
#
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# ept1_dredge <- dredge_model
#
# best_model_ML <- get.models(ept1_dredge, subset = 1)[[1]]
#
# ept1_best_model_beek <- update(best_model_ML, REML = TRUE)
#
# summary(ept1_best_model_beek)
#
# plot_model_vif(ept1_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

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

save(swd_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "swd_best_model_beek.rdata"))

plot_model_vif(swd_best_model_beek, "VIF Check")

################################################################################
# model fitten stikstof
################################################################################

y_var <- "n_t_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

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

save(ntot_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "ntot_best_model_beek.rdata"))

plot_model_vif(ntot_best_model_beek, "VIF Check")

################################################################################
# model fitten fosfor
################################################################################
y_var <- "p_t_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

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

ptot_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_beek)

save(ptot_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "ptot_best_model_beek.rdata"))

plot_model_vif(ptot_best_model_beek, "VIF Check")

################################################################################
# model fitten o2
################################################################################
y_var <- "o2_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "t_s", "ec_20_log")

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

save(o2_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "o2_best_model_beek.rdata"))

plot_model_vif(o2_best_model_beek, "VIF Check")

################################################################################
# model fitten czv
################################################################################

y_var <- "czv_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

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

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_beek)

save(czv_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "czv_best_model_beek.rdata"))

plot_model_vif(czv_best_model_beek, "VIF Check")

################################################################################
# model fitten ec20
################################################################################

y_var <- "ec_20_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

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

save(ec20_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "ec20_best_model_beek.rdata"))

plot_model_vif(ec20_best_model_beek, "VIF Check")

################################################################################
# model fitten T
################################################################################

y_var <- "t_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

t_dredge <- dredge_model

best_model_ML <- get.models(t_dredge, subset = 1)[[1]]

t_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(t_best_model_beek)

plot_model_vif(t_best_model_beek, "VIF Check")
################################################################################
# model fitten zs
################################################################################

y_var <- "zs_log"
predictors <- c(clean_klimaat, clean_landuse, clean_lozingen, "ekc2_waterlichaam_s")

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

zs_dredge <- dredge_model

best_model_ML <- get.models(zs_dredge, subset = 1)[[1]]

zs_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(zs_best_model_beek)

save(zs_best_model_beek, file = here("source", "analyse", "sem", "dredge_output", "zs_best_model_beek.rdata"))

plot_model_vif(zs_best_model_beek, "VIF Check")

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
  ec20_best_model_beek,
  zs_best_model_beek
)

summary(sem_mmif_beek)

# model updaten op basis van dSepS
mmif_best_model_updated <- update(mmif_best_model_beek, . ~ . + intensiteit_combo_afstr_s + n_t_log + p_t_log + o2_s + verharding_afstr_s)

ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . )
+ spei6_s)

ptot_best_model_updated <- update(ptot_best_model_beek, . ~ .)

# + lozingen_riool_ie_log  + lozingen_rwzi_ie_log)
# czv_best_model_updated <- update(czv_best_model_beek, . ~ . - p_t_log - zs_log + intensiteit_combo_afstr_s + t_s)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + verharding_afstr_s)

o2_best_model_updated <- update(o2_best_model_beek, . ~ . - (1 | meetplaats))
# + p_sum_7d_s  # singularity
# t_best_model_updated <-  update(t_best_model_beek, . ~ . - (1 | meetplaats)) # singularity
zs_best_model_updated <- update(zs_best_model_beek, . ~ .)

mmif_sem_nat_sv_beek <- psem(mmif_best_model_updated,
                 ntot_best_model_updated,
                 ptot_best_model_updated,
                 # czv_best_model_updated,
                 o2_best_model_updated,
                 ec20_best_model_updated,
                 # t_best_model_updated,
                 zs_best_model_updated,
                 n_t_log %~~% p_t_log,
                 n_t_log %~~% t_s,
                 p_t_log %~~% t_s,
                 ec_20_log %~~% t_s,
                 zs_log %~~% t_s,
                 p_t_log %~~% ec_20_log
)
summary(mmif_sem_nat_sv_beek)

# het optimale model clean maken!
f_mmif  <- formula(mmif_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
# f_czv   <- formula(czv_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
# f_t     <- formula(t_best_model_updated)
f_zs     <- formula(zs_best_model_updated)

mmif_clean <- glmmTMB(
  formula     = f_mmif,
  data        = dredge_data,
  family      = ordbeta()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
# czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())

mmif_sem_nat_sv_beek_clean <- psem(
  mmif_clean,
  ntot_clean,
  ptot_clean,
  # czv_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  zs_clean,
  n_t_log %~~% p_t_log,
  n_t_log %~~% t_s,
  p_t_log %~~% t_s,
  ec_20_log %~~% t_s,
  zs_log %~~% t_s,
  p_t_log %~~% ec_20_log
)

mmif_sem_nat_sv_beek_clean %>% summary

save(mmif_sem_nat_sv_beek_clean, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "mmif_sem_nat_sv_beek_clean.rdata"))

load(file = here("source", "analyse", "sem", "mi_nat_sv_beek", "mmif_sem_nat_sv_beek_clean.rdata"))


# SAC test
model <- mmif_clean
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

library(DHARMa)
library(ggplot2)

# Zet je modellen in een lijst
model_list <- list(
  mmif = mmif_best_model_updated,
  ntot = ntot_best_model_updated,
  ptot = ptot_best_model_updated,
  # czv  = czv_best_model_updated,
  ec20 = ec20_best_model_updated,
  o2   = o2_best_model_updated,
  # t = t_best_model_updated,
  zs = zs_best_model_updated
)

# Loop voor validatie
for (name in names(model_list)) {
  cat("\n--- Diagnostiek voor model:", name, "---\n")

  # 1. Bereken residuen (simulatie-gebaseerd)
  res <- simulateResiduals(fittedModel = model_list[[name]], n = 1000)

  # 2. Plot residuen (QQ-plot en Residual vs Predicted)
  plot(res)
  title(main = paste("DHARMa residuals:", name), line = 2)

  # 3. Test specifiek op overdispersie en outliers
  print(testDispersion(res))
  print(testOutliers(res))
}

load(file = here("source", "analyse", "sem", "mi_nat_sv_beek", "mmif_sem_nat_sv_beek_clean.rdata"))


coefs_filled <- standardize_psem(mmif_sem_nat_sv_beek_clean)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten

ggsave(
  filename =  here("output", "figuren", "SEM_mi_nat_sv_beek_mmif.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_mmif.html"), selfcontained = TRUE)

#############################
# EPT
#############################
sem_ept_beek <- psem(
  ept_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  # czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek,
  # t_best_model_beek,
  zs_best_model_beek
)

summary(sem_ept_beek)
# updaten

ept_best_model_updated <- update(ept_best_model_beek, . ~ . + p_t_log + n_t_log - (1 | meetplaats))


ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . )
+ spei6_s)

ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . )

ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + verharding_afstr_s)

o2_best_model_updated <- update(o2_best_model_beek, . ~ . - (1|meetplaats))

# t_best_model_updated <- update(t_best_model_beek, . ~ . - (1|meetplaats))

zs_best_model_updated <- update(zs_best_model_beek, . ~ .)

ept_sem_nat_sv_beek <- psem(ept_best_model_updated,
                             ntot_best_model_updated,
                             ptot_best_model_updated,
                             o2_best_model_updated,
                             ec20_best_model_updated,
                            # t_best_model_updated,
                            zs_best_model_updated,
                            n_t_log %~~% p_t_log,
                            zs_log %~~% t_s,
                            n_t_log %~~% t_s,
                            p_t_log %~~% t_s,
                            ec_20_log %~~% t_s,
                            p_t_log %~~% ec_20_log)

summary(ept_sem_nat_sv_beek)
r.squaredGLMM(ept_best_model_updated)


save(ept_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "ept_sem_nat_sv_beek.rdata"))

# het optimale model clean maken!
f_ept  <- formula(ept_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_zs   <- formula(zs_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
# f_t     <- formula(t_best_model_updated)


ept_clean <- glmmTMB(
  formula     = f_ept,
  data        = dredge_data,
  family = binomial(link = "logit"),
  weights = dredge_data$ta_xw
)
ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
zs_clean   <- glmmTMB(f_zs,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian()) # Nu zonder singularity-ruis
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian()) # Nu zonder singularity-ruis

ept_sem_nat_sv_beek_clean <- psem(
  ept_clean,
  ntot_clean,
  ptot_clean,
  zs_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  n_t_log %~~% p_t_log,
  zs_log %~~% t_s,
  n_t_log %~~% t_s,
  p_t_log %~~% t_s,
  ec_20_log %~~% t_s,
  p_t_log %~~% ec_20_log)

ept_sem_nat_sv_beek_clean %>% summary

save(ept_sem_nat_sv_beek_clean, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "ept_sem_nat_sv_beek_clean.rdata"))

load(file = here("source", "analyse", "sem", "mi_nat_sv_beek", "ept_sem_nat_sv_beek_clean.rdata"))

library(DHARMa)
library(ggplot2)

# Zet je modellen in een lijst
model_list <- list(
  ept = ept_clean,
  ntot = ntot_clean,
  ptot = ptot_clean,
  zs  = zs_clean,
  ec20 = ec20_clean,
  o2   = o2_clean,
  # t = t_clean
)

# Loop voor validatie
for (name in names(model_list)) {
  cat("\n--- Diagnostiek voor model:", name, "---\n")

  # 1. Bereken residuen (simulatie-gebaseerd)
  res <- simulateResiduals(fittedModel = model_list[[name]], n = 1000)

  # 2. Plot residuen (QQ-plot en Residual vs Predicted)
  plot(res)
  title(main = paste("DHARMa residuals:", name), line = 2)

  # 3. Test specifiek op overdispersie en outliers
  print(testDispersion(res))
  print(testOutliers(res))
}


coefs_filled <- standardize_psem(ept_sem_nat_sv_beek_clean)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten

ggsave(
  filename =  here("output", "figuren", "SEM_mi_nat_sv_beek_ept.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)

source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_ept.html"), selfcontained = TRUE)

# SAC test
model <- ept_clean
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




## swd

sem_swd_beek <- psem(
  swd_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  # czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek,
  zs_best_model_beek
)

summary(sem_swd_beek)
r.squaredGLMM(swd_best_model_beek)
# updaten

swd_best_model_updated <- update(swd_best_model_beek, . ~ . + intensiteit_combo_afstr_s + spei6_s + n_t_log + p_t_log)

ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . )
# czv_best_model_updated <- update(czv_best_model_beek, . ~ . + t_s - p_t_log + intensiteit_combo_afstr_s)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ .  + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_beek, . ~ . - (1|meetplaats))

swd_sem_nat_sv_beek <- psem(swd_best_model_updated,
                            ntot_best_model_updated,
                            ptot_best_model_updated,
                            # czv_best_model_updated,
                            o2_best_model_updated,
                            ec20_best_model_updated,
                            n_t_log %~~% p_t_log,
                            t_s %~~% n_t_log,
                            t_s %~~% p_t_log,
                            t_s %~~% ec_20_log,
                            t_s %~~% zs_log,
                            p_t_log %~~% ec_20_log)
summary(swd_sem_nat_sv_beek)

# het optimale model clean maken!
f_swd  <- formula(swd_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_zs   <- formula(zs_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
# f_t     <- formula(t_best_model_updated)


swd_clean <- glmmTMB(
  formula     = f_swd,
  data        = dredge_data,
  family = gaussian
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
zs_clean   <- glmmTMB(f_zs,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian()) # Nu zonder singularity-ruis
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian()) # Nu zonder singularity-ruis

swd_sem_nat_sv_beek_clean <- psem(
  swd_clean,
  ntot_clean,
  ptot_clean,
  zs_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  n_t_log %~~% p_t_log,
  t_s %~~% n_t_log,
  t_s %~~% p_t_log,
  t_s %~~% ec_20_log,
  t_s %~~% zs_log,
  p_t_log %~~% ec_20_log)

swd_sem_nat_sv_beek_clean %>% summary

save(swd_sem_nat_sv_beek_clean, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "swd_sem_nat_sv_beek_clean.rdata"))

# SAC test
model <- swd_clean
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

coefs_filled <- standardize_psem(swd_sem_nat_sv_beek_clean)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_swd.html"), selfcontained = TRUE)


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
# test model full dredge MMIF
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
                 family = gaussian)

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
model <- glmmTMB(data = dredge_data_fd, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge_fd.R"))

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
model <- glmmTMB(data = dredge_data_fd, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge_fd.R"))

ptot_dredge <- dredge_model

best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

ptot_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_beek)

plot_model_vif(ptot_best_model_beek, "VIF Check")

################################################################################
# model fitten o2
################################################################################
y_var <- "o2_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "czv_log", "t_s", "ec_20_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data_fd, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge_fd.R"))

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
model <- glmmTMB(data = dredge_data_fd, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge_fd.R"))

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_beek)

plot_model_vif(czv_best_model_beek, "VIF Check")

################################################################################
# model fitten ec20
################################################################################

y_var <- "ec_20_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data_fd, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge_fd.R"))

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_beek)

plot_model_vif(ec20_best_model_beek, "VIF Check")


#######################"""
sem_fdisp_beek <- psem(
  fdisp_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  o2_best_model_beek,
  ec20_best_model_beek
)
summary(sem_fdisp_beek)

fdisp_best_model_updated <- update(fdisp_best_model_beek, . ~ . + n_t_log + p_t_log + intensiteit_combo_afstr_s + lozingen_riool_ie_log)
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ . + t_s)
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . + t_s + lozingen_riool_ie_log)
czv_best_model_updated <- update(czv_best_model_beek, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + intensiteit_combo_afstr_s + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_beek, . ~ . + p_sum_7d_s - (1 | meetplaats))

fdisp_sem_nat_sv_beek <- psem(fdisp_best_model_updated,
                            ntot_best_model_updated,
                            ptot_best_model_updated,
                            czv_best_model_updated,
                            o2_best_model_updated,
                            ec20_best_model_updated,
                            n_t_log %~~% p_t_log,
                            ec_20_log %~~% p_t_log)
summary(fdisp_sem_nat_sv_beek)
