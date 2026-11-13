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
data_subset <- mi_nat_sv_rivier %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl, geom, waterloop,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs, cl, ec_20_fc, spear_pesticides, spear_tu_estimated,
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
                maand = as.factor(lubridate::month(monsternamedatum)),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                ec_20_log = log(ec_20),
                zs_log = log(zs),
                cl_log = log(cl),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1))

# 1. Definieer je ruwe lijsten met variabelen (nog niet gefilterd)
# -----------------------------------------------------------
raw_responses <- c("mmif", "ta_xw", "ns_tw", "mt_sw", "ep_tw", "sw_dw", "stress_prop")

raw_fysico        <- c("t_s", "o2_s", "ec_20_log", "zs_log", "cl_log") # o2_verz_s weg en keuze o2"p_h_s"
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

data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, owl,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, cl, czv,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  vis_miss()

data_subset2 <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop, ec_20,
    n_t_log, p_t_log, czv_log, cl_log, cl_s, czv, ec_20_fc, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
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

brak <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, waterloop,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop, ec_20,
    n_t_log, p_t_log, czv_log, cl, czv, ec_20_fc, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit %>%
  filter(cl > 200000) %>%
  st_as_sf() %>%
  mapview()

dredge_data <- data_subset2
dredge_data_rivier <- dredge_data
save(dredge_data_rivier, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "dredge_data_rivier.rdata"))

################################################################################
# model fitten MMIF
################################################################################

y_var <- "mmif"
predictors <- c(clean_klimaat, "t_s", "o2_s", "ec_20_log", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

mmif_dredge <- dredge_model

best_model_ML <- get.models(mmif_dredge, subset = 1)[[1]]

mmif_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(mmif_best_model_rivier)
save(mmif_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mmif_best_model_rivier.rdata"))

plot_model_vif(mmif_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten
sw(mmif_dredge)

################################################################################
# model fitten EPT
################################################################################
y_var <- "ept_prop"
predictors <- c(clean_klimaat, "t_s", "o2_s", "ec_20_log", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|meetplaats) - (1|owl))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

ept_dredge <- dredge_model

best_model_ML <- get.models(ept_dredge, subset = 1)[[1]]

ept_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ept_best_model_rivier)
save(ept_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "ept_best_model_rivier.rdata"))
plot_model_vif(ept_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten tax
################################################################################
y_var <- "ta_xw"
predictors <- c(clean_klimaat, "t_s", "o2_s", "ec_20_log", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = poisson)
performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|owl))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = poisson)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

tax_dredge <- dredge_model

best_model_ML <- get.models(tax_dredge, subset = 1)[[1]]

tax_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(tax_best_model_rivier)
save(tax_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "tax_best_model_rivier.rdata"))
plot_model_vif(tax_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten nst
################################################################################
y_var <- "nst_prop"
predictors <- c(clean_klimaat, "t_s", "o2_s", "ec_20_log", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|meetplaats) - (1|owl))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

nst_dredge <- dredge_model

best_model_ML <- get.models(nst_dredge, subset = 1)[[1]]

nst_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(nst_best_model_rivier)
save(nst_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "nst_best_model_rivier.rdata"))
plot_model_vif(nst_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten


################################################################################
# model fitten stress
################################################################################
# y_var <- "stress_prop"
# predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")
#
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = binomial(link = "logit"),
#                  weights = dredge_data$ta_xw)
#
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# stress_dredge <- dredge_model
#
# best_model_ML <- get.models(stress_dredge, subset = 1)[[1]]
#
# stress_best_model_rivier <- update(best_model_ML, REML = TRUE)
#
# summary(stress_best_model_rivier)
#
# plot_model_vif(stress_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten mts
################################################################################
y_var <- "mt_sw_prop"
predictors <- c(clean_klimaat, "t_s", "o2_s", "ec_20_log", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")

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

source(here("source" , "analyse", "sem", "dredge.R"))

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
save(mts_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "mts_best_model_rivier.rdata"))
# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_rivier, plot = TRUE)

plot_model_vif(mts_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten swd
################################################################################
y_var <- "sw_dw"
predictors <- c(clean_klimaat, "t_s", "o2_s", "ec_20_log", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")
source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|bekken))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

swd_dredge <- dredge_model

best_model_ML <- get.models(swd_dredge, subset = 1)[[1]]

swd_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(swd_best_model_rivier)
save(swd_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "swd_best_model_rivier.rdata"))
plot_model_vif(swd_best_model_rivier, "VIF Check")

################################################################################
# model fitten spear pesticides
################################################################################
#
# y_var <- "spear_pesticides"
# predictors <- c(clean_klimaat, "t_s", "o2_s", "zs_log", "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = ordbeta)
#
# performance::check_singularity(model)
# formula_obj <- update(formula_obj, . ~ . - (1|bekken))
#
# # options(na.action = "na.fail") # Verplicht voor dredge
# # model <- glmmTMB(data = dredge_data, formula = formula_obj,
# #                  REML = FALSE,
# #                  family = gaussian)
# #
# # performance::check_singularity(model)
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# spear_dredge <- dredge_model
#
# best_model_ML <- get.models(spear_dredge, subset = 1)[[1]]
#
# spear_best_model_rivier <- update(best_model_ML, REML = TRUE)
#
# summary(spear_best_model_rivier)
# save(spear_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "spear_best_model_riviers.rdata"))
#
# plot_model_vif(spear_best_model_rivier, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten
# sw(spear_dredge)

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
save(ntot_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "ntot_best_model_rivier.rdata"))
plot_model_vif(ntot_best_model_rivier, "VIF Check")

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

formula_obj <- update(formula_obj, . ~ .  - (1|meetplaats))

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
save(ptot_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "ptot_best_model_rivier.rdata"))
plot_model_vif(ptot_best_model_rivier, "VIF Check")

################################################################################
# model fitten o2
################################################################################
y_var <- "o2_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "t_s", "zs_log", "czv_log", "ec_20_log")

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

o2_dredge <- dredge_model

best_model_ML <- get.models(o2_dredge, subset = 1)[[1]]

o2_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(o2_best_model_rivier)
save(o2_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "o2_best_model_rivier.rdata"))
plot_model_vif(o2_best_model_rivier, "VIF Check")

################################################################################
# model fitten czv
################################################################################

y_var <- "czv_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

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

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_rivier)
save(czv_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "czv_best_model_rivier.rdata"))

plot_model_vif(czv_best_model_rivier, "VIF Check")

################################################################################
# model fitten ec20
################################################################################

y_var <- "ec_20_log"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

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

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_rivier)
save(ec20_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "ec20_best_model_rivier.rdata"))
plot_model_vif(ec20_best_model_rivier, "VIF Check")

################################################################################
# model fitten cl
################################################################################
#
# y_var <- "cl_log"
# predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)
#
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = gaussian)
# performance::check_singularity(model)
#
# formula_obj <- update(formula_obj, . ~ . - (1|meetplaats))
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = gaussian)
#
# performance::check_singularity(model)
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# cl_dredge <- dredge_model
#
# best_model_ML <- get.models(cl_dredge, subset = 1)[[1]]
#
# cl_best_model_rivier <- update(best_model_ML, REML = TRUE)
#
# summary(cl_best_model_rivier)
# save(cl_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "cl_best_model_rivier.rdata"))
#
#
# plot_model_vif(cl_best_model_rivier, "VIF Check")

################################################################################
# model fitten zs
################################################################################

y_var <- "zs_log"
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

zs_dredge <- dredge_model

best_model_ML <- get.models(zs_dredge, subset = 1)[[1]]

zs_best_model_rivier <- update(best_model_ML, REML = TRUE)

summary(zs_best_model_rivier)
save(zs_best_model_rivier, file = here("source", "analyse", "sem", "dredge_output", "zs_best_model_rivier.rdata"))
plot_model_vif(zs_best_model_rivier, "VIF Check")

################################################################################
# sem models fitten
################################################################################

# MMIF

load(file = here("source", "analyse", "sem", "dredge_output", "mmif_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "ept_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "swd_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "tax_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mts_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "nst_best_model_rivier.rdata"))


load(file = here("source", "analyse", "sem", "dredge_output", "ntot_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "ptot_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "czv_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "zs_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "o2_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "cl_best_model_rivier.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "ec20_best_model_rivier.rdata"))

sem_mmif_rivier <- psem(
  mmif_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  o2_best_model_rivier,
  zs_best_model_rivier,
  ec20_best_model_rivier
)

summary(sem_mmif_rivier)

# model updaten op basis van dSepS
mmif_best_model_updated <- update(mmif_best_model_rivier, . ~ . + o2_s)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + spei6_s)
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + zs_log)
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_mmif  <- formula(mmif_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20 <- formula(ec20_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
f_czv     <- formula(czv_best_model_updated)

mmif_clean <- glmmTMB(
  formula     = f_mmif,
  data        = dredge_data,
  family      = ordbeta()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
czv_clean     <- glmmTMB(f_czv,     data = dredge_data, family = gaussian())


mmif_sem_nat_sv_rivier_clean <- psem(
  mmif_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  czv_clean,
  zs_clean,
  ec20_clean,
  # n_t_log %~~% ec_20_log,
  # p_t_log %~~% ec_20_log,
  n_t_log %~~% t_s,
  p_t_log %~~% t_s,
  czv_log %~~% n_t_log,
  czv_log %~~% p_t_log
  # zs_log %~~% o2_s,

)

mmif_sem_nat_sv_rivier_clean %>% summary

save(mmif_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "mmif_sem_nat_sv_rivier_clean.rdata"))
bewaar_r2(mmif_sem_nat_sv_rivier_clean,       "mi",   "rivier", "mmif")


load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "mmif_sem_nat_sv_rivier_clean.rdata"))


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


model_list <- list(
  mmif = mmif_clean,
  ntot = ntot_clean,
  ptot = ptot_clean,
  czv  = czv_clean,
  ec20 = ec20_clean,
  o2   = o2_clean,
  # t = t_clean,
  zs = zs_clean
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

coefs_filled <- standardize_psem(mmif_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_rivier_mmif.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_mmif_rivier.html"), selfcontained = TRUE)
#
# test <- glmmTMB(mmif ~ jaar_s + (1 | meetplaats) +
#   (1 | bekken) + (1 | owl) + p_t_log,
#   data = dredge_data_rivier)
# summary(test)
# plot_model_vif(test, "VIF Check")

mmif_clean %>% plot_model(., type = "pred", show.data = TRUE)
o2_clean %>% plot_model(., type = "pred", show.data = TRUE)

######
# EPT
######

sem_ept_rivier <- psem(
  ept_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier,
  zs_best_model_rivier,
  czv_best_model_rivier
)

summary(sem_ept_rivier)

# model updaten op basis van dSepS
ept_best_model_updated <- update(ept_best_model_rivier, . ~ . )
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + spei6_s)
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + zs_log)
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_ept  <- formula(ept_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_czv  <- formula(czv_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
f_ec20     <- formula(ec20_best_model_updated)

ept_clean <- glmmTMB(
  formula     = f_ept,
  data        = dredge_data,
  family      = binomial(link = "logit"),
  weights = dredge_data$ta_xw
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
czv_clean  <- glmmTMB(f_czv,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
ec20_clean     <- glmmTMB(f_ec20,     data = dredge_data, family = gaussian())


ept_sem_nat_sv_rivier_clean <- psem(
  ept_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  czv_clean,
  zs_clean,
  ec20_clean,
  # n_t_log %~~% ec_20_log,
  # p_t_log %~~% ec_20_log,
  p_t_log %~~% t_s,
  n_t_log %~~% t_s,
  czv_log %~~% n_t_log,
  czv_log %~~% p_t_log
  # zs_log %~~% o2_s,

)


ept_sem_nat_sv_rivier_clean %>% summary
bewaar_r2(ept_sem_nat_sv_rivier_clean,        "mi",   "rivier", "ept")

save(ept_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "ept_sem_nat_sv_rivier_clean.rdata"))

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
model_list <- list(
  ept = ept_clean
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "ept_sem_nat_sv_rivier_clean.rdata"))

coefs_filled <- standardize_psem(ept_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_rivier_ept.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_ept_rivier.html"), selfcontained = TRUE)

######
## swd
######

sem_swd_rivier <- psem(
  swd_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  zs_best_model_rivier,
  o2_best_model_rivier,
  czv_best_model_rivier,
  ec20_best_model_rivier
)

summary(sem_swd_rivier)

# model updaten op basis van dSepS
swd_best_model_updated <- update(swd_best_model_rivier, . ~ . + intensiteit_combo_afstr_s + ec_20_log)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + spei6_s)
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + zs_log)
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_swd  <- formula(swd_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_zs     <- formula(zs_best_model_updated)

swd_clean <- glmmTMB(
  formula     = f_swd,
  data        = dredge_data,
  family      = gaussian
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
czv_clean  <- glmmTMB(f_czv,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
ec20_clean     <- glmmTMB(f_ec20,     data = dredge_data, family = gaussian())


swd_sem_nat_sv_rivier_clean <- psem(
  swd_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  czv_clean,
  zs_clean,
  ec20_clean,
  p_t_log %~~% t_s,
  n_t_log %~~% t_s,
  czv_log %~~% n_t_log,
  czv_log %~~% p_t_log
  # zs_log %~~% o2_s,

)

swd_sem_nat_sv_rivier_clean %>% summary
bewaar_r2(swd_sem_nat_sv_rivier_clean,        "mi",   "rivier", "swd")

save(swd_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "swd_sem_nat_sv_rivier_clean.rdata"))


model_list <- list(
  swd = swd_clean
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


load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "swd_sem_nat_sv_rivier_clean.rdata"))

coefs_filled <- standardize_psem(swd_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_rivier_swd.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_swd_rivier.html"), selfcontained = TRUE)

##########
#Tax
##########

sem_tax_rivier <- psem(
  tax_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  czv_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier,
  # ph_best_model_rivier,
  zs_best_model_rivier
)

summary(sem_tax_rivier)

# model updaten op basis van dSepS
tax_best_model_updated <- update(tax_best_model_rivier, . ~ . + o2_s)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + spei6_s)
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + zs_log)
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_tax  <- formula(tax_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
f_czv     <- formula(czv_best_model_updated)

tax_clean <- glmmTMB(
  formula     = f_tax,
  data        = dredge_data,
  family      = poisson()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
czv_clean     <- glmmTMB(f_czv,     data = dredge_data, family = gaussian())


tax_sem_nat_sv_rivier_clean <- psem(
  tax_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  czv_clean,
  zs_clean,
  ec20_clean,
  n_t_log %~~% t_s,
  p_t_log %~~% t_s,
  czv_log %~~% n_t_log,
  czv_log %~~% p_t_log
  # zs_log %~~% o2_s
)

tax_sem_nat_sv_rivier_clean %>% summary
bewaar_r2(tax_sem_nat_sv_rivier_clean,        "mi",   "rivier", "tax")

save(tax_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "tax_sem_nat_sv_rivier_clean.rdata"))

# SAC test
model <- tax_clean
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
  tax = tax_clean
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "tax_sem_nat_sv_rivier_clean.rdata"))

coefs_filled <- standardize_psem(tax_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_rivier_tax.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_tax_rivier.html"), selfcontained = TRUE)

######
# mts
######

sem_mts_rivier <- psem(
  mts_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier,
  zs_best_model_rivier,
  czv_best_model_rivier
)

summary(sem_mts_rivier)

# model updaten op basis van dSepS
mts_best_model_updated <- update(mts_best_model_rivier, . ~ . + o2_s)
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + spei6_s)
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + zs_log)
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_mts  <- formula(mts_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_czv  <- formula(czv_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
f_ec20     <- formula(ec20_best_model_updated)

mts_clean <- glmmTMB(
  formula     = f_mts,
  data        = dredge_data,
  family      = ordbeta
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
czv_clean  <- glmmTMB(f_czv,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
ec20_clean     <- glmmTMB(f_ec20,     data = dredge_data, family = gaussian())


mts_sem_nat_sv_rivier_clean <- psem(
  mts_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  czv_clean,
  zs_clean,
  ec20_clean,
  # n_t_log %~~% ec_20_log,
  # p_t_log %~~% ec_20_log,
  p_t_log %~~% t_s,
  n_t_log %~~% t_s,
  czv_log %~~% n_t_log,
  czv_log %~~% p_t_log
  # zs_log %~~% o2_s,

)


mts_sem_nat_sv_rivier_clean %>% summary
bewaar_r2(mts_sem_nat_sv_rivier_clean,        "mi",   "rivier", "mts")

save(mts_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "mts_sem_nat_sv_rivier_clean.rdata"))

# SAC test
model <- mts_clean
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
  mts = mts_clean
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "mts_sem_nat_sv_rivier_clean.rdata"))

coefs_filled <- standardize_psem(mts_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_rivier_mts.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_mts_rivier.html"), selfcontained = TRUE)



######
# nst
######

sem_nst_rivier <- psem(
  nst_best_model_rivier,
  ntot_best_model_rivier,
  ptot_best_model_rivier,
  o2_best_model_rivier,
  ec20_best_model_rivier,
  zs_best_model_rivier,
  czv_best_model_rivier
)

summary(sem_nst_rivier)

# model updaten op basis van dSepS
nst_best_model_updated <- update(nst_best_model_rivier, . ~ . )
ntot_best_model_updated <- update(ntot_best_model_rivier, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_rivier, . ~ . + spei6_s)
ec20_best_model_updated <- update(ec20_best_model_rivier, . ~ . )
o2_best_model_updated <- update(o2_best_model_rivier, . ~ . + zs_log)
zs_best_model_updated <- update(zs_best_model_rivier, . ~ . )
czv_best_model_updated <- update(czv_best_model_rivier, . ~ . )

# het optimale model clean maken!
f_nst  <- formula(nst_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_czv  <- formula(czv_best_model_updated)
f_zs     <- formula(zs_best_model_updated)
f_ec20    <- formula(ec20_best_model_updated)

nst_clean <- glmmTMB(
  formula     = f_nst,
  data        = dredge_data,
  family      = binomial(link = "logit"),
  weights = dredge_data$ta_xw
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
czv_clean  <- glmmTMB(f_czv,  data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())
ec20_clean     <- glmmTMB(f_ec20,     data = dredge_data, family = gaussian())


nst_sem_nat_sv_rivier_clean <- psem(
  nst_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  czv_clean,
  zs_clean,
  ec20_clean,
  # n_t_log %~~% ec_20_log,
  # p_t_log %~~% ec_20_log,
  p_t_log %~~% t_s,
  n_t_log %~~% t_s,
  czv_log %~~% n_t_log,
  czv_log %~~% p_t_log
  # zs_log %~~% o2_s,

)


nst_sem_nat_sv_rivier_clean %>% summary
bewaar_r2(nst_sem_nat_sv_rivier_clean,        "mi",   "rivier", "nst")

save(nst_sem_nat_sv_rivier_clean, file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "nst_sem_nat_sv_rivier_clean.rdata"))

# SAC test
model <- nst_clean
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
  nst = nst_clean
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "nst_sem_nat_sv_rivier_clean.rdata"))

coefs_filled <- standardize_psem(nst_sem_nat_sv_rivier_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_rivier_nst.rds")


source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_nst_rivier.html"), selfcontained = TRUE)

########
# SAC: spatial autocorrelation
#######

psems_mi_rivier <- list(mmif = mmif_sem_nat_sv_rivier_clean, tax = tax_sem_nat_sv_rivier_clean, swd = swd_sem_nat_sv_rivier_clean,
                        nst = nst_sem_nat_sv_rivier_clean, mts = mts_sem_nat_sv_rivier_clean, ept = ept_sem_nat_sv_rivier_clean)
sac_tab <- sac_psem_set(psems_mi_rivier, data = dredge_data)
plot_sac_overview(sac_tab)                      # tegelplot: I per model en test, * = BH-significant

pl <- sac_plan(sac_tab)                         # eerst bekijken wat er gedraaid wordt
pl                                              # run_site / run_owl zijn aan te passen

fu <- sac_followup(sac_tab, psems_mi_rivier, dredge_data,
                   plan = pl, out_dir = "output/figuren/sac/sac_mi_rivier")
fu$resultaten    # één rij per model en schaal, met oordeel
fu$coef          # alle coëfficiënten, iid versus ruimtelijk
fu$zin           # concepttekst voor de methodesectie

# bekijk welke estimates van significantie veranderden
fu$coef %>%
  filter(sig_change | sign_change) %>%
  select(model, term, est_iid, est_sp, se_iid, se_sp, p_iid, p_sp, shift_se)

fu$coef %>% filter(sig_change | sign_change) %>%
  select(model, term, est_iid, est_sp, p_iid, p_sp)



