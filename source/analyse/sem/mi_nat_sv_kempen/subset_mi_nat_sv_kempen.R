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
data_subset <- mi_nat_sv_kempen %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, deelbekken, owl, vhag, geom,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs, cl,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone, spear_pesticides, spear_tu_estimated,
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
                vhag = as.factor(vhag),
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
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1)) %>%
    group_by(jaar) %>%
    mutate(
      t_gemiddeld_jaar = mean(t, na.rm = TRUE),    # Het klimaateffect
      t_anomalie = t - t_gemiddeld_jaar            # Het seizoenseffect
    ) %>%
    ungroup() %>%
    mutate(
      t_gemiddeld_jaar_s = scale(t_gemiddeld_jaar),
      t_anomalie_s = scale(t_anomalie)
    )

# 1. Definieer je ruwe lijsten met variabelen (nog niet gefilterd)
# -----------------------------------------------------------
raw_fysico        <- c("t_s", "o2_s", "ec_20_log", "zs_log") # o2_verz_s weg en keuze o2 en "p_h_s" weg
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

# bekijken ec20 en cl
data_subset %>% ggplot(aes(cl, ec_20)) + geom_point()
data_subset %>% filter(cl > 2000000) %>% View
data_subset2 <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, maand, deelbekken, owl, vhag, geom,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, o2_verz_s, cl, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
    t_gemiddeld_jaar_s,
    t_anomalie_s
  ) %>%
  na.omit %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  )

dredge_data <- data_subset2
dredge_data_kempen <- dredge_data

save(dredge_data_kempen, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "dredge_data_kempen.rdata"))

################################################################################
# model fitten MMIF
################################################################################

y_var <- "mmif"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")
source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = TRUE,
                 family = ordbeta)
performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

mmif_dredge <- dredge_model

best_model_ML <- get.models(mmif_dredge, subset = 1)[[1]]

mmif_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(mmif_best_model_kempen)
save(mmif_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "mmif_best_model_kempen.rdata"))

plot_model_vif(mmif_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten EPT
################################################################################
y_var <- "ept_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")
source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)
performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|meetplaats))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)

performance::check_singularity(model)
source(here("source" , "analyse", "sem", "dredge.R"))

ept_dredge <- dredge_model

best_model_ML <- get.models(ept_dredge, subset = 1)[[1]]

ept_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(ept_best_model_kempen)
save(ept_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "ept_best_model_kempen.rdata"))
plot_model_vif(ept_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

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
performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

tax_dredge <- dredge_model

best_model_ML <- get.models(tax_dredge, subset = 1)[[1]]

tax_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(tax_best_model_kempen)
save(tax_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "tax_best_model_kempen.rdata"))
plot_model_vif(tax_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten nst
################################################################################
y_var <- "nst_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")
source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = dredge_data$ta_xw)
performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

nst_dredge <- dredge_model

best_model_ML <- get.models(nst_dredge, subset = 1)[[1]]

nst_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(nst_best_model_kempen)
save(nst_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "nst_best_model_kempen.rdata"))
plot_model_vif(nst_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten
################################################################################
# model fitten stress
################################################################################
# y_var <- "stress_prop"
# predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")
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
# stress_best_model_kempen <- update(best_model_ML, REML = TRUE)
#
# summary(stress_best_model_kempen)
#
# plot_model_vif(stress_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten mts
################################################################################

y_var <- "mt_sw_prop"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")
source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = TRUE,
                 family = ordbeta)
performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|bekken) )

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

mts_dredge <- dredge_model

best_model_ML <- get.models(mts_dredge, subset = 1)[[1]]

mts_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(mts_best_model_kempen)
save(mts_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "mts_best_model_kempen.rdata"))

plot_model_vif(mts_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

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

swd_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(swd_best_model_kempen)
save(swd_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "swd_best_model_kempen.rdata"))
plot_model_vif(swd_best_model_kempen, "VIF Check")

################################################################################
# model fitten spear pesticides
################################################################################
#
# y_var <- "spear_pesticides"
# predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log")
#
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
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = ordbeta)
#
# performance::check_singularity(model)
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# spear_dredge <- dredge_model
#
# best_model_ML <- get.models(spear_dredge, subset = 1)[[1]]
#
# spear_best_model_kempen <- update(best_model_ML, REML = TRUE)
#
# summary(spear_best_model_kempen)
# save(spear_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "spear_best_model_kempen.rdata"))
#
# plot_model_vif(spear_best_model_kempen, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten
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

source(here("source" , "analyse", "sem", "dredge.R"))

ntot_dredge <- dredge_model

best_model_ML <- get.models(ntot_dredge, subset = 1)[[1]]

ntot_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(ntot_best_model_kempen)
save(ntot_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "ntot_best_model_kempen.rdata"))

plot_model_vif(ntot_best_model_kempen, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge.R"))

ptot_dredge <- dredge_model

best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

ptot_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(ptot_best_model_kempen)
save(ptot_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "ptot_best_model_kempen.rdata"))
plot_model_vif(ptot_best_model_kempen, "VIF Check")

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
performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

o2_dredge <- dredge_model

best_model_ML <- get.models(o2_dredge, subset = 1)[[1]]

o2_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(o2_best_model_kempen)
save(o2_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "o2_best_model_kempen.rdata"))
plot_model_vif(o2_best_model_kempen, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge.R"))

zs_dredge <- dredge_model

best_model_ML <- get.models(zs_dredge, subset = 1)[[1]]

zs_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(zs_best_model_kempen)
save(zs_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "zs_best_model_kempen.rdata"))

plot_model_vif(zs_best_model_kempen, "VIF Check")

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

performance::check_singularity(model)

formula_obj <- update(formula_obj, . ~ . - (1|bekken))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

performance::check_singularity(model)

source(here("source" , "analyse", "sem", "dredge.R"))

ec20_dredge <- dredge_model

best_model_ML <- get.models(ec20_dredge, subset = 1)[[1]]

ec20_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(ec20_best_model_kempen)
save(ec20_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "ec20_best_model_kempen.rdata"))

plot_model_vif(ec20_best_model_kempen, "VIF Check")

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

source(here("source" , "analyse", "sem", "dredge.R"))

czv_dredge <- dredge_model

best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

czv_best_model_kempen <- update(best_model_ML, REML = TRUE)

summary(czv_best_model_kempen)

save(czv_best_model_kempen, file = here("source", "analyse", "sem", "dredge_output", "czv_best_model_kempen.rdata"))

plot_model_vif(czv_best_model_kempen, "VIF Check")

################################################################################
# model fitten T
################################################################################
#
# y_var <- "t_s"
# predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)
#
# source(here("source", "analyse", "sem", "dredge_formula.R"))
#
# options(na.action = "na.fail") # Verplicht voor dredge
# model <- glmmTMB(data = dredge_data, formula = formula_obj,
#                  REML = FALSE,
#                  family = gaussian)
#
# source(here("source" , "analyse", "sem", "dredge.R"))
#
# t_dredge <- dredge_model
#
# best_model_ML <- get.models(t_dredge, subset = 1)[[1]]
#
# t_best_model_kempen <- update(best_model_ML, REML = TRUE)
#
# summary(t_best_model_kempen)
#
# plot_model_vif(t_best_model_kempen, "VIF Check")

################################################################################
# sem models fitten
################################################################################
load(file = here("source", "analyse", "sem", "dredge_output", "spear_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mmif_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "ept_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "swd_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "nst_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "mts_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "tax_best_model_kempen.rdata"))

load(file = here("source", "analyse", "sem", "dredge_output", "ntot_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "ptot_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "o2_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "ec20_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "zs_best_model_kempen.rdata"))
load(file = here("source", "analyse", "sem", "dredge_output", "czv_best_model_kempen.rdata"))

######
# MMIF
######

sem_mmif_kempen <- psem(
  mmif_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  czv_best_model_kempen,
  zs_best_model_kempen
  # t_best_model_kempen
)

summary(sem_mmif_kempen)

# model updaten op basis van dSepS
mmif_best_model_updated <- update(mmif_best_model_kempen, . ~ . + zs_log + p_t_log + o2_s)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . )
czv_best_model_updated <- update(czv_best_model_kempen, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s + n_t_log + p_t_log)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ . )
# t_best_model_updated <- update(t_best_model_kempen, . ~ . )


# het optimale model clean maken!
f_mmif  <- formula(mmif_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv   <- formula(czv_best_model_updated)
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
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())


mmif_sem_nat_sv_kempen_clean <- psem(
  mmif_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  zs_clean,
  zs_log %~~% ec_20_log,
  ec_20_log %~~% t_s,
  ec_20_log %~~% czv_log,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  p_t_log %~~% n_t_log
)

mmif_sem_nat_sv_kempen_clean %>% summary

save(mmif_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mmif_sem_nat_sv_kempen_clean.rdata"))
bewaar_r2(mmif_sem_nat_sv_kempen_clean,       "mi",   "kempen", "mmif")

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mmif_sem_nat_sv_kempen_clean.rdata"))

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

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mmif_sem_nat_sv_kempen_clean.rdata"))

coefs_filled <- standardize_psem(mmif_sem_nat_sv_kempen_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_kempen_mmif.rds")

# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten

# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_mmif.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_mmif_kempen.html"), selfcontained = TRUE)

######
# EPT
######

sem_ept_kempen <- psem(
  ept_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  czv_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  zs_best_model_kempen
)

summary(sem_ept_kempen)
# updaten

ept_best_model_updated <- update(ept_best_model_kempen, . ~ . + p_t_log + zs_log)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . )
czv_best_model_updated <- update(czv_best_model_kempen, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s + n_t_log + p_t_log)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ . )

# het optimale model clean maken!
f_ept  <- formula(ept_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_zs   <- formula(zs_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_czv     <- formula(czv_best_model_updated)


ept_clean <- glmmTMB(
  formula     = f_ept,
  data        = dredge_data,
  family = binomial(link = "logit"),
  weights = dredge_data$ta_xw
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
# ph_clean   <- glmmTMB(f_ph,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
czv_clean     <- glmmTMB(f_czv,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())


ept_sem_nat_sv_kempen_clean <- psem(
  ept_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  zs_clean,
  ec_20_log %~~% t_s,
  ec_20_log %~~% czv_log,
  zs_log %~~% ec_20_log,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  n_t_log %~~% p_t_log
)

ept_sem_nat_sv_kempen_clean %>% summary
bewaar_r2(ept_sem_nat_sv_kempen_clean,        "mi",   "kempen", "ept")

save(ept_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "ept_sem_nat_sv_kempen_clean.rdata"))

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
# Zet je modellen in een lijst
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


coefs_filled <- standardize_psem(ept_sem_nat_sv_kempen_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_kempen_ept.rds")

# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten
#
# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_mmif.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_ept_kempen.html"), selfcontained = TRUE)

######
## swd
######

sem_swd_kempen <- psem(
  swd_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  zs_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  czv_best_model_kempen
)

summary(sem_swd_kempen)

# updaten

swd_best_model_updated <- update(swd_best_model_kempen, . ~ . +  p_t_log)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ . )
czv_best_model_updated <- update(czv_best_model_kempen, . ~ .)
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s + n_t_log + p_t_log)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )

#Ntotaal en CZV uit het model want doodlopend
# het optimale model clean maken!
f_swd  <- formula(swd_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_zs   <- formula(zs_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_czv     <- formula(czv_best_model_updated)


swd_clean <- glmmTMB(
  formula     = f_swd,
  data        = dredge_data,
  family = gaussian
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
# ph_clean   <- glmmTMB(f_ph,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
czv_clean     <- glmmTMB(f_czv,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())


swd_sem_nat_sv_kempen_clean <- psem(
  swd_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  zs_clean,
  # ph_clean,
  ec_20_log %~~% t_s,
  ec_20_log %~~% czv_log,
  zs_log %~~% ec_20_log,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  n_t_log %~~% p_t_log
)

swd_sem_nat_sv_kempen_clean %>% summary
bewaar_r2(swd_sem_nat_sv_kempen_clean,        "mi",   "kempen", "swd")

save(swd_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "swd_sem_nat_sv_kempen_clean.rdata"))

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

# Zet je modellen in een lijst
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

coefs_filled <- standardize_psem(swd_sem_nat_sv_kempen_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_kempen_swd.rds")

# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten
#
# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_mmif.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_swd_kempen.html"), selfcontained = TRUE)

#####
# TAX
#####

sem_tax_kempen <- psem(
  tax_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  czv_best_model_kempen,
  zs_best_model_kempen
  # t_best_model_kempen
)

summary(sem_tax_kempen)

# model updaten op basis van dSepS
tax_best_model_updated <- update(tax_best_model_kempen, . ~ . + lozingen_riool_ie_log + lozingen_rwzi_ie_log + zs_log)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . ) # loop dood dus uit model
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . )
czv_best_model_updated <- update(czv_best_model_kempen, . ~ . ) # loop dood dus laten we uit het model!
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s + n_t_log + p_t_log)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ . )
# t_best_model_updated <- update(t_best_model_kempen, . ~ . )


# het optimale model clean maken!
f_tax  <- formula(tax_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv  <- formula(czv_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
# f_t     <- formula(t_best_model_updated)
f_zs     <- formula(zs_best_model_updated)

tax_clean <- glmmTMB(
  formula     = f_tax,
  data        = dredge_data,
  family      = poisson
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())


tax_sem_nat_sv_kempen_clean <- psem(
  tax_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  zs_clean,
  ec_20_log %~~% t_s,
  p_t_log %~~% czv_log,
  czv_log %~~% ec_20_log,
  n_t_log %~~% czv_log,
  p_t_log %~~% n_t_log,
  zs_log %~~% ec_20_log
)

tax_sem_nat_sv_kempen_clean %>% summary

bewaar_r2(tax_sem_nat_sv_kempen_clean,        "mi",   "kempen", "tax")

save(tax_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "tax_sem_nat_sv_kempen_clean.rdata"))

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

library(DHARMa)
library(ggplot2)

# Zet je modellen in een lijst
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "tax_sem_nat_sv_kempen_clean.rdata"))

coefs_filled <- standardize_psem(tax_sem_nat_sv_kempen_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_kempen_tax.rds")

# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten

# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_tax.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_tax_kempen.html"), selfcontained = TRUE)


######
# MTS
######

sem_mts_kempen <- psem(
  mts_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  czv_best_model_kempen,
  zs_best_model_kempen
  # t_best_model_kempen
)

summary(sem_mts_kempen)

# model updaten op basis van dSepS
mts_best_model_updated <- update(mts_best_model_kempen, . ~ . + verharding_oever_s + o2_s + p_t_log)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . )
czv_best_model_updated <- update(czv_best_model_kempen, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s + n_t_log + p_t_log)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ . )
# t_best_model_updated <- update(t_best_model_kempen, . ~ . )


# het optimale model clean maken!
f_mts  <- formula(mts_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv   <- formula(czv_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
# f_t     <- formula(t_best_model_updated)
f_zs     <- formula(zs_best_model_updated)

mts_clean <- glmmTMB(
  formula     = f_mts,
  data        = dredge_data,
  family      = ordbeta()
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
# t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())


mts_sem_nat_sv_kempen_clean <- psem(
  mts_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  zs_clean,
  zs_log %~~% ec_20_log,
  ec_20_log %~~% t_s,
  ec_20_log %~~% czv_log,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  p_t_log %~~% n_t_log
)

mts_sem_nat_sv_kempen_clean %>% summary
bewaar_r2(mts_sem_nat_sv_kempen_clean,        "mi",   "kempen", "mts")

save(mts_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mts_sem_nat_sv_kempen_clean.rdata"))

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mts_sem_nat_sv_kempen_clean.rdata"))

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

library(DHARMa)
library(ggplot2)

# Zet je modellen in een lijst
model_list <- list(
  mts = mts_clean,
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mts_sem_nat_sv_kempen_clean.rdata"))

coefs_filled <- standardize_psem(mts_sem_nat_sv_kempen_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_kempen_mts.rds")

# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten

# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_mts.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_mts_kempen.html"), selfcontained = TRUE)

######
# nst
######

sem_nst_kempen <- psem(
  nst_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  czv_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  zs_best_model_kempen
)

summary(sem_nst_kempen)
# updaten

nst_best_model_updated <- update(nst_best_model_kempen, . ~ . + intensiteit_combo_afstr_s + lozingen_rwzi_ie_log)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . )
czv_best_model_updated <- update(czv_best_model_kempen, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s + n_t_log + p_t_log)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ . )

# het optimale model clean maken!
f_nst  <- formula(nst_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_zs   <- formula(zs_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_czv     <- formula(czv_best_model_updated)


nst_clean <- glmmTMB(
  formula     = f_nst,
  data        = dredge_data,
  family = binomial(link = "logit"),
  weights = dredge_data$ta_xw
)

ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
# ph_clean   <- glmmTMB(f_ph,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian())
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
czv_clean     <- glmmTMB(f_czv,     data = dredge_data, family = gaussian())
zs_clean     <- glmmTMB(f_zs,     data = dredge_data, family = gaussian())


nst_sem_nat_sv_kempen_clean <- psem(
  nst_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  # t_clean,
  zs_clean,
  ec_20_log %~~% t_s,
  ec_20_log %~~% czv_log,
  zs_log %~~% ec_20_log,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  n_t_log %~~% p_t_log
)

nst_sem_nat_sv_kempen_clean %>% summary
bewaar_r2(nst_sem_nat_sv_kempen_clean,        "mi",   "kempen", "nst")

save(nst_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "nst_sem_nat_sv_kempen_clean.rdata"))

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
# Zet je modellen in een lijst
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


coefs_filled <- standardize_psem(nst_sem_nat_sv_kempen_clean)[,-9]
saveRDS(coefs_filled, file = "source/analyse/sem/synthese/sem_output/mi_kempen_nst.rds")

# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten
#
# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_mmif.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_nst_kempen.html"), selfcontained = TRUE)

################################################################################
# spear pesticides
################################################################################


spear_pesticides_model <- glmmTMB(
  spear_pesticides ~ intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + verharding_oever_s +
    lozingen_rwzi_ie_log + lozingen_riool_ie_log + lozingen_industrie_ie_log + overstorten_blootstelling_index_log + (1 |bekken) + (1 | meetplaats) + (1 | owl),
  data = dredge_data,
  family = ordbeta
)
spear_pesticides_model %>% summary()
plot_model_vif(spear_pesticides_model)

options(na.action = "na.fail") # Verplicht voor dredge
spear_pesticides_model <- glmmTMB(
  spear_pesticides ~ intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + verharding_oever_s +
    lozingen_rwzi_ie_log + lozingen_riool_ie_log + lozingen_industrie_ie_log + overstorten_blootstelling_index_log + (1 |bekken) + (1 | meetplaats) + (1 | owl),
  data = dredge_data,
  family = ordbeta,
  REML = FALSE
)
d <- dredge(
  global.model = spear_pesticides_model,
  rank = "AICc",
  fixed = "cond(jaar_s)",
  trace = 2
)
best_model_ML <- get.models(d, subset = 1)[[1]]
averaged_model <- model.avg(d, subset = delta <= 2)
summary(averaged_model)
spear_best <- update(best_model_ML, REML = TRUE)

summary(spear_best)
plot_model(spear_best, "pred")
sw(d)

#sem

sem_spear_kempen <- psem(
  spear_best_model_kempen,
  ntot_best_model_kempen,
  ptot_best_model_kempen,
  # czv_best_model_kempen,
  o2_best_model_kempen,
  ec20_best_model_kempen,
  zs_best_model_kempen
)

summary(sem_spear_kempen)

# model updaten op basis van dSepS
spear_best_model_updated <- update(spear_best_model_kempen, . ~ . + lozingen_rwzi_ie_log + p_t_log)
ntot_best_model_updated <- update(ntot_best_model_kempen, . ~ . )
ptot_best_model_updated <- update(ptot_best_model_kempen, . ~ . + natuur_oever_s)
ec20_best_model_updated <- update(ec20_best_model_kempen, . ~ . + ekc2_waterlichaam_s)
o2_best_model_updated <- update(o2_best_model_kempen, . ~ . )
zs_best_model_updated <- update(zs_best_model_kempen, . ~ .)

# het optimale model clean maken!
f_spear  <- formula(spear_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
# f_t     <- formula(t_best_model_updated)
# f_czv   <- formula(czv_best_model_updated)
f_zs     <- formula(zs_best_model_updated)

spear_clean <- glmmTMB(
  formula     = f_spear,
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

spear_sem_nat_sv_kempen_clean <- psem(
  spear_clean,
  ntot_clean,
  ptot_clean,
  o2_clean,
  ec20_clean,
  zs_clean,
  ec_20_log %~~% t_s,
  # n_t_log %~~% t_s,
  # p_t_log %~~% t_s,
  n_t_log %~~% p_t_log,
  # zs_log %~~% ec_20_log,
  p_t_log %~~% ec_20_log,
  n_t_log %~~% ec_20_log)
  # o2_s %~~% zs_log)


spear_sem_nat_sv_kempen_clean %>% summary

save(spear_sem_nat_sv_kempen_clean, file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "spear_sem_nat_sv_kempen_clean.rdata"))

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "spear_sem_nat_sv_kempen_clean.rdata"))


# SAC test
model <- spear_clean
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
  spear = spear_clean
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

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "spear_sem_nat_sv_kempen_clean.rdata"))


coefs_filled <- standardize_psem(spear_sem_nat_sv_kempen_clean)[,-9]
# source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten
#
# ggsave(
#   filename =  here("output", "figuren", "SEM_mi_nat_sv_kempen_spear.png"),
#   plot = last_plot(), # Expliciet de laatste plot kiezen
#   width = 40,
#   height = 20,
#   units = "cm",
#   dpi = 300,
#   bg = "white"
# )
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_spear_kempen.html"), selfcontained = TRUE)

########
# SAC: spatial autocorrelation
#######

psems_mi_kempen <- list(mmif = mmif_sem_nat_sv_kempen_clean, tax = tax_sem_nat_sv_kempen_clean, swd = swd_sem_nat_sv_kempen_clean,
                      nst = nst_sem_nat_sv_kempen_clean, mts = mts_sem_nat_sv_kempen_clean, ept = ept_sem_nat_sv_kempen_clean)
sac_tab <- sac_psem_set(psems_mi_kempen, data = dredge_data)
plot_sac_overview(sac_tab)                      # tegelplot: I per model en test, * = BH-significant

pl <- sac_plan(sac_tab)                         # eerst bekijken wat er gedraaid wordt
pl                                              # run_site / run_owl zijn aan te passen

fu <- sac_followup(sac_tab, psems_mi_kempen, dredge_data,
                   plan = pl, out_dir = "output/figuren/sac/sac_mi_kempen")
fu$resultaten    # één rij per model en schaal, met oordeel
fu$coef          # alle coëfficiënten, iid versus ruimtelijk
fu$zin           # concepttekst voor de methodesectie

# bekijk welke estimates van significantie veranderden
fu$coef %>%
  filter(sig_change | sign_change) %>%
  select(model, term, est_iid, est_sp, se_iid, se_sp, p_iid, p_sp, shift_se)

fu$coef %>% filter(sig_change | sign_change) %>%
  select(model, term, est_iid, est_sp, p_iid, p_sp)



