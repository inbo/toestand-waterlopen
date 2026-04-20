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
data_subset <- mafy_nat_sv_beek %>%
  drop_na(meetplaats, jaar) %>%
  rename(n_t = qual_n_t,
         p_t = qual_p_t,
         czv = qual_czv,
         no2 = qual_no2,
         no3 = qual_no3,
         nh4 = qual_nh4,
         zs = qual_zs) %>%
  select(meetplaats, monsternamedatum, jaar, bekken,
         index_zonder_gep, vo_zonder_gep, ts_zonder_gep, v_zonder_gep, gv_zonder_gep,
         t, p_h, o2, o2_verz, ec_20, secchi,
         czv, n_t, no2, no3, nh4, p_t, zs,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr,
         # verharding_oever, natuur_oever, intensiteit_combo_oeverzone,
         spei6, n_extreme_3m, p_sum_7d,
         perc_schaduw,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(bekken = as.factor(bekken),
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
raw_responses <- c("index_zonder_gep", "vo_zonder_gep", "ts_zonder_gep", "v_zonder_gep", "gv_zonder_gep")

raw_fysico        <- c("t_s", "p_h_s", "ec_20_s", "zs_s", "secchi_s") # o2 weglaten want niet relevant voor mafy
raw_nutrients     <- c("czv_log", "n_t_log", "no2_log", "no3_log", "nh4_log", "p_t_log")
raw_hydmo         <- c("breedte_diepte_ratio_s", "sinuositeit_s", "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s", "ekc2_traject_s", "stroomsnelheid_s")
raw_landuse <- c("verharding_afstr_s", "natuur_afstr_s", "intensiteit_combo_afstr_s", "perc_schaduw_s")
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
clean_landuse <- c("intensiteit_combo_afstr_s", "verharding_afstr_s") # geen schaduw perc -> niet in dredge drivers

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

data_subset2 <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken,
    index_zonder_gep, vo_zonder_gep, ts_zonder_gep, v_zonder_gep, gv_zonder_gep,
    n_t_log, p_t_log,
    czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
    perc_schaduw_s
  ) %>%
  na.omit

dredge_data <- data_subset2

vis_miss(dredge_data)
################################################################################
# model fitten mafy index
################################################################################

y_var <- "index_zonder_gep"
predictors <- c(clean_klimaat, clean_fysico, "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "perc_schaduw_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_mafy.R"))

mafy_index_dredge <- dredge_model

best_model_ML <- get.models(mafy_index_dredge, subset = 1)[[1]]

mafy_index_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(mafy_index_best_model_beek)

plot_model_vif(mafy_index_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

importance_mafy_index <- sw(mafy_index_dredge)

################################################################################
# model fitten verstoring
################################################################################
y_var <- "v_zonder_gep"
predictors <- c(clean_klimaat, clean_fysico, "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "perc_schaduw_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_mafy.R"))

verstoring_dredge <- dredge_model

best_model_ML <- get.models(verstoring_dredge, subset = 1)[[1]]

verstoring_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(verstoring_best_model_beek)

plot_model_vif(verstoring_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten groeivorm
################################################################################
y_var <- "gv_zonder_gep"
predictors <- c(clean_klimaat, clean_fysico, "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "perc_schaduw_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_mafy.R"))

gv_dredge <- dredge_model

best_model_ML <- get.models(gv_dredge, subset = 1)[[1]]

gv_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(gv_best_model_beek)

plot_model_vif(gv_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten typespecificiteit
################################################################################
y_var <- "ts_zonder_gep"
predictors <- c(clean_klimaat, clean_fysico, "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "perc_schaduw_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_mafy.R"))

typspec_dredge <- dredge_model

best_model_ML <- get.models(typspec_dredge, subset = 1)[[1]]

typspec_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(typspec_best_model_beek)

plot_model_vif(typspec_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten vegetatie-ontwikkeling
################################################################################
y_var <- "vo_zonder_gep"
predictors <- c(clean_klimaat, clean_fysico, "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "perc_schaduw_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge_mafy.R"))

vo_dredge <- dredge_model

best_model_ML <- get.models(vo_dredge, subset = 1)[[1]]

vo_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(vo_best_model_beek)

plot_model_vif(vo_best_model_beek, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

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
# model fitten zs
################################################################################
y_var <- "zs_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "czv_log", "t_s", "ec_20_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

zs_dredge <- dredge_model

best_model_ML <- get.models(zs_dredge, subset = 1)[[1]]

zs_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(zs_best_model_beek)

plot_model_vif(zs_best_model_beek, "VIF Check")

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
# model fitten secchi
################################################################################

y_var <- "secchi_s"
predictors <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = dredge_data, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

source(here("source" , "analyse", "sem", "dredge.R"))

secchi_dredge <- dredge_model

best_model_ML <- get.models(secchi_dredge, subset = 1)[[1]]

secchi_best_model_beek <- update(best_model_ML, REML = TRUE)

summary(secchi_best_model_beek)

plot_model_vif(secchi_best_model_beek, "VIF Check")

################################################################################
# sem models fitten
################################################################################

# mafy index

sem_mafy_index_beek <- psem(
  mafy_index_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  zs_best_model_beek,
  ec20_best_model_beek,
  secchi_best_model_beek
)

summary(sem_mafy_index_beek)

# model updaten op basis van dSepS
mafy_index_best_model_updated <- update(mafy_index_best_model_beek, . ~ . + p_sum_7d_s + p_t_log + intensiteit_combo_afstr_s)
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ .)
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . + perc_schaduw_s + lozingen_industrie_ie_log)
czv_best_model_updated <- update(czv_best_model_beek, . ~ . + perc_schaduw_s)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + intensiteit_combo_afstr_s + verharding_afstr_s + lozingen_rwzi_ie_log)
zs_best_model_updated <- update(zs_best_model_beek, . ~ .)
secchi_best_model_updated <- update(secchi_best_model_beek, . ~ . + perc_schaduw_s + lozingen_industrie_ie_log)

mafy_index_sem_nat_sv_beek <- psem(mafy_index_best_model_updated,
                             ntot_best_model_updated,
                             ptot_best_model_updated,
                             czv_best_model_updated,
                             zs_best_model_updated,
                             ec20_best_model_updated,
                             secchi_best_model_updated,
                             n_t_log %~~% p_t_log)
summary(mafy_index_sem_nat_sv_beek)

# ec20, czv en zs lopen dood!!

save(mafy_index_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mafy_nat_sv_beek", "mafy_index_sem_nat_sv_beek.rdata"))

sem_resultaat <- mafy_index_sem_nat_sv_beek
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

## verstoring

sem_verstoring_beek <- psem(
  verstoring_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  zs_best_model_beek,
  ec20_best_model_beek,
  secchi_best_model_beek
)

summary(sem_verstoring_beek)
# updaten

verstoring_best_model_updated <- update(verstoring_best_model_beek, . ~ . + czv_log + n_t_log + p_t_log)
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ .  + t_s + lozingen_rwzi_ie_log)
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . + perc_schaduw_s + lozingen_industrie_ie_log)
czv_best_model_updated <- update(czv_best_model_beek, . ~ . + perc_schaduw_s)
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . + intensiteit_combo_afstr_s + verharding_afstr_s + lozingen_rwzi_ie_log)
zs_best_model_updated <- update(zs_best_model_beek, . ~ . + lozingen_rwzi_ie_log + t_s)
secchi_best_model_updated <- update(secchi_best_model_beek, . ~ . + perc_schaduw_s + lozingen_industrie_ie_log + czv_log)

verstoring_sem_nat_sv_beek <- psem(verstoring_best_model_updated,
                            ntot_best_model_updated,
                            ptot_best_model_updated,
                            czv_best_model_updated,
                            zs_best_model_updated,
                            secchi_best_model_updated,
                            ec20_best_model_updated,
                            n_t_log %~~% p_t_log)
summary(verstoring_sem_nat_sv_beek)
r.squaredGLMM(verstoring_best_model_updated)
save(verstoring_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "verstoring_sem_nat_sv_beek.rdata"))

### ZS en EC20 lopen dood!

sem_resultaat <- verstoring_sem_nat_sv_beek
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

## typspec

sem_typspec_beek <- psem(
  typspec_best_model_beek,
  ntot_best_model_beek,
  ptot_best_model_beek,
  czv_best_model_beek,
  zs_best_model_beek,
  ec20_best_model_beek,
  secchi_best_model_beek
)

summary(sem_typspec_beek)
# updaten

typspec_best_model_updated <- update(typspec_best_model_beek, . ~ . )
ntot_best_model_updated <- update(ntot_best_model_beek, . ~ .  )
ptot_best_model_updated <- update(ptot_best_model_beek, . ~ . )
czv_best_model_updated <- update(czv_best_model_beek, . ~ . )
ec20_best_model_updated <- update(ec20_best_model_beek, . ~ . )
zs_best_model_updated <- update(zs_best_model_beek, . ~ . )
secchi_best_model_updated <- update(secchi_best_model_beek, . ~ . )

typspec_sem_nat_sv_beek <- psem(typspec_best_model_updated,
                                   ntot_best_model_updated,
                                   ptot_best_model_updated,
                                   czv_best_model_updated,
                                   zs_best_model_updated,
                                   secchi_best_model_updated,
                                   ec20_best_model_updated,
                                   n_t_log %~~% p_t_log)
summary(typspec_sem_nat_sv_beek)
r.squaredGLMM(typspec_best_model_updated)
save(typspec_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "typspec_sem_nat_sv_beek.rdata"))

### ZS en EC20 lopen dood!

sem_resultaat <- typspec_sem_nat_sv_beek
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

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
