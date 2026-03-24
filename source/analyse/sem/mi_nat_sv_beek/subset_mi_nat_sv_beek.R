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

plot_model_vif(hydmo_test, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

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

################################################################################
# model fitten MMIF
################################################################################

y_var <- "mmif"
predictors <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

source(here("source", "analyse", "sem", "dredge_formula.R"))

options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

source(here("source" , "analyse", "sem", "dredge.R"))

mmif_dredge <- dredge_model
# m_mmif <- glmmTMB(data = data_subset2, formula = formula_obj,
#                   REML = FALSE,
#                   family = ordbeta)
#
#
# # --- START TIMER ---
# start_tijd <- Sys.time()
# cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")
#
# aantal_cores <- detectCores() - 1
# cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")
#
# # STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
# mijn_cluster <- makeCluster(aantal_cores)
#
# # Exporteer naar de nieuwe clusternaam
# clusterExport(mijn_cluster, varlist = c("data_subset2", "m_mmif"))
#
# clusterEvalQ(mijn_cluster, {
#   library(glmmTMB)
#   options(na.action = "na.fail")
# })
#
# cat("Dredge is bezig over meerdere cores...\n")
#
#
# dredge_model <- dredge(
#   global.model = m_mmif,
#   cluster = mijn_cluster,     # <--- Hier geven we het door
#   rank = "AICc",
#   m.lim = c(1, 4),
#   fixed = "cond(jaar_s)",
#   trace = 2
# )
#
# # STAP 5: Netjes opruimen
# stopCluster(mijn_cluster)
# options(na.action = "na.omit")
#
# # --- STOP TIMER ---
# eind_tijd <- Sys.time()
# cat("\n--- KLAAR! ---\n")
# cat("Totale rekentijd: ")
# print(eind_tijd - start_tijd)
# cat("----------------\n\n")
#
# print(head(dredge_model, 10))
# mmif_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(mmif_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
mmif_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(mmif_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten EPT
################################################################################

options(na.action = "na.fail") # Verplicht voor dredge

ept_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

x_vars_schoon <- setdiff(ept_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "ept_prop"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_ept <- glmmTMB(data = data_subset2, formula = formula_obj,
                  REML = FALSE,
                  family = binomial(link = "logit"),
                 weights = data_subset2$ta_xw)


# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_ept"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_ept,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
ept_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(ept_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
ept_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(ept_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(ept_best_model_REML , "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten tax
################################################################################

options(na.action = "na.fail") # Verplicht voor dredge

tax_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s")

x_vars_schoon <- setdiff(tax_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "ta_xw"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_tax <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = poisson)


# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_tax"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_tax,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
tax_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(tax_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
tax_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(tax_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(tax_best_model_REML , "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten nst
################################################################################

options(na.action = "na.fail") # Verplicht voor dredge

nst_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

x_vars_schoon <- setdiff(nst_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "nst_prop"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_nst <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = data_subset2$ta_xw)


# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_nst"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_nst,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
nst_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(nst_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
nst_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(nst_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(nst_best_model_REML , "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten stress
################################################################################

options(na.action = "na.fail") # Verplicht voor dredge

stress_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

x_vars_schoon <- setdiff(stress_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "stress_prop"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_stress <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = binomial(link = "logit"),
                 weights = data_subset2$ta_xw)


# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_stress"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_stress,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
stress_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(stress_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
stress_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(stress_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(stress_best_model_REML , "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten mts
################################################################################

options(na.action = "na.fail") # Verplicht voor dredge

mts_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

x_vars_schoon <- setdiff(mts_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "mt_sw_prop"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_mts <- glmmTMB(data = data_subset2, formula = formula_obj,
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
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_mts"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_mts,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
mts_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(mts_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
mts_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(mts_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(mts_best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten swd
################################################################################

options(na.action = "na.fail") # Verplicht voor dredge

swd_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s")

x_vars_schoon <- setdiff(swd_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "mt_sw_prop"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_swd <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)


# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_swd"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_swd,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
swd_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(swd_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
swd_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(swd_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(swd_best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

################################################################################
# model fitten Nutrienten
################################################################################

# ntot

options(na.action = "na.fail") # Verplicht voor dredge

nutrienten_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

x_vars_schoon <- setdiff(nutrienten_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "n_t_log"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_ntot <- glmmTMB(data = data_subset2, formula = formula_obj,
                  REML = FALSE,
                  family = gaussian)

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_ntot"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_ntot,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
ntot_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(ntot_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
ntot_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(ntot_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(ntot_best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

# ptot

options(na.action = "na.fail") # Verplicht voor dredge

nutrienten_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)

x_vars_schoon <- setdiff(nutrienten_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "p_t_log"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_ptot <- glmmTMB(data = data_subset2, formula = formula_obj,
                  REML = FALSE,
                  family = gaussian)

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_ptot"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_ptot,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
ptot_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(ptot_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
ptot_best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(ptot_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten


################################################################################
# model fitten o2 + czv
################################################################################

# o2

options(na.action = "na.fail") # Verplicht voor dredge

fysico_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "czv_log", "t_s", "ec_20_s")

x_vars_schoon <- setdiff(fysico_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "o2_s"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_o2 <- glmmTMB(data = data_subset2, formula = formula_obj,
                  REML = FALSE,
                  family = gaussian)

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_o2"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_o2,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
o2_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(o2_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
o2_best_model_REML <- update(best_model_ML, REML = TRUE)
test <- update(best_model_ML, . ~ . + n_t_log + p_t_log, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(o2_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(o2_best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

# czv

options(na.action = "na.fail") # Verplicht voor dredge

fysico_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

x_vars_schoon <- setdiff(fysico_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "czv_log"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_czv <- glmmTMB(data = data_subset2, formula = formula_obj,
                REML = FALSE,
                family = gaussian)

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_czv"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_czv,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
czv_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(czv_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
czv_best_model_REML <- update(best_model_ML, REML = TRUE)
test <- update(best_model_ML, . ~ . + n_t_log + p_t_log, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(czv_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(czv_best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten

# ec20

options(na.action = "na.fail") # Verplicht voor dredge

fysico_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log")

x_vars_schoon <- setdiff(fysico_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "ec_20_s"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_ec_20 <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = gaussian)

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_ec_20"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_ec_20,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
  fixed = "cond(jaar_s)",
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
ec_20_dredge <- dredge_model

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(ec_20_dredge, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
ec_20_best_model_REML <- update(best_model_ML, REML = TRUE)
test <- update(best_model_ML, . ~ . + n_t_log + p_t_log, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(ec_20_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(ec_20_best_model_REML, "VIF Check") # "intensiteit_combo_oeverzone_s" weggelaten


################################################################################
# sem model fitten
################################################################################

sem_mmif <- psem(
  mmif_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_mmif)
sem_resultaat <- sem_mmif
# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source("source/analyse/sem/sem_standardised_coef_flexible.R")

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem.R"))

# # modellen updaten op basis van dSep _> eerst 3*
# mmif_best_model_updated <- update(mmif_best_model_REML, . ~ . + intensiteit_combo_afstr_s + n_t_log + p_t_log + verharding_afstr_s + lozingen_riool_ie_log)
# ntot_best_model_updated <- update(ntot_best_model_REML, . ~ . + t_s)
# ptot_best_model_updated <- update(ptot_best_model_REML, . ~ . + t_s)
# czv_best_model_updated <- czv_best_model_REML
# ec_20_best_model_updated <- update(ec_20_best_model_REML, . ~ . + t_s)
# o2_best_model_updated <- o2_best_model_REML

mmif_best_model_updated <- update(mmif_best_model_REML, . ~ . + intensiteit_combo_afstr_s + t_s + n_t_log + p_t_log + o2_s  + verharding_afstr_s)
ntot_best_model_updated <- update(ntot_best_model_REML, . ~ . + t_s + lozingen_industrie_ie_log)
ptot_best_model_updated <- update(ptot_best_model_REML, . ~ . + t_s + lozingen_riool_ie_log)
czv_best_model_updated <- czv_best_model_REML
ec_20_best_model_updated <- update(ec_20_best_model_REML, . ~ . + t_s + intensiteit_combo_afstr_s + natuur_oever_s + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_REML, . ~ . + p_sum_7d_s)

mmif_sem_nat_sv_beek <- psem(mmif_best_model_updated,
                 ntot_best_model_updated,
                 ptot_best_model_updated,
                 czv_best_model_updated,
                 o2_best_model_updated,
                 ec_20_best_model_updated,
                 n_t_log %~~% p_t_log)
summary(mmif_sem_nat_sv_beek)

save(mmif_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "mmif_sem_nat_sv_beek.rdata"))

sem_resultaat <- mmif_sem_nat_sv_beek
# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source("source/analyse/sem/sem_standardised_coef_flexible.R")

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem.R"))

## ept

sem_ept <- psem(
  ept_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_ept)

ept_best_model_updated <- update(ept_best_model_REML, . ~ . + czv_log + intensiteit_combo_afstr_s + verharding_afstr_s)
ntot_best_model_updated <- update(ntot_best_model_REML, . ~ . + t_s + lozingen_industrie_ie_log)
ptot_best_model_updated <- update(ptot_best_model_REML, . ~ . + t_s + lozingen_riool_ie_log)
czv_best_model_updated <- czv_best_model_REML
ec_20_best_model_updated <- update(ec_20_best_model_REML, . ~ . + t_s + intensiteit_combo_afstr_s + verharding_afstr_s)
o2_best_model_updated <- update(o2_best_model_REML, . ~ . + p_sum_7d_s)

ept_sem_nat_sv_beek <- psem(ept_best_model_updated,
                             ntot_best_model_updated,
                             ptot_best_model_updated,
                             czv_best_model_updated,
                             o2_best_model_updated,
                             ec_20_best_model_updated,
                             n_t_log %~~% p_t_log)
summary(ept_sem_nat_sv_beek)

save(ept_sem_nat_sv_beek, file = here("source", "analyse", "sem", "mi_nat_sv_beek", "ept_sem_nat_sv_beek.rdata"))

sem_resultaat <- ept_sem_nat_sv_beek
# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source("source/analyse/sem/sem_standardised_coef_flexible.R")

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem.R"))


## tax

sem_tax <- psem(
  tax_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_tax)

## nst

sem_nst <- psem(
  nst_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_nst)

## mts

sem_mts <- psem(
  mts_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_mts)

## swd

sem_swd <- psem(
  swd_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_swd)

## stress

sem_stress <- psem(
  stress_best_model_REML,
  ntot_best_model_REML,
  ptot_best_model_REML,
  czv_best_model_REML,
  o2_best_model_REML,
  ec_20_best_model_REML
)

summary(sem_stress)
