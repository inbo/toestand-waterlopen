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


# 4. STAP B: Selectie per SEM-pad (Screening)
# -----------------------------------------------------------

# --- PATH 1: Nutriënten (N_T) ---
# Hypothese: Gedreven door Afstroomgebied Landgebruik + Lozingen
cat("\n--- Screening voor Nutriënten (N_T) ---\n")
res_nt <- screen_predictors(
  data = data_subset,
  response_var = "n_t_log",
  candidate_vars = c(clean_landuse, clean_lozingen, clean_klimaat), # GEEN oever hier
  family = gaussian
)
print(head(res_nt, 5))

# --- PATH 2: Zuurstof (O2) ---
# Hypothese: Gedreven door Nutriënten (algen), Organische belasting (CZV) & Fysica (Temp/Stroming)
# We testen hier welke nutriënt/fysica variabelen het beste werken
cat("\n--- Screening voor Zuurstof (O2) ---\n")
res_o2 <- screen_predictors(
  data = data_subset,
  response_var = "o2_verz_s",
  candidate_vars = c("n_t_log", "p_t_log", "czv_s", "t_s", "stroomsnelheid_s"),
  family = gaussian
)
print(head(res_o2, 5))


# --- PATH 3: Biologie (MMIF) ---
# Hypothese: Gedreven door:
# 1. Chemie (selecteer de winnaars van hierboven, bvb n_t_log en o2)
# 2. Water Habitat (uit clean_habitat)
# 3. Oever Habitat (uit clean_land_oever -> DIRECT effect op adulten)
# 4. Klimaat (uit raw_klimaat)

cat("\n--- Screening voor Biologie (MMIF) ---\n")
# Stel dat N_T en O2 de belangrijkste chemische variabelen bleken:
vars_voor_bio <- c(clean_nutrients, clean_fysico, clean_lozingen, clean_hydmo, clean_landuse, clean_klimaat)

res_mmif <- screen_predictors(
  data = data_subset %>% drop_na,
  response_var = "mmif",
  candidate_vars = vars_voor_bio,
  family = glmmTMB::ordbeta # Of gaussian voor snelle check
)
print(head(res_mmif, 10))

data_subset2 <- data_subset %>%
  na.omit
res_mmif_no_na <- screen_predictors(
  data = data_subset2,
  response_var = "mmif",
  candidate_vars = clean_all,
  family = glmmTMB::ordbeta # Of gaussian voor snelle check
)
print(head(res_mmif_no_na, 10))

# Installeer de packages als je ze nog niet hebt:
# install.packages(c("performance", "ggplot2", "scales"))

library(performance)
library(ggplot2)
library(dplyr)

# ==============================================================================
# FUNCTIE: MAAK EEN VIF BARCHART VOOR EEN FIT MODEL
# ==============================================================================


# 1. Fit je geselecteerde model
m_habitat <- glmmTMB(
  mmif ~ verharding_afstr_s + intensiteit_combo_afstr_s + verharding_oever_s + natuur_oever_s +
    jaar_s,
  data = data_subset,
  family = gaussian,
  na.action = "na.omit"
)
summary(m_habitat)
# 2. Check de VIF visueel
plot_model_vif(m_habitat, "VIF Check: Habitat Model")

m_hydmo <- glmmTMB(
  mmif ~ breedte_diepte_ratio_s + sinuositeit_s + bodemsub_s + doodhout_s + profiel_s + ekc2_waterlichaam_s + stroomsnelheid_s + jaar_s,
  data = data_subset,
  family = glmmTMB::ordbeta,
  REML = FALSE
)

m_hydmo <- glmmTMB(
  mmif ~ breedte_diepte_ratio_s + doodhout_s + profiel_s + ekc2_waterlichaam_s + stroomsnelheid_s + jaar_s,
  data = data_subset,
  family = glmmTMB::ordbeta,
  REML = FALSE
)

summary(m_hydmo)
plot_model_vif(m_hydmo, "VIF Check: Hydmo Model")
check_outliers(data_subset2$n_t)

################################################################################
# model fitten BIOLOGIE
################################################################################

data_subset2 <- data_subset %>%
  na.omit
options(na.action = "na.fail") # Verplicht voor dredge

mmif_variabelen <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "natuur_oever_s", "ekc2_waterlichaam_s", "profiel_s", "breedte_diepte_ratio_s")

x_vars_schoon <- setdiff(mmif_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")
y_var <- "mmif"

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_mmif <- glmmTMB(data = data_subset2, formula = formula_obj,
                  REML = FALSE,
                  family = ordbeta)

model_family <- ordbeta
# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_mmif"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_mmif,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 6),
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
mmif_dredge <- dredge_model

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
# model fitten Nutrienten
################################################################################

# ntot
data_subset2 <- data_subset %>%
  na.omit
options(na.action = "na.fail") # Verplicht voor dredge

nutrienten_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", "profiel_s", "breedte_diepte_ratio_s", clean_landuse, clean_lozingen)

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
  m.lim = c(1, 6),
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

data_subset2 <- data_subset %>%
  na.omit
options(na.action = "na.fail") # Verplicht voor dredge

nutrienten_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", "profiel_s", "breedte_diepte_ratio_s", clean_landuse, clean_lozingen)

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
  m.lim = c(1, 6),
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
# model fitten Nutrienten
################################################################################

# O2

data_subset2 <- data_subset %>%
  na.omit
options(na.action = "na.fail") # Verplicht voor dredge

fysico_variabelen <- c(clean_klimaat, "ekc2_waterlichaam_s", "profiel_s", "breedte_diepte_ratio_s", clean_landuse, clean_lozingen, "n_t_log", "p_t_log", "czv_log")

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
  m.lim = c(1, 6),
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

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(o2_best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)

plot_model_vif(o2_best_model_REML, "VIF Check: Hydmo Model") # "intensiteit_combo_oeverzone_s" weggelaten


