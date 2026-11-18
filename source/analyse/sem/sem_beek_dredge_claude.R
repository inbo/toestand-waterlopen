# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# ==============================================================================
# 1. DATA
# ==============================================================================

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

# Ruwe lijsten met variabelen (nog niet gefilterd)
raw_responses <- c("mmif", "ta_xw", "ns_tw", "mt_sw", "ep_tw", "sw_dw", "stress_prop")

raw_fysico        <- c("t_s",  "o2_s", "ec_20_log", "zs_log") # o2_verz_s weg en keuze o2 "p_h_s",
raw_nutrients     <- c("czv_log", "n_t_log", "no2_log", "no3_log", "nh4_log", "p_t_log")
raw_hydmo         <- c("breedte_diepte_ratio_s", "sinuositeit_s", "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s", "ekc2_traject_s", "stroomsnelheid_s")
raw_landuse       <- c("verharding_afstr_s", "natuur_afstr_s", "intensiteit_combo_afstr_s", "verharding_oever_s", "natuur_oever_s", "intensiteit_combo_oeverzone_s")
raw_lozingen      <- c("lozingen_rwzi_ie_log", "lozingen_riool_ie_log", "lozingen_industrie_ie_log", "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log")
raw_klimaat       <- c("spei6_s", "n_extreme_3m_s", "p_sum_7d_s")

# Correlogrammen
plot_groep_correlogram(data_subset, raw_responses, "Responsen (Beken)")
plot_groep_correlogram(data_subset, raw_fysico, "Fysico-chemie basisvariabelen (Beken)")
plot_groep_correlogram(data_subset, raw_lozingen, "Lozingen & Overstorten (Beken)")
plot_groep_correlogram(data_subset, raw_nutrients, "Nutriënten (Beken)")
plot_groep_correlogram(data_subset, raw_hydmo, "Hydromorfologie (Beken)")
plot_groep_correlogram(data_subset, raw_landuse, "Landgebruik (Beken)")
plot_groep_correlogram(data_subset, raw_klimaat, "Klimaat (Beken)")

# Opschonen collineariteit (binnen groepen)
clean_fysico    <- filter_collinear_vars(data_subset, raw_fysico)
clean_nutrients <- filter_collinear_vars(data_subset, raw_nutrients)
clean_landuse   <- c("verharding_afstr_s", "intensiteit_combo_afstr_s", "verharding_oever_s", "intensiteit_combo_oeverzone_s")
clean_hydmo     <- filter_collinear_vars(data_subset, raw_hydmo)
clean_lozingen  <- filter_collinear_vars(data_subset, raw_lozingen)
clean_klimaat   <- filter_collinear_vars(data_subset, raw_klimaat)

raw_all   <- c(clean_fysico, clean_nutrients, clean_landuse, clean_hydmo, clean_lozingen, clean_klimaat)
clean_all <- filter_collinear_vars(data_subset, raw_all)

# ------------------------------------------------------------------------------
# dredge_data: enkel kolommen die effectief in modellen zitten, zodat een NA in
# een ongebruikte kolom (spear_*, cl_s, o2_verz_s, ec_20, ...) geen rijen kost.
# De functie run_dredge() filtert per model nog eens op volledige cases.
# ------------------------------------------------------------------------------
dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, vhag,
    mmif, ept_prop, ep_tw, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(c("t_s", "o2_s", "ec_20_log", "zs_log")),  # responsen/knopen van abiotische deelmodellen
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico)
  ) %>%
  na.omit() %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  )
cat("dredge_data:", nrow(dredge_data), "rijen\n")

data_dir <- here("source", "analyse", "sem", "mi_nat_sv_beek")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

dredge_data_beek <- dredge_data
save(dredge_data_beek, file = file.path(data_dir, "dredge_data_beek.rdata"))

vis_miss(data_subset %>%
           select(
             meetplaats, monsternamedatum, jaar_s, bekken,
             mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
             n_t_log, p_t_log, czv_log,
             ekc2_waterlichaam_s,
             all_of(clean_klimaat),
             all_of(clean_lozingen),
             all_of(clean_landuse),
             all_of(clean_fysico)
           ))

# ==============================================================================
# 2. SPECIFICATIES DEELMODELLEN (alles wat per model verschilt)
# ==============================================================================

mi_pred   <- c(clean_klimaat, clean_fysico, "verharding_oever_s", "intensiteit_combo_oeverzone_s",
               "ekc2_waterlichaam_s", "overstorten_blootstelling_index_log", "czv_log")
abio_pred <- c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen, "zs_log")

specs <- list(
  # --- macro-invertebraten (MI) ---
  mmif = list(y = "mmif",       family = glmmTMB::ordbeta(), pred = mi_pred),
  ept  = list(y = "ept_prop",   family = binomial("logit"),  pred = mi_pred, w = "ta_xw"),
  tax  = list(y = "ta_xw",      family = poisson(),          pred = mi_pred),
  nst  = list(y = "nst_prop",   family = binomial("logit"),  pred = mi_pred, w = "ta_xw"),
  mts  = list(y = "mt_sw_prop", family = glmmTMB::ordbeta(), pred = mi_pred),
  swd  = list(y = "sw_dw",      family = gaussian(),         pred = mi_pred),
  # --- abiotische deelmodellen ---
  ntot = list(y = "n_t_log",    family = gaussian(), pred = abio_pred),
  ptot = list(y = "p_t_log",    family = gaussian(), pred = abio_pred),
  czv  = list(y = "czv_log",    family = gaussian(), pred = abio_pred),
  o2   = list(y = "o2_s",       family = gaussian(),
              pred = c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen,
                       "n_t_log", "p_t_log", "t_s", "ec_20_log", "czv_log")),
  ec20 = list(y = "ec_20_log",  family = gaussian(),
              pred = c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen,
                       "p_t_log", "n_t_log")),
  zs   = list(y = "zs_log",     family = gaussian(),
              pred = c(clean_klimaat, clean_landuse, clean_lozingen, "ekc2_waterlichaam_s"))
  # --- uitgeschakeld (vroeger uitgecommentarieerd) ---
  # ept1   = list(y = "ep_tw",       family = poisson(),  pred = setdiff(mi_pred, "czv_log")),
  # stress = list(y = "stress_prop", family = binomial("logit"), pred = mi_pred, w = "ta_xw"),
  # t      = list(y = "t_s",         family = gaussian(), pred = c(clean_klimaat, "ekc2_waterlichaam_s", clean_landuse, clean_lozingen)),
  # spear  = list(y = "spear_pesticides", family = glmmTMB::ordbeta(),
  #               pred = setdiff(mi_pred, "czv_log"))  # vereist spear_pesticides in dredge_data
)

# ==============================================================================
# 3. DREDGE VOOR ALLE DEELMODELLEN (met cache)
# ==============================================================================

dredge_dir <- here("source", "analyse", "sem", "dredge_output", "mi", "beek")
dir.create(dredge_dir, showWarnings = FALSE, recursive = TRUE)

# Modellen die je opnieuw wilt laten rekenen ondanks een bestaand cachebestand,
# bv. herbereken <- c("ept", "ptot"). Wijzigt de data of een spec? Verwijder dan
# de .rds van die modellen of zet ze hier.
herbereken <- character(0)

dredge_res <- purrr::imap(specs, function(s, naam) {
  pad <- file.path(dredge_dir, paste0(naam, "_dredge.rds"))
  if (file.exists(pad) && !(naam %in% herbereken)) {
    message("Cache gebruikt voor: ", naam)
    return(readRDS(pad))
  }
  res <- run_dredge(dredge_data, y_var = s$y, predictors = s$pred,
                    family = s$family, weights_var = s$w)
  saveRDS(res, pad)
  res
})

best_models <- purrr::map(dredge_res, "best_model")

# Overzichtstabel (handig voor de methodesectie: welke random effects bleven staan?)
dredge_overzicht <- purrr::imap_dfr(dredge_res, function(r, naam) {
  tibble::tibble(model          = naam,
                 y              = specs[[naam]]$y,
                 n              = r$n,
                 random_final   = paste(r$random_final, collapse = ", "),
                 random_dropped = paste(r$random_dropped, collapse = ", "),
                 n_delta2       = r$n_delta2,
                 singular_best  = r$singular_best,
                 minuten        = round(as.numeric(r$runtime, units = "mins"), 1))
})
print(dredge_overzicht, n = Inf)
saveRDS(dredge_overzicht, file.path(dredge_dir, "dredge_overzicht.rds"))

# Beste modellen opslaan en beschikbaar maken onder de oude namen
# (mmif_best_model_beek, ept_best_model_beek, ..., zs_best_model_beek)
best_namen <- stats::setNames(best_models, paste0(names(best_models), "_best_model_beek"))
list2env(best_namen, envir = .GlobalEnv)

purrr::iwalk(best_namen, function(m, obj) {
  assign(obj, m)
  save(list = obj, file = file.path(dredge_dir, paste0(obj, ".rdata")), envir = environment())
})

# Variabele-importantie (vroeger enkel voor mmif)
importance <- purrr::map(dredge_res, "importance")
importance_mmif <- importance$mmif

# Samenvattingen en VIF-check (zet op FALSE om over te slaan)
toon_resultaten <- TRUE
if (toon_resultaten) {
  purrr::iwalk(best_models, function(m, naam) {
    cat("\n########", naam, "########\n")
    print(summary(m))
    print(plot_model_vif(m, paste("VIF Check:", naam)))
  })
}

################################################################################
# 4. SEM MODELLEN FITTEN, ITERATIEF UPDATEN & OPSLAAN (SCHONE START)
################################################################################

# --- 4A. Laden dredge-resultaten & data (indien nog niet in geheugen) ----------
dredge_dir   <- here("source", "analyse", "sem", "dredge_output", "mi", "beek")
sem_out_dir  <- here("source", "analyse", "sem", "sem_output")
synthese_dir <- here("source", "analyse", "sem", "sem_output")
fig_dir      <- here("output", "figuren2")

dir.create(sem_out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(synthese_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

if (!exists("dredge_data")) {
  load(file.path(data_dir, "dredge_data_beek.rdata"))
  dredge_data <- dredge_data_beek
}

model_meta <- list(
  mmif = list(family = glmmTMB::ordbeta(), w = NULL),
  ept  = list(family = binomial("logit"),  w = "ta_xw"),
  tax  = list(family = poisson(),          w = NULL),
  nst  = list(family = binomial("logit"),  w = "ta_xw"),
  mts  = list(family = glmmTMB::ordbeta(), w = NULL),
  swd  = list(family = gaussian(),         w = NULL),
  ntot = list(family = gaussian(),         w = NULL),
  ptot = list(family = gaussian(),         w = NULL),
  czv  = list(family = gaussian(),         w = NULL),
  o2   = list(family = gaussian(),         w = NULL),
  ec20 = list(family = gaussian(),         w = NULL),
  zs   = list(family = gaussian(),         w = NULL)
)

if (!exists("best_models")) {
  best_models <- purrr::imap(model_meta, function(m, naam) {
    obj_naam <- paste0(naam, "_best_model_beek")
    env <- new.env()
    load(file.path(dredge_dir, paste0(obj_naam, ".rdata")), envir = env)
    env[[obj_naam]]
  })
}

# --- 4B. Helperfuncties -------------------------------------------------------

refit_clean_submodel <- function(naam, add_vars = character(0), base_models = best_models) {
  meta <- model_meta[[naam]]
  f <- formula(base_models[[naam]])

  if (length(add_vars) > 0) {
    f <- update.formula(f, as.formula(paste(". ~ . +", paste(add_vars, collapse = " + "))))
  }

  # Gebruik dredge_data$<kolom> zodat piecewiseSEM::dSep() de weights altijd terugvindt
  if (is.null(meta$w)) {
    mod <- eval(bquote(glmmTMB(.(f), data = dredge_data, family = .(meta$family), REML = TRUE)))
  } else {
    w_expr <- str2lang(paste0("dredge_data$", meta$w))
    mod <- eval(bquote(glmmTMB(.(f), data = dredge_data, family = .(meta$family),
                               weights = .(w_expr), REML = TRUE)))
  }

  if (performance::check_singularity(mod)) {
    warning("Singularity gedetecteerd in deelmodel: ", naam, call. = FALSE)
  }

  vif_vals <- tryCatch(performance::check_collinearity(mod)$VIF, error = function(e) numeric(0))
  if (length(vif_vals) > 0 && any(vif_vals > 5, na.rm = TRUE)) {
    warning(sprintf("Hoge VIF (max %.1f) in deelmodel '%s'.", max(vif_vals, na.rm = TRUE), naam), call. = FALSE)
  }

  mod
}

# Fit één psem met jouw huidige keuzes en toon Fisher's C, R2 + relevante d-sep paden
inspecteer_psem <- function(naam, add = character(0), cor = list(),
                            abio_list = abio_clean, p_grens = 0.10) {
  mi_clean <- refit_clean_submodel(naam, add_vars = add)
  alle_modellen <- c(setNames(list(mi_clean), naam), abio_list)

  sem_fit <- do.call(piecewiseSEM::psem, c(unname(alle_modellen), cor))
  sem_sum <- summary(sem_fit, standardize = "none", .progressBar = FALSE)

  cat("\n======================================================\n")
  cat("PSEM:", toupper(naam), "\n")
  cat("======================================================\n")
  print(sem_sum$Cstat)
  print(sem_sum$R2)

  # Lege kolomnamen (significance stars) benoemen zodat dplyr niet crasht
  dtab <- as.data.frame(sem_sum$dTable)
  names(dtab)[is.na(names(dtab)) | names(dtab) == ""] <- "Sig"

  dsep_rel <- dtab %>%
    dplyr::filter(P.Value < p_grens) %>%
    dplyr::arrange(P.Value)

  cat("\n--- Ontbrekende d-sep paden (p <", p_grens, ") ---\n")
  if (nrow(dsep_rel) == 0) {
    cat("Geen significante ontbrekende paden meer!\n")
  } else {
    print(dsep_rel, row.names = FALSE)
  }

  # Toon ook meteen de coëfficiënten van het responsmodel zelf
  resp_var <- all.vars(formula(mi_clean))[1]

  ctab <- as.data.frame(sem_sum$coefficients)
  names(ctab)[is.na(names(ctab)) | names(ctab) == ""] <- "Sig"
  cat("\n--- Paden in deelmodel:", resp_var, "---\n")
  print(ctab %>% dplyr::filter(Response == resp_var), row.names = FALSE)

  list(psem = sem_fit, mi_model = mi_clean, summary = sem_sum)
}

# DHARMa-validatie + opslaan van één afgewerkte SEM
bewaar_en_valideer_psem <- function(res_obj, naam, n_sim = 1000) {
  sem_obj <- res_obj$psem
  mi_mod  <- res_obj$mi_model

  # 1. DHARMa-check voor het responsmodel
  cat("\n--- DHARMa diagnostiek voor:", naam, "---\n")
  sim <- DHARMa::simulateResiduals(fittedModel = mi_mod, n = n_sim)
  plot(sim)
  title(main = paste("DHARMa residuals:", naam), line = 2)
  print(DHARMa::testDispersion(sim))
  print(DHARMa::testOutliers(sim))

  # 2. R2 bewaren & psem opslaan
  bewaar_r2(sem_obj, "mi", "beek", naam)
  obj_naam <- paste0(naam, "_sem_nat_sv_beek_clean")
  assign(obj_naam, sem_obj)
  save(list = obj_naam, file = file.path(sem_out_dir, paste0(obj_naam, ".rdata")))

  # 3. Gestandaardiseerde coëfficiënten voor synthese
  coefs_filled <- standardize_psem(sem_obj)[, -9]
  saveRDS(coefs_filled, file = file.path(synthese_dir, paste0("mi_beek_", naam, ".rds")))

  # 4. Interactieve netwerkfiguur
  source(here("source", "analyse", "sem", "figuur_sem_interactive.R"), local = TRUE)
  htmlwidgets::saveWidget(
    network,
    file = file.path(fig_dir, paste0("psem_inter_netw_mi_nat_sv_", naam, "_beek.html")),
    selfcontained = TRUE
  )

  message("Opgeslagen: ", obj_naam)
  invisible(sem_obj)
}

# --- 4C. Abiotische deelmodellen (gedeeld door alle SEMs) --------------------

# Vul hier eventuele extra paden voor abiotische modellen in (bv. list(o2 = c("zs_log")))
# en draai dit blokje opnieuw als je onderweg een abiotisch d-sep pad toevoegt:
abio_updates <- list(
)

abio_namen <- c("ntot", "ptot", "czv", "o2", "ec20", "zs")
abio_clean <- purrr::map(abio_namen, ~ refit_clean_submodel(.x, add_vars = abio_updates[[.x]])) %>%
  setNames(abio_namen)

# Optioneel: DHARMa-check voor de 6 abiotische modellen (1x draaien volstaat)
purrr::iwalk(abio_clean, function(mod, naam) {
  sim <- DHARMa::simulateResiduals(fittedModel = mod, n = 1000)
  plot(sim)
  title(main = paste("DHARMa residuals:", naam), line = 2)
})

# ==============================================================================
# 4D. ITERATIEF FITTEN PER RESPONS
# Werkwijze: draai inspecteer_psem() -> bekijk d-sep -> vul add/cor aan ->
# draai opnieuw tot tevreden -> draai bewaar_en_valideer_psem().
# ==============================================================================

abio_updates <- list(
  o2 = c("zs_log")
)

basis_cor_errors <- list(
  p_t_log %~~% t_s,
  n_t_log %~~% t_s,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  n_t_log %~~% p_t_log
  )

# Herfit abiotische modellen met eventuele updates
abio_clean <- purrr::map(abio_namen, ~ refit_clean_submodel(.x, add_vars = abio_updates[[.x]])) %>%
  setNames(abio_namen)

# --- 1. MMIF ------------------------------------------------------------------
res_mmif <- inspecteer_psem(
  naam = "mmif",
  add  = c("intensiteit_combo_afstr_s",
           "n_t_log",
           "p_t_log",
           "verharding_afstr_s",
           "o2_s"
           ),
  cor  = basis_cor_errors
)

# Tevreden? Opslaan & valideren:
bewaar_en_valideer_psem(res_mmif, "mmif")

# --- 2. EPT -------------------------------------------------------------------
res_ept <- inspecteer_psem(
  naam = "ept",
  add  = c("zs_log"),
  cor  = basis_cor_errors
)
bewaar_en_valideer_psem(res_ept, "ept")

# --- 3. SWD -------------------------------------------------------------------
res_swd <- inspecteer_psem(
  naam = "swd",
  add  = c(),
  cor  = list()
)
bewaar_en_valideer_psem(res_swd, "swd")

# --- 4. TAX -------------------------------------------------------------------
res_tax <- inspecteer_psem(
  naam = "tax",
  add  = c(),
  cor  = list()
)
bewaar_en_valideer_psem(res_tax, "tax")

# --- 5. NST -------------------------------------------------------------------
res_nst <- inspecteer_psem(
  naam = "nst",
  add  = c(),
  cor  = list()
)
bewaar_en_valideer_psem(res_nst, "nst")

# --- 6. MTS -------------------------------------------------------------------
res_mts <- inspecteer_psem(
  naam = "mts",
  add  = c(),
  cor  = list()
)
bewaar_en_valideer_psem(res_mts, "mts")

# --- Bundelen voor SAC-analyse achteraf ---------------------------------------
psems_mi_beek <- list(
  mmif = res_mmif$psem,
  tax  = res_tax$psem,
  swd  = res_swd$psem,
  nst  = res_nst$psem,
  mts  = res_mts$psem,
  ept  = res_ept$psem
)

########
# SAC: spatial autocorrelation
#######

psems_mi_beek <- list(mmif = mmif_sem_nat_sv_beek_clean, tax = tax_sem_nat_sv_beek_clean, swd = swd_sem_nat_sv_beek_clean,
                      nst = nst_sem_nat_sv_beek_clean, mts = mts_sem_nat_sv_beek_clean, ept = ept_sem_nat_sv_beek_clean)
sac_tab <- sac_psem_set(psems_mi_beek, data = dredge_data)
plot_sac_overview(sac_tab)                      # tegelplot: I per model en test, * = BH-significant

pl <- sac_plan(sac_tab)                         # eerst bekijken wat er gedraaid wordt
pl                                              # run_site / run_owl zijn aan te passen

fu <- sac_followup(sac_tab, psems_mi_beek, dredge_data,
                   plan = pl, out_dir = "output/spatial_autocorrelation/sac_mi_beek")
fu$resultaten    # één rij per model en schaal, met oordeel
fu$coef          # alle coëfficiënten, iid versus ruimtelijk
fu$zin           # concepttekst voor de methodesectie

# bekijk welke estimates verschilden?
fu$coef %>% filter(model == "nst_prop", schaal == "owl", term != "(Intercept)") %>%
  select(term, est_iid, est_sp, se_iid, se_sp, se_ratio, shift_se, p_iid, p_sp)

# “Ruimtelijke autocorrelatie werd per deelmodel getoetst met Moran’s I op DHARMa-residuen en op de random-effect intercepts (BH-correctie over alle tests). Voor de vier deelmodellen met een significant restpatroon (nst_prop, n_t_log, ec_20_log) werd een ruimtelijk gecorreleerd random effect (exponentiële covariantie) op meetplaats- of owl-niveau gefit. Dit verbeterde de modelfit (dAIC −36 tot −51), maar veranderde de schattingen en standaardfouten van de fixed effects niet in die mate dat conclusies wijzigden. De modellen met iid random effects zijn daarom behouden.”

