# --- Packages en Data inladen ---
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))
source(here::here("source", "analyse", "mafy_datasets_prep.R"))

library(glmmTMB)
library(dplyr)
library(tidyr)
library(purrr)
library(corrplot) # Voor de correlatiematrix
library(broom.mixed) # Voor het netjes uittrekken van modelresultaten

# --- Correlatiematrix Hydromorfologie ---
hydro_vars <- c("dml_allu", "dml_bedd", "dml_long", "dml_late", "dml_vert", "ekc2_waterlichaam")

# Neem één grote dataset (bijv. de gecombineerde of gewoon beek) om de correlatie te checken
cor_data <- data5 %>%
  select(all_of(hydro_vars)) %>%
  drop_na()

cor_matrix <- cor(cor_data, method = "spearman")

# Visualiseer de correlaties
corrplot(cor_matrix,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         title = "Spearman Correlaties Hydromorfologische Variabelen",
         mar = c(0,0,1,0))

# --- Functie voor Data Voorbereiding en Modellering ---
fit_hydro_model <- function(data, response_var, family_arg, weight_col = NULL) {
  # 1. Basiscontroles
  if (is.null(data)) return(NULL) # Voor ontbrekende datasets

  if (!response_var %in% names(data)) return(NULL)

  hydro_vars <- c("dml_allu", "dml_bedd", "dml_long", "dml_late", "dml_vert", "ekc2_waterlichaam")

  vars_to_keep <- c(response_var, hydro_vars, "meetplaats", "owl", "bekken")
  if (!is.null(weight_col)) {
    vars_to_keep <- c(vars_to_keep, weight_col)
  }

  if (!all(vars_to_keep %in% names(data))) return(NULL)

  # 2. Data opschonen
  df_clean <- data %>%
    select(all_of(vars_to_keep)) %>%
    drop_na()

  if (family_arg$family == "poisson") {
    df_clean[[response_var]] <- round(df_clean[[response_var]])
  }

  if (nrow(df_clean) < 10) return(NULL)

  # 3. Voorspellers schalen
  df_scaled <- df_clean %>%
    mutate(across(all_of(hydro_vars), ~ as.numeric(scale(.)), .names = "{.col}_s"))

  # --- NIEUW: 4. Hulpfunctie om dynamisch te fitten ---
  fit_model_with_re <- function(re_terms) {
    f <- reformulate(
      termlabels = c("dml_allu_s", "dml_bedd_s", "dml_long_s",
                     "dml_late_s", "dml_vert_s",
                     # "ekc2_waterlichaam_s",
                     re_terms), # Variabele random effects
      response = response_var
    )

    # Gebruik suppressWarnings om de console schoon te houden tijdens de test-fits
    if (!is.null(weight_col)) {
      weights_vec <- df_scaled[[weight_col]]
      model <- suppressWarnings(glmmTMB(f,
                                        data = df_scaled,
                                        family = family_arg,
                                        weights = weights_vec,
                                        REML = FALSE,
                                        control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS"))))
    } else {
      model <- suppressWarnings(glmmTMB(f,
                                        data = df_scaled,
                                        family = family_arg,
                                        REML = FALSE,
                                        control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS"))))
    }
    return(model)
  }

  # --- NIEUW: 5. Iteratief fitten en Singulariteit checken ---
  # Start met de volledige set random effects
  current_re_terms <- c("(1|meetplaats)", "(1|owl)", "(1|bekken)")
  model <- fit_model_with_re(current_re_terms)

  # Instellingen voor de loop
  max_iter <- length(current_re_terms)
  iter <- 0
  singularity_resolved <- FALSE

  while (!singularity_resolved && iter < max_iter && length(current_re_terms) > 0) {
    iter <- iter + 1

    # Haal de varianties van de random effects op
    vc <- glmmTMB::VarCorr(model)$cond

    problem_res <- c()
    if (length(vc) > 0) {
      for (re_name in names(vc)) {
        # De diagonaal van de matrix bevat de variantie.
        # Als deze < 0.00001 is, draagt de RE feitelijk niets bij (singulier).
        if (as.numeric(vc[[re_name]][1, 1]) < 1e-5) {
          problem_res <- c(problem_res, re_name)
        }
      }
    }

    if (length(problem_res) == 0) {
      # Geen singuliere RE's gevonden, breek uit de loop
      singularity_resolved <- TRUE
    } else {
      # Zet de namen (bijv. "owl") om naar formule-termen (bijv. "(1|owl)")
      terms_to_remove <- paste0("(1|", problem_res, ")")

      # Verwijder de problematische termen uit de huidige lijst
      current_re_terms <- setdiff(current_re_terms, terms_to_remove)

      # Fit het model opnieuw zonder de singuliere random effects
      model <- fit_model_with_re(current_re_terms)
    }
  }

  return(model)
}
safe_fit <- safely(fit_hydro_model)

# Wrapper voor Macro-invertebraten
run_all_mi <- function(dataset) {
  list(
    mmif     = safe_fit(dataset, "mmif", ordbeta()),
    ept_prop = safe_fit(dataset, "ept_prop", binomial(link = "logit"), weight_col = "ta_xw"),
    sw_dw    = safe_fit(dataset, "sw_dw", gaussian()),
    ta_xw    = safe_fit(dataset, "ta_xw", poisson())
  )
}

# Wrapper voor Macrofyten
run_all_mafy <- function(dataset) {
  list(
    index_nieuw   = safe_fit(dataset, "index_nieuw", ordbeta()),
    v_zonder_gep  = safe_fit(dataset, "v_zonder_gep", ordbeta()),
    ts_zonder_gep = safe_fit(dataset, "ts_zonder_gep", ordbeta()),
    vo_zonder_gep = safe_fit(dataset, "vo_zonder_gep", ordbeta()),
    gv_zonder_gep = safe_fit(dataset, "gv_zonder_gep", ordbeta())
  )
}

# --- Datasets in een lijst zetten ---
# Maak een tibble om alles overzichtelijk te houden
analyses <- tibble(
  groep = c("beek", "kempen", "rtnt", "rivier", "polder"),

  # 5 elementen
  mi_data = list(mi_nat_sv_beek, mi_nat_sv_kempen, mi_rtnt, mi_nat_sv_rivier, mi_nat_sv_polder),

  # 5 elementen: gebruik NULL op de 3e positie (voor rtnt)
  mafy_data = list(mafy_nat_sv_beek, mafy_nat_sv_kempen, NULL, mafy_nat_sv_rivier, mafy_nat_sv_polder)
)


# Data in de tibble (let op de NULL bij rtnt voor mafy!)
analyses <- tibble(
  groep = c("beek", "kempen", "rtnt", "rivier", "polder"),
  mi_data = list(mi_nat_sv_beek, mi_nat_sv_kempen, mi_rtnt, mi_nat_sv_rivier, mi_nat_sv_polder),
  mafy_data = list(mafy_nat_sv_beek, mafy_nat_sv_kempen, NULL, mafy_nat_sv_rivier, mafy_nat_sv_polder)
)

# Start met modelleren (Dit kan even duren! Er worden nu zo'm 40+ modellen gefit)
results_all <- analyses %>%
  mutate(
    mi_models = map(mi_data, run_all_mi),
    mafy_models = map(mafy_data, run_all_mafy)
  )
# Extraheer het ept_prop model voor de "beek" groep (rij 1)
ept_beek_model <- results_all$mi_models[[1]]$sw_dw$result
summary(ept_beek_model)

# Voorbeeld om te zien of het model met vo_zonder_gep voor polder (rij 5) gefaald is:
results_all$mafy_models[[5]]$vo_zonder_gep$error


# --- Hulpfunctie om modellen uit een lijst te trekken en netjes te maken ---
extract_tidy_models <- function(model_list) {
  # Als de hele lijst NULL is (bijv. macrofyten bij rtnt), doe niets
  if (is.null(model_list)) return(NULL)

  # Behoud alleen de elementen waar effectief een model in zit ($result is niet NULL)
  valid_models <- keep(model_list, ~ !is.null(.x$result))

  # Als er geen enkel model gelukt is, stop dan
  if (length(valid_models) == 0) return(NULL)

  # Loop over de geldige modellen, haal de coëfficiënten eruit en voeg de index-naam toe
  imap_dfr(valid_models, function(safe_obj, index_name) {
    model <- safe_obj$result

    # Haal vaste effecten (hydromorfologie) eruit
    tidied <- tidy(model, effects = "fixed", conf.int = TRUE)

    # Voeg een kolom toe zodat we weten over welke index dit gaat
    tidied$index <- index_name
    return(tidied)
  })
}

# --- 1. Pak MI modellen uit ---
mi_effects <- results_all %>%
  select(groep, mi_models) %>%
  mutate(coefs = map(mi_models, extract_tidy_models)) %>%
  select(-mi_models) %>%
  unnest(coefs) %>%
  mutate(biota = "Macro-invertebraten")

# --- 2. Pak MAFY modellen uit ---
mafy_effects <- results_all %>%
  select(groep, mafy_models) %>%
  mutate(coefs = map(mafy_models, extract_tidy_models)) %>%
  select(-mafy_models) %>%
  unnest(coefs) %>%
  mutate(biota = "Macrofyten")

# --- 3. Alles samenvoegen en filteren op significantie ---
alle_significante_effecten <- bind_rows(mi_effects, mafy_effects) %>%
  filter(term != "(Intercept)") %>% # Intercept is meestal niet boeiend voor de interpretatie
  filter(p.value < 0.05) %>%        # Hou enkel p < 0.05 over
  select(biota, groep, index, term, estimate, conf.low, conf.high, p.value) %>%
  arrange(biota, index, groep, p.value) # Sorteer alles netjes

# Bekijk de tabel in een apart scherm in RStudio
View(alle_significante_effecten)

# Of print de top resultaten in je console
print(alle_significante_effecten, n = 20)
