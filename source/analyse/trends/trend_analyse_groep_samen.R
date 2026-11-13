library(dplyr)
library(tidyr)
library(purrr)
library(glmmTMB)
library(ggeffects)
library(ggplot2)
library(patchwork)
library(corrplot)
library(performance)
library(sjPlot)
library(buildmer)
library(nlme)
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# ==============================================================================
# 1. FUNCTIE AANPASSEN (Initiele MMIF toegevoegd)
# ==============================================================================
bereken_trends <- function(df) {

  # A. Veilige helper-functie voor de predictoren
  bereken_helling <- function(var_naam) {
    df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar))
    if (nrow(df_var) < 4) return(NA_real_)

    formule <- as.formula(paste0(var_naam, " ~ jaar"))
    tryCatch({
      model <- lm(formule, data = df_var)
      return(coef(model)["jaar"])
    }, error = function(e) NA_real_)
  }

  # B. Bepaal de initiële (start) waarde van MMIF voor deze meetplaats
  init_mmif <- tryCatch({
    df %>%
      filter(!is.na(mmif), !is.na(jaar)) %>%
      arrange(jaar) %>% # Zorg dat we chronologisch correct zitten
      slice(1) %>%      # Pak de allereerste rij (oudste jaar)
      pull(mmif)
  }, error = function(e) NA_real_)

  # Handvat voor het geval er helemaal geen data is
  if (length(init_mmif) == 0) init_mmif <- NA_real_

  # C. Respons (MMIF) apart met Sinclair methode
  trend_mmif <- tryCatch({
    df_mmif <- df %>% filter(!is.na(mmif), !is.na(jaar))
    if (nrow(df_mmif) < 4) {
      NA_real_
    } else {
      lm_mmif <- lm(log(mmif + 0.01) ~ jaar, data = df_mmif)
      (exp(coef(lm_mmif)["jaar"]) - 1) * 100
    }
  }, error = function(e) NA_real_)

  # D. Predictoren berekenen
  trend_n_t   <- bereken_helling("n_t")
  trend_p_t   <- bereken_helling("p_t")
  trend_o2    <- bereken_helling("o2")
  trend_t     <- bereken_helling("t")
  trend_ec_20 <- bereken_helling("ec_20")
  trend_czv   <- bereken_helling("czv")

  n_geldige_jaren <- sum(!is.na(df$mmif))

  # E. Resultaat teruggeven, nu INCLUSIEF init_mmif
  return(tibble(
    n_obs       = n_geldige_jaren,
    init_mmif   = init_mmif,       # <--- Nieuw toegevoegd!
    trend_mmif  = trend_mmif,
    trend_n_t   = as.numeric(trend_n_t),
    trend_p_t   = as.numeric(trend_p_t),
    trend_o2    = as.numeric(trend_o2),
    trend_t     = as.numeric(trend_t),
    trend_ec_20 = as.numeric(trend_ec_20),
    trend_czv   = as.numeric(trend_czv)
  ))
}
bereken_trends_gls <- function(df) {

  # A. Veilige helper-functie met GLS-AR1 en LM fallback
  bereken_helling <- function(var_naam) {
    df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar))
    if (nrow(df_var) < 4) return(NA_real_)

    formule <- as.formula(paste0(var_naam, " ~ jaar"))

    # Probeer eerst GLS met AR(1)
    tryCatch({
      model_gls <- gls(formule, data = df_var, correlation = corAR1(form = ~ jaar))
      return(coef(model_gls)["jaar"])
    }, error = function(e1) {
      # Fallback: als GLS faalt, gebruik gewone lm
      tryCatch({
        model_lm <- lm(formule, data = df_var)
        return(coef(model_lm)["jaar"])
      }, error = function(e2) NA_real_)
    })
  }

  # B. Initiële (start) waarde MMIF (blijft identiek)
  init_mmif <- tryCatch({
    df %>%
      filter(!is.na(mmif), !is.na(jaar)) %>%
      arrange(jaar) %>%
      slice(1) %>%
      pull(mmif)
  }, error = function(e) NA_real_)
  if (length(init_mmif) == 0) init_mmif <- NA_real_

  # C. Respons (MMIF) met Sinclair EN hybride GLS/LM
  trend_mmif <- tryCatch({
    df_mmif <- df %>% filter(!is.na(mmif), !is.na(jaar))
    if (nrow(df_mmif) < 4) {
      NA_real_
    } else {
      helling <- tryCatch({
        # Probeer eerst GLS
        m_gls <- gls(log(mmif + 0.01) ~ jaar, data = df_mmif, correlation = corAR1(form = ~ jaar))
        coef(m_gls)["jaar"]
      }, error = function(e1) {
        # Fallback naar LM
        m_lm <- lm(log(mmif + 0.01) ~ jaar, data = df_mmif)
        coef(m_lm)["jaar"]
      })
      (exp(helling) - 1) * 100
    }
  }, error = function(e) NA_real_)

  # D. Predictoren berekenen
  trend_n_t   <- bereken_helling("n_t")
  trend_p_t   <- bereken_helling("p_t")
  trend_o2    <- bereken_helling("o2")
  trend_t     <- bereken_helling("t")
  trend_ec_20 <- bereken_helling("ec_20")
  trend_czv   <- bereken_helling("czv")

  n_geldige_jaren <- sum(!is.na(df$mmif))

  # E. Resultaat teruggeven
  return(tibble(
    n_obs       = n_geldige_jaren,
    init_mmif   = init_mmif,
    trend_mmif  = trend_mmif,
    trend_n_t   = as.numeric(trend_n_t),
    trend_p_t   = as.numeric(trend_p_t),
    trend_o2    = as.numeric(trend_o2),
    trend_t     = as.numeric(trend_t),
    trend_ec_20 = as.numeric(trend_ec_20),
    trend_czv   = as.numeric(trend_czv)
  ))
}

# ==============================================================================
# 2. HET GEZAMENLIJKE MODEL DRAAIEN
# ==============================================================================
# A. Samenvoegen tot één dataset
df_master_raw <- bind_rows(lijst_datasets, .id = "groep") %>%
  filter(groep %in% c("Beek", "Kempen"))

# B. Trends berekenen en NAs eruit filteren
df_master_trends <- df_master_raw %>%
  # HIER ZIT DE MAGIC: Voeg ekc2_waterlichaam toe aan de groepering!
  group_by(meetplaats, owl, groep, bekken, ekc2_waterlichaam) %>%
  nest() %>%
  mutate(trends = map(data, bereken_trends)) %>%
  filter(!map_lgl(trends, is.null)) %>%
  unnest(trends) %>%
  ungroup() %>%
  drop_na(starts_with("trend_"), init_mmif)

# C. Predictoren schalen (Z-scores)
df_master_model <- df_master_trends %>%
  mutate(across(starts_with("trend_") & !matches("trend_mmif"), ~as.numeric(scale(.))))

# D. Dataset opschonen (outliers filteren)
df_master_model_clean <- df_master_model %>%
  filter(
    trend_mmif > -50 & trend_mmif < 50,
    abs(trend_n_t) < 4,
    abs(trend_p_t) < 4,
    abs(trend_o2) < 4,
    abs(trend_t) < 4,
    abs(trend_ec_20) < 4,
    abs(trend_czv) < 4
  )

cat("Aantal meetplaatsen verwijderd wegens extreme waarden:",
    nrow(df_master_model) - nrow(df_master_model_clean), "\n")

# D. Het Master-Model fitten (met trend_czv en groep als fixed effect)
model_master <- glmmTMB(
  trend_mmif ~ (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t) +
    groep,
  # weights = n_obs,
  family = t_family(),
  data = df_master_model_clean
)

# Resultaten bekijken
summary(model_master)
# 1. Haal de originele (geobserveerde) MMIF-trends uit je opgeschoonde dataset
geobserveerd <- df_master_model_clean$trend_mmif

# 2. Haal de voorspelde trends op (wat jouw model 'denkt' dat de trend zou moeten zijn)
voorspeld <- predict(model_master, type = "response")

# 3. Bereken de correlatie (Pearson) tussen die twee en zet hem in het kwadraat
pseudo_r2 <- cor(geobserveerd, voorspeld)^2

# Print het resultaat mooi afgerond
cat("De pseudo-R² van het Master-Model is:", round(pseudo_r2, 3), "\n")
check_singularity(model_master)
res_trend <- simulateResiduals(model_master, plot = TRUE)

#gam
# Laad het mgcv package in
library(mgcv)

# 1. Het GAM Model
model_master_gam <- gam(
  trend_mmif ~ s(trend_n_t) +
    s(trend_p_t) +
    s(trend_o2) +
    s(trend_ec_20) +
    s(trend_czv) +
    s(trend_t) +
    groep, # Groep blijft een gewone lineaire / vaste term (geen curve!)

  family = scat(), # Equivalent van t_family() in mgcv
  # weights = n_obs, # Kan je hier gewoon weer aanzetten als je wilt!
  data = df_master_model_clean,
  method = "REML"  # Cruciaal voor correcte penalizatie van GAMs
)

# 2. Bekijk de resultaten
summary(model_master_gam)

# 3. Genereer de GAM plots (om de curves te zien!)
# shade = TRUE voegt betrouwbaarheidsintervallen toe
# pages = 1 zet alle plots gezellig samen op 1 pagina
plot(model_master_gam, shade = TRUE, pages = 1, col = "blue")

# 4. Check of je curves complex genoeg mochten zijn van het model
# Kijk hierbij of de p-waarden aan de rechterkant NIET significant zijn (dat is goed!)
gam.check(model_master_gam)


model_master_init <- glmmTMB(
  trend_mmif ~ init_mmif * (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t) +
    groep,
  # weights = n_obs,
  family = t_family(),
  data = df_master_model_clean
)

# Resultaten bekijken
summary(model_master_init)

# 1. Haal de originele (geobserveerde) MMIF-trends uit je opgeschoonde dataset
geobserveerd <- df_master_model_clean$trend_mmif

# 2. Haal de voorspelde trends op (wat jouw model 'denkt' dat de trend zou moeten zijn)
voorspeld <- predict(model_master_init, type = "response")

# 3. Bereken de correlatie (Pearson) tussen die twee en zet hem in het kwadraat
pseudo_r2 <- cor(geobserveerd, voorspeld)^2

# Print het resultaat mooi afgerond
cat("De pseudo-R² van het Master-Model is:", round(pseudo_r2, 3), "\n")

check_singularity(model_master_init)
res_trend <- simulateResiduals(model_master_init, plot = TRUE)

#test sac

xy <- data5 %>%
  distinct(meetplaats, geom) %>%
  select(meetplaats, geom) %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]) %>%
  select(-geom)


coords_trend <- df_master_model_clean %>%
  select(meetplaats) %>% # Behoud enkel de volgorde van de meetplaatsen in het model
  left_join(xy, by = "meetplaats")

# 3. Voer de ruimtelijke autocorrelatie test direct uit
testSpatialAutocorrelation(
  res_trend,
  x = coords_trend$x,
  y = coords_trend$y
)

######
# init model backwards test
###########"
# Installeer het package eenmalig als je het nog niet hebt:
# install.packages("buildmer")


cat("▶ Bezig met backwards selection via buildmer. Dit kan even duren...\n")

# Gebruik buildglmmTMB met nagenoeg dezelfde syntax als je normale model
model_geoptimaliseerd <- buildglmmTMB(
  formula = trend_mmif ~ init_mmif * (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t) + groep,
  data = df_master_model_clean,
  family = t_family(link = "identity"), # De robuuste verdeling behouden!
  # Optioneel: weights = n_obs,
  buildmerControl = buildmerControl(
    direction = "backward"  )
)

# 1. Bekijk de samenvatting (welke variabelen hebben het overleefd?)
summary(model_geoptimaliseerd)

# 2. Extract het pure glmmTMB model (om te gebruiken voor je plots en DHARMa)
# (buildmer stopt het echte model in een 'slot' genaamd @model)
model_master <- model_geoptimaliseerd@model

# 3. Check je verklaarde variantie van het nieuwe, simpelere model
geobserveerd <- df_master_model_clean$trend_mmif
voorspeld <- predict(model_master, type = "response")
pseudo_r2 <- cor(geobserveerd, voorspeld)^2
cat("Pseudo-R² van het geoptimaliseerde model:", round(pseudo_r2, 3), "\n")


# ==============================================================================
# 1. DE HARDE STATISTIEK: VIF BEREKENEN VOOR HET MASTER-MODEL
# ==============================================================================
cat("\n▶ Collineariteit (VIF) berekenen voor het Master-Model...\n")
vif_master <- check_collinearity(model_master)
print(vif_master)
# plot(vif_master) # Optioneel: als je het 'see' package hebt, geeft dit een mooie plot


# ==============================================================================
# 2. DE VISUELE CHECK: CORRELATIEPLOT VAN DE MASTER-DATASET
# ==============================================================================
cat("\n▶ Correlatieplot genereren...\n")

# Selecteer enkel de 5 predictoren uit de samengevoegde dataset
df_predictoren_master <- df_master_model %>%
  select(trend_n_t, trend_p_t, trend_o2, trend_ec_20, trend_czv, trend_t)

# Korte, overzichtelijke namen geven voor in de plot
colnames(df_predictoren_master) <- c("N", "P", "O2", "EC20", "CZV", "T")

# Correlatiematrix berekenen (Spearman)
cor_matrix_master <- cor(df_predictoren_master, method = "spearman")

# Maak de plot
corrplot(
  cor_matrix_master,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  title = "Correlatie Predictor-trends (Geheel Vlaanderen)",
  mar = c(0, 0, 2, 0)
)


# ==============================================================================
# 3. DE FOREST PLOT VOOR HET MASTER-MODEL
# ==============================================================================
cat("\n▶ Forest plot genereren...\n")

# sjPlot genereert de basisplot
plot_master <- plot_model(
  model_master,
  type = "est",
  vline.color = "grey50",
  sort.est = TRUE,
  title = "Gezamenlijk Master-Model: Effect op MMIF-trend",
  axis.title = "Effectgrootte (Z-score)"
)

# Transparantie toevoegen voor niet-significante effecten (jouw slimme truc!)
plot_master_finaal <- plot_master +
  aes(alpha = p.value < 0.05) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.3),
    guide = "none"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Laat de forest plot zien
print(plot_master_finaal)



# ==============================================================================
# FUNCTIE: MAAK ÉÉN MARGINALE PLOT
# ==============================================================================
plot_marginaal <- function(predictor_var, mooie_naam) {

  # 1. Bereken de marginale effecten via ggeffects
  # Dit berekent de voorspelde MMIF-trend, terwijl alle ANDERE variabelen
  # (inclusief groep) op hun gemiddelde worden vastgehouden.
  voorsp <- ggpredict(model_master, terms = predictor_var)

  # 2. Bouw de plot laag voor laag op
  p <- ggplot() +
    # Nul-lijn (stippellijn waar MMIF helemaal niet verandert)
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # LAAG 1: Ruwe datapunten op de achtergrond, ingekleurd per typologie
    geom_point(data = df_master_model_clean,
               aes(x = .data[[predictor_var]], y = trend_mmif, color = groep),
               alpha = 0.5, size = 1.5) +

    # LAAG 2: Het betrouwbaarheidsinterval van de modelvoorspelling
    geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high),
                alpha = 0.2, fill = "black", inherit.aes = FALSE) +

    # LAAG 3: De strakke regressielijn van het model
    geom_line(data = voorsp, aes(x = x, y = predicted),
              linewidth = 1.2, color = "black", inherit.aes = FALSE) +

    # Kleuren instellen voor de typologieën (mooi, duidelijk palet)
    scale_color_brewer(palette = "Set1") +

    # Labels toevoegen
    # Let op: de x-as toont Z-scores, want we hebben de predictoren geschaald!
    labs(
      title = mooie_naam,
      x = paste("Trend", mooie_naam, "(Z-score)"),
      y = "Trend MMIF (% verandering/jaar)",
      color = "Typologie"
    ) +

    # Opmaak
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  return(p)
}

# ==============================================================================
# GENEREREN VAN ALLE PLOTS EN COMBINEREN
# ==============================================================================
cat("▶ Marginale effectplots berekenen...\n")

# De exacte kolomnamen in je dataset
predictoren <- c("trend_n_t", "trend_p_t", "trend_o2", "trend_ec_20", "trend_czv", "trend_t")

# De namen die we op de plot willen zien
weergave_namen <- c("Stikstof (N)", "Fosfor (P)", "Zuurstof (O2)", "Geleidbaarheid (EC20)", "CZV", "Temperatuur")

# Gebruik map2 om de functie voor alle 5 de variabelen toe te passen
lijst_marginale_plots <- map2(predictoren, weergave_namen, plot_marginaal)

# Plak ze samen met patchwork
figuur_marginaal <- wrap_plots(lijst_marginale_plots, ncol = 2) +
  plot_annotation(
    title = "Marginale Effecten: Predictortrends vs MMIF-trend",
    subtitle = "Zwarte lijn is de modelvoorspelling. Datapunten zijn de individuele meetplaatsen."
  ) +
  plot_layout(guides = "collect") & # Zet alle legendes samen tot 1 legende
  theme(legend.position = "bottom")

# Bekijken
print(figuur_marginaal)




# ==============================================================================
# FUNCTIE: MAAK ÉÉN INTERACTIE PLOT (Predictor vs Init_MMIF)
# ==============================================================================
plot_marginaal_interactie <- function(predictor_var, mooie_naam, model, df_clean) {

  # 1. Startwaarden (percentielen) voor init_mmif berekenen
  q_waarden <- quantile(df_clean$init_mmif, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_waarden_rond <- round(q_waarden, 2)

  # 2. Voorspellingen via ggeffects (predictor interactie met init_mmif)
  term_x <- paste0(predictor_var, " [all]")
  term_group <- paste0("init_mmif [", paste(q_waarden_rond, collapse = ", "), "]")

  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  # 3. Ruwe data voorbereiden voor de punten (kleur indelen op basis van percentiel)
  df_plot <- df_clean %>%
    filter(!is.na(.data[[predictor_var]]), !is.na(init_mmif)) %>%
    mutate(
      group_plot = case_when(
        init_mmif <= q_waarden[1] ~ as.character(q_waarden_rond[1]),
        init_mmif > q_waarden[1] & init_mmif <= q_waarden[3] ~ as.character(q_waarden_rond[2]),
        TRUE ~ as.character(q_waarden_rond[3])
      ),
      group_plot = factor(group_plot, levels = as.character(q_waarden_rond))
    )

  # 4. Kleuren en labels
  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77") # Oranje, Paars, Groen
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_waarden_rond, ")")

  # 5. Bouw de plot
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # # Ruwe datapunten op de achtergrond
    # geom_point(data = df_plot, aes(x = .data[[predictor_var]], y = trend_mmif, color = group_plot),
    #            alpha = 0.4, size = 1.5) +

    # Onzekerheidsmarges (ribbons) van het model
    geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
                alpha = 0.15, inherit.aes = FALSE) +

    # Voorspellingslijnen van het model
    geom_line(data = voorsp, aes(x = x, y = predicted, color = group),
              linewidth = 1.2, inherit.aes = FALSE) +

    scale_color_manual(name = "Startkwaliteit (MMIF)", values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = "Startkwaliteit (MMIF)", values = mijn_kleuren, labels = labels_aangepast) +

    labs(
      title = mooie_naam,
      x = paste("Trend", mooie_naam, "(Z-score)"),
      y = "Trend MMIF (%/jaar)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  return(p)
}

# ==============================================================================
# GENEREREN VAN DE PLOTS EN COMBINEREN
# ==============================================================================
cat("▶ Marginale interactieplots berekenen...\n")

# LET OP: Voeg hier ENKEL de variabelen in die overleefden in je model_master_best!
predictoren <- c("trend_n_t", "trend_p_t", "trend_czv")
weergave_namen <- c("Stikstof (N)", "Fosfor (P)",  "CZV")

# Gebruik purrr::map2 om alles te genereren
lijst_interactie_plots <- map2(predictoren, weergave_namen,
                               ~plot_marginaal_interactie(.x, .y, model_master, df_master_model_clean))

# Plak ze samen met patchwork
figuur_interacties <- wrap_plots(lijst_interactie_plots, ncol = 2) +
  plot_annotation(
    title = "Invloed van de Startkwaliteit op het Herstel (Interacties)",
    subtitle = "Gekleurde lijnen representeren beken met een initieel Slechte, Matige of Goede MMIF-score."
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Bekijken
print(figuur_interacties)



# ==============================================================================
# FUNCTIE: MAAK ÉÉN INTERACTIE PLOT (Predictor vs Init_MMIF + Echte Nul)
# ==============================================================================
# Let op: we voegen 'df_raw_trends' toe als extra argument!
plot_marginaal_interactie <- function(predictor_var, mooie_naam, model, df_clean, df_raw_trends) {

  # 1. Startwaarden (percentielen) voor init_mmif berekenen
  q_waarden <- quantile(df_clean$init_mmif, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_waarden_rond <- round(q_waarden, 2)

  # 2. Voorspellingen via ggeffects (predictor interactie met init_mmif)
  term_x <- paste0(predictor_var, " [all]")
  term_group <- paste0("init_mmif [", paste(q_waarden_rond, collapse = ", "), "]")

  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  # 3. Bereken de "Echte Nul" (absolute trend = 0) op de Z-schaal voor DEZE variabele
  mu_var    <- mean(df_raw_trends[[predictor_var]], na.rm = TRUE)
  sigma_var <- sd(df_raw_trends[[predictor_var]], na.rm = TRUE)
  z_nul     <- (0 - mu_var) / sigma_var

  # 4. Ruwe data voorbereiden voor de punten (kleur indelen op basis van percentiel)
  df_plot <- df_clean %>%
    filter(!is.na(.data[[predictor_var]]), !is.na(init_mmif)) %>%
    mutate(
      group_plot = case_when(
        init_mmif <= q_waarden[1] ~ as.character(q_waarden_rond[1]),
        init_mmif > q_waarden[1] & init_mmif <= q_waarden[3] ~ as.character(q_waarden_rond[2]),
        TRUE ~ as.character(q_waarden_rond[3])
      ),
      group_plot = factor(group_plot, levels = as.character(q_waarden_rond))
    )

  # 5. Kleuren en labels
  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77") # Oranje, Paars, Groen
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_waarden_rond, ")")

  # 6. Bouw de plot
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # --- DE NIEUWE RODE LIJN VOOR "GEEN VERANDERING" IN PREDICTOR ---
    geom_vline(xintercept = z_nul, linetype = "dotted", color = "red", linewidth = 1) +

    # Optioneel: Ruwe datapunten op de achtergrond (commentaar weggehaald als je ze wilt zien)
    geom_point(data = df_plot, aes(x = .data[[predictor_var]], y = trend_mmif, color = group_plot),
               alpha = 0.4, size = 1.5) +

    # Onzekerheidsmarges (ribbons) van het model
    geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
                alpha = 0.15, inherit.aes = FALSE) +

    # Voorspellingslijnen van het model
    geom_line(data = voorsp, aes(x = x, y = predicted, color = group),
              linewidth = 1.2, inherit.aes = FALSE) +

    scale_color_manual(name = "Startkwaliteit (MMIF)", values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = "Startkwaliteit (MMIF)", values = mijn_kleuren, labels = labels_aangepast) +

    labs(
      title = mooie_naam,
      x = paste("Trend", mooie_naam, "(Z-score)"),
      y = "Trend MMIF (%/jaar)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  return(p)
}

# ==============================================================================
# GENEREREN VAN DE PLOTS EN COMBINEREN
# ==============================================================================
cat("▶ Marginale interactieplots berekenen...\n")

# LET OP: Voeg hier ENKEL de variabelen in die overleefden in je model_master_best!
predictoren <- c("trend_n_t", "trend_p_t",  "trend_czv")
weergave_namen <- c("Stikstof (N)", "Fosfor (P)",  "CZV")

# Gebruik purrr::map2 om alles te genereren.
# We geven nu ook expliciet de ongeschaalde df_master_trends dataset mee!
lijst_interactie_plots <- map2(
  predictoren,
  weergave_namen,
  ~plot_marginaal_interactie(.x, .y, model_master, df_master_model_clean, df_master_trends)
)

# Plak ze samen met patchwork
figuur_interacties <- wrap_plots(lijst_interactie_plots, ncol = 2) +
  plot_annotation(
    title = "Invloed van de Startkwaliteit op het Herstel (Interacties)",
    subtitle = "De rode stippellijn duidt een absolute trend van 0 aan (geen verandering in waterkwaliteit).",
    theme = theme(plot.subtitle = element_text(color = "red4")) # Subtitel een kleurtje geven voor de duidelijkheid
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Bekijken
print(figuur_interacties)
