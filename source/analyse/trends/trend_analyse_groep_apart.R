# ==============================================================================
# 0. PACKAGES INLADEN
# ==============================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(glmmTMB)
library(sjPlot)
library(ggplot2)
library(patchwork)
library(corrplot)
library(performance)


# ==============================================================================
# 1. DATA PREPARATIE (Lijst maken)
# ==============================================================================
# Plaats al je ruwe datasets in een benoemde lijst.
# Zorg dat de kolommen 'meetplaats', 'owl', 'jaar', 'mmif', 'n_t', etc. kloppen.
lijst_datasets <- list(
  "Beek"   = mi_nat_sv_beek,
  "Kempen" = mi_nat_sv_kempen,
  "Rivier" = mi_nat_sv_rivier,
  "Polder" = mi_nat_sv_polder,
  "Rtnt"   = mi_rtnt
)

# ==============================================================================
# 2. FUNCTIE: TRENDS BEREKENEN PER MEETPLAATS (FLEXIBELE N-CHECK)
# ==============================================================================
bereken_trends <- function(df) {

  # A. Veilige helper-functie voor de predictoren
  bereken_helling <- function(var_naam) {
    # 1. Filter de NA's eruit voor deze specifieke variabele
    df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar))

    # 2. Check of we na het filteren nog steeds minimaal 3 punten hebben
    if (nrow(df_var) < 3) return(NA_real_)

    # 3. Bereken de trend
    formule <- as.formula(paste0(var_naam, " ~ jaar"))
    tryCatch({
      model <- lm(formule, data = df_var)
      return(coef(model)["jaar"])
    }, error = function(e) NA_real_)
  }

  # B. Respons (MMIF) apart met Sinclair methode
  trend_mmif <- tryCatch({
    df_mmif <- df %>% filter(!is.na(mmif), !is.na(jaar))

    # Zelfde check: minimaal 3 punten nodig voor de respons
    if (nrow(df_mmif) < 3) {
      NA_real_
    } else {
      lm_mmif <- lm(log(mmif + 0.01) ~ jaar, data = df_mmif)
      (exp(coef(lm_mmif)["jaar"]) - 1) * 100
    }
  }, error = function(e) NA_real_)

  # C. Pas de veilige helper toe op alle predictoren
  trend_n_t   <- bereken_helling("n_t")
  trend_p_t   <- bereken_helling("p_t")
  trend_o2    <- bereken_helling("o2")
  trend_ec_20 <- bereken_helling("ec_20")
  trend_czv <- bereken_helling("czv")
  # trend_lb    <- bereken_helling("intensiteit_combo_afstr")

  # Bepaal het aantal observaties voor de modelweging (bijv. op basis van de respons)
  # Dit is eerlijker dan gewoon nrow(df), omdat het de echte hoeveelheid data weerspiegelt
  n_geldige_jaren <- sum(!is.na(df$mmif))

  # Resultaat teruggeven
  return(tibble(
    n_obs       = n_geldige_jaren,
    trend_mmif  = trend_mmif,
    trend_n_t   = as.numeric(trend_n_t),
    trend_p_t   = as.numeric(trend_p_t),
    trend_o2    = as.numeric(trend_o2),
    trend_ec_20 = as.numeric(trend_ec_20),
    trend_czv    = as.numeric(trend_czv)
    # trend_lb    = as.numeric(trend_lb)
  ))
}

# ==============================================================================
# 3. FUNCTIE: DATA TRANSFOREREN & MODEL FITTEN
# ==============================================================================
fit_trend_model <- function(df_raw) {

  # A. Maak de dataset "plat" (van rijen per jaar naar 1 rij per meetplaats met trends)
  df_trends <- df_raw %>%
    # Optioneel: als df_raw een spatial object (sf) is, haal de geometrie eraf!
    # sf::st_drop_geometry() %>%
    group_by(meetplaats, owl) %>%
    nest() %>%
    mutate(trends = map(data, bereken_trends)) %>%
    filter(!map_lgl(trends, is.null)) %>% # Gooi meetplaatsen met N<3 weg
    unnest(trends) %>%
    ungroup() %>%
    # Verwijder rijen waar de trendberekening per ongeluk een NA opleverde
    drop_na(starts_with("trend_"))

  # B. Predictoren schalen (Z-scores) voor eerlijke effectgroottes in de plot
  df_model <- df_trends %>%
    mutate(across(starts_with("trend_") & !matches("trend_mmif"), ~as.numeric(scale(.))))

  # C. Model fitten (MET WEGING: weights = n_obs)
  model <- glmmTMB(
    trend_mmif ~ trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + (1 | owl),
    weights = n_obs,
    data = df_model,
    # t_family() kan hier ook als de residual plots later 'dikke staarten' aantonen!
    # family = t_family(link = "identity")
  )

  return(model)
}

# ==============================================================================
# 4. FUNCTIE: FOREST PLOTS MAKEN
# ==============================================================================
maak_forest_plot <- function(model, naam) {

  # sjPlot genereert een ggplot object op basis van het model
  p <- plot_model(
    model,
    type = "est",
    vline.color = "grey50",
    sort.est = TRUE,
    title = naam,
    axis.title = "Effect op de trend van MMIF"
  )

  # Jouw slimmigheidje om niet-significante effecten transparant te maken
  p <- p +
    # Optioneel de assen vastzetten (afhankelijk van de spreiding in je data)
    # ylim(-5, 5) +
    aes(alpha = p.value < 0.05) +
    scale_alpha_manual(
      values = c("TRUE" = 1, "FALSE" = 0.3),
      guide = "none"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  return(p)
}

# ==============================================================================
# 5. UITVOERING
# ==============================================================================
cat("▶ Modellen fitten per typologie...\n")
modellen_lijst_trends <- map(lijst_datasets, fit_trend_model)

cat("▶ Forest plots genereren...\n")
forest_plots <- imap(modellen_lijst_trends, ~ maak_forest_plot(.x, .y))

# Combineer alles via patchwork
figuur_forest <- wrap_plots(forest_plots, ncol = 2) +
  plot_annotation(
    title = "Samenhang tussen veranderingen in predictoren en de ecologische trend (MMIF)",
    subtitle = "Z-geschaalde predictortrends; gewogen naar het aantal metingen (N >= 3)"
  )

# Laat de plot zien
print(figuur_forest)



# ==============================================================================
# 1. VIF BEREKENEN EN PRINTEN VOOR ALLE TYPOLOGIEËN
# ==============================================================================
cat("▶ Collineariteit (VIF) berekenen voor alle typologieën...\n")

# Bereken de VIF voor alle modellen (deze lijst had je al gemaakt via fit_trend_model)
vif_resultaten <- map(modellen_lijst_trends, check_collinearity)

# Loop over de resultaten en print ze met een mooi overzichtje in de console
iwalk(vif_resultaten, ~ {
  cat("\n==================================================\n")
  cat("VIF Resultaten voor Typologie:", toupper(.y), "\n")
  cat("==================================================\n")
  print(.x)
})


# ==============================================================================
# 2. CORRPLOTS MAKEN VOOR ALLE TYPOLOGIEËN IN ÉÉN OVERZICHTSBEELD
# ==============================================================================
# Corrplot gebruikt "Base R" graphics (geen ggplot).
# We forceren het plotvenster in een raster van 2 rijen en 3 kolommen.
par(mfrow = c(2, 3))

for (typo_naam in names(lijst_datasets)) {

  df_raw <- lijst_datasets[[typo_naam]]

  # A. Data prepareren (we gebruiken de robuuste 'bereken_trends' functie)
  df_trends <- df_raw %>%
    group_by(meetplaats, owl) %>%
    nest() %>%
    mutate(trends = map(data, bereken_trends)) %>%
    filter(!map_lgl(trends, is.null)) %>%
    unnest(trends) %>%
    ungroup() %>%
    drop_na(starts_with("trend_"))

  # B. Selecteer enkel de predictoren
  df_predictoren <- df_trends %>%
    select(trend_n_t, trend_p_t, trend_o2, trend_ec_20, trend_czv)

  # Korte namen geven zodat de plot niet rommelig wordt
  colnames(df_predictoren) <- c("N", "P", "O2", "EC20", "CZV")

  # C. Correlatiematrix berekenen (Spearman is veiliger voor ecologische trends)
  cor_matrix <- cor(df_predictoren, method = "spearman")

  # D. Teken de plot in het huidige raster-vakje
  corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    number.cex = 0.8,       # Maakt de correlatie-cijfertjes iets kleiner
    tl.col = "black",
    tl.srt = 45,            # Labels 45 graden schuin
    diag = FALSE,
    title = paste("Correlaties:", typo_naam),
    mar = c(0, 0, 2, 0)     # Voorkomt dat de titel buiten de plot valt
  )
}

# Zet het grafische venster weer netjes terug naar de standaard (1 plot per scherm)
par(mfrow = c(1, 1))

