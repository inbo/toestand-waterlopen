library(dplyr)
library(tidyr)
library(purrr)
library(glmmTMB)
library(ggplot2)
library(patchwork)
library(performance)
library(sjPlot)
library(DHARMa)

# ==============================================================================
# 1. DATA VOORBEREIDEN
# ==============================================================================
df_lange_termijn <- mi_data_analyse %>%
  filter(statuut != "Kunstmatig")

# ==============================================================================
# 2. DE SLANKE "MASTER" PIJPLEIN FUNCTIE (Enkel O2, T, EC20)
# ==============================================================================
analyseer_lange_termijn <- function(resp_var, data_raw) {

  cat(paste("\n▶ Start lange-termijn analyse voor:", toupper(resp_var), "...\n"))

  # A. Dynamische trend-berekening
  bereken_trends_dynamisch <- function(df) {
    bereken_helling <- function(var_naam) {
      df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar))
      if (nrow(df_var) < 4) return(NA_real_)
      tryCatch({
        model <- lm(as.formula(paste0(var_naam, " ~ jaar")), data = df_var)
        coef(model)["jaar"]
      }, error = function(e) NA_real_)
    }

    # Initiële startwaarde
    init_val <- tryCatch({
      df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar)) %>%
        arrange(jaar) %>% slice(1) %>% pull(.data[[resp_var]])
    }, error = function(e) NA_real_)
    if (length(init_val) == 0) init_val <- NA_real_

    # Sinclair Trend voor respons
    trend_resp <- tryCatch({
      df_resp <- df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar))
      if (nrow(df_resp) < 4) { NA_real_ } else {
        formule_resp <- as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar"))
        lm_resp <- lm(formule_resp, data = df_resp)
        (exp(coef(lm_resp)["jaar"]) - 1) * 100
      }
    }, error = function(e) NA_real_)

    # Enkel de 3 geselecteerde predictoren!
    return(tibble(
      n_obs       = sum(!is.na(df[[resp_var]])),
      init_val    = init_val,
      trend_resp  = trend_resp,
      trend_o2    = as.numeric(bereken_helling("o2")),
      trend_t     = as.numeric(bereken_helling("t")),
      trend_ec_20 = as.numeric(bereken_helling("ec_20"))
    ))
  }

  # B. Pas functie toe & schaal predictoren
  # Let op: check of 'groep' exact zo heet in mi_data_analyse (of pas aan naar bvb. 'typologie')
  df_trends <- data_raw %>%
    group_by(meetplaats, owl, groep, bekken) %>%
    nest() %>%
    mutate(trends = map(data, bereken_trends_dynamisch)) %>%
    filter(!map_lgl(trends, is.null)) %>%
    unnest(trends) %>%
    ungroup() %>%
    drop_na(starts_with("trend_"), init_val) %>%
    mutate(across(c(trend_o2, trend_ec_20, trend_t), ~as.numeric(scale(.))))

  # Hernoem de kolommen dynamisch
  trend_kolom <- paste0("trend_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "init_val"] <- paste0("init_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "trend_resp"] <- trend_kolom

  # C. Outliers filteren EN NAs verwijderen voor de modelvariabelen
  df_clean <- df_trends %>%
    filter(
      .data[[trend_kolom]] > -50 & .data[[trend_kolom]] < 50,
      abs(trend_o2) < 4, abs(trend_t) < 4, abs(trend_ec_20) < 4
    ) %>%
    # HIER IS DE FIX: gooi elke rij met een NA in 'groep' resoluut weg
    drop_na(groep)

  # D. Model fitten (GLMMTMB t-family, met enkel 3 predictoren)
  formule_model <- as.formula(paste0(trend_kolom, " ~ (trend_o2 + trend_ec_20 + trend_t) + groep"))
  model_fit <- glmmTMB(formule_model, family = t_family(), data = df_clean)

  # E. Pseudo R2 berekenen (met extra veiligheid voor missende data)
  voorspeld <- predict(model_fit, type = "response")
  pseudo_r2 <- cor(df_clean[[trend_kolom]], voorspeld, use = "complete.obs")^2

  # F. Forest Plot genereren
  plot_titel <- paste0(toupper(resp_var), "\n(Pseudo-R2 = ", round(pseudo_r2, 2), ")")

  p_forest <- plot_model(
    model_fit, type = "est", vline.color = "grey50", sort.est = TRUE,
    title = plot_titel, axis.title = "Effectgrootte"
  ) +
    aes(alpha = p.value < 0.05) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3), guide = "none") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

  return(list(
    model     = model_fit,
    data      = df_clean,
    r2        = pseudo_r2,
    plot      = p_forest
  ))
}

# ==============================================================================
# 3. UITVOEREN VOOR DE SET 1989-2023
# ==============================================================================
respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Voer de loop uit over de nieuwe lange-termijn dataset
resultaten_lt <- map(respons_lijst, ~analyseer_lange_termijn(.x, df_lange_termijn))
names(resultaten_lt) <- respons_lijst

# Alle forest plots samenvoegen
alle_plots_lt <- map(resultaten_lt, "plot")
master_figuur_lt <- wrap_plots(alle_plots_lt, ncol = 2) +
  plot_annotation(
    title = "Lange Termijn (1989-2023): Effecten op Macro-invertebraten",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

print(master_figuur_lt)


# Zorg dat de juiste packages geladen zijn
library(ggeffects)
library(ggplot2)
library(patchwork)
library(purrr)

# ==============================================================================
# 1. FUNCTIE: MAAK ÉÉN MARGINALE PLOT VANUIT DE RESULTATENLIJST
# ==============================================================================
plot_marginaal_lt <- function(resp_var, predictor_var, mooie_naam, resultaten_lijst) {

  # A. Haal het model en de bijbehorende data uit de lijst
  mod <- resultaten_lijst[[resp_var]]$model
  df_clean <- resultaten_lijst[[resp_var]]$data
  trend_kolom <- paste0("trend_", resp_var)

  # B. Bereken de marginale effecten (andere variabelen blijven op gemiddelde)
  voorsp <- ggpredict(mod, terms = predictor_var)

  # C. Bouw de plot
  p <- ggplot() +
    # Grijze nullijn (geen ecologisch herstel)
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # Ruwe datapunten (gekleurd per typologie/groep)
    geom_point(data = df_clean, aes(x = .data[[predictor_var]], y = .data[[trend_kolom]], color = groep),
               alpha = 0.4, size = 1.5) +

    # Model onzekerheidsmarge (ribbon)
    geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high),
                alpha = 0.2, fill = "black", inherit.aes = FALSE) +

    # Regressielijn van het model
    geom_line(data = voorsp, aes(x = x, y = predicted),
              linewidth = 1.2, color = "black", inherit.aes = FALSE) +

    # Kleuren en labels
    scale_color_brewer(palette = "Set1", name = "Typologie") +
    labs(
      title = toupper(resp_var),
      x = paste("Trend", mooie_naam, "(Z-score)"),
      y = "Trend Respons (%/jaar)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none" # We verbergen individuele legendes, patchwork regelt dit straks!
    )

  return(p)
}

# ==============================================================================
# 2. GENEREREN VAN DE PLOTS VOOR ZUURSTOF (O2)
# ==============================================================================
cat("▶ Marginale plots voor O2 genereren...\n")

respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Maak een lijst met 6 O2 plots
plots_o2 <- map(respons_lijst, ~plot_marginaal_lt(.x, "trend_o2", "O2", resultaten_lt))

# Combineer ze met patchwork
figuur_o2 <- wrap_plots(plots_o2, ncol = 2) +
  plot_layout(guides = "collect") + # Één gedeelde legende voor alle plots
  plot_annotation(
    title = "Marginaal Effect van Zuurstoftrend (O2) op Ecologisch Herstel",
    subtitle = "Lineaire modellen (1989-2023), gecontroleerd voor temperatuur en EC20.",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "bottom")
  )

print(figuur_o2)

# ==============================================================================
# 3. GENEREREN VAN DE PLOTS VOOR GELEIDBAARHEID (EC20)
# ==============================================================================
cat("▶ Marginale plots voor EC20 genereren...\n")

# Maak een lijst met 6 EC20 plots
plots_ec20 <- map(respons_lijst, ~plot_marginaal_lt(.x, "trend_ec_20", "EC20", resultaten_lt))

# Combineer ze met patchwork
figuur_ec20 <- wrap_plots(plots_ec20, ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Marginaal Effect van Geleidbaarheid (EC20) op Ecologisch Herstel",
    subtitle = "Lineaire modellen (1989-2023), gecontroleerd voor temperatuur en zuurstof.",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "bottom")
  )

print(figuur_ec20)
