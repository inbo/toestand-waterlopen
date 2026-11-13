library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)       # Voor de GAM modellen!
library(ggeffects)  # Voor de marginale effecten plots
library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. DE GAM PIJPLEIN FUNCTIE
# ==============================================================================
analyseer_gam_lt <- function(resp_var, data_raw) {

  cat(paste("▶ Fitten van GAM voor:", toupper(resp_var), "...\n"))

  # A. Dynamische trend-berekening (Zelfde als voorheen)
  bereken_trends_dynamisch <- function(df) {
    bereken_helling <- function(var_naam) {
      df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar))
      if (nrow(df_var) < 4) return(NA_real_)
      tryCatch(coef(lm(as.formula(paste0(var_naam, " ~ jaar")), data = df_var))["jaar"],
               error = function(e) NA_real_)
    }

    init_val <- tryCatch(df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar)) %>%
                           arrange(jaar) %>% slice(1) %>% pull(.data[[resp_var]]),
                         error = function(e) NA_real_)
    if (length(init_val) == 0) init_val <- NA_real_

    trend_resp <- tryCatch({
      df_resp <- df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar))
      if (nrow(df_resp) < 4) { NA_real_ } else {
        lm_resp <- lm(as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar")), data = df_resp)
        (exp(coef(lm_resp)["jaar"]) - 1) * 100
      }
    }, error = function(e) NA_real_)

    return(tibble(
      n_obs       = sum(!is.na(df[[resp_var]])),
      init_val    = init_val,
      trend_resp  = trend_resp,
      trend_o2    = as.numeric(bereken_helling("o2")),
      trend_t     = as.numeric(bereken_helling("t")),
      trend_ec_20 = as.numeric(bereken_helling("ec_20"))
    ))
  }

  # B. Data transformeren, unnesten
  df_trends_raw <- data_raw %>%
    group_by(meetplaats, owl, groep, bekken) %>%
    nest() %>%
    mutate(trends = map(data, bereken_trends_dynamisch)) %>%
    filter(!map_lgl(trends, is.null)) %>%
    unnest(trends) %>%
    ungroup() %>%
    drop_na(starts_with("trend_"), init_val)

  # BEREKEN DE ECHTE NUL VOOR O2
  # 1. Wat was de gemiddelde trend en de spreiding (vóór schaling)?
  mu_o2 <- mean(df_trends_raw$trend_o2, na.rm = TRUE)
  sigma_o2 <- sd(df_trends_raw$trend_o2, na.rm = TRUE)

  # 2. Waar ligt de absolute trend van 0 op onze Z-schaal?
  z_nul_o2 <- (0 - mu_o2) / sigma_o2

  # Nu pas schalen we de data voor het model
  df_trends <- df_trends_raw %>%
    mutate(across(c(trend_o2, trend_ec_20, trend_t), ~as.numeric(scale(.))))

  trend_kolom <- paste0("trend_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "init_val"] <- paste0("init_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "trend_resp"] <- trend_kolom

  # C. Outliers filteren EN missende groepen verwijderen
  df_clean <- df_trends %>%
    filter(
      .data[[trend_kolom]] > -50 & .data[[trend_kolom]] < 50,
      abs(trend_o2) < 4, abs(trend_t) < 4, abs(trend_ec_20) < 4
    ) %>%
    drop_na(groep) %>%
    mutate(groep = as.factor(groep)) # GAMs eisen vaak expliciet dat dit een factor is

  # D. Het GAM Model!
  # Let op de s() rondom continue predictoren. 'groep' krijgt geen s() want het is categorisch.
  formule_gam <- as.formula(paste0(trend_kolom, " ~ s(trend_o2) + s(trend_ec_20) + s(trend_t) + groep"))

  model_gam <- gam(
    formule_gam,
    family = scat(),  # mgcv's robuuste t-verdeling voor uitschieters
    data = df_clean,
    method = "REML"   # Zorgt voor correcte 'wiggliness' penalty
  )

  # E. Marginaal effect berekenen voor zuurstof (O2)
  voorsp_o2 <- ggpredict(model_gam, terms = "trend_o2")

  # F. Plot maken
  p_o2 <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # Ruwe data op de achtergrond, ingekleurd per typologie
    # geom_point(data = df_clean, aes(x = trend_o2, y = .data[[trend_kolom]], color = groep),
    #            alpha = 0.4, size = 1.5) +

    # Het GAM betrouwbaarheidsinterval
    geom_ribbon(data = voorsp_o2, aes(x = x, ymin = conf.low, ymax = conf.high),
                alpha = 0.2, fill = "black", inherit.aes = FALSE) +

    # De GAM curve!
    geom_line(data = voorsp_o2, aes(x = x, y = predicted),
              linewidth = 1.2, color = "black", inherit.aes = FALSE) +
    geom_vline(xintercept = z_nul_o2, linetype = "dotted", color = "red", linewidth = 1) +
    scale_color_brewer(palette = "Set1", name = "Typologie") +
    labs(
      title = toupper(resp_var),
      x = "Trend O2 (Z-score)",
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  return(list(
    model   = model_gam,
    data    = df_clean,
    plot_o2 = p_o2
  ))
}

# ==============================================================================
# 2. RUN DE GAM LOOP VOOR ALLE VARIABELEN
# ==============================================================================
respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Gebruik de historische dataset
resultaten_gam_lt <- map(respons_lijst, ~analyseer_gam_lt(.x, df_lange_termijn))
names(resultaten_gam_lt) <- respons_lijst

# ==============================================================================
# 3. PLAK ALLE O2-CURVES SAMEN
# ==============================================================================
# Trek alle O2 plots uit de lijst
alle_o2_plots <- map(resultaten_gam_lt, "plot_o2")

# Combineer met patchwork, forceer 1 gedeelde legende onderaan
figuur_o2_gam <- wrap_plots(alle_o2_plots, ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Niet-Lineaire Effecten (GAM): Trend O2 vs Ecologische Responsen",
    subtitle = "Zwarte lijn is de partiële GAM-curve, aangepast voor T, EC20 en typologie (1989-2023).",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "bottom")
  )

print(figuur_o2_gam)


library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)
library(ggeffects)
library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. DE GAM INTERACTIE PIJPLEIN FUNCTIE
# ==============================================================================
analyseer_gam_interactie <- function(resp_var, data_raw) {

  cat(paste("▶ Fitten van GAM + Interactie voor:", toupper(resp_var), "...\n"))

  # A. Dynamische trend-berekening (Identiek)
  bereken_trends_dynamisch <- function(df) {
    bereken_helling <- function(var_naam) {
      df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar))
      if (nrow(df_var) < 4) return(NA_real_)
      tryCatch(coef(lm(as.formula(paste0(var_naam, " ~ jaar")), data = df_var))["jaar"],
               error = function(e) NA_real_)
    }

    init_val <- tryCatch(df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar)) %>%
                           arrange(jaar) %>% slice(1) %>% pull(.data[[resp_var]]),
                         error = function(e) NA_real_)
    if (length(init_val) == 0) init_val <- NA_real_

    trend_resp <- tryCatch({
      df_resp <- df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar))
      if (nrow(df_resp) < 4) { NA_real_ } else {
        lm_resp <- lm(as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar")), data = df_resp)
        (exp(coef(lm_resp)["jaar"]) - 1) * 100
      }
    }, error = function(e) NA_real_)

    return(tibble(
      n_obs       = sum(!is.na(df[[resp_var]])),
      init_val    = init_val,
      trend_resp  = trend_resp,
      trend_o2    = as.numeric(bereken_helling("o2")),
      trend_t     = as.numeric(bereken_helling("t")),
      trend_ec_20 = as.numeric(bereken_helling("ec_20"))
    ))
  }

  # B. Data transformeren en de ECHTE NUL berekenen
  df_trends_raw <- data_raw %>%
    group_by(meetplaats, owl, groep, bekken) %>%
    nest() %>%
    mutate(trends = map(data, bereken_trends_dynamisch)) %>%
    filter(!map_lgl(trends, is.null)) %>%
    unnest(trends) %>%
    ungroup() %>%
    drop_na(starts_with("trend_"), init_val)

  # Bereken de Z-score waar O2 precies 0 mg/L/jaar is
  mu_o2 <- mean(df_trends_raw$trend_o2, na.rm = TRUE)
  sigma_o2 <- sd(df_trends_raw$trend_o2, na.rm = TRUE)
  z_nul_o2 <- (0 - mu_o2) / sigma_o2

  # Nu pas schalen we voor het model
  df_trends <- df_trends_raw %>%
    mutate(across(c(trend_o2, trend_ec_20, trend_t), ~as.numeric(scale(.))))

  trend_kolom <- paste0("trend_", resp_var)
  init_kolom  <- paste0("init_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "init_val"] <- init_kolom
  colnames(df_trends)[colnames(df_trends) == "trend_resp"] <- trend_kolom

  # C. Outliers en NAs filteren
  df_clean <- df_trends %>%
    filter(
      .data[[trend_kolom]] > -50 & .data[[trend_kolom]] < 50,
      abs(trend_o2) < 4, abs(trend_t) < 4, abs(trend_ec_20) < 4
    ) %>%
    drop_na(groep) %>%
    mutate(groep = as.factor(groep))

  # D. Het GAM Model MET Interactie
  # ti() is de 'tensor interaction' in mgcv.
  formule_gam <- as.formula(paste0(
    trend_kolom, " ~ s(trend_o2) + s(trend_ec_20) + s(trend_t) + ",
    init_kolom, " + ti(trend_o2, ", init_kolom, ") + groep"
  ))

  model_gam <- gam(formule_gam, family = scat(), data = df_clean, method = "REML")

  # E. Voorspellingen voor 3 startkwaliteiten (Slecht, Matig, Goed)
  q_w_raw <- quantile(df_clean[[init_kolom]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

  # FIX VOOR DEELMAATLATTEN (zoals EP_TW) MET VEEL NULLEN IN 1989:
  # Als percentielen exact gelijk zijn, tellen we er 0.01 bij op zodat R unieke groepen kan maken.
  if (q_w_raw[1] == q_w_raw[2]) q_w_raw[2] <- q_w_raw[1] + 0.01
  if (q_w_raw[2] == q_w_raw[3]) q_w_raw[3] <- q_w_raw[2] + 0.01
  if (q_w_raw[1] == q_w_raw[3]) q_w_raw[3] <- q_w_raw[3] + 0.02

  q_w <- round(q_w_raw, 2)

  terms_arg <- c("trend_o2 [all]", paste0(init_kolom, " [", q_w[1], ",", q_w[2], ",", q_w[3], "]"))
  voorsp_o2 <- ggpredict(model_gam, terms = terms_arg)

  # F. Data labelen voor de plot
  df_plot <- df_clean %>%
    mutate(
      group_plot = case_when(
        .data[[init_kolom]] <= q_w[1] ~ as.character(q_w[1]),
        .data[[init_kolom]] > q_w[1] & .data[[init_kolom]] <= q_w[3] ~ as.character(q_w[2]),
        TRUE ~ as.character(q_w[3])
      ),
      group_plot = factor(group_plot, levels = as.character(q_w))
    )

  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_w, ")")
  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")

  # G. Plot genereren
  p_o2 <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # De "Echte Nul" lijn voor O2 (rood gestippeld)
    geom_vline(xintercept = z_nul_o2, linetype = "dotted", color = "red", linewidth = 1) +

    # Ruwe datapunten ingekleurd op startkwaliteit
    # geom_point(data = df_plot, aes(x = trend_o2, y = .data[[trend_kolom]], color = group_plot),
    #            alpha = 0.4, size = 1.5) +

    # GAM curves en intervallen
    geom_ribbon(data = voorsp_o2, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
                alpha = 0.15, inherit.aes = FALSE) +
    geom_line(data = voorsp_o2, aes(x = x, y = predicted, color = group),
              linewidth = 1.2, inherit.aes = FALSE) +

    scale_color_manual(values = mijn_kleuren, labels = labels_aangepast, name = "Startkwaliteit") +
    scale_fill_manual(values = mijn_kleuren, labels = labels_aangepast, name = "Startkwaliteit") +
    labs(
      title = toupper(resp_var),
      x = "Trend O2 (Z-score)",
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  return(list(model = model_gam, data = df_plot, plot_o2 = p_o2))
}

# ==============================================================================
# 2. RUN DE LOOP EN MAAK DE EINDPLOT
# ==============================================================================
respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Uitvoeren (vervang df_lange_termijn met jouw datasetnaam indien nodig)
resultaten_gam_int <- map(respons_lijst, ~analyseer_gam_interactie(.x, df_lange_termijn))
names(resultaten_gam_int) <- respons_lijst

alle_o2_plots <- map(resultaten_gam_int, "plot_o2")

figuur_o2_interactie <- wrap_plots(alle_o2_plots, ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Effect van O2-trend op Herstel, afhankelijk van Startkwaliteit",
    subtitle = "De rode stippellijn markeert het punt waar de zuurstofconcentratie NIET veranderde (absolute trend = 0).",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "bottom")
  )

print(figuur_o2_interactie)
