library(dplyr)
library(tidyr)
library(purrr)
library(glmmTMB)
library(ggplot2)
library(patchwork)
library(performance)
library(sjPlot)
library(DHARMa)
source("source/analyse/mi_datasets_prep.R")

# ==============================================================================
# 0. DATA PREPARATIE (Lijst maken)
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
# 1. DE "MASTER" PIJPLEIN FUNCTIE PER RESPONS VARIABELE
# ==============================================================================
# We definiëren een functie die de naam van een respons pakt (bijv. "ep_tw")
# en de hele flow uitvoert.

analyseer_maatlat <- function(resp_var, data_raw) {
  cat(paste("\n▶ Start analyse voor:", toupper(resp_var), "...\n"))

  # A. Dynamische trend-berekening (MET MAANDCORRECTIE)
  bereken_trends_dynamisch <- function(df) {

    bereken_helling <- function(var_naam) {
      df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar), !is.na(maand_num))
      if (nrow(df_var) < 4) return(NA_real_)

      tryCatch({ # Poging 1: Met maandcorrectie
        model <- lm(as.formula(paste0(var_naam, " ~ jaar + as.factor(maand_num)")), data = df_var)
        coef(model)["jaar"]
      }, error = function(e) {
        tryCatch({ # Fallback: Zonder maandcorrectie (bij te weinig spreiding in de maanden)
          model_fb <- lm(as.formula(paste0(var_naam, " ~ jaar")), data = df_var)
          coef(model_fb)["jaar"]
        }, error = function(e2) NA_real_)
      })
    }

    init_val <- tryCatch({
      df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar)) %>%
        arrange(jaar) %>% slice(1) %>% pull(.data[[resp_var]])
    }, error = function(e) NA_real_)
    if (length(init_val) == 0) init_val <- NA_real_

    # Sinclair Trend voor de respons
    trend_resp <- tryCatch({
      df_resp <- df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar), !is.na(maand_num))
      if (nrow(df_resp) < 4) { NA_real_ } else {
        tryCatch({
          form_resp <- as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar + as.factor(maand_num)"))
          (exp(coef(lm(form_resp, data = df_resp))["jaar"]) - 1) * 100
        }, error = function(e) {
          form_fb <- as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar"))
          (exp(coef(lm(form_fb, data = df_resp))["jaar"]) - 1) * 100
        })
      }
    }, error = function(e) NA_real_)

    return(tibble(
      n_obs       = sum(!is.na(df[[resp_var]])), init_val = init_val, trend_resp = trend_resp,
      trend_n_t   = as.numeric(bereken_helling("n_t")), trend_p_t = as.numeric(bereken_helling("p_t")),
      trend_o2    = as.numeric(bereken_helling("o2")), trend_t = as.numeric(bereken_helling("t")),
      trend_ec_20 = as.numeric(bereken_helling("ec_20")), trend_czv = as.numeric(bereken_helling("czv"))
    ))
  }

  # B. Pas functie toe & schaal predictoren
  df_trends <- data_raw %>%
    group_by(meetplaats, owl, groep, bekken, ekc2_waterlichaam) %>% nest() %>%
    mutate(trends = map(data, bereken_trends_dynamisch)) %>%
    filter(!map_lgl(trends, is.null)) %>% unnest(trends) %>% ungroup() %>%
    drop_na(starts_with("trend_"), init_val) %>%
    mutate(across(c(trend_n_t, trend_p_t, trend_o2, trend_ec_20, trend_czv, trend_t), ~as.numeric(scale(.))))

  trend_kolom <- paste0("trend_", resp_var)
  init_kolom  <- paste0("init_", resp_var)

  colnames(df_trends)[colnames(df_trends) == "init_val"] <- init_kolom
  colnames(df_trends)[colnames(df_trends) == "trend_resp"] <- trend_kolom

  # C. Outliers filteren
  df_clean <- df_trends %>%
    filter(.data[[trend_kolom]] > -50 & .data[[trend_kolom]] < 50,
           abs(trend_n_t) < 4, abs(trend_p_t) < 4, abs(trend_o2) < 4,
           abs(trend_t) < 4, abs(trend_ec_20) < 4, abs(trend_czv) < 4) %>%
    drop_na(groep)

  # D. Model fitten
  formule_model <- as.formula(paste0(trend_kolom, " ~ ", init_kolom, " + (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t) + groep"))
  model_fit <- glmmTMB(formule_model, family = t_family(), data = df_clean)

  voorspeld <- predict(model_fit, type = "response")
  pseudo_r2 <- cor(df_clean[[trend_kolom]], voorspeld)^2

  p_forest <- plot_model(model_fit, type = "est", vline.color = "grey50", sort.est = TRUE,
                         title = paste0(toupper(resp_var), "\n(Pseudo-R² = ", round(pseudo_r2, 2), ")"), axis.title = "Effectgrootte") +
    aes(alpha = p.value < 0.05) + scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3), guide = "none") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

  return(list(model = model_fit, data = df_clean, r2 = pseudo_r2, plot = p_forest))
}

# ==============================================================================
# 2. RUN DE PIJPLEIN VOOR ALLE MAATLATTEN
# ==============================================================================
# Samenvoegen tot één master raw-dataset (voer dit 1x uit)
df_master_raw <- bind_rows(lijst_datasets, .id = "groep") %>%
  mutate(maand_num = as.numeric(lubridate::month(monsternamedatum)))
# %>%
#   filter(groep %in% c("Beek", "Kempen", "Rivier"))

# De 6 variabelen die we willen testen
respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Gebruik map() om de pipeline voor elk van de 6 variabelen uit te voeren
resultaten_lijst <- map(respons_lijst, ~analyseer_maatlat(.x, df_master_raw))
names(resultaten_lijst) <- respons_lijst # Geef de lijst handige namen

# ==============================================================================
# 3. HET EINDRESULTAAT: 1 GROTE PLOT
# ==============================================================================
# Haal alle individuele forest plots uit de resultatenlijst
alle_plots <- map(resultaten_lijst, "plot")

# Plak ze allemaal samen in een raster van 2 kolommen met patchwork
master_figuur <- wrap_plots(alle_plots, ncol = 2) +
  plot_annotation(
    title = "Overzicht Waterkwaliteit-effecten per Deelmaatlat",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

# Toon de meesterplot!
print(master_figuur)


# Check de summary van het EP_TW model
summary(resultaten_lijst$ep_tw$model)

# Check of het EP_TW model singularity problemen heeft
check_singularity(resultaten_lijst$ep_tw$model)

# Draai en plot de DHARMa residuals voor EP_TW
res_ep_tw <- simulateResiduals(resultaten_lijst$ep_tw$model, plot = TRUE)

# ==============================================================================
# 4. DE PIJPLEIN FUNCTIE INCLUSIEF INTERACTIES MET STARTWAARDE
# ==============================================================================
analyseer_maatlat_interactie <- function(resp_var, data_raw) {

  cat(paste("\n▶ Start INTERACTIE-analyse voor:", toupper(resp_var), "...\n"))

  # A. Dynamische trend-berekening (Met seizoenscorrectie)
  bereken_trends_dynamisch <- function(df) {
    bereken_helling <- function(var_naam) {
      df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar), !is.na(maand_num))
      if (nrow(df_var) < 4) return(NA_real_)
      tryCatch(coef(lm(as.formula(paste0(var_naam, " ~ jaar + as.factor(maand_num)")), data = df_var))["jaar"],
               error = function(e) tryCatch(coef(lm(as.formula(paste0(var_naam, " ~ jaar")), data = df_var))["jaar"], error = function(e2) NA_real_))
    }

    init_val <- tryCatch(df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar)) %>% arrange(jaar) %>% slice(1) %>% pull(.data[[resp_var]]), error = function(e) NA_real_)
    if (length(init_val) == 0) init_val <- NA_real_

    trend_resp <- tryCatch({
      df_resp <- df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar), !is.na(maand_num))
      if (nrow(df_resp) < 4) { NA_real_ } else {
        tryCatch((exp(coef(lm(as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar + as.factor(maand_num)")), data = df_resp))["jaar"]) - 1) * 100,
                 error = function(e) (exp(coef(lm(as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar")), data = df_resp))["jaar"]) - 1) * 100)
      }
    }, error = function(e) NA_real_)

    return(tibble(n_obs = sum(!is.na(df[[resp_var]])), init_val = init_val, trend_resp = trend_resp,
                  trend_n_t = as.numeric(bereken_helling("n_t")), trend_p_t = as.numeric(bereken_helling("p_t")),
                  trend_o2 = as.numeric(bereken_helling("o2")), trend_t = as.numeric(bereken_helling("t")),
                  trend_ec_20 = as.numeric(bereken_helling("ec_20")), trend_czv = as.numeric(bereken_helling("czv"))))
  }

  df_trends <- data_raw %>% group_by(meetplaats, owl, groep, bekken) %>% nest() %>%
    mutate(trends = map(data, bereken_trends_dynamisch)) %>% filter(!map_lgl(trends, is.null)) %>%
    unnest(trends) %>% ungroup() %>% drop_na(starts_with("trend_"), init_val)

  # Hernoemen
  trend_kolom <- paste0("trend_", resp_var)
  init_kolom <- paste0("init_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "init_val"] <- init_kolom
  colnames(df_trends)[colnames(df_trends) == "trend_resp"] <- trend_kolom

  # B. SCHALEN (Dit moet ALTIJD vóór het filteren gebeuren!)
  df_trends <- df_trends %>%
    mutate(across(c(trend_n_t, trend_p_t, trend_o2, trend_ec_20, trend_czv, trend_t, all_of(init_kolom)), ~as.numeric(scale(.))))

  # C. FILTEREN (Nu de Z-scores berekend zijn, werkt de 'abs < 4' logica perfect)
  df_clean <- df_trends %>%
    filter(.data[[trend_kolom]] > -50 & .data[[trend_kolom]] < 50,
           abs(trend_n_t) < 4, abs(trend_p_t) < 4, abs(trend_o2) < 4,
           abs(trend_t) < 4, abs(trend_ec_20) < 4, abs(trend_czv) < 4) %>%
    drop_na(groep) %>%
    mutate(groep = as.factor(groep))

  # D. Modellen opbouwen (Dynamisch met of zonder 'groep')
  n_groepen <- length(unique(df_clean$groep))

  if (n_groepen > 1) {
    formule_int  <- as.formula(paste0(trend_kolom, " ~ ", init_kolom, " * (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t) + groep"))
    formule_main <- as.formula(paste0(trend_kolom, " ~ ", init_kolom, " + (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t) + groep"))
  } else {
    formule_int  <- as.formula(paste0(trend_kolom, " ~ ", init_kolom, " * (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t)"))
    formule_main <- as.formula(paste0(trend_kolom, " ~ ", init_kolom, " + (trend_n_t + trend_p_t + trend_o2 + trend_ec_20 + trend_czv + trend_t)"))
  }

  # E. Fitten en Singularity Fallback
  model_fit <- glmmTMB(formule_int, family = t_family(), data = df_clean)

  coef_se <- summary(model_fit)$coefficients$cond[, "Std. Error"]
  is_singular <- any(is.na(coef_se)) || any(coef_se > 100)

  if (is_singular) {
    cat("  ⚠️ Singularity gedetecteerd (te complexe interacties). Fallback naar simpeler model...\n")
    model_fit <- glmmTMB(formule_main, family = t_family(), data = df_clean)
    plot_subtitel <- "Let op: Singularity gedetecteerd. Startwaarde-interacties weggelaten."
  } else {
    plot_subtitel <- NULL
  }

  # F. Pseudo R² en Plot
  voorspeld <- predict(model_fit, type = "response")
  pseudo_r2 <- cor(df_clean[[trend_kolom]], voorspeld)^2

  plot_titel <- paste0(toupper(resp_var), "\n(Pseudo-R² = ", round(pseudo_r2, 2), ")")

  p_forest <- plot_model(model_fit, type = "est", vline.color = "grey50", sort.est = TRUE,
                         title = plot_titel, axis.title = "Effectgrootte") +
    aes(alpha = p.value < 0.05) + scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3), guide = "none") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

  if (!is.null(plot_subtitel)) p_forest <- p_forest + labs(subtitle = plot_subtitel)

  return(list(model = model_fit, data = df_clean, r2 = pseudo_r2, plot = p_forest))
}

# ==============================================================================
# 5. RUN DE LOOP VOOR DE INTERACTIE-MODELLEN
# ==============================================================================
# Voer dit uit over je bestaande df_master_raw
resultaten_interactie <- map(respons_lijst, ~analyseer_maatlat_interactie(.x, df_master_raw))
names(resultaten_interactie) <- respons_lijst

# Haal alle individuele forest plots uit de lijst
alle_plots_int <- map(resultaten_interactie, "plot")

# Plak ze allemaal samen met patchwork
master_figuur_int <- wrap_plots(alle_plots_int, ncol = 2) +
  plot_annotation(
    title = "Effecten per Deelmaatlat INCLUSIEF Startwaarde-Interacties",
    subtitle = "Gekleurde termen met ':' tonen de interactie tussen startkwaliteit en waterkwaliteitstrends.",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

# Toon de meesterplot!
print(master_figuur_int)

resultaten_interactie$ta_xw$model %>% simulateResiduals(., plot = TRUE)
resultaten_interactie$sw_dw$model %>% simulateResiduals(., plot = TRUE)

library(dplyr)
library(purrr)
library(ggeffects)
library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. FUNCTIE OM SIGNIFICANTE INTERACTIES TE ZOEKEN EN TE PLOTTEN
# ==============================================================================
genereer_sig_interactie_plots <- function(resp_var, resultaten_lijst) {

  mod <- resultaten_lijst[[resp_var]]$model
  df_clean <- resultaten_lijst[[resp_var]]$data
  init_kolom <- paste0("init_", resp_var)

  # A. Haal de p-waarden uit het glmmTMB model
  coefs <- summary(mod)$coefficients$cond

  # B. Zoek naar significante interacties (bevat ':', 'trend_' en de 'init_' kolom)
  is_interactie <- grepl(":", rownames(coefs)) &
    grepl("trend_", rownames(coefs)) &
    grepl(init_kolom, rownames(coefs))
  is_significant <- coefs[, "Pr(>|z|)"] < 0.05

  sig_termen <- rownames(coefs)[is_interactie & is_significant]

  # Als er géén significante interacties zijn, stop dan en geef NULL terug
  if (length(sig_termen) == 0) {
    cat(" Geen significante interacties voor:", toupper(resp_var), "\n")
    return(NULL)
  }

  cat("✔ Significante interacties voor", toupper(resp_var), ":", paste(sig_termen, collapse = ", "), "\n")

  # C. Filter de pure predictor-namen eruit (bijv. haal "trend_o2" uit "init_mmif:trend_o2")
  predictoren <- gsub(paste0(init_kolom, ":|:", init_kolom), "", sig_termen)

  # D. Genereer een plot voor elke significante predictor
  plots_lijst <- map(predictoren, function(pred) {

    # Bepaal een mooie naam voor de as-labels
    mooie_naam <- case_when(
      pred == "trend_n_t" ~ "Stikstof (N)",
      pred == "trend_p_t" ~ "Fosfor (P)",
      pred == "trend_o2" ~ "Zuurstof (O2)",
      pred == "trend_t" ~ "Temperatuur",
      pred == "trend_ec_20" ~ "EC20",
      pred == "trend_czv" ~ "CZV",
      TRUE ~ pred
    )

    # 1. Startwaarden (percentielen) berekenen
    q_w_raw <- quantile(df_clean[[init_kolom]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

    # ZERO-INFLATION FIX (Voor als percentielen exact gelijk zijn, bvb bij EP_TW)
    if (q_w_raw[1] == q_w_raw[2]) q_w_raw[2] <- q_w_raw[1] + 0.01
    if (q_w_raw[2] == q_w_raw[3]) q_w_raw[3] <- q_w_raw[2] + 0.01
    if (q_w_raw[1] == q_w_raw[3]) q_w_raw[3] <- q_w_raw[3] + 0.02
    q_w <- round(q_w_raw, 2)

    # 2. Voorspellingen via ggeffects
    term_x <- paste0(pred, " [all]")
    term_group <- paste0(init_kolom, " [", q_w[1], ",", q_w[2], ",", q_w[3], "]")
    voorsp <- ggpredict(mod, terms = c(term_x, term_group))

    # 3. Plot opmaken
    labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_w, ")")
    mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77") # Oranje, Paars, Groen

    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

      geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
                  alpha = 0.15, inherit.aes = FALSE) +
      geom_line(data = voorsp, aes(x = x, y = predicted, color = group),
                linewidth = 1.2, inherit.aes = FALSE) +

      scale_color_manual(name = "Startkwaliteit", values = mijn_kleuren, labels = labels_aangepast) +
      scale_fill_manual(name = "Startkwaliteit", values = mijn_kleuren, labels = labels_aangepast) +

      labs(
        title = paste(toupper(resp_var), "-", mooie_naam),
        x = paste("Trend", mooie_naam, "(Z-score)"),
        y = "Trend Respons (%/jaar)"
      ) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))

    return(p)
  })

  return(plots_lijst)
}

# ==============================================================================
# 2. RUN DE LOOP EN COMBINEER ALLE PLOTS
# ==============================================================================
cat("\n▶ Zoeken naar significante interacties in de resultaten...\n")

respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Pas de functie toe op alle 6 de maatlatten
# We gebruiken je eerder gemaakte 'resultaten_interactie' lijst!
alle_sig_plots_genest <- map(respons_lijst, ~genereer_sig_interactie_plots(.x, resultaten_interactie))

# Omdat sommige modellen 0, 1 of meerdere plots teruggeven, moeten we de lijst 'platstrijken'
# compact() gooit de NULLs eruit, flatten() maakt er één strakke lijst van.
alle_sig_plots_plat <- alle_sig_plots_genest %>%
  compact() %>%
  flatten()

# Check of we überhaupt iets te plotten hebben
if (length(alle_sig_plots_plat) > 0) {

  cat("\n▶ Totaal aantal significante interactie-plots gegenereerd:", length(alle_sig_plots_plat), "\n")

  # Plak ze allemaal samen met patchwork
  master_sig_interacties <- wrap_plots(alle_sig_plots_plat, ncol = 2) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Significante Interacties: Herstel vs. Startkwaliteit",
      subtitle = "Enkel de wiskundig significante interactie-effecten (p < 0.05) worden getoond.",
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                    legend.position = "bottom")
    )

  print(master_sig_interacties)

} else {
  cat("\nGeen enkele significante interactie gevonden over alle modellen heen!\n")
}

library(dplyr)
library(purrr)
library(ggeffects)
library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. FUNCTIE OM ENKEL SIGNIFICANTE HOOFDEFFECTEN TE ZOEKEN EN TE PLOTTEN
# ==============================================================================
genereer_sig_main_plots <- function(resp_var, resultaten_lijst) {

  mod <- resultaten_lijst[[resp_var]]$model
  df_clean <- resultaten_lijst[[resp_var]]$data

  init_kolom <- paste0("init_", resp_var)
  trend_kolom <- paste0("trend_", resp_var)

  # A. Haal de p-waarden uit het glmmTMB model
  coefs <- summary(mod)$coefficients$cond
  is_significant <- coefs[, "Pr(>|z|)"] < 0.05
  sig_termen <- rownames(coefs)[is_significant]

  # B. Zoek de variabelen die een significante interactie hebben (deze slaan we hier over!)
  is_interactie <- grepl(":", sig_termen) & grepl("trend_", sig_termen) & grepl(init_kolom, sig_termen)
  sig_interacties <- sig_termen[is_interactie]
  pred_met_int <- gsub(paste0(init_kolom, ":|:", init_kolom), "", sig_interacties)

  # C. Zoek de significante HOOFDeffecten (alleen onze predictoren)
  # Dit negeert automatisch de 'groep' effecten (bijv. groepkempen) en de '(Intercept)' of 'init_' term.
  alle_predictoren <- c("trend_n_t", "trend_p_t", "trend_o2", "trend_t", "trend_ec_20", "trend_czv")
  sig_hoofdeffecten <- sig_termen[sig_termen %in% alle_predictoren]

  # D. FILTER: Behoud de hoofdeffecten die NIET in het interactie-lijstje staan
  te_plotten <- setdiff(sig_hoofdeffecten, pred_met_int)

  # Als er geen overblijven, stop dan voor deze maatlat
  if (length(te_plotten) == 0) {
    cat(" Geen pure (single) significante effecten over voor:", toupper(resp_var), "\n")
    return(NULL)
  }

  cat("✔ Pure significante hoofdeffecten voor", toupper(resp_var), ":", paste(te_plotten, collapse = ", "), "\n")

  # E. Genereer een plot voor elke overgebleven significante predictor
  plots_lijst <- map(te_plotten, function(pred) {

    mooie_naam <- case_when(
      pred == "trend_n_t" ~ "Stikstof (N)",
      pred == "trend_p_t" ~ "Fosfor (P)",
      pred == "trend_o2" ~ "Zuurstof (O2)",
      pred == "trend_t" ~ "Temperatuur",
      pred == "trend_ec_20" ~ "EC20",
      pred == "trend_czv" ~ "CZV",
      TRUE ~ pred
    )

    # 1. Voorspellingen via ggeffects (enkel de 'pred' variabele varieert)
    voorsp <- ggpredict(mod, terms = pred)

    # 2. Bouw de plot
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

      # Ruwe datapunten in de achtergrond (we kleuren ze per typologie/groep voor inzicht)
      geom_point(data = df_clean, aes(x = .data[[pred]], y = .data[[trend_kolom]], color = groep),
                 alpha = 0.4, size = 1.5) +

      # Zwarte confidence ribbon (hoofdeffect)
      geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high),
                  alpha = 0.2, fill = "black", inherit.aes = FALSE) +

      # Zwarte regressielijn (hoofdeffect)
      geom_line(data = voorsp, aes(x = x, y = predicted),
                linewidth = 1.2, color = "black", inherit.aes = FALSE) +

      scale_color_brewer(palette = "Set1", name = "Typologie") +
      labs(
        title = paste(toupper(resp_var), "-", mooie_naam),
        x = paste("Trend", mooie_naam, "(Z-score)"),
        y = "Trend Respons (%/jaar)"
      ) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))

    return(p)
  })

  return(plots_lijst)
}

# ==============================================================================
# 2. RUN DE LOOP EN COMBINEER ALLE PLOTS
# ==============================================================================
cat("\n▶ Zoeken naar puur significante hoofdeffecten (zonder interactie)...\n")

respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# Pas de functie toe
alle_main_plots_genest <- map(respons_lijst, ~genereer_sig_main_plots(.x, resultaten_interactie))

# Filters NULLs eruit en maak er één lijst van
alle_main_plots_plat <- alle_main_plots_genest %>%
  compact() %>%
  flatten()

# Check of we iets te plotten hebben
if (length(alle_main_plots_plat) > 0) {

  cat("\n▶ Totaal aantal significante hoofdeffect-plots gegenereerd:", length(alle_main_plots_plat), "\n")

  master_sig_main <- wrap_plots(alle_main_plots_plat, ncol = 2) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Significante Hoofdeffecten (Zonder Startwaarde-Interactie)",
      subtitle = "Deze lijnen tonen het algemene effect van een trend, ongeacht de historische startkwaliteit.",
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                    legend.position = "bottom")
    )

  print(master_sig_main)

} else {
  cat("\nGeen enkele puur significante variabele (zonder interactie) gevonden!\n")
}

library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)
library(ggeffects)
library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. FUNCTIE: GAM MODELLEN FITTEN (MET ROBUUSTE INTERACTIES)
# ==============================================================================
analyseer_gam_volledig <- function(resp_var, data_raw) {
  cat(paste("\n▶ Fitten van GAM + Interacties voor:", toupper(resp_var), "...\n"))

  bereken_trends_dynamisch <- function(df) {
    bereken_helling <- function(var_naam) {
      df_var <- df %>% filter(!is.na(.data[[var_naam]]), !is.na(jaar), !is.na(maand_num))
      if (nrow(df_var) < 4) return(NA_real_)
      tryCatch(coef(lm(as.formula(paste0(var_naam, " ~ jaar + as.factor(maand_num)")), data = df_var))["jaar"],
               error = function(e) tryCatch(coef(lm(as.formula(paste0(var_naam, " ~ jaar")), data = df_var))["jaar"], error = function(e2) NA_real_))
    }

    init_val <- tryCatch(df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar)) %>% arrange(jaar) %>% slice(1) %>% pull(.data[[resp_var]]), error = function(e) NA_real_)
    if (length(init_val) == 0) init_val <- NA_real_

    trend_resp <- tryCatch({
      df_resp <- df %>% filter(!is.na(.data[[resp_var]]), !is.na(jaar), !is.na(maand_num))
      if (nrow(df_resp) < 4) { NA_real_ } else {
        tryCatch((exp(coef(lm(as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar + as.factor(maand_num)")), data = df_resp))["jaar"]) - 1) * 100,
                 error = function(e) (exp(coef(lm(as.formula(paste0("log(", resp_var, " + 0.01) ~ jaar")), data = df_resp))["jaar"]) - 1) * 100)
      }
    }, error = function(e) NA_real_)

    return(tibble(n_obs = sum(!is.na(df[[resp_var]])), init_val = init_val, trend_resp = trend_resp,
                  trend_n_t = as.numeric(bereken_helling("n_t")), trend_p_t = as.numeric(bereken_helling("p_t")),
                  trend_o2 = as.numeric(bereken_helling("o2")), trend_t = as.numeric(bereken_helling("t")),
                  trend_ec_20 = as.numeric(bereken_helling("ec_20")), trend_czv = as.numeric(bereken_helling("czv"))))
  }

  df_trends <- data_raw %>% group_by(meetplaats, owl, groep, bekken, ekc2_waterlichaam) %>% nest() %>%
    mutate(trends = map(data, bereken_trends_dynamisch)) %>% filter(!map_lgl(trends, is.null)) %>%
    unnest(trends) %>% ungroup() %>% drop_na(starts_with("trend_"), init_val) %>%
    mutate(across(c(trend_n_t, trend_p_t, trend_o2, trend_ec_20, trend_czv, trend_t), ~as.numeric(scale(.))))

  trend_kolom <- paste0("trend_", resp_var); init_kolom <- paste0("init_", resp_var)
  colnames(df_trends)[colnames(df_trends) == "init_val"] <- init_kolom
  colnames(df_trends)[colnames(df_trends) == "trend_resp"] <- trend_kolom

  df_clean <- df_trends %>% filter(.data[[trend_kolom]] > -50 & .data[[trend_kolom]] < 50, abs(trend_n_t) < 4, abs(trend_p_t) < 4, abs(trend_o2) < 4, abs(trend_t) < 4, abs(trend_ec_20) < 4, abs(trend_czv) < 4) %>% drop_na(groep) %>% mutate(groep = as.factor(groep))

  predictoren <- c("trend_n_t", "trend_p_t", "trend_o2", "trend_ec_20", "trend_czv", "trend_t")
  n_uniek_init <- length(unique(df_clean[[init_kolom]]))
  k_init <- min(4, n_uniek_init)
  main_effects <- paste0("s(", predictoren, ", k = 4)", collapse = " + ")

  if (k_init < 3) {
    cat(" (Info: Te weinig unieke startwaarden, lineaire term gebruikt voor", init_kolom, ") ")
    interacties <- paste0(predictoren, ":", init_kolom, collapse = " + ")
    formule_string <- paste0(trend_kolom, " ~ ", main_effects, " + ", init_kolom, " + ", interacties, " + groep")
  } else {
    interacties <- paste0("ti(", predictoren, ", ", init_kolom, ", k = c(4, ", k_init, "))", collapse = " + ")
    formule_string <- paste0(trend_kolom, " ~ ", main_effects, " + s(", init_kolom, ", k = ", k_init, ") + ", interacties, " + groep")
  }

  model_gam <- gam(as.formula(formule_string), family = scat(), data = df_clean, method = "REML")
  return(list(model = model_gam, data = df_clean, init_kolom = init_kolom, trend_kolom = trend_kolom))
}

# ==============================================================================
# 2. FUNCTIE: SLIMME PLOT SELECTIE (3 LIJNEN OF 1 LIJN)
# ==============================================================================
plot_significante_gam_effecten <- function(resp_var, resultaten_lijst) {

  mod <- resultaten_lijst[[resp_var]]$model
  df_clean <- resultaten_lijst[[resp_var]]$data
  init_kolom <- resultaten_lijst[[resp_var]]$init_kolom
  trend_kolom <- resultaten_lijst[[resp_var]]$trend_kolom

  # A. P-waarden uit de smooth-tabel van het model halen
  s_table <- summary(mod)$s.table
  p_waarden <- s_table[, "p-value"]
  sig_termen <- rownames(s_table)[p_waarden < 0.05]

  # Omdat we voor ep_tw misschien lineaire termen (parametric) hebben gebruikt, checken we die ook
  p_table <- summary(mod)$p.table
  p_waarden_par <- p_table[, "Pr(>|z|)"]
  sig_termen_par <- rownames(p_table)[p_waarden_par < 0.05]

  # B. Welke predictoren hebben een significante INTERACTIE? (ti-termen of lineaire :)
  int_termen_ti <- grep("ti\\(", sig_termen, value = TRUE)
  sig_int_vars_ti <- gsub("ti\\((trend_[^,]+),.*", "\\1", int_termen_ti)

  int_termen_par <- grep("trend_.*:", sig_termen_par, value = TRUE)
  sig_int_vars_par <- gsub("(trend_[^:]+):.*", "\\1", int_termen_par)

  sig_int_vars <- unique(c(sig_int_vars_ti, sig_int_vars_par))

  # C. Welke predictoren hebben een significant HOOFDEFFECT? (s-termen)
  main_termen <- grep("^s\\(trend_", sig_termen, value = TRUE)
  sig_main_vars <- gsub("s\\((trend_[^\\)]+)\\)", "\\1", main_termen)

  # D. Bepaal de "pure" hoofdeffecten (wel s(), maar geen interactie)
  pure_main_vars <- setdiff(sig_main_vars, sig_int_vars)

  alle_te_plotten <- unique(c(sig_int_vars, pure_main_vars))

  if(length(alle_te_plotten) == 0) return(NULL) # Geen plots voor deze maatlat

  # E. Loop over de variabelen en plot voorwaardelijk
  plots_lijst <- map(alle_te_plotten, function(pred) {

    mooie_naam <- case_when(
      pred == "trend_n_t" ~ "Stikstof (N)", pred == "trend_p_t" ~ "Fosfor (P)",
      pred == "trend_o2" ~ "Zuurstof (O2)", pred == "trend_t" ~ "Temperatuur",
      pred == "trend_ec_20" ~ "EC20", pred == "trend_czv" ~ "CZV", TRUE ~ pred
    )

    # --- SCENARIO 1: SIGNIFICANTE INTERACTIE (3 LIJNEN) ---
    if (pred %in% sig_int_vars) {

      q_w_raw <- quantile(df_clean[[init_kolom]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
      # Zero-inflation fix
      if (q_w_raw[1] == q_w_raw[2]) q_w_raw[2] <- q_w_raw[1] + 0.01
      if (q_w_raw[2] == q_w_raw[3]) q_w_raw[3] <- q_w_raw[2] + 0.01
      if (q_w_raw[1] == q_w_raw[3]) q_w_raw[3] <- q_w_raw[3] + 0.02
      q_w <- round(q_w_raw, 2)

      voorsp <- ggpredict(mod, terms = c(paste0(pred, " [all]"), paste0(init_kolom, " [", q_w[1], ",", q_w[2], ",", q_w[3], "]")))
      labels <- paste0(c("Slecht (", "Matig (", "Goed ("), q_w, ")")

      p <- ggplot() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, inherit.aes = FALSE) +
        geom_line(data = voorsp, aes(x = x, y = predicted, color = group), linewidth = 1.2, inherit.aes = FALSE) +
        scale_color_manual(name = "Startkwaliteit", values = c("#d95f02", "#7570b3", "#1b9e77"), labels = labels) +
        scale_fill_manual(name = "Startkwaliteit", values = c("#d95f02", "#7570b3", "#1b9e77"), labels = labels) +
        labs(title = paste(toupper(resp_var), "-", mooie_naam, "\n(Interactie)"), x = paste("Trend", mooie_naam, "(Z-score)"), y = "Trend Respons (%)") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = 0.5))

      return(p)

      # --- SCENARIO 2: PUUR HOOFDEFFECT (1 LIJN) ---
    } else {

      voorsp <- ggpredict(mod, terms = paste0(pred, " [all]"))

      p <- ggplot() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_point(data = df_clean, aes(x = .data[[pred]], y = .data[[trend_kolom]], color = groep), alpha = 0.3, size = 1) +
        geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "black", inherit.aes = FALSE) +
        geom_line(data = voorsp, aes(x = x, y = predicted), linewidth = 1.2, color = "black", inherit.aes = FALSE) +
        scale_color_brewer(palette = "Set1", name = "Typologie") +
        labs(title = paste(toupper(resp_var), "-", mooie_naam, "\n(Hoofdeffect)"), x = paste("Trend", mooie_naam, "(Z-score)"), y = "Trend Respons (%)") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = 0.5))

      return(p)
    }
  })

  return(plots_lijst)
}

# ==============================================================================
# 3. UITVOEREN VAN DE GEHELE PIJPLEIN & GEPAGINEERD PLOTTEN
# ==============================================================================
respons_lijst <- c("mmif", "ep_tw", "ns_tw", "mt_sw", "sw_dw", "ta_xw")

# 1. Modellen fitten
cat("\n[1/2] GAMs fitten voor alle maatlatten...\n")
resultaten_gam <- map(respons_lijst, ~analyseer_gam_volledig(.x, df_master_raw))
names(resultaten_gam) <- respons_lijst

# 2. Plots genereren
cat("\n[2/2] Significante marginale effecten plotten...\n")
alle_plots_gam <- map(respons_lijst, ~plot_significante_gam_effecten(.x, resultaten_gam)) %>%
  compact() %>%
  flatten()

# 3. Splitsen en Plotten
if (length(alle_plots_gam) > 0) {

  # Hoeveel plots wil je maximaal per scherm/figuur? (4 of 6 is meestal ideaal)
  plots_per_pagina <- 4

  # Verdeel de lange lijst in kleine blokjes (chunks)
  chunks <- split(alle_plots_gam, ceiling(seq_along(alle_plots_gam) / plots_per_pagina))
  totaal_paginas <- length(chunks)

  cat(paste("\nSucces! Er zijn", length(alle_plots_gam), "significante plots gevonden. Deze worden verdeeld over", totaal_paginas, "pagina's.\n"))

  # Loop over elk blokje en print een aparte figuur
  walk2(chunks, seq_along(chunks), function(chunk, pagina_nummer) {

    p <- wrap_plots(chunk, ncol = 2) +
      plot_layout(guides = "collect") +
      plot_annotation(
        title = paste0("Niet-Lineaire Effecten (Pagina ", pagina_nummer, "/", totaal_paginas, ")"),
        subtitle = "Interactieplots = 3 lijnen (startkwaliteit). Hoofdeffectplots = 1 zwarte lijn.",
        theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                      legend.position = "bottom")
      )

    print(p) # Print de pagina naar je RStudio viewer

  })

} else {
  cat("Geen enkele significante variabele gevonden over alle modellen!\n")
}
