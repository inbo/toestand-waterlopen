# Laad de benodigde extra packages (installeer ze eerst met install.packages() indien nodig)
library(dplyr)
library(glmmTMB)
library(sjPlot)
library(ggplot2)
library(purrr)     # Voor het itereren over lijsten
library(patchwork) # Voor het maken van multi-panel figuren

# 1. Plaats al je datasets in een benoemde lijst
lijst_datasets <- list(
  "Beek"   = mi_nat_sv_beek,
  "Kempen" = mi_nat_sv_kempen,
  "Rivier" = mi_nat_sv_rivier,
  "Polder" = mi_nat_sv_polder,
  "Rtnt"   = mi_rtnt
)

# 2. Maak een functie die de data prepareert én het model fit
fit_mijn_model <- function(df) {

  # Data transformatie (Mundlak / within-subject centering)
  df_temp <- df %>%
    group_by(meetplaats) %>%
    mutate(
      n_t_mean   = mean(n_t, na.rm = TRUE),
      n_t_dev    = n_t - n_t_mean,
      p_t_mean   = mean(p_t, na.rm = TRUE),
      p_t_dev    = p_t - p_t_mean,
      o2_mean    = mean(o2, na.rm = TRUE),
      o2_dev     = o2 - o2_mean,
      ec_20_mean = mean(ec_20, na.rm = TRUE),
      ec_20_dev  = ec_20 - ec_20_mean,
      lb_mean    = mean(intensiteit_combo_afstr, na.rm = TRUE),
      lb_dev     = intensiteit_combo_afstr - lb_mean
    ) %>%
    ungroup() %>%
    # as.numeric(scale(x)) voorkomt dat scale() een matrix-kolom maakt,
    # wat soms problemen geeft bij sjPlot.
    mutate(across(ends_with("_mean") | ends_with("_dev") | matches("jaar"),
                  ~ as.numeric(scale(.))))

  # Model fitten
  model <- glmmTMB(
    data = df_temp,
    mmif ~
      n_t_dev + p_t_dev + o2_dev + ec_20_dev + lb_dev +
      n_t_mean + p_t_mean + o2_mean + ec_20_mean + lb_mean +
      jaar + (1 | owl/meetplaats)
  )

  return(model)
}

# 3. Voer de functie uit voor elke dataset in de lijst
# Dit geeft een nieuwe lijst terug, gevuld met de 5 gefitte modellen
modellen_lijst <- map(lijst_datasets, fit_mijn_model)


maak_forest_plot <- function(model, naam) {

  # 1. Maak de basisplot met een smallere as (pas c(-0.1, 0.1) aan naar jouw data)
  p <- plot_model(
    model,
    type = "est",
    vline.color = "red",
    sort.est = TRUE,
           # VERANDER DIT om je x-as smaller of breder te maken
    title = naam,
    axis.title = "Effectgrootte (mmif)"
  )

  # 2. Voeg transparantie (alpha) toe voor niet-significante effecten
  # We kijken in de verborgen data van de plot of p.value < 0.05 is
  p <- p +
    ylim(-0.1, 0.1)+
    aes(alpha = p.value < 0.05) +
    scale_alpha_manual(
      values = c("TRUE" = 1, "FALSE" = 0.3), # TRUE = 100% zichtbaar, FALSE = 30% zichtbaar
      guide = "none"                         # Verbergt de legende voor transparantie
    )

  return(p)
}

# Pas deze plot-functie opnieuw toe op al je modellen
forest_plots <- imap(modellen_lijst, maak_forest_plot)

# En combineer ze weer
figuur_forest <- wrap_plots(forest_plots, ncol = 2) +
  plot_annotation(title = "Vergelijking van effectgroottes per watertype")

figuur_forest

