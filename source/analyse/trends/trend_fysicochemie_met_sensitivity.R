library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(patchwork)

# --- 1. Data Voorbereiding (Long format maken) ---
chem_long_base <- data5 %>%
  drop_na(mmif) %>%
  filter(if_all(c("ept", "swd", "nst", "tax", "mts"), ~ .x %% 1 == 0)) %>%
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  mutate(
    subset = case_when(
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
      statuut == "Kunstmatig" | groep %in% c("rivier", "beek", "kempen", "polder") ~ "kunstmatig",
      type == "RtNt"                                                      ~ "rtnt",
      TRUE                                                                ~ "overig"
    )
  ) %>%
  filter(subset != "overig") %>%
  mutate(
    jaar_num = as.numeric(format(monsternamedatum, "%Y")),
    subset = as.factor(subset),
    meetplaats = as.factor(meetplaats),
    bekken = as.factor(bekken),
    owl = as.factor(owl)
  ) %>%
  select(meetplaats, jaar_num, subset, bekken, owl, no3, no2, n_t, p_t, zs, czv, nh4) %>%
  pivot_longer(
    cols = c(n_t, no3, no2, p_t, czv, zs, nh4),
    names_to = "variabele",
    values_to = "waarde",
    values_drop_na = TRUE
  )

# --- 2. Data Splitsen en Samenvoegen ---
# Set 1: Alle data
chem_all <- chem_long_base %>%
  mutate(filter_type = "Alle data")

# Set 2: Enkel meetplaatsen met minstens 4 metingen per variabele
chem_filtered <- chem_long_base %>%
  group_by(variabele, meetplaats) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(filter_type = "Min. 4 metingen")

# Bind ze samen en nest op variabele, subset én filter_type
chem_nested <- bind_rows(chem_all, chem_filtered) %>%
  mutate(filter_type = factor(filter_type, levels = c("Alle data", "Min. 4 metingen"))) %>%
  group_nest(variabele, subset, filter_type)


# --- 3. Modellen fitten voor ALLES tegelijk ---
cat("▶ BAM modellen aan het fitten... Dit kan even duren...\n")
n_cores <- parallel::detectCores() - 1

chem_models <- chem_nested %>%
  mutate(
    model = map(data, ~ bam(
      waarde ~ s(jaar_num, k = 4) +
        s(meetplaats, bs = "re"),
      family = gaussian(),
      data = .x,
      method = "fREML",
      discrete = TRUE,
      nthreads = n_cores
    )),

    p_val_jaar = map_dbl(model, ~ summary(.x)$s.table["s(jaar_num)", "p-value"]),
    is_significant = factor(ifelse(p_val_jaar < 0.05, "Ja", "Nee"), levels = c("Ja", "Nee")),

    predictions = map(model, ~ as.data.frame(
      ggpredict(.x,
                terms = "jaar_num [all]",
                type = "fixed",
                exclude = c("s(meetplaats)", "s(bekken)", "s(owl)"))
    ))
  )


# --- 4. Resultaten uitpakken ---
chem_trends <- chem_models %>%
  select(variabele, subset, filter_type, is_significant, predictions) %>%
  unnest(predictions)


# --- 5. Automatische Visualisatie ---
plot_chem_trend <- function(var_naam, data_trends) {

  df_plot <- data_trends %>% filter(variabele == var_naam)

  p <- ggplot(df_plot, aes(x = x, y = predicted, color = filter_type, fill = filter_type)) +
    # Betrouwbaarheidsinterval met alpha o.b.v. significantie
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, alpha = is_significant), color = NA) +
    # Trendlijn met linetype o.b.v. significantie
    geom_line(aes(linetype = is_significant), linewidth = 1) +

    facet_wrap(~ subset, nrow = 1, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("MGCV BAM-trends voor", toupper(var_naam)),
      subtitle = "Vergelijking: Alle data vs. Meetplaatsen met ≥ 4 metingen",
      x = "Jaar",
      y = paste("Voorspelde concentratie (", toupper(var_naam), ")"),
      color = "Dataset",
      fill = "Dataset"
    ) +
    scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
    scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
    # Gebruik contrasterende kleuren voor de twee datasets
    scale_color_manual(values = c("Alle data" = "#E69F00", "Min. 4 metingen" = "#0072B2")) +
    scale_fill_manual(values = c("Alle data" = "#E69F00", "Min. 4 metingen" = "#0072B2")) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

# Genereer de plots
variabelen_lijst <- unique(chem_trends$variabele)
lijst_met_plots <- map(variabelen_lijst, ~plot_chem_trend(.x, chem_trends))
names(lijst_met_plots) <- variabelen_lijst

# Bekijk ze:
print(lijst_met_plots$n_t)
print(lijst_met_plots$p_t)
print(lijst_met_plots$zs)
print(lijst_met_plots$czv)
print(lijst_met_plots$no3)
print(lijst_met_plots$no2)
print(lijst_met_plots$nh4)
