library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(patchwork)

# --- 1. Data Voorbereiding voor alle nutriënten ---
# We nemen aan dat 'data5' al de 'subset' kolom bevat.
# Indien niet, voeg de case_when code uit je vorige script hier in.
chem_nested <- data5 %>%
  drop_na(mmif) %>%
  filter(if_all(c("ept", "swd", "nst", "tax", "mts"), ~ .x %% 1 == 0)) %>%
  # group_by(meetplaats) %>%
  # filter(n() >= 4) %>%
  # ungroup() %>%
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  mutate(
    subset = case_when(
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
      statuut == "Kunstmatig" | groep %in% c("rivier", "beek", "kempen", "polder")                                            ~ "kunstmatig", # aanpssen zodat enkel waterlopen zijn en geen zout!
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
  # Selecteer enkel de kolommen die we nodig hebben om geheugen te sparen
  select(meetplaats, jaar_num, subset, bekken, owl, no3, no2, n_t, p_t, zs, czv, nh4) %>%
  # HIER ZIT DE MAGIC: we smelten de 6 variabelen om naar 2 kolommen ('variabele' en 'waarde')
  pivot_longer(
    cols = c(n_t, no3, no2, p_t, czv, zs, nh4),
    names_to = "variabele",
    values_to = "waarde",
    values_drop_na = TRUE # Gooi direct de NA's eruit per variabele
  ) %>%
  # Groepeer nu op ZOWEL subset ALS variabele, en nest de data
  group_nest(variabele, subset)


# --- 2. Modellen fitten voor ALLES tegelijk ---
cat("▶ BAM modellen aan het fitten voor 6 variabelen. Dit kan even duren...\n")
n_cores <- parallel::detectCores() - 1

chem_models <- chem_nested %>%
  mutate(
    # Fit het BAM model
    model = map(data, ~ bam(
      waarde ~ s(jaar_num, k = 4) +
        s(meetplaats, bs = "re"),
        # s(bekken, bs = "re") +
        # s(owl, bs = "re"),
      family = gaussian(),
      data = .x,
      method = "fREML",
      discrete = TRUE,
      nthreads = n_cores
    )),

    # Extracteer p-waarden en voorspellingen
    p_val_jaar = map_dbl(model, ~ summary(.x)$s.table["s(jaar_num)", "p-value"]),
    is_significant = factor(ifelse(p_val_jaar < 0.05, "Ja", "Nee"), levels = c("Ja", "Nee")),

    predictions = map(model, ~ as.data.frame(
      ggpredict(.x,
                terms = "jaar_num [all]",
                type = "fixed",
                exclude = c("s(meetplaats)", "s(bekken)", "s(owl)"))
    ))
  )


# --- 3. Resultaten uitpakken ---
chem_trends <- chem_models %>%
  select(variabele, subset, is_significant, predictions) %>%
  unnest(predictions)


# --- 4. Automatische Visualisatie (Één plot per parameter) ---
# We maken een slimme functie die de data filtert en plot
plot_chem_trend <- function(var_naam, data_trends) {

  df_plot <- data_trends %>% filter(variabele == var_naam)

  p <- ggplot(df_plot, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +
    geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +

    facet_wrap(~ subset, nrow = 1, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("MGCV BAM-trends voor", toupper(var_naam)),
      subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
      x = "Jaar",
      y = paste("Voorspelde concentratie (", toupper(var_naam), ")")
    ) +
    scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
    scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
    guides(color = "none", fill = "none") +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

# Genereer de plots voor alle unieke variabelen
variabelen_lijst <- unique(chem_trends$variabele)
lijst_met_plots <- map(variabelen_lijst, ~plot_chem_trend(.x, chem_trends))
names(lijst_met_plots) <- variabelen_lijst

# Je kunt ze nu één voor één bekijken, bijvoorbeeld:
print(lijst_met_plots$n_t)
print(lijst_met_plots$p_t)
print(lijst_met_plots$zs)
print(lijst_met_plots$czv)
print(lijst_met_plots$no3)
print(lijst_met_plots$no2)
print(lijst_met_plots$nh4)
