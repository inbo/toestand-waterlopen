library(tidyverse)

# Stap 1: Bereken de gemiddelden voor de horizontale en verticale lijnen
gemiddelden <- dredge_data_rivier %>%
  summarise(
    czv_log = mean(czv_log, na.rm = TRUE),
    n_t_log = mean(n_t_log, na.rm = TRUE),
    p_t_log = mean(p_t_log, na.rm = TRUE),
    cl_log = mean(cl_log, na.rm = TRUE),
    overstorten_blootstelling_index_log = mean(overstorten_blootstelling_index_log, na.rm = TRUE),
    lozingen_industrie_ie_log = mean(lozingen_industrie_ie_log, na.rm = TRUE),
    lozingen_rwzi_ie_log = mean(lozingen_rwzi_ie_log, na.rm = TRUE),
    lozingen_riool_ie_log = mean(lozingen_riool_ie_log, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "gemiddelde")

ec20_gemiddelde <- mean(dredge_data$ec_20_log, na.rm = TRUE)

# Stap 2: Maak de slimme plot met groene markeringen
dredge_data %>%
  # NIEUW: Maak een TRUE/FALSE kolom aan voor de verzilte punten.
  # (coalesce zorgt ervoor dat eventuele NA's geen foutmelding geven)
  mutate(is_high_cl = coalesce(cl_log > log(200000), FALSE)) %>%

  # Selecteer de variabelen PLUS onze nieuwe is_high_cl vlag
  select(ec_20_log, czv_log, n_t_log, p_t_log, cl_log, overstorten_blootstelling_index_log,
         lozingen_industrie_ie_log, lozingen_riool_ie_log, lozingen_rwzi_ie_log, is_high_cl) %>%

  pivot_longer(cols = c(czv_log, n_t_log, p_t_log, cl_log, overstorten_blootstelling_index_log,
                        lozingen_industrie_ie_log, lozingen_rwzi_ie_log, lozingen_riool_ie_log),
               names_to = "parameter", values_to = "waarde") %>%

  # NIEUW: Koppel de kleur (color) aan de is_high_cl kolom
  ggplot(aes(x = ec_20_log, y = waarde, color = is_high_cl)) +
  geom_point(alpha = 0.6, size = 2) +

  # NIEUW: Bepaal handmatig de kleuren: FALSE = oud blauw, TRUE = groen
  scale_color_manual(values = c("FALSE" = "#2c3e50", "TRUE" = "#00ff00")) +

  # Rode stippellijn voor het gemiddelde van de parameters (Horizontaal)
  geom_hline(data = gemiddelden, aes(yintercept = gemiddelde),
             color = "red", linetype = "dashed", size = 1) +

  # Blauwe stippellijn voor het gemiddelde van EC20 (Verticaal)
  geom_vline(xintercept = ec20_gemiddelde,
             color = "blue", linetype = "dashed", size = 1) +

  # Splits op in drie (of meer) aparte grafiekjes
  facet_wrap(~ parameter, scales = "free_y") +
  theme_bw() +

  # NIEUW: Verberg de TRUE/FALSE legende, dat is mooier voor de plot
  theme(legend.position = "none") +

  labs(
    title = "Chemische Vingerafdruk van EC20 Pieken",
    subtitle = "Kwadrant rechtsboven = Zout + Vuil (Riool/Mest)\nKwadrant rechtsonder = Alleen Zout (Strooizout/Kwel)\nGroene punten = Zwaar verzilte monsters (Cl > 200 mg/L)",
    x = "EC20 (Geleidbaarheid, log-geschaald)",
    y = "Concentratie (log-geschaald)"
  )
