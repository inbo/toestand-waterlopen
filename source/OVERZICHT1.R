library(mgcv)
# --- 1. Data Voorbereiding (met transformatie voor Beta) ---
n_total <- nrow(data4)

mi_nested <- data4 %>%
  # Eerst de scores naar 0-1 brengen
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  # Transformeren om exacte 0 en 1 te vermijden voor de Beta-familie
  mutate(across(c(mmif, ept, swd, nst, tax, mts),
                ~ (.x * (n_total - 1) + 0.5) / n_total)) %>%
  mutate(subset = case_when(
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
    statuut == "Kunstmatig"                                            ~ "kunstmatig",
    type == "RtNt"                                                     ~ "rtnt",
    TRUE                                                               ~ "overig"
  )) %>%
  filter(subset != "overig") %>%
  # filter(subset == "nat_sv_kempen") %>%
  pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
               names_to = "index",
               values_to = "waarde") %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept")),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken)) %>%
  group_nest(subset, index)

# --- 2. Modellen fitten met de Beta-familie ---
mi_models <- mi_nested %>%
  mutate(
    model = map(data, ~ gam(waarde ~ s(jaar_num, k = 3) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re"),
                            data = .x,
                            family = betar(link = "logit"), # DIT VOORKOMT NEGATIEVE WAARDEN
                            method = "REML")),

    # Gebruik ggpredict en zet terug naar de 0-1 schaal
    # ggpredict doet dit automatisch voor de link-functie
    predictions = map(model, ~ as.data.frame(ggpredict(.x, terms = "jaar_num [all]")))
  )


#
# ########paralel
#
# library(mgcv)
# library(furrr)
# library(ggeffects)
# library(tidyverse)
#
# # --- 1. Voorbereiding & Parallel Setup ---
# # Bereken n voor de Smithson & Verkuilen transformatie
# n_total <- nrow(data4)
#
# # Stel parallelle workers in (beschikbare cores minus 1)
# plan(multisession, workers = parallel::detectCores() - 1)
#
# # Maak een 'veilige' versie van gam en ggpredict voor foutafhandeling
# safe_gam <- possibly(function(...) {
#   mgcv::gam(...)
# }, otherwise = NULL)
#
# safe_predict <- possibly(function(...) {
#   as.data.frame(ggeffects::ggpredict(...))
# }, otherwise = NULL)
#
# # --- 2. Data Pipeline & Nesting ---
# mi_nested <- data4 %>%
#   # Stap A: Normaliseer scores naar 0-1 en pas Beta-correctie toe
#   mutate(across(c("mmif", "ept", "swd", "nst", "tax", "mts"),
#                 ~ ((ifelse(cur_column() == "mmif", .x, .x / 4) * (n_total - 1) + 0.5) / n_total))) %>%
#   # Stap B: Subsets definiëren
#   mutate(
#     subset = case_when(
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
#       statuut == "Kunstmatig"                                            ~ "kunstmatig",
#       type == "RtNt"                                                     ~ "rtnt",
#       TRUE                                                               ~ "overig"
#     )
#   ) %>%
#   filter(subset != "overig") %>%
#   # Stap C: Voorbereiden voor nesting
#   pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
#                names_to = "index",
#                values_to = "waarde") %>%
#   mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
#          subset = as.factor(subset),
#          index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept")),
#          meetplaats = as.factor(meetplaats),
#          bekken = as.factor(bekken)) %>%
#   group_nest(subset, index)
#
# # --- 3. Parallelle Modelfitting met Progress Bar ---
# mi_results <- mi_nested %>%
#   mutate(
#     # Fitten van de GAM modellen
#     model = future_map(data, ~ safe_gam(
#       waarde ~ s(jaar_num, k = 3) + s(meetplaats, bs = "re") + s(bekken, bs = "re"),
#       data = .x,
#       family = betar(link = "logit"),
#       method = "REML"
#     ), .progress = TRUE, .options = furrr_options(seed = TRUE)),
#
#     # Genereren van de predicties (alleen als model gelukt is)
#     predictions = future_map(model, ~ {
#       if (is.null(.x)) return(NULL)
#       safe_predict(.x, terms = "jaar_num [all]")
#     }, .progress = TRUE, .options = furrr_options(seed = TRUE))
#   )
#
# # Sluit de parallelle sessies
# plan(sequential)
#
# # --- 4. Opschonen en Klaarmaken voor Plot ---
# mi_trends <- mi_results %>%
#   filter(!map_lgl(predictions, is.null)) %>% # Verwijder mislukte modellen
#   select(subset, index, predictions) %>%
#   unnest(predictions)
#
# # --- 5. Visualisatie ---
# ggplot(mi_trends, aes(x = x, y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset), alpha = 0.2) +
#   geom_line(aes(color = subset), size = 1) +
#   facet_grid(index ~ subset, scales = "free_y") +
#   scale_y_continuous(limits = c(0, 1)) +
#   theme_minimal() +
#   labs(title = "Trends in MMIF en Deelmaatlatten (Beta-GAM)",
#        subtitle = "Parallel berekend met foutafhandeling",
#        x = "Jaar", y = "Voorspelde score (0-1)")
#

# --- 3. Uitpakken en Visualiseren ---
mi_trends <- mi_models %>%
  select(subset, index, predictions) %>%
  unnest(predictions)

# Optioneel: als je de deelmaatlatten terug naar 0-4 wilt schalen in de plot:
# mi_trends <- mi_trends %>%
#   mutate(across(c(predicted, conf.low, conf.high), ~ ifelse(index == "mmif", .x, .x * 4)))

ggplot(mi_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset), alpha = 0.2) +
  geom_line(aes(color = subset), size = 1) +
  facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) + # Forceert de as tussen 0 en 1
  labs(title = "Correcte GAM-trends (Beta-regressie)",
       subtitle = "Indices genormaliseerd naar 0-1 schaal",
       x = "Jaar", y = "Voorspelde waarde (0-1)")

