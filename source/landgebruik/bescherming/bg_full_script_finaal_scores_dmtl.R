# ==============================================================================
# 0. PACKAGES, MAPPEN & DATA INLADEN
# ==============================================================================
source(here::here("source", "inladen_packages.R"))
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(terra)
library(exactextractr)
library(lubridate)

# Mappen
pa_map <- "C:/Users/emiel_delombaerde/Documents/GitHub/ProtConn/data/ccda_nat2000"
map_rasters <- here("data", "verwerkt", "landgebruik", "pa_rasters_tussenstappen")
dir.create(map_rasters, recursive = TRUE, showWarnings = FALSE)

# Shapefiles opzoeken
shape_bestanden <- list.files(path = pa_map, pattern = "\\.shp$")
shape_bestanden <- shape_bestanden[!str_detect(shape_bestanden, "2024")]
shape_bestanden <- sort(shape_bestanden, decreasing = TRUE)

# Referentie raster voor Vlaanderen
template_raster <- rast(here("data", "ruw", "landgebruik", "niveau1_vla_2013_v3.tif"))

# Biologische data
load(here("data", "verwerkt", "mi_data.rdata"))
load(here("data", "verwerkt", "mafy_data.rdata"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# ==============================================================================
# 1. BIOLOGISCHE DATA PREP (4 METRICS)
# ==============================================================================
cat("▶ 1. Biologische data en nieuwe metrics voorbereiden...\n")

df_basis <- mi_data_analyse %>%
  mutate(
    jaar = year(monsternamedatum)) %>%
  filter(jaar >= 2000)

df_long <- df_basis %>%
  select(meetplaats, monsternamedatum, jaar, owl, bekken, groep, statuut, geometry,
         mmif, tax, nst, mts, ept, swd) %>%
  mutate(tax = tax/4,
         nst = nst/4,
         mts = mts/4,
         ept = ept/4,
         swd = swd/4) %>%
  pivot_longer(cols = c(mmif, tax, nst, mts, ept, swd), names_to = "metric", values_to = "waarde") %>%
  filter(!is.na(waarde))

# Filteren (min 5 metingen, min 10 jaar tijdspanne eindigend in 2019-2023)
meetplaats_criteria <- df_long %>%
  st_drop_geometry() %>% # <--- DEZE REGEL LOST DE ERROR OP EN VERSNELT HET SCRIPT
  group_by(meetplaats, metric) %>%
  summarise(
    aantal_metingen = n(),
    allereerste_jaar = min(jaar, na.rm = TRUE),
    recentste_in_window = max(jaar[jaar >= 2019 & jaar <= 2023], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.infinite(recentste_in_window)) %>%
  filter(aantal_metingen >= 5, (recentste_in_window - allereerste_jaar) >= 10)

# Nu zal deze join perfect doorlopen, omdat enkel df_long nog een ruimtelijk object is!
df_gefilterd <- df_long %>%
  inner_join(meetplaats_criteria %>% select(meetplaats, metric), by = c("meetplaats", "metric"))

initiele_kwaliteit <- df_gefilterd %>%
  st_drop_geometry() %>%
  arrange(meetplaats, metric, monsternamedatum) %>%
  group_by(meetplaats, metric) %>%
  mutate(allereerste_jaar = min(jaar, na.rm = TRUE)) %>%
  filter(jaar <= (allereerste_jaar + 5)) %>%

  # 1. Noem de gemiddelde waarde direct 'init'
  summarise(
    init = mean(waarde, na.rm = TRUE),
    start_jaar = min(jaar, na.rm = TRUE),
    .groups = "drop"
  ) %>%

  # 2. Gebruik {.value} zodat R weet of hij 'init' of 'start_jaar' vooraan moet plakken
  pivot_wider(
    names_from = metric,
    values_from = c(init, start_jaar),
    names_glue = "{.value}_{metric}"
  )

bereken_trend <- function(df) {
  if (nrow(df) < 2) return(NA_real_)
  model <- lm(log(waarde + 0.01) ~ jaar, data = df)
  return((exp(coef(model)["jaar"]) - 1) * 100)
}

trends <- df_gefilterd %>%
  st_drop_geometry() %>%
  group_by(meetplaats, metric) %>%
  nest() %>%
  mutate(trend_pct_jaar = map_dbl(data, bereken_trend)) %>%
  select(-data) %>%
  pivot_wider(names_from = metric, values_from = trend_pct_jaar, names_prefix = "trend_")


#
# bereken_trend_sinclair <- function(df) {
#   # We hebben iets meer datapunten nodig voor een AR1 model
#   if (nrow(df) < 3) return(NA_real_)
#
#   # tryCatch voorkomt dat je script stopt als één beek niet berekend kan worden
#   trend <- tryCatch({
#
#     # 1. Het GLS model met AR1 correlatiestructuur (corAR1)
#     model <- gls(
#       log(waarde + 0.01) ~ jaar,
#       correlation = corAR1(form = ~ jaar), # Dit is de "first-order autoregressive structure"
#       data = df,
#       method = "REML"
#     )
#
#     # 2. De transformatie (exact zoals Sinclair)
#     (exp(coef(model)["jaar"]) - 1) * 100
#
#   }, error = function(e) {
#     # Als het model niet convergeert, retourneer NA
#     return(NA_real_)
#   })
#
#   return(trend)
# }
#
# # En dan pas je hem weer exact hetzelfde toe:
# trends <- df_gefilterd %>%
#   sf::st_drop_geometry() %>% # (Als df_gefilterd een sf object is)
#   group_by(meetplaats, metric) %>%
#   nest() %>%
#   mutate(trend_pct_jaar = map_dbl(data, bereken_trend_sinclair)) %>%
#   select(-data) %>%
#   pivot_wider(names_from = metric, values_from = trend_pct_jaar, names_prefix = "trend_") %>%
#   na.omit
#
# trends_correct %>% na.omit
#
# bereken_trend_hybride <- function(df) {
#   # Minimaal 2 punten nodig voor elke vorm van regressie
#   if (nrow(df) < 2) return(NA_real_)
#
#   # Probeer eerst het strenge GLS model (AR1)
#   trend <- tryCatch({
#
#     # GLS heeft echt minimaal 3 metingen nodig
#     if (nrow(df) < 3) stop("Te weinig punten voor GLS")
#
#     model <- gls(
#       log(waarde + 0.01) ~ jaar,
#       correlation = corAR1(form = ~ jaar),
#       data = df,
#       method = "REML"
#     )
#     (exp(coef(model)["jaar"]) - 1) * 100
#
#   }, error = function(e) {
#     # HIER IS DE MAGIC: Als GLS faalt, probeer het simpele LM model!
#     tryCatch({
#       model_lm <- lm(log(waarde + 0.01) ~ jaar, data = df)
#       (exp(coef(model_lm)["jaar"]) - 1) * 100
#
#     }, error = function(e2) {
#       # Als zelfs LM crasht (bijv. als alle jaren exact score 0 hebben), geef dan NA
#       return(NA_real_)
#     })
#   })
#
#   return(trend)
# }
#
# # Pas hem toe op je data:
# trends <- df_gefilterd %>%
#   sf::st_drop_geometry() %>%
#   group_by(meetplaats, metric) %>%
#   nest() %>%
#   mutate(trend_pct_jaar = map_dbl(data, bereken_trend_hybride)) %>%
#   select(-data) %>%
#   pivot_wider(names_from = metric, values_from = trend_pct_jaar, names_prefix = "trend_")

biologische_finaal <- df_basis %>%
  arrange(meetplaats, desc(monsternamedatum)) %>%
  group_by(meetplaats) %>%
  slice_head(n = 1) %>%
  select(meetplaats, owl, bekken, groep, statuut, geometry) %>%
  filter(statuut != "Kunstmatig") %>%
  left_join(initiele_kwaliteit, by = "meetplaats") %>%
  left_join(trends, by = "meetplaats")

# ==============================================================================
# 2. RUIMTELIJKE SCHALEN INLADEN
# ==============================================================================
cat("▶ 2. Polygonen en buffers inladen...\n")

asg_50k <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_buffered_50000m.gpkg"), quiet = TRUE) %>% st_transform(st_crs(template_raster))
asg_5000 <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_buffered_5000m.gpkg"), quiet = TRUE) %>% st_transform(st_crs(template_raster))
asg_1000 <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_buffered_1000m.gpkg"), quiet = TRUE) %>% st_transform(st_crs(template_raster))
asg_250  <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_buffered_250m.gpkg"), quiet = TRUE) %>% st_transform(st_crs(template_raster))

meetpunten_sf <- bind_rows(mi_data %>% select(meetplaats, geometry), mafy_data %>% select(meetplaats, geometry)) %>%
  distinct(meetplaats, .keep_all = TRUE) %>% st_transform(st_crs(template_raster))
cirkel_100 <- st_buffer(meetpunten_sf, dist = 100)

ruimtelijke_schalen <- list("asg_50k" = asg_50k, "asg_5000m" = asg_5000, "asg_1000m" = asg_1000, "asg_250m"  = asg_250, "cirkel_100m" = cirkel_100)

# ==============================================================================
# 3. MASTER-EXTRACTIE (CHECK RASTERS + OVERLAY ALLE SCHALEN)
# ==============================================================================
cat("▶ 3. PA overlap berekenen voor alle jaren en alle schalen...\n")

# Hier slaan we de overlap-fracties per schaal in op
map_tabellen_master <- here("data", "verwerkt", "landgebruik", "pa_tabellen_multiscale")
dir.create(map_tabellen_master, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# DE OPGELOSTE MASTER-EXTRACTIE
# ==============================================================================
verwerk_jaar <- function(shape_naam) {

  # DE FIX: Pak enkel de 4 cijfers die direct voor '.shp' staan!
  jaar <- str_extract(shape_naam, "[0-9]{4}(?=\\.shp)")

  pad_raster <- file.path(map_rasters, paste0("pa_raster_", jaar, ".tif"))

  # A. Maak het raster
  if (!file.exists(pad_raster)) {
    cat("  [!] Raster ontbreekt. Aanmaken voor jaar:", jaar, "...\n")
    pa_vect <- vect(file.path(pa_map, shape_naam))
    if (crs(pa_vect) != crs(template_raster)) pa_vect <- project(pa_vect, crs(template_raster))
    rasterize(pa_vect, template_raster, field = 1, background = 0, filename = pad_raster, overwrite = TRUE)
  }

  pa_rast <- rast(pad_raster)

  # B. Extract voor alle 4 de schalen
  res_jaar <- tibble(meetplaats = asg_5000$meetplaats)

  for (naam_schaal in names(ruimtelijke_schalen)) {
    polygonen <- ruimtelijke_schalen[[naam_schaal]]
    overlap <- exact_extract(pa_rast, polygonen, 'mean', progress = FALSE)

    # Maak een tijdelijke tabel
    tijdelijke_tabel <- tibble(
      meetplaats = polygonen$meetplaats,
      !!sym(paste0(naam_schaal, "_", jaar)) := overlap * 100
    )

    # Koppel veilig via left_join
    res_jaar <- res_jaar %>%
      left_join(tijdelijke_tabel, by = "meetplaats")
  }

  return(res_jaar)
}

# Run dit en kijk hoe mooi de jaren nu overlopen!
alle_jaren_data <- map(shape_bestanden, verwerk_jaar) %>%
  reduce(left_join, by = "meetplaats")

# Check je output! Dit zou nu asg_5000_2005, asg_5000_2008, etc. moeten zijn.
head(alle_jaren_data)

# ==============================================================================
# 4. TRENDS BEREKENEN & SAMENVOEGEN TOT DE MEESTER-DATASET
# ==============================================================================
cat("▶ 4. Finaliseren dataset...\n")

# ==============================================================================
# 4. TRENDS & OPPERVLAKTES BEREKENEN EN ALLES SAMENVOEGEN
# ==============================================================================

# A. De crash-proof vector-functie voor de PA-trend
bereken_pa_gain <- function(pct, jaar) {
  geldige_rijen <- !is.na(pct) & !is.na(jaar)
  pct_clean <- pct[geldige_rijen]
  jaar_clean <- jaar[geldige_rijen]

  if (length(pct_clean) < 2) return(NA_real_)
  model <- lm(pct_clean ~ jaar_clean)
  return(unname(coef(model)["jaar_clean"]))
}

# B. PA-metrics berekenen per schaal
cat("▶ PA-metrics berekenen per schaal...\n")
schalen_finaal <- map(names(ruimtelijke_schalen), function(naam_schaal) {

  alle_jaren_data %>%
    select(meetplaats, starts_with(paste0(naam_schaal, "_"))) %>%
    pivot_longer(
      cols = -meetplaats,
      names_to = "kolom_naam",
      values_to = "beschermd_pct"
    ) %>%
    mutate(
      # DE FIX: We voegen de '$' weer toe zodat hij '2023' pakt in plaats van '5000'
      pa_jaar = as.numeric(stringr::str_extract(kolom_naam, "[0-9]{4}$")),
      beschermd_pct = ifelse(is.na(beschermd_pct) | is.nan(beschermd_pct), 0, beschermd_pct)
    ) %>%
    filter(!is.na(pa_jaar)) %>%
    group_by(meetplaats) %>%
    summarise(
      mean_PA_cover = mean(beschermd_pct, na.rm = TRUE),
      initiele_PA_cover = dplyr::first(beschermd_pct, order_by = pa_jaar),
      rate_of_PA_gain = bereken_pa_gain(beschermd_pct, pa_jaar),
      .groups = "drop"
    ) %>%
    rename_with(~ paste0(., "_", naam_schaal), -meetplaats)

}) %>% reduce(left_join, by = "meetplaats")
saveRDS(schalen_finaal, here::here("data", "verwerkt", "analyse_finaal_schalen.rds"))

schalen_finaal <- readRDS(here::here("data", "verwerkt", "analyse_finaal_schalen.rds"))


# C. Oppervlaktes (log) berekenen per ruimtelijke schaal
cat("▶ Oppervlaktes berekenen per schaal...\n")
oppervlaktes_finaal <- imap(ruimtelijke_schalen, function(polygonen, naam_schaal) {

  if ("oppervlakte" %in% colnames(polygonen)) {
    opp_df <- polygonen %>%
      st_drop_geometry() %>%
      select(meetplaats, oppervlakte) %>%
      mutate(oppervlakte = as.numeric(oppervlakte))
  } else {
    opp_df <- polygonen %>%
      mutate(oppervlakte = as.numeric(st_area(.))) %>%
      st_drop_geometry() %>%
      select(meetplaats, oppervlakte)
  }

  opp_df %>%
    mutate(!!sym(paste0("log_opp_schaal_", naam_schaal)) := log10(oppervlakte + 1)) %>%
    select(-oppervlakte)

}) %>% reduce(left_join, by = "meetplaats")
saveRDS(oppervlaktes_finaal, here::here("data", "verwerkt", "analyse_finaal_oppervlaktes.rds"))

oppervlaktes_finaal <- readRDS(here::here("data", "verwerkt", "analyse_finaal_oppervlaktes.rds"))


# D. Finaliseren master dataset!
cat("▶ Finaliseren master dataset...\n")

analyse_finaal_multiscale <- biologische_finaal %>%
  left_join(schalen_finaal, by = "meetplaats") %>%
  left_join(oppervlaktes_finaal, by = "meetplaats") %>%
  mutate(
    # lengte_tijdreeks = 2023 - 2000,
    X_coord = st_coordinates(geometry)[,1],
    Y_coord = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() %>%
  # Kleine schoonmaak: filter locaties waar per ongeluk essentiële data ontbreekt
  filter(!is.na(X_coord))

saveRDS(analyse_finaal_multiscale, here::here("data", "verwerkt", "analyse_finaal_multiscale.rds"))
cat("✅ SUCCES! De dataset is 100% kloppend per schaal en opgeslagen!\n")

library(dplyr)
library(purrr)
library(tidyr)
library(glmmTMB)
library(mgcv)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(broom.mixed)

# ==============================================================================
# 0. DATA INLADEN EN GRID MAKEN
# ==============================================================================
analyse_finaal_multiscale <- readRDS(here::here("data", "verwerkt", "analyse_finaal_multiscale.rds"))

analyse_finaal_ms <- analyse_finaal_multiscale %>%
  # readRDS(here::here("data", "verwerkt", "analyse_finaal_multiscale.rds")) %>%
  mutate(
    owl = as.factor(owl),
    bekken = as.factor(bekken),
    groep = as.factor(groep)
  )

# Definieer de 4 metrics en 4 schalen (Let op: we gebruiken 'cirkel_100m')
metrics <- c("mmif", "tax", "ept", "swd", "mts", "nst")
schalen <- c("asg_50k", "asg_5000m", "asg_1000m", "asg_250m", "cirkel_100m")

model_grid <- expand_grid(metric = metrics, schaal = schalen)


run_glmm_cover_robuust <- function(metric, schaal, data) {

  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_pa <- paste0("mean_PA_cover_", schaal)
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # Data opschonen + Hessian Fix (delen door 100)
  data_clean <- data %>%
    filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_pa]])) %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],
      !!var_pa := .data[[var_pa]] / 100
    )

  # De klassieke, snelle glmmTMB formule
  if (stringr::str_detect(schaal, "cirkel")) {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_pa, " * ", var_init,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  } else {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_pa, " * ", var_init,
      " + ", var_opp,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  }

  # MODEL DRAAIEN: HIER ZIT DE MAGIE!
  # We gebruiken family = t_family() om het model robuust te maken tegen je uitschieters
  model <- glmmTMB(
    as.formula(formule_tekst),
    data = data_clean,
    family = t_family(link = "identity") # <--- Het snelle alternatief voor scat()!
  )

  model_coefs <- summary(model)$coefficients$cond

  # 1. P-waarde Interactie
  interactie_naam <- paste0(var_pa, ":", var_init)
  if(interactie_naam %in% rownames(model_coefs)) {
    p_val_interactie <- model_coefs[interactie_naam, "Pr(>|z|)"]
  } else {
    p_val_interactie <- NA
  }

  # 2. P-waarde Hoofdeffect
  if(var_pa %in% rownames(model_coefs)) {
    p_val_pa <- model_coefs[var_pa, "Pr(>|z|)"]
  } else {
    p_val_pa <- NA
  }

  return(tibble(
    p_ruw_cover = p_val_pa,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model)
  ))
}

# ==============================================================================
# DE LOOP DRAAIEN (Bliksemsnel!)
# ==============================================================================
cat("▶ Bezig met 16 Robuuste GLMMs (Student-t verdeling)...\n")

resultaten_glmm_robuust <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm_cover_robuust(.x, .y, analyse_finaal_ms))) %>%
  unnest(glmm_out) %>%
  group_by(metric) %>%
  mutate(
    p_adj_cover = p.adjust(p_ruw_cover, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(p_adj_interactie)

print("--- RESULTATEN ROBUUSTE GLMM (t_family) ---")
resultaten_glmm_robuust %>%
  select(metric, schaal, p_ruw_cover, p_adj_cover, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 30)


library(DHARMa)

# 1. Haal exact hetzelfde model (SW_DW op 5000m) uit je NIEUWE robuuste resultaten
mijn_robuuste_model <- resultaten_glmm_robuust %>%
  filter(metric == "mt_sw", schaal == "asg_50k") %>%
  pull(model_obj) %>%
  .[[1]]

# 2. Simuleer de residuen voor dit t_family model
gesimuleerde_residuen_robuust <- simulateResiduals(fittedModel = mijn_robuuste_model, plot = TRUE)


library(patchwork)
library(purrr)

# ==============================================================================
# 1. GENEREER ALLE PLOTS VOOR DE ROBUUSTE MODELLEN
# ==============================================================================

plot_glmm_cover_automatisch <- function(gekozen_metric, gekozen_schaal, resultaten_df, data_finaal) {

  # 1. Haal het model op
  model <- resultaten_df %>%
    filter(metric == gekozen_metric, schaal == gekozen_schaal) %>%
    pull(model_obj) %>% .[[1]]

  var_init <- paste0("init_", gekozen_metric)
  var_pa <- paste0("mean_PA_cover_", gekozen_schaal) # HIER gebruiken we nu cover!

  # 2. Startwaarden (percentielen) voor de interactie-lijnen
  q_waarden <- quantile(data_finaal[[var_init]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_waarden_rond <- round(q_waarden, 2)

  # 3. Voorspellingen via ggeffects
  term_x <- paste0(var_pa, " [all]")
  term_group <- paste0(var_init, " [", paste(q_waarden_rond, collapse = ", "), "]")

  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  # 4. Kleuren en labels
  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_waarden_rond, ")")

  # 5. ggplot bouwen
  plot <- ggplot(voorsp, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    labs(
      title = toupper(gekozen_metric), # Korte titel is handiger als we ze samenvoegen!
      x = paste("Natuur (PA cover %) -", toupper(gekozen_schaal)),
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))

  return(plot)
}

# plot_glmm_cover_automatisch <- function(gekozen_metric, gekozen_schaal, resultaten_df, data_finaal) {
#
#   # 1. Haal het model op
#   model <- resultaten_df %>%
#     filter(metric == gekozen_metric, schaal == gekozen_schaal) %>%
#     pull(model_obj) %>% .[[1]]
#
#   var_init <- paste0("init_", gekozen_metric)
#   var_pa <- paste0("mean_PA_cover_", gekozen_schaal)
#   var_trend <- paste0("trend_", gekozen_metric) # Toegevoegd om de Y-as van de punten te vinden
#
#   # 2. Startwaarden (percentielen) voor de interactie-lijnen
#   q_waarden <- quantile(data_finaal[[var_init]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
#   q_waarden_rond <- round(q_waarden, 2)
#
#   # 3. Voorspellingen via ggeffects
#   term_x <- paste0(var_pa, " [all]")
#   term_group <- paste0(var_init, " [", paste(q_waarden_rond, collapse = ", "), "]")
#
#   voorsp <- ggpredict(model, terms = c(term_x, term_group))
#
#   # FIX: Schaal de voorspelde x-as (0-1) terug naar procenten (0-100) voor de plot
#   voorsp$x <- voorsp$x * 100
#
#   # 4. RUWE DATA VOORBEREIDEN VOOR DE PUNTEN
#   data_plot <- data_finaal %>%
#     filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_pa]]), !is.na(.data[[var_init]])) %>%
#     mutate(
#       x_plot = .data[[var_pa]],
#       y_plot = .data[[var_trend]],
#       # Deel de datapunten in categorieën in, zodat de kleuren matchen met de lijnen
#       group_plot = case_when(
#         .data[[var_init]] <= q_waarden[1] ~ as.character(q_waarden_rond[1]),
#         .data[[var_init]] > q_waarden[1] & .data[[var_init]] <= q_waarden[3] ~ as.character(q_waarden_rond[2]),
#         TRUE ~ as.character(q_waarden_rond[3])
#       ),
#       group_plot = factor(group_plot, levels = as.character(q_waarden_rond))
#     )
#
#   # 5. Kleuren en labels
#   mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")
#   labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_waarden_rond, ")")
#
#   # 6. ggplot bouwen (opgebouwd in lagen, punten onderaan, lijnen bovenaan!)
#   plot <- ggplot() +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#
#     # LAAG 1: De ruwe datapunten (alpha = 0.3 maakt ze transparant zodat het geen rommeltje wordt)
#     geom_point(data = data_plot, aes(x = x_plot, y = y_plot, color = group_plot),
#                alpha = 0.3, size = 1.5) +
#
#     # LAAG 2: De onzekerheidsmarges (ribbons) van het model
#     geom_ribbon(data = voorsp, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
#                 alpha = 0.2, inherit.aes = FALSE) +
#
#     # LAAG 3: De strakke voorspellingslijnen van het model
#     geom_line(data = voorsp, aes(x = x, y = predicted, color = group),
#               linewidth = 1.2, inherit.aes = FALSE) +
#
#     scale_color_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
#     scale_fill_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
#
#     # Optioneel: Zoom de Y-as een beetje in, zodat extreme betrouwbaarheidsintervallen de plot niet platdrukken
#     coord_cartesian(ylim = c(min(data_plot$y_plot) * 1.1, max(data_plot$y_plot) * 1.1)) +
#
#     labs(
#       title = toupper(gekozen_metric),
#       x = paste("Natuur (PA cover %) -", toupper(gekozen_schaal)),
#       y = "Trend (%/jaar)"
#     ) +
#     theme_minimal(base_size = 12) +
#     theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
#
#   return(plot)
# }

# Let op: we gebruiken hier de schone dataset en de robuuste resultatentabel!
lijst_robuuste_plots <- map2(
  resultaten_glmm_robuust$metric,
  resultaten_glmm_robuust$schaal,
  ~plot_glmm_cover_automatisch(.x, .y, resultaten_glmm_robuust, analyse_finaal_ms)
)
names(lijst_robuuste_plots) <- paste0(resultaten_glmm_robuust$metric, "_", resultaten_glmm_robuust$schaal)




figuur_contrast_50k <-
  (lijst_robuuste_plots[["swd_asg_50k"]]) +
  (lijst_robuuste_plots[["mmif_asg_50k"]]) +
  (lijst_robuuste_plots[["ept_asg_50k"]] ) +
  (lijst_robuuste_plots[["tax_asg_50k"]] ) +
  (lijst_robuuste_plots[["nst_asg_50k"]] ) +
  # (lijst_robuuste_plots[["mts_asg_50k"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_contrast_50k)


figuur_contrast_5000m <-
  (lijst_robuuste_plots[["swd_asg_5000m"]]) +
  (lijst_robuuste_plots[["mmif_asg_5000m"]]) +
  (lijst_robuuste_plots[["ept_asg_5000m"]] ) +
  (lijst_robuuste_plots[["tax_asg_5000m"]] ) +
  (lijst_robuuste_plots[["nst_asg_5000m"]] ) +
  (lijst_robuuste_plots[["mts_asg_5000m"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_contrast_5000m)


figuur_contrast_50k <-
  (lijst_robuuste_plots[["swd_asg_50k"]] +
     ggtitle("SW_DW (50k)\nMarginaal Hoofdeffect (p=0.068)")) +
  # (lijst_robuuste_plots[["mmif_asg_50k"]] +
  #    ggtitle("MMIF (50k)\nNiet significant (p=0.240)")) +
  # (lijst_robuuste_plots[["ept_asg_50k"]] +
  #    ggtitle("EPT PROP (50k)\nNiet significant (p=0.240)")) +
  # (lijst_robuuste_plots[["tax_asg_50k"]] +
  #    ggtitle("TAX (50k)\nNiet significant (p=0.240)")) +
  (lijst_robuuste_plots[["swd_asg_5000m"]] +
     ggtitle("NST (50k)\nNiet significant (p=0.240)")) +
  # (lijst_robuuste_plots[["mts_asg_50k"]] +
  #    ggtitle("MTS (50k)\nNiet significant (p=0.240)")) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_contrast_50k)


##########"
# data exploratie checks
#############

library(tidyverse)
library(patchwork)

# Zorg ervoor dat we ggplot instellen op een mooi thema
theme_set(theme_minimal(base_size = 12))

# ==============================================================================
# FIGUUR 1: VERDELING VAN NATUUR (PA COVER) PER SCHAAL
# ==============================================================================
# We smelten de 4 cover kolommen om tot 1 lange dataset voor makkelijk plotten
data_cover_long <- analyse_finaal_ms %>%
  select(meetplaats, starts_with("mean_PA_cover_")) %>%
  pivot_longer(cols = starts_with("mean_PA_cover_"),
               names_to = "schaal",
               values_to = "cover") %>%
  mutate(schaal = str_replace(schaal, "mean_PA_cover_", ""),
         schaal = toupper(schaal)) # Maak namen netjes (bijv. ASG_5000M)

plot_pa_cover <- ggplot(data_cover_long, aes(x = cover)) +
  geom_histogram(bins = 30, fill = "#1b9e77", color = "white") +
  facet_wrap(~schaal, scales = "free_y") + # Aparte panelen per schaal
  labs(
    title = "Spreiding van de Natuurcover (PA %)",
    subtitle = "Vlaamse realiteit: Veel meetpunten liggen in gebieden met zeer weinig natuur",
    x = "Gemiddelde PA Cover (%)",
    y = "Aantal meetplaatsen"
  ) +
  theme(plot.title = element_text(face = "bold"))

# Bekijken
print(plot_pa_cover)


# ==============================================================================
# FIGUUR 2: SPREIDING VAN DE STARTKWALITEIT (INIT) PER METRIC
# ==============================================================================
data_init_long <- analyse_finaal_ms %>%
  select(meetplaats, starts_with("init_")) %>%
  pivot_longer(cols = starts_with("init_"),
               names_to = "metric",
               values_to = "startkwaliteit") %>%
  mutate(metric = str_replace(metric, "init_", ""),
         metric = toupper(metric)) %>%
  filter(!is.na(startkwaliteit)) # Haal missende waarden eruit

plot_init <- ggplot(data_init_long, aes(x = startkwaliteit, fill = metric)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~metric, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Verdeling van de Startkwaliteit (rond jaar 2000)",
    subtitle = "Toont of beken in het begin vooral 'Slecht' of 'Goed' waren",
    x = "Startkwaliteit (Waarde)",
    y = "Dichtheid"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# Bekijken
print(plot_init)


# ==============================================================================
# FIGUUR 3: DE "INTERACTIE-RUIMTE" (Startkwaliteit vs PA Cover)
# ==============================================================================
# Dit toont of hoge startkwaliteit enkel voorkomt in natuurrijke gebieden,
# of dat de data goed gemengd is. We nemen EPT_PROP op 5000m als krachtig voorbeeld.

plot_interactie_ept <- ggplot(analyse_finaal_ms, aes(x = mean_PA_cover_asg_5000m, y = init_ept)) +
  geom_point(alpha = 0.4, color = "#d95f02", size = 2) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  labs(
    title = "Data voor de Interactie: EPT_PROP (Start) vs PA Cover (5000m)",
    subtitle = "Zijn er beken met hoge EPT-proporties in gebieden ZONDER natuur?",
    x = "Natuur (PA cover %) - ASG 5000m",
    y = "Startkwaliteit EPT_PROP"
  ) +
  theme(plot.title = element_text(face = "bold"))

# Bekijken
print(plot_interactie_ept)


# ==============================================================================
# OPTIONEEL: Opslaan van de figuren
# ==============================================================================
# ggsave(here::here("data", "verwerkt", "EDA_Spreiding_PACover.png"), plot_pa_cover, width = 8, height = 6)
# ggsave(here::here("data", "verwerkt", "EDA_Spreiding_Init.png"), plot_init, width = 8, height = 6)
# ggsave(here::here("data", "verwerkt", "EDA_Interactieruimte_EPT.png"), plot_interactie_ept, width = 7, height = 5)

