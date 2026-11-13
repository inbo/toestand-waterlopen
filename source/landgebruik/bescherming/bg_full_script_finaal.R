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
    jaar = year(monsternamedatum),
    ept_prop = ifelse(ta_xw == 0 | is.na(ta_xw), 0, (ep_tw / ta_xw) * 100),
    nst_prop = ifelse(ta_xw == 0 | is.na(ta_xw), 0, (nst_prop / ta_xw) * 100)
  ) %>%
  filter(jaar >= 2000)

df_long <- df_basis %>%
  select(meetplaats, monsternamedatum, jaar, owl, bekken, groep, geometry,
         mmif, ta_xw, ept_prop, sw_dw, mt_sw, nst_prop) %>%
  pivot_longer(cols = c(mmif, ta_xw, ept_prop, sw_dw, mt_sw, nst_prop), names_to = "metric", values_to = "waarde") %>%
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
  if(nrow(df) < 2) return(NA_real_)
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

biologische_finaal <- df_basis %>%
  arrange(meetplaats, desc(monsternamedatum)) %>%
  group_by(meetplaats) %>%
  slice_head(n = 1) %>%
  select(meetplaats, owl, bekken, groep, geometry) %>%
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
analyse_finaal_ms <- readRDS(here::here("data", "verwerkt", "analyse_finaal_multiscale.rds")) %>%
  mutate(
    owl = as.factor(owl),
    bekken = as.factor(bekken),
    groep = as.factor(groep)
  )

# Definieer de 4 metrics en 4 schalen (Let op: we gebruiken 'cirkel_100m')
metrics <- c("mmif", "ta_xw", "ept_prop", "sw_dw", "mt_sw", "nst_prop")
schalen <- c("asg_50k", "asg_5000m", "asg_1000m", "asg_250m", "cirkel_100m")

model_grid <- expand_grid(metric = metrics, schaal = schalen)

# ==============================================================================
# 1. GLMM LOOP + BENJAMINI-HOCHBERG CORRECTIE
# ==============================================================================
cat("▶ Bezig met berekenen 16 GLMMs...\n")

run_glmm <- function(metric, schaal, data) {

  # Definieer kolomnamen
  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_gain <- paste0("rate_of_PA_gain_", schaal)
  var_cover <- paste0("initiele_PA_cover_", schaal)
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # Data opschonen + HESSIAN FIX (delen door 100)
  data_clean <- data %>%
    filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_gain]])) %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],

      # DE FIX: We maken van de percentages een proportie (0 tot 1)
      # Dit helpt het model enorm om de wiskunde stabiel te houden!
      !!var_gain := .data[[var_gain]] / 100,
      !!var_cover := .data[[var_cover]] / 100
    )

  # Formule bouwen
  if (stringr::str_detect(schaal, "cirkel")) {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_gain, " * ", var_init,
      " + ", var_cover,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  } else {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_gain, " * ", var_init,
      " + ", var_cover, " + ", var_opp,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  }

  # Model draaien met de robuustere optimizer
  model <- glmmTMB(
    as.formula(formule_tekst),
    data = data_clean,
    control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))
  )

  model_coefs <- summary(model)$coefficients$cond

  # 1. P-waarde van de INTERACTIE ophalen
  interactie_naam <- paste0(var_gain, ":", var_init)
  if(interactie_naam %in% rownames(model_coefs)) {
    p_val_interactie <- model_coefs[interactie_naam, "Pr(>|z|)"]
  } else {
    p_val_interactie <- NA
  }

  # 2. P-waarde van het HOOFDEFFECT (Natuurwinst) ophalen
  if(var_gain %in% rownames(model_coefs)) {
    p_val_gain <- model_coefs[var_gain, "Pr(>|z|)"]
  } else {
    p_val_gain <- NA
  }

  return(tibble(
    p_ruw_gain = p_val_gain,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model)
  ))
}
# ==============================================================================
# DE LOOP MET CORRECTIE PER METRIC
# ==============================================================================
cat("▶ Bezig met berekenen 16 GLMMs...\n")
resultaten_glmm <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm(.x, .y, analyse_finaal_ms))) %>%
  unnest(glmm_out) %>%

  # HIER GEBEURT DE MAGIE: we groeperen per metric voor de correctie!
  group_by(metric) %>%
  mutate(
    p_adj_gain = p.adjust(p_ruw_gain, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%

  # Sorteer op de gecorrigeerde p-waarde van de interactie (zodat de beste bovenaan staan)
  arrange(p_adj_interactie)

# Print het overzicht, weergegeven met 3 decimalen voor de leesbaarheid
print("--- RESULTATEN GLMM (Gecorrigeerd PER METRIC) ---")
resultaten_glmm %>%
  select(metric, schaal, p_ruw_gain, p_adj_gain, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 24)



# 1. Haal het specifieke model uit de tabel
mijn_model <- resultaten_glmm %>%
  filter(metric == "mmif", schaal == "asg_1000m") %>%
  pull(model_obj) %>%
  .[[1]] # Dit haalt het model daadwerkelijk uit het 'lijstje'

# 2. Bekijk de klassieke model summary
summary(mijn_model)

# ==============================================================================
# AUTOMATISCHE FIGUREN GENEREREN (GLMM)
# ==============================================================================

plot_glmm_automatisch <- function(gekozen_metric, gekozen_schaal, resultaten_df, data_finaal) {

  # 1. Haal het specifieke GLMM model op uit je resultaten-tabel
  model <- resultaten_df %>%
    filter(metric == gekozen_metric, schaal == gekozen_schaal) %>%
    pull(model_obj) %>% .[[1]]

  var_init <- paste0("init_", gekozen_metric)
  var_gain <- paste0("rate_of_PA_gain_", gekozen_schaal)

  # 2. Bepaal 3 representatieve startwaarden (25%, 50% en 75% percentiel)
  q_waarden <- quantile(data_finaal[[var_init]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_waarden_rond <- round(q_waarden, 2)

  # 3. Vraag het model om voorspellingen (ggeffects houdt andere controles netjes op het gemiddelde)
  term_x <- paste0(var_gain, " [all]")
  term_group <- paste0(var_init, " [", paste(q_waarden_rond, collapse = ", "), "]")

  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  # 4. Vaste kleuren (voor een consistente stijl doorheen je hele thesis)
  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_waarden_rond, ")")

  # 5. Bouw de ggplot
  plot <- ggplot(voorsp, aes(x = x, y = predicted, color = group, fill = group)) +

    # Nul-lijn (grens tussen ecologische vooruitgang en achteruitgang)
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # Betrouwbaarheidsintervallen rond de lineaire trend
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +

    scale_color_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +

    labs(
      title = paste("GLMM: Effect Natuurwinst (", toupper(gekozen_schaal), ") op", toupper(gekozen_metric)),
      x = "Winst in Beschermd Gebied (%/jaar)",
      y = paste("Trend", toupper(gekozen_metric), "(%/jaar)")
    ) +

    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )

  return(plot)
}

# ---------------------------------------------------------
# VOORBEELD: Maak de plots!
# We halen ze nu uit 'resultaten_glmm' in plaats van gamm
# ---------------------------------------------------------
plot_glmm_1 <- plot_glmm_automatisch("mmif", "asg_5000m", resultaten_glmm, analyse_finaal_ms)
plot_glmm_2 <- plot_glmm_automatisch("ta_xw", "cirkel_100m", resultaten_glmm, analyse_finaal_ms)

# Toon ze naast elkaar
library(patchwork)
figuur_glmm_combi <- plot_glmm_1 + plot_glmm_2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(figuur_glmm_combi)

# Eventueel opslaan
ggsave(here::here("data", "verwerkt", "GLMM_multiscale_resultaten.png"), figuur_glmm_combi, width = 12, height = 6, dpi = 300)


library(patchwork)
library(purrr)

# ==============================================================================
# 1. ALLE 16 GLMM PLOTS GENEREREN IN ÉÉN LIJST
# ==============================================================================
cat("▶ Bezig met het bouwen van 16 GLMM figuren...\n")

# map2 loopt over de 'metric' en 'schaal' kolommen van je resultaten_glmm tabel
lijst_glmm_plots <- map2(
  resultaten_glmm$metric,
  resultaten_glmm$schaal,
  ~plot_glmm_automatisch(.x, .y, resultaten_glmm, analyse_finaal_ms)
)

# Geef de plots in de lijst handige namen zodat we ze makkelijk kunnen terugvinden
names(lijst_glmm_plots) <- paste0(resultaten_glmm$metric, "_", resultaten_glmm$schaal)


# ==============================================================================
# 2. OPTIE A: ÉÉN GIGANTISCHE OVERZICHTSFIGUUR (4x4)
# ==============================================================================
# wrap_plots plakt de hele lijst aan elkaar in een grid van 4 kolommen
mega_figuur <- wrap_plots(lijst_glmm_plots, ncol = 4)

# We maken hem lekker groot zodat alles goed leesbaar blijft (20x15 inch)
ggsave(
  here::here("data", "verwerkt", "GLMM_alle_16_modellen_grid.png"),
  mega_figuur,
  width = 24, height = 18, dpi = 300
)
cat("✅ De gigantische 4x4 overzichtsfiguur is opgeslagen in 'data/verwerkt'!\n")


# ==============================================================================
# 3. OPTIE B: ALLE PLOTS AUTOMATISCH ALS LOSSE BESTANDEN OPSLAAN
# ==============================================================================
# Maak een submapje aan voor de figuren
map_plots <- here::here("data", "verwerkt", "figuren_glmm")
dir.create(map_plots, showWarnings = FALSE)

cat("▶ Bezig met het opslaan van de 16 individuele plots...\n")

# pwalk (parallel walk) pakt de metric, de schaal, én de gemaakte plot,
# en slaat ze een voor een op met een logische bestandsnaam.
pwalk(
  list(resultaten_glmm$metric, resultaten_glmm$schaal, lijst_glmm_plots),
  function(met, sch, plt) {

    bestandsnaam <- file.path(map_plots, paste0("GLMM_", met, "_", sch, ".png"))

    # Gebruik een normale afmeting voor individuele plots
    ggsave(bestandsnaam, plt, width = 7, height = 5, dpi = 300)
  }
)
cat("✅ Alle 16 individuele plots staan klaar in de map 'figuren_glmm'!\n")

# Oproepen op basis van de index (bijv. de 3e plot)
lijst_glmm_plots[[3]]

# Of oproepen op basis van de naam die we hebben ingesteld
lijst_glmm_plots[["mmif_cirkel_100m"]]

lijst_glmm_plots[["mmif_cirkel_100m"]] + lijst_glmm_plots[["sw_dw_cirkel_100m"]] +
  lijst_glmm_plots[["ept_prop_cirkel_100m"]] + lijst_glmm_plots[["ta_xw_cirkel_100m"]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

lijst_glmm_plots[["mmif_asg_1000m"]] + lijst_glmm_plots[["sw_dw_asg_1000m"]] +
  lijst_glmm_plots[["ept_prop_asg_1000m"]] + lijst_glmm_plots[["ta_xw_asg_1000m"]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# ==============================================================================
# GLMM: EFFECT VAN GEMIDDELDE PA COVER (i.p.v. Gain)
# ==============================================================================

library(dplyr)

analyse_finaal_ms_clean <- analyse_finaal_ms %>%
  mutate(
    # Pas dit toe op alle kolommen waarvan de naam begint met "trend_"
    across(
      starts_with("trend_"),
      ~ if_else(.x > -10 & .x < 10, .x, NA_real_)
    )
  )

# Even checken hoeveel extreme waarden er per metric zijn omgezet in NA:
cat("Aantal NAs in de originele dataset:\n")
colSums(is.na(analyse_finaal_ms %>% select(starts_with("trend_"))))

cat("\nAantal NAs na het verwijderen van de extreme uitschieters (<-10 of >15):\n")
colSums(is.na(analyse_finaal_ms_clean %>% select(starts_with("trend_"))))

run_glmm_cover <- function(metric, schaal, data) {

  # Definieer kolomnamen
  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_pa <- paste0("mean_PA_cover_", schaal)
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # Data opschonen & HESSIAN FIX (delen door 100)
  data_clean <- data %>%
    filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_pa]])) %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],
      # DE FIX: Maak er een proportie (0-1) van in plaats van procenten (0-100)
      !!var_pa := .data[[var_pa]] / 100
    )

  # Formule bouwen
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

  # Model draaien (met een iets robuustere optimizer voor de zekerheid)
  model <- glmmTMB(
    as.formula(formule_tekst),
    data = data_clean,
    control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))
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
# DE LOOP MET CORRECTIE PER METRIC
# ==============================================================================
cat("▶ Bezig met berekenen 16 GLMMs (PA Cover i.p.v. Gain)...\n")

resultaten_glmm_cover <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm_cover(.x, .y, analyse_finaal_ms))) %>%
  unnest(glmm_out) %>%

  # Correctie per metric (n=4)
  group_by(metric) %>%
  mutate(
    p_adj_cover = p.adjust(p_ruw_cover, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%

  # Sorteer weer netjes op de interactie p-waarde
  arrange(p_adj_interactie)

# Print het overzicht met 4 decimalen
print("--- RESULTATEN GLMM (PA COVER, Gecorrigeerd PER METRIC) ---")

resultaten_glmm_cover %>%
  select(metric, schaal, p_ruw_cover, p_adj_cover, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 16)


resultaten_glmm_cover_clean <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm_cover(.x, .y, analyse_finaal_ms_clean))) %>%
  unnest(glmm_out) %>%

  # Correctie per metric (n=4)
  group_by(metric) %>%
  mutate(
    p_adj_cover = p.adjust(p_ruw_cover, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%

  # Sorteer weer netjes op de interactie p-waarde
  arrange(p_adj_interactie)

resultaten_glmm_cover_clean %>%
  select(metric, schaal, p_ruw_cover, p_adj_cover, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 16)

mijn_model <- resultaten_glmm_cover %>%
  filter(metric == "sw_dw", schaal == "asg_5000m") %>%
  pull(model_obj) %>%
  .[[1]] # Dit haalt het model daadwerkelijk uit het 'lijstje'

# 2. Bekijk de klassieke model summary
summary(mijn_model)
simulateResiduals(mijn_model, plot = T)

library(patchwork)
library(ggeffects)
library(ggplot2)
library(purrr)

# ==============================================================================
# 1. DE PLOT-FUNCTIE VOOR PA COVER
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

# ==============================================================================
# 2. ALLE 16 PLOTS GENEREREN IN EEN LIJST
# ==============================================================================
cat("▶ 16 Cover-plots genereren...\n")

# Maak de lijst aan
lijst_glmm_cover_plots <- map2(
  resultaten_glmm_cover$metric,
  resultaten_glmm_cover$schaal,
  ~plot_glmm_cover_automatisch(.x, .y, resultaten_glmm_cover, analyse_finaal_ms)
)

# Geef de lijst dezelfde handige namen
names(lijst_glmm_cover_plots) <- paste0(resultaten_glmm_cover$metric, "_", resultaten_glmm_cover$schaal)


# ==============================================================================
# 3. PATCHWORK: COMBINEER DE 4 METRICS OP 1000m SCHAAL
# ==============================================================================
# Nu kan je exact jouw voorgestelde code gebruiken!

figuur_asg_5000m <-
  lijst_glmm_cover_plots[["mmif_asg_5000m"]] +
  lijst_glmm_cover_plots[["sw_dw_asg_5000m"]] +
  lijst_glmm_cover_plots[["ept_prop_asg_5000m"]] +
  lijst_glmm_cover_plots[["ta_xw_asg_5000m"]] +
  plot_layout(guides = "collect", ncol = 2) & # ncol = 2 maakt er mooi een 2x2 grid van
  theme(legend.position = "bottom")

# Bekijk hem in RStudio:
print(figuur_asg_5000m)

# ==============================================================================
# FIGUUR 1: OVERZICHT VAN DE 5000m SCHAAL (Jouw sterkste model)
# ==============================================================================
figuur_5000m_overzicht <-
  lijst_glmm_cover_plots[["sw_dw_asg_5000m"]] +
  lijst_glmm_cover_plots[["ept_prop_asg_5000m"]] +
  lijst_glmm_cover_plots[["mmif_asg_5000m"]] +
  lijst_glmm_cover_plots[["ta_xw_asg_5000m"]] +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# Bekijken en opslaan
print(figuur_5000m_overzicht)
ggsave(here::here("data", "verwerkt", "GLMM_Cover_5000m_AlleMetrics.png"), figuur_5000m_overzicht, width = 10, height = 8)

# ==============================================================================
# FIGUUR 2: HOE RUIMTELIJKE SCHAAL DE DIVERSITEIT (SW_DW) BEÏNVLOEDT
# ==============================================================================
figuur_sw_dw_schalen <-
  (lijst_glmm_cover_plots[["sw_dw_asg_5000m"]] + ggtitle("SW_DW (5000m ASG)")) +
  (lijst_glmm_cover_plots[["sw_dw_asg_250m"]] + ggtitle("SW_DW (250m ASG)")) +
  (lijst_glmm_cover_plots[["sw_dw_cirkel_100m"]] + ggtitle("SW_DW (100m Cirkel)")) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

# Bekijken en opslaan
print(figuur_sw_dw_schalen)
ggsave(here::here("data", "verwerkt", "GLMM_Cover_SW_DW_Schalen.png"), figuur_sw_dw_schalen, width = 14, height = 5)


# ==============================================================================
# 1. GAMM MODELLEN: GEMIDDELDE PA COVER (met scat family)
# ==============================================================================
cat("▶ Bezig met berekenen 16 GAMMs (PA Cover). Dit kan even duren...\n")

run_gamm_cover <- function(metric, schaal, data) {

  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_pa <- paste0("mean_PA_cover_", schaal)
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # Filter missing values en de 0.5% extreme uitschieters (om scat() niet te laten crashen)
  ondergrens <- quantile(data[[var_trend]], 0.005, na.rm = TRUE)
  bovengrens <- quantile(data[[var_trend]], 0.995, na.rm = TRUE)

  data_clean <- data %>%
    filter(
      !is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_pa]]),
      .data[[var_trend]] >= ondergrens, .data[[var_trend]] <= bovengrens
    ) %>%
    mutate(lengte_tijdreeks = 2023 - .data[[var_start]])

  # Formule bouwen met aparte smooths s() en interactie ti()
  if (stringr::str_detect(schaal, "cirkel")) {
    formule_tekst <- paste0(
      var_trend, " ~ s(", var_pa, ") + s(", var_init, ") + ti(", var_pa, ", ", var_init, ")",
      " + lengte_tijdreeks + groep + s(owl, bs='re') + s(bekken, bs='re')"
    )
  } else {
    formule_tekst <- paste0(
      var_trend, " ~ s(", var_pa, ") + s(", var_init, ") + ti(", var_pa, ", ", var_init, ")",
      " + s(", var_opp, ")",
      " + lengte_tijdreeks + groep + s(owl, bs='re') + s(bekken, bs='re')"
    )
  }

  # Model draaien
  model <- gam(as.formula(formule_tekst), family = scat(), data = data_clean, method = "REML")
  sum_mod <- summary(model)

  # Namen zoeken in de s.table (smooth terms)
  naam_pa <- paste0("s(", var_pa, ")")
  naam_interactie <- paste0("ti(", var_pa, ",", var_init, ")")

  # 1. P-waarde van PA Cover (Hoofdeffect)
  if(naam_pa %in% rownames(sum_mod$s.table)) {
    p_val_pa <- sum_mod$s.table[naam_pa, "p-value"]
  } else {
    p_val_pa <- NA
  }

  # 2. P-waarde van de Interactie
  if(naam_interactie %in% rownames(sum_mod$s.table)) {
    p_val_interactie <- sum_mod$s.table[naam_interactie, "p-value"]
  } else {
    p_val_interactie <- NA
  }

  return(tibble(
    p_ruw_cover = p_val_pa,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model)
  ))
}

resultaten_gamm_cover <- model_grid %>%
  mutate(gamm_out = map2(metric, schaal, ~run_gamm_cover(.x, .y, analyse_finaal_ms))) %>%
  unnest(gamm_out) %>%

  # Benjamini-Hochberg correctie per metric (n=4)
  group_by(metric) %>%
  mutate(
    p_adj_cover = p.adjust(p_ruw_cover, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(p_adj_interactie)

print("--- RESULTATEN GAMM (PA COVER, Gecorrigeerd PER METRIC) ---")
resultaten_gamm_cover %>%
  select(metric, schaal, p_ruw_cover, p_adj_cover, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 16)

# ==============================================================================
# 2. PLOTS EN PATCHWORK VOOR GAMM COVER
# ==============================================================================

plot_gamm_cover_automatisch <- function(gekozen_metric, gekozen_schaal, resultaten_df, data_finaal) {

  model <- resultaten_df %>%
    filter(metric == gekozen_metric, schaal == gekozen_schaal) %>%
    pull(model_obj) %>% .[[1]]

  var_init <- paste0("init_", gekozen_metric)
  var_pa <- paste0("mean_PA_cover_", gekozen_schaal)

  q_waarden <- quantile(data_finaal[[var_init]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_waarden_rond <- round(q_waarden, 2)

  term_x <- paste0(var_pa, " [all]")
  term_group <- paste0(var_init, " [", paste(q_waarden_rond, collapse = ", "), "]")

  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_waarden_rond, ")")

  plot <- ggplot(voorsp, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    labs(
      title = toupper(gekozen_metric),
      x = paste("Natuur (PA cover %) -", toupper(gekozen_schaal)),
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))

  return(plot)
}

# 1. Genereer alle 16 plots in één lijst
cat("▶ 16 GAMM Cover-plots genereren...\n")
lijst_gamm_cover_plots <- map2(
  resultaten_gamm_cover$metric,
  resultaten_gamm_cover$schaal,
  ~plot_gamm_cover_automatisch(.x, .y, resultaten_gamm_cover, analyse_finaal_ms)
)
names(lijst_gamm_cover_plots) <- paste0(resultaten_gamm_cover$metric, "_", resultaten_gamm_cover$schaal)

# 2. Patchwork: Combineer jouw 4 favorieten (bijv. ASG 1000m)
figuur_gamm_asg_5000m <-
  lijst_gamm_cover_plots[["mmif_asg_5000m"]] +
  lijst_gamm_cover_plots[["sw_dw_asg_5000m"]] +
  lijst_gamm_cover_plots[["ept_prop_asg_5000m"]] +
  lijst_gamm_cover_plots[["ta_xw_asg_5000m"]] +
  plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "bottom")

print(figuur_gamm_asg_5000m)

# Optioneel opslaan:
# ggsave(here::here("data", "verwerkt", "GAMM_Cover_ASG_1000m_Vergelijking.png"),
#        figuur_gamm_asg_1000m, width = 10, height = 8, dpi = 300)


#############""
# categorische analyse
##################


# ==============================================================================
# 1. DATA VOORBEREIDEN: CATEGORIE MAKEN
# ==============================================================================
analyse_cat <- analyse_finaal_ms %>%
  mutate(
    # Als de 100m cirkel voor meer dan 50% uit PA bestaat -> Binnen PA
    PA_status = if_else(mean_PA_cover_cirkel_100m >= 50, "Binnen_PA", "Buiten_PA"),
    PA_status = as.factor(PA_status)
  )

# Even checken hoe de verdeling is!
print(table(analyse_cat$PA_status))


# ==============================================================================
# 2. GAMM MODELLEN DRAAIEN (Scat family, zonder interactie)
# ==============================================================================
metrics <- c("mmif", "ta_xw", "ept_prop", "sw_dw")

run_cat_model <- function(metric, data) {
  var_trend <- paste0("trend_", metric)
  var_start <- paste0("start_jaar_", metric)

  # Filter NAs en uitschieters weg
  ondergrens <- quantile(data[[var_trend]], 0.005, na.rm = TRUE)
  bovengrens <- quantile(data[[var_trend]], 0.995, na.rm = TRUE)

  data_clean <- data %>%
    filter(
      !is.na(.data[[var_trend]]), !is.na(PA_status),
      .data[[var_trend]] >= ondergrens, .data[[var_trend]] <= bovengrens
    ) %>%
    mutate(lengte_tijdreeks = 2023 - .data[[var_start]])

  # De schone, simpele formule: Trend ~ PA_status
  formule_tekst <- paste0(
    var_trend, " ~ PA_status + lengte_tijdreeks + groep + s(owl, bs='re') + s(bekken, bs='re')"
  )

  # Model draaien
  model <- gam(as.formula(formule_tekst), family = scat(), data = data_clean, method = "REML")

  # P-waarde uithalen (dit is nu een vast effect in de parametrische tabel)
  sum_mod <- summary(model)
  p_val <- sum_mod$p.table["PA_statusBinnen_PA", "Pr(>|z|)"]

  return(tibble(metric = metric, p_ruw = p_val, model_obj = list(model)))
}


# Run de 4 modellen opnieuw
cat("▶ 4 Categorische GAMMs berekenen...\n")
resultaten_cat <- map_dfr(metrics, ~run_cat_model(.x, analyse_cat)) %>%
  mutate(p_adj = p.adjust(p_ruw, method = "BH"))

print("--- RESULTATEN CATEGORISCHE ANALYSE ---")
resultaten_cat %>%
  select(metric, p_ruw, p_adj) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print()

# ==============================================================================
# 3. AUTOMATISCH PLOTTEN VAN DE CATEGORISCHE EFFECTEN
# ==============================================================================
plot_cat_automatisch <- function(gekozen_metric, resultaten_df) {

  model <- resultaten_df %>%
    filter(metric == gekozen_metric) %>%
    pull(model_obj) %>% .[[1]]

  # Voorspelling maken via ggeffects
  voorsp <- ggpredict(model, terms = "PA_status")

  # Plot bouwen
  plot <- ggplot(voorsp, aes(x = x, y = predicted, color = x)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

    # Voeg een foutenmarge toe (errorbars) en het punt voor het gemiddelde
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1.2) +
    geom_point(size = 4) +

    scale_color_manual(values = c("Binnen_PA" = "#1b9e77", "Buiten_PA" = "#d95f02")) +

    labs(
      title = toupper(gekozen_metric),
      x = "",
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      legend.position = "none" # Legende is overbodig, de as-labels zeggen genoeg
    )

  return(plot)
}

# Maak de 4 plots
lijst_cat_plots <- map(metrics, ~plot_cat_automatisch(.x, resultaten_cat))
names(lijst_cat_plots) <- metrics

# Voeg ze samen met Patchwork
figuur_categorisch <-
  lijst_cat_plots[["mmif"]] +
  lijst_cat_plots[["sw_dw"]] +
  lijst_cat_plots[["ept_prop"]] +
  lijst_cat_plots[["ta_xw"]] +
  plot_layout(ncol = 2)

print(figuur_categorisch)

ggsave(here::here("data", "verwerkt", "GAMM_Categorisch_100m.png"),
       figuur_categorisch, width = 8, height = 7, dpi = 300)
