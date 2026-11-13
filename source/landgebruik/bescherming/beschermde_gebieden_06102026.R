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
library(glmmTMB)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(DHARMa)

# Mappen
pa_map <- "C:/Users/emiel_delombaerde/Documents/GitHub/ProtConn/data/ccda_nat2000"
map_rasters <- here("data", "verwerkt", "landgebruik", "pa_rasters_tussenstappen")
dir.create(map_rasters, recursive = TRUE, showWarnings = FALSE)
map_tabellen_master <- here("data", "verwerkt", "landgebruik", "pa_tabellen_multiscale")
dir.create(map_tabellen_master, recursive = TRUE, showWarnings = FALSE)

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
# 1. BIOLOGISCHE DATA PREP (INCLUSIEF SEIZOENSCORRECTIE)
# ==============================================================================
cat("▶ 1. Biologische data en nieuwe metrics voorbereiden...\n")

df_basis <- mi_data_analyse %>%
  mutate(
    jaar = lubridate::year(monsternamedatum),
    maand_num = lubridate::month(monsternamedatum) # ✔ TOEGEVOEGD: Maand voor seizoenscorrectie
  ) %>%
  filter(jaar >= 2000)

df_long <- df_basis %>%
  select(meetplaats, monsternamedatum, jaar, maand_num, owl, bekken, groep, statuut, geometry,
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
  st_drop_geometry() %>%
  group_by(meetplaats, metric) %>%
  summarise(
    aantal_metingen = n(),
    allereerste_jaar = min(jaar, na.rm = TRUE),
    recentste_in_window = max(jaar[jaar >= 2019 & jaar <= 2023], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.infinite(recentste_in_window)) %>%
  filter(aantal_metingen >= 5, (recentste_in_window - allereerste_jaar) >= 10)

df_gefilterd <- df_long %>%
  inner_join(meetplaats_criteria %>% select(meetplaats, metric), by = c("meetplaats", "metric"))

initiele_kwaliteit <- df_gefilterd %>%
  st_drop_geometry() %>%
  arrange(meetplaats, metric, monsternamedatum) %>%
  group_by(meetplaats, metric) %>%
  mutate(allereerste_jaar = min(jaar, na.rm = TRUE)) %>%
  filter(jaar <= (allereerste_jaar + 5)) %>%
  summarise(
    init = mean(waarde, na.rm = TRUE),
    start_jaar = min(jaar, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = metric,
    values_from = c(init, start_jaar),
    names_glue = "{.value}_{metric}"
  )

# ✔ VERNIEUWDE TREND-FUNCTIE: Met seizoenscorrectie (tryCatch constructie)
bereken_trend <- function(df) {
  if (nrow(df) < 2) return(NA_real_)

  trend <- tryCatch({
    # Poging 1: Model MET seizoenscorrectie
    model <- lm(log(waarde + 0.01) ~ jaar + as.factor(maand_num), data = df)
    (exp(coef(model)["jaar"]) - 1) * 100
  }, error = function(e) {
    # Fallback: Model ZONDER seizoenscorrectie (als er te weinig variatie in maanden is)
    tryCatch({
      model_fb <- lm(log(waarde + 0.01) ~ jaar, data = df)
      (exp(coef(model_fb)["jaar"]) - 1) * 100
    }, error = function(e2) NA_real_)
  })
  return(trend)
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
# 3. MASTER-EXTRACTIE (PA OVERLAP BEREKENEN)
# ==============================================================================
cat("▶ 3. PA overlap berekenen voor alle jaren en alle schalen...\n")

verwerk_jaar <- function(shape_naam) {
  jaar <- str_extract(shape_naam, "[0-9]{4}(?=\\.shp)")
  pad_raster <- file.path(map_rasters, paste0("pa_raster_", jaar, ".tif"))

  if (!file.exists(pad_raster)) {
    cat("  [!] Raster ontbreekt. Aanmaken voor jaar:", jaar, "...\n")
    pa_vect <- vect(file.path(pa_map, shape_naam))
    if (crs(pa_vect) != crs(template_raster)) pa_vect <- project(pa_vect, crs(template_raster))
    rasterize(pa_vect, template_raster, field = 1, background = 0, filename = pad_raster, overwrite = TRUE)
  }

  pa_rast <- rast(pad_raster)
  res_jaar <- tibble(meetplaats = asg_5000$meetplaats)

  for (naam_schaal in names(ruimtelijke_schalen)) {
    polygonen <- ruimtelijke_schalen[[naam_schaal]]
    overlap <- exact_extract(pa_rast, polygonen, 'mean', progress = FALSE)
    tijdelijke_tabel <- tibble(
      meetplaats = polygonen$meetplaats,
      !!sym(paste0(naam_schaal, "_", jaar)) := overlap * 100
    )
    res_jaar <- res_jaar %>% left_join(tijdelijke_tabel, by = "meetplaats")
  }
  return(res_jaar)
}

alle_jaren_data <- map(shape_bestanden, verwerk_jaar) %>%
  reduce(left_join, by = "meetplaats")

# ==============================================================================
# 4. TRENDS BEREKENEN & SAMENVOEGEN TOT DE MEESTER-DATASET
# ==============================================================================
cat("▶ 4. Oppervlaktes en PA-trends berekenen en finaliseren...\n")

bereken_pa_gain <- function(pct, jaar) {
  geldige_rijen <- !is.na(pct) & !is.na(jaar)
  pct_clean <- pct[geldige_rijen]
  jaar_clean <- jaar[geldige_rijen]
  if (length(pct_clean) < 2) return(NA_real_)
  model <- lm(pct_clean ~ jaar_clean)
  return(unname(coef(model)["jaar_clean"]))
}

schalen_finaal <- map(names(ruimtelijke_schalen), function(naam_schaal) {
  alle_jaren_data %>%
    select(meetplaats, starts_with(paste0(naam_schaal, "_"))) %>%
    pivot_longer(cols = -meetplaats, names_to = "kolom_naam", values_to = "beschermd_pct") %>%
    mutate(
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

oppervlaktes_finaal <- imap(ruimtelijke_schalen, function(polygonen, naam_schaal) {
  if ("oppervlakte" %in% colnames(polygonen)) {
    opp_df <- polygonen %>% st_drop_geometry() %>% select(meetplaats, oppervlakte) %>% mutate(oppervlakte = as.numeric(oppervlakte))
  } else {
    opp_df <- polygonen %>% mutate(oppervlakte = as.numeric(st_area(.))) %>% st_drop_geometry() %>% select(meetplaats, oppervlakte)
  }
  opp_df %>% mutate(!!sym(paste0("log_opp_schaal_", naam_schaal)) := log10(oppervlakte + 1)) %>% select(-oppervlakte)
}) %>% reduce(left_join, by = "meetplaats")

analyse_finaal_multiscale <- biologische_finaal %>%
  left_join(schalen_finaal, by = "meetplaats") %>%
  left_join(oppervlaktes_finaal, by = "meetplaats") %>%
  mutate(
    X_coord = st_coordinates(geometry)[,1],
    Y_coord = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() %>%
  filter(!is.na(X_coord))

saveRDS(analyse_finaal_multiscale, here::here("data", "verwerkt", "analyse_finaal_multiscale.rds"))
cat("✅ SUCCES! Master dataset aangemaakt en opgeslagen!\n")

# ==============================================================================
# 5. ROBUUSTE MODELLERING (MET SCALING & SINGULARITY CHECKS)
# ==============================================================================
analyse_finaal_ms <- analyse_finaal_multiscale %>%
  mutate(
    owl = as.factor(owl),
    bekken = as.factor(bekken),
    groep = as.factor(groep)
  )

metrics <- c("mmif", "tax", "ept", "swd", "mts", "nst")
schalen <- c("asg_50k", "asg_5000m", "asg_1000m", "asg_250m", "cirkel_100m")
model_grid <- expand_grid(metric = metrics, schaal = schalen)

run_glmm_cover_robuust <- function(metric, schaal, data) {

  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_pa <- paste0("mean_PA_cover_", schaal)
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # 1. UNGROUP en NAs filteren
  data_clean <- data %>%
    ungroup() %>% # <--- DE FIX: Verwijdert de verborgen meetplaats-groepering!
    filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_pa]]))

  # Check of er wel genoeg data overblijft om een model te draaien
  if(nrow(data_clean) < 5) {
    return(tibble(p_ruw_cover = NA, p_ruw_interactie = NA, model_obj = list(NULL), fallback_gebruikt = NA))
  }

  # 2. Een veilige schaalfunctie
  safe_scale <- function(x) {
    if(is.na(sd(x, na.rm = TRUE)) || sd(x, na.rm = TRUE) == 0) return(x - mean(x, na.rm = TRUE)) else return(as.numeric(scale(x)))
  }

  # 3. Z-scores berekenen (nu gaat dit over de hele dataset in plaats van per rij)
  data_clean <- data_clean %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],
      !!var_pa := safe_scale(.data[[var_pa]] / 100),
      !!var_init := safe_scale(.data[[var_init]]),
      lengte_tijdreeks = safe_scale(lengte_tijdreeks)
    )

  if (!stringr::str_detect(schaal, "cirkel")) {
    data_clean <- data_clean %>% mutate(!!var_opp := safe_scale(.data[[var_opp]]))
  }

  # 4. DYNAMISCHE FORMULE
  n_groepen <- length(unique(data_clean$groep))
  groep_term <- if (n_groepen > 1) " + groep" else ""

  if (stringr::str_detect(schaal, "cirkel")) {
    formule_int  <- paste0(var_trend, " ~ ", var_pa, " * ", var_init, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
    formule_main <- paste0(var_trend, " ~ ", var_pa, " + ", var_init, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
  } else {
    formule_int  <- paste0(var_trend, " ~ ", var_pa, " * ", var_init, " + ", var_opp, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
    formule_main <- paste0(var_trend, " ~ ", var_pa, " + ", var_init, " + ", var_opp, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
  }

  # 5. MODEL DRAAIEN
  model <- tryCatch({
    glmmTMB(as.formula(formule_int), data = data_clean, family = t_family(link = "identity"))
  }, error = function(e) NULL)

  is_singular <- FALSE
  if (!is.null(model)) {
    coef_se <- tryCatch(summary(model)$coefficients$cond[, "Std. Error"], error = function(e) NA)
    is_singular <- any(is.na(coef_se)) || any(coef_se > 100)
  }

  # Fallback naar model zonder interactie als model 1 faalt of singular is
  if (is.null(model) || is_singular) {
    model <- tryCatch({
      glmmTMB(as.formula(formule_main), data = data_clean, family = t_family(link = "identity"))
    }, error = function(e) NULL)
    is_singular <- TRUE
  }

  if (is.null(model)) {
    return(tibble(p_ruw_cover = NA, p_ruw_interactie = NA, model_obj = list(NULL), fallback_gebruikt = NA))
  }

  model_coefs <- summary(model)$coefficients$cond

  interactie_naam <- paste0(var_pa, ":", var_init)
  p_val_interactie <- if(interactie_naam %in% rownames(model_coefs)) model_coefs[interactie_naam, "Pr(>|z|)"] else NA
  p_val_pa <- if(var_pa %in% rownames(model_coefs)) model_coefs[var_pa, "Pr(>|z|)"] else NA

  return(tibble(
    p_ruw_cover = p_val_pa,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model),
    fallback_gebruikt = is_singular
  ))
}
cat("▶ Bezig met Robuuste GLMMs (Student-t verdeling)...\n")
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

print(resultaten_glmm_robuust %>% select(metric, schaal, p_ruw_cover, p_adj_cover, p_ruw_interactie, p_adj_interactie, fallback_gebruikt), n = 30)

# ==============================================================================
# 6. AUTOMATISCH PLOTTEN (MET ZERO INFLATION FIX)
# ==============================================================================

plot_glmm_cover_automatisch <- function(gekozen_metric, gekozen_schaal, resultaten_df, data_finaal) {

  res_rij <- resultaten_df %>% filter(metric == gekozen_metric, schaal == gekozen_schaal)
  model <- res_rij %>% pull(model_obj) %>% .[[1]]
  is_fallback <- res_rij %>% pull(fallback_gebruikt)

  var_init <- paste0("init_", gekozen_metric)
  var_pa <- paste0("mean_PA_cover_", gekozen_schaal)

  # ✔ STARTWAARDEN BEREKENEN: Eerst ruw (voor labels), dan geschaald (voor het model)
  q_w_raw <- quantile(data_finaal[[var_init]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_w_raw_rond <- round(q_w_raw, 2)

  gemiddelde_init <- mean(data_finaal[[var_init]], na.rm = TRUE)
  sd_init <- sd(data_finaal[[var_init]], na.rm = TRUE)
  q_w_scaled <- (q_w_raw - gemiddelde_init) / sd_init

  # ✔ ZERO-INFLATION FIX op de geschaalde percentielen
  if (q_w_scaled[1] == q_w_scaled[2]) q_w_scaled[2] <- q_w_scaled[1] + 0.01
  if (q_w_scaled[2] == q_w_scaled[3]) q_w_scaled[3] <- q_w_scaled[2] + 0.01
  if (q_w_scaled[1] == q_w_scaled[3]) q_w_scaled[3] <- q_w_scaled[3] + 0.02

  q_w_scaled_rond <- round(q_w_scaled, 3)

  # Voorspellingen
  term_x <- paste0(var_pa, " [all]")
  term_group <- paste0(var_init, " [", paste(q_w_scaled_rond, collapse = ", "), "]")
  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_w_raw_rond, ")")
  subtitel <- if (is_fallback) "Let op: Singularity gedetecteerd (Interactie weggelaten)" else NULL

  plot <- ggplot(voorsp, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    labs(
      title = toupper(gekozen_metric),
      subtitle = subtitel,
      x = paste("Natuur (PA cover Z-score) -", toupper(gekozen_schaal)),
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "darkred", hjust = 0.5)
    )

  return(plot)
}

# Plots genereren en opslaan in een lijst
lijst_robuuste_plots <- map2(
  resultaten_glmm_robuust$metric,
  resultaten_glmm_robuust$schaal,
  ~plot_glmm_cover_automatisch(.x, .y, resultaten_glmm_robuust, analyse_finaal_ms)
)
names(lijst_robuuste_plots) <- paste0(resultaten_glmm_robuust$metric, "_", resultaten_glmm_robuust$schaal)

# Voorbeeld: Print de plots voor de 5000m schaal
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

# ==============================================================================
# 7. ROBUUSTE MODELLERING VOOR PA-GAIN (TOENAME IN NATUUR)
# ==============================================================================
cat("\n▶ Start analyse voor PA-GAIN (Toename in beschermde gebieden)...\n")

# 1. Modelfunctie voor Gain
run_glmm_gain_robuust <- function(metric, schaal, data) {

  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_pa <- paste0("rate_of_PA_gain_", schaal) # <--- HIER gebruiken we nu de GAIN
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  data_clean <- data %>%
    ungroup() %>%
    filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_pa]]))

  if(nrow(data_clean) < 5) {
    return(tibble(p_ruw_gain = NA, p_ruw_interactie = NA, model_obj = list(NULL), fallback_gebruikt = NA))
  }

  safe_scale <- function(x) {
    if(is.na(sd(x, na.rm = TRUE)) || sd(x, na.rm = TRUE) == 0) return(x - mean(x, na.rm = TRUE)) else return(as.numeric(scale(x)))
  }

  # Z-scores berekenen
  data_clean <- data_clean %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],
      !!var_pa := safe_scale(.data[[var_pa]]), # Bij gain delen we niet door 100
      !!var_init := safe_scale(.data[[var_init]]),
      lengte_tijdreeks = safe_scale(lengte_tijdreeks)
    )

  if (!stringr::str_detect(schaal, "cirkel")) {
    data_clean <- data_clean %>% mutate(!!var_opp := safe_scale(.data[[var_opp]]))
  }

  n_groepen <- length(unique(data_clean$groep))
  groep_term <- if (n_groepen > 1) " + groep" else ""

  if (stringr::str_detect(schaal, "cirkel")) {
    formule_int  <- paste0(var_trend, " ~ ", var_pa, " * ", var_init, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
    formule_main <- paste0(var_trend, " ~ ", var_pa, " + ", var_init, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
  } else {
    formule_int  <- paste0(var_trend, " ~ ", var_pa, " * ", var_init, " + ", var_opp, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
    formule_main <- paste0(var_trend, " ~ ", var_pa, " + ", var_init, " + ", var_opp, " + lengte_tijdreeks", groep_term, " + (1|owl) + (1|bekken)")
  }

  # Model draaien met Singularity Fallback
  model <- tryCatch(glmmTMB(as.formula(formule_int), data = data_clean, family = t_family(link = "identity")), error = function(e) NULL)

  is_singular <- FALSE
  if (!is.null(model)) {
    coef_se <- tryCatch(summary(model)$coefficients$cond[, "Std. Error"], error = function(e) NA)
    is_singular <- any(is.na(coef_se)) || any(coef_se > 100)
  }

  if (is.null(model) || is_singular) {
    model <- tryCatch(glmmTMB(as.formula(formule_main), data = data_clean, family = t_family(link = "identity")), error = function(e) NULL)
    is_singular <- TRUE
  }

  if (is.null(model)) return(tibble(p_ruw_gain = NA, p_ruw_interactie = NA, model_obj = list(NULL), fallback_gebruikt = NA))

  model_coefs <- summary(model)$coefficients$cond

  interactie_naam <- paste0(var_pa, ":", var_init)
  p_val_interactie <- if(interactie_naam %in% rownames(model_coefs)) model_coefs[interactie_naam, "Pr(>|z|)"] else NA
  p_val_pa <- if(var_pa %in% rownames(model_coefs)) model_coefs[var_pa, "Pr(>|z|)"] else NA

  return(tibble(
    p_ruw_gain = p_val_pa,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model),
    fallback_gebruikt = is_singular
  ))
}

# 2. De Loop Draaien voor Gain
cat("▶ Bezig met Robuuste GLMMs voor PA GAIN...\n")
resultaten_glmm_gain <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm_gain_robuust(.x, .y, analyse_finaal_ms))) %>%
  unnest(glmm_out) %>%
  group_by(metric) %>%
  mutate(
    p_adj_gain = p.adjust(p_ruw_gain, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(p_adj_interactie)

print(resultaten_glmm_gain %>% select(metric, schaal, p_ruw_gain, p_adj_gain, p_ruw_interactie, fallback_gebruikt), n = 30)

# ==============================================================================
# 8. AUTOMATISCH PLOTTEN VOOR PA-GAIN
# ==============================================================================

plot_glmm_gain_automatisch <- function(gekozen_metric, gekozen_schaal, resultaten_df, data_finaal) {

  res_rij <- resultaten_df %>% filter(metric == gekozen_metric, schaal == gekozen_schaal)
  model <- res_rij %>% pull(model_obj) %>% .[[1]]
  is_fallback <- res_rij %>% pull(fallback_gebruikt)

  var_init <- paste0("init_", gekozen_metric)
  var_pa <- paste0("rate_of_PA_gain_", gekozen_schaal)

  # Startwaarden
  q_w_raw <- quantile(data_finaal[[var_init]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  q_w_raw_rond <- round(q_w_raw, 2)

  gemiddelde_init <- mean(data_finaal[[var_init]], na.rm = TRUE)
  sd_init <- sd(data_finaal[[var_init]], na.rm = TRUE)
  q_w_scaled <- (q_w_raw - gemiddelde_init) / sd_init

  # Zero-inflation fix
  if (q_w_scaled[1] == q_w_scaled[2]) q_w_scaled[2] <- q_w_scaled[1] + 0.01
  if (q_w_scaled[2] == q_w_scaled[3]) q_w_scaled[3] <- q_w_scaled[2] + 0.01
  if (q_w_scaled[1] == q_w_scaled[3]) q_w_scaled[3] <- q_w_scaled[3] + 0.02
  q_w_scaled_rond <- round(q_w_scaled, 3)

  # Voorspellingen
  term_x <- paste0(var_pa, " [all]")
  term_group <- paste0(var_init, " [", paste(q_w_scaled_rond, collapse = ", "), "]")
  voorsp <- ggpredict(model, terms = c(term_x, term_group))

  mijn_kleuren <- c("#d95f02", "#7570b3", "#1b9e77")
  labels_aangepast <- paste0(c("Slecht (", "Matig (", "Goed ("), q_w_raw_rond, ")")
  subtitel <- if (is_fallback) "Let op: Singularity gedetecteerd (Interactie weggelaten)" else NULL

  plot <- ggplot(voorsp, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    scale_fill_manual(name = paste("Start", toupper(gekozen_metric)), values = mijn_kleuren, labels = labels_aangepast) +
    labs(
      title = toupper(gekozen_metric),
      subtitle = subtitel,
      x = paste("Natuur toename (Gain Z-score) -", toupper(gekozen_schaal)), # Aangepaste label
      y = "Trend (%/jaar)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "darkred", hjust = 0.5)
    )

  return(plot)
}

# Plots genereren en opslaan in een lijst
lijst_robuuste_plots_gain <- map2(
  resultaten_glmm_gain$metric,
  resultaten_glmm_gain$schaal,
  ~plot_glmm_gain_automatisch(.x, .y, resultaten_glmm_gain, analyse_finaal_ms)
)
names(lijst_robuuste_plots_gain) <- paste0(resultaten_glmm_gain$metric, "_", resultaten_glmm_gain$schaal)

# Check bijvoorbeeld het resultaat voor 50k:
figuur_gain_50k <-
  (lijst_robuuste_plots_gain[["swd_asg_50k"]]) +
  (lijst_robuuste_plots_gain[["mmif_asg_50k"]]) +
  (lijst_robuuste_plots_gain[["ept_asg_50k"]] ) +
  (lijst_robuuste_plots_gain[["tax_asg_50k"]] ) +
  (lijst_robuuste_plots_gain[["nst_asg_50k"]] ) +
  (lijst_robuuste_plots_gain[["mts_asg_50k"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_gain_50k)

figuur_gain_250m <-
  (lijst_robuuste_plots_gain[["swd_asg_250m"]]) +
  (lijst_robuuste_plots_gain[["mmif_asg_250m"]]) +
  (lijst_robuuste_plots_gain[["ept_asg_250m"]] ) +
  (lijst_robuuste_plots_gain[["tax_asg_250m"]] ) +
  (lijst_robuuste_plots_gain[["nst_asg_250m"]] ) +
  (lijst_robuuste_plots_gain[["mts_asg_250m"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_gain_250m)

figuur_gain_5000m <-
  (lijst_robuuste_plots_gain[["swd_asg_5000m"]]) +
  (lijst_robuuste_plots_gain[["mmif_asg_5000m"]]) +
  (lijst_robuuste_plots_gain[["ept_asg_5000m"]] ) +
  (lijst_robuuste_plots_gain[["tax_asg_5000m"]] ) +
  (lijst_robuuste_plots_gain[["nst_asg_5000m"]] ) +
  (lijst_robuuste_plots_gain[["mts_asg_5000m"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_gain_5000m)

figuur_gain_1000m <-
  (lijst_robuuste_plots_gain[["swd_asg_1000m"]]) +
  (lijst_robuuste_plots_gain[["mmif_asg_1000m"]]) +
  (lijst_robuuste_plots_gain[["ept_asg_1000m"]] ) +
  (lijst_robuuste_plots_gain[["tax_asg_1000m"]] ) +
  (lijst_robuuste_plots_gain[["nst_asg_1000m"]] ) +
  (lijst_robuuste_plots_gain[["mts_asg_1000m"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_gain_1000m)

figuur_gain_100m <-
  (lijst_robuuste_plots_gain[["swd_cirkel_100m"]]) +
  (lijst_robuuste_plots_gain[["mmif_cirkel_100m"]]) +
  (lijst_robuuste_plots_gain[["ept_cirkel_100m"]] ) +
  (lijst_robuuste_plots_gain[["tax_cirkel_100m"]] ) +
  (lijst_robuuste_plots_gain[["nst_cirkel_100m"]] ) +
  (lijst_robuuste_plots_gain[["mts_cirkel_100m"]] ) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
print(figuur_gain_1000m)
