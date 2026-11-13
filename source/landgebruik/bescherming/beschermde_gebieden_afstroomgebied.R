# Installeer deze pakketten als je ze nog niet hebt:
# install.packages("terra")
# install.packages("exactextractr")

source(here::here("source", "inladen_packages.R"))
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(terra)
library(exactextractr)
load(here("data", "verwerkt", "mi_data.rdata"))
load(here("data", "verwerkt", "mafy_data.rdata"))

# ==============================================================================
# 0. MAPPEN AANMAKEN VOOR TUSSENSTAPPEN
# ==============================================================================
# Maak de mappen aan als ze nog niet bestaan
map_rasters <- here("data", "verwerkt", "landgebruik", "pa_rasters_tussenstappen")
map_tabellen <- here("data", "verwerkt", "landgebruik", "pa_tabellen_tussenstappen")

dir.create(map_rasters, recursive = TRUE, showWarnings = FALSE)
dir.create(map_tabellen, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. DATA INLADEN
# ==============================================================================
watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_buffered_5000m.gpkg"))

pa_map <- "C:/Users/emiel_delombaerde/Documents/GitHub/ProtConn/data/ccda_nat2000"
shape_bestanden <- list.files(path = pa_map, pattern = "\\.shp$")
shape_bestanden <- shape_bestanden[!stringr::str_detect(shape_bestanden, "2024")] # geen 2024
shape_bestanden <- sort(shape_bestanden, decreasing = TRUE) # beginnen met recentste

template_raster <- rast(here("data", "ruw", "landgebruik", "niveau1_vla_2013_v3.tif"))
watersheds_buffered <- st_transform(watersheds_buffered, st_crs(template_raster))

# ==============================================================================
# 2. DE SUPERSNELLE RASTER-FUNCTIE (MET CACHING)
# ==============================================================================
haal_jaar_op <- function(bestandsnaam) {
  # (?=\\.shp) zorgt ervoor dat hij alleen de 4 cijfers pakt die direct voor .shp staan
  stringr::str_extract(bestandsnaam, "[0-9]{4}(?=\\.shp)")
}

bereken_pa_per_jaar_raster <- function(bestandsnaam) {

  jaar <- haal_jaar_op(bestandsnaam)

  # Definieer de bestandsnamen voor de tussenstappen
  pad_naar_tabel <- file.path(map_tabellen, paste0("pa_fractie_", jaar, ".rds"))
  pad_naar_raster <- file.path(map_rasters, paste0("pa_raster_", jaar, ".tif"))

  # ==========================================================
  # CHECK: Is dit jaar al berekend? Dan direct inladen!
  # ==========================================================
  if (file.exists(pad_naar_tabel)) {
    cat("✔ Jaar", jaar, "werd al eerder berekend. Tussenstap ingeladen.\n")
    return(readRDS(pad_naar_tabel))
  }

  # ==========================================================
  # ALS HET JAAR NOG NIET BESTAAT -> BEREKENEN
  # ==========================================================
  cat("▶ Bezig met jaar:", jaar, "...\n")
  pad_naar_shape <- file.path(pa_map, bestandsnaam)

  pa_vect <- vect(pad_naar_shape)

  if (crs(pa_vect) != crs(template_raster)) {
    pa_vect <- project(pa_vect, crs(template_raster))
  }

  # Rasterize en sla meteen het raster op schijf op (geheugenefficiënt!)
  pa_rast <- rasterize(pa_vect, template_raster, field = 1, background = 0,
                       filename = pad_naar_raster, overwrite = TRUE)

  # Bereken de overlap
  pa_fractie <- exact_extract(pa_rast, watersheds_buffered, 'mean', progress = FALSE)

  # Resultaat formatteren
  resultaat <- tibble(
    meetplaats = watersheds_buffered$meetplaats,
    !!sym(paste0("beschermd_pct_", jaar)) := pa_fractie * 100
  )

  # Sla de opgemaakte tabel op voor de volgende keer
  saveRDS(resultaat, pad_naar_tabel)
  cat("  -> Opgeslagen in tussenstappen map.\n")

  return(resultaat)
}

# ==============================================================================
# 3. UITVOEREN EN SAMENVOEGEN
# ==============================================================================
resultaten_pa_lijst <- map(shape_bestanden, bereken_pa_per_jaar_raster)

finale_pa_data <- resultaten_pa_lijst %>%
  reduce(left_join, by = "meetplaats")

# ==============================================================================
# 4. KOPPELEN AAN BIOLOGISCHE DATA VIA HET MEETJAAR
# ==============================================================================
pa_long <- finale_pa_data %>%
  pivot_longer(
    cols = -meetplaats,
    names_to = "pa_jaar",
    names_prefix = "beschermd_pct_",
    values_to = "beschermd_pct"
  ) %>%
  mutate(pa_jaar = as.numeric(pa_jaar))

pa_afstroomgebied_tijdreeks <- mi_data %>%
  select(meetplaats, monsternamedatum) %>%
  st_drop_geometry() %>%
  bind_rows(mafy_data %>% select(meetplaats, monsternamedatum) %>% st_drop_geometry()) %>%

  mutate(meet_jaar = lubridate::year(monsternamedatum)) %>%
  left_join(pa_long, by = c("meetplaats", "meet_jaar" = "pa_jaar")) %>%
  mutate(beschermd_pct = replace_na(beschermd_pct, 0))

# ==============================================================================
# 5. OPSLAAN
# ==============================================================================
save(pa_afstroomgebied_tijdreeks, file = here("data", "verwerkt", "landgebruik", "pa_afstroomgebied_jaren.rdata"))
print("Ruimtelijke intersectie voltooid en succesvol gekoppeld op exact meetjaar!")
