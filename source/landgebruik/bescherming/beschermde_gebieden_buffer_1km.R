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
# 0. MAPPEN INSTELLEN (GEBRUIK BESTAANDE RASTERS)
# ==============================================================================
# Verwijs naar de map waar je .tif bestanden al staan!
map_rasters <- here("data", "verwerkt", "landgebruik", "pa_rasters_tussenstappen")

# Maak een NIEUWE map voor de tabellen, zodat we de watershed-data niet overschrijven
map_tabellen_1km <- here("data", "verwerkt", "landgebruik", "pa_tabellen_1km_tussenstappen")
dir.create(map_tabellen_1km, recursive = TRUE, showWarnings = FALSE)

# Lijst met alle reeds berekende rasters (.tif)
raster_bestanden <- list.files(path = map_rasters, pattern = "\\.tif$", full.names = TRUE)

# ==============================================================================
# 1. DATA INLADEN EN 1KM BUFFERS MAKEN
# ==============================================================================
# Haal de unieke ruimtelijke meetplaatsen op
meetpunten_sf <- bind_rows(
  mi_data %>% select(meetplaats, geometry),
  mafy_data %>% select(meetplaats, geometry)
) %>%
  distinct(meetplaats, .keep_all = TRUE)

# Maak een cirkel van 1km rond elk meetpunt
punten_1km <- st_buffer(meetpunten_sf, dist = 1000)

# Laad 1 raster in om de projectie te checken
template_raster <- rast(raster_bestanden[1])
punten_1km <- st_transform(punten_1km, st_crs(template_raster))

# ==============================================================================
# 2. DE SUPERSNELLE OVERLAY-FUNCTIE (ZONDER RASTERIZE)
# ==============================================================================
bereken_pa_1km <- function(raster_pad) {

  # Haal het jaar uit de bestandsnaam (bijv. "pa_raster_2005.tif" -> "2005")
  jaar <- stringr::str_extract(basename(raster_pad), "[0-9]{4}")

  pad_naar_tabel <- file.path(map_tabellen_1km, paste0("pa_fractie_1km_", jaar, ".rds"))

  # CHECK: Is dit jaar al berekend voor 1km?
  if (file.exists(pad_naar_tabel)) {
    cat("✔ Jaar", jaar, "werd al berekend. Tussenstap ingeladen.\n")
    return(readRDS(pad_naar_tabel))
  }

  cat("▶ Bezig met extraheren 1km buffer voor jaar:", jaar, "...\n")

  # Laad het bestaande raster in het geheugen
  pa_rast <- rast(raster_pad)

  # Bereken de overlap voor de 1km buffers
  pa_fractie <- exact_extract(pa_rast, punten_1km, 'mean', progress = FALSE)

  # Resultaat formatteren
  resultaat <- tibble(
    meetplaats = punten_1km$meetplaats,
    !!sym(paste0("beschermd_pct_", jaar)) := pa_fractie * 100
  )

  # Sla op in de nieuwe 1km map
  saveRDS(resultaat, pad_naar_tabel)
  cat("  -> Opgeslagen in tussenstappen map (1km).\n")

  return(resultaat)
}

# ==============================================================================
# 3. UITVOEREN EN SAMENVOEGEN
# ==============================================================================
resultaten_pa_lijst <- map(raster_bestanden, bereken_pa_1km)

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

pa_1km_tijdreeks <- mi_data %>%
  select(meetplaats, monsternamedatum) %>%
  st_drop_geometry() %>%
  bind_rows(mafy_data %>% select(meetplaats, monsternamedatum) %>% st_drop_geometry()) %>%

  mutate(meet_jaar = lubridate::year(monsternamedatum)) %>%
  left_join(pa_long, by = c("meetplaats", "meet_jaar" = "pa_jaar")) %>%
  mutate(beschermd_pct = replace_na(beschermd_pct, 0))

# ==============================================================================
# 5. OPSLAAN
# ==============================================================================
save(pa_1km_tijdreeks, file = here("data", "verwerkt", "landgebruik", "pa_1km_buffer_jaren.rdata"))
print("Ruimtelijke intersectie voor 1km voltooid en succesvol gekoppeld op exact meetjaar!")
