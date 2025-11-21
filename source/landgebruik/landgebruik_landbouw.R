library(sf)
library(tidyverse)
library(mapview)
library(here)

#inlezen data
afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_5000m.gpkg"))

lbg_24 <- st_read(here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", "Lbgbrprc24.shp"))

