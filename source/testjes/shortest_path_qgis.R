library(here)
library()
qgis_show_help("native:shortestpathlayertopoint")

rivers <- st_read(here("data", "ruw", "waterlopen", "wlas_network.shp"))
mafy_snapped <- st_read(here("data", "verwerkt", "hydrologisch", "mafy_snapped.gpkg"))


qgis_run_algorithm(
  "native:shortestpathlayertopoint",
  INPUT = st_geometry(rivers),
  START_POINTS = mafy_snapped %>% filter(meetplaats == "TR860000.5") %>% st_geometry(),
  END_POINT = mafy_snapped %>% filter(meetplaats == "TR862450.1") %>% st_geometry(),
  OUTPUT = "test.shp")

mafy_snapped %>% filter(meetplaats == "TR860000.5") %>% select(geom)
mafy_snapped %>% filter(meetplaats == "TR862450.1")
TR862450.1
