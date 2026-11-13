# Installeer pakketten indien nodig
# install.packages(c("sf", "dplyr", "igraph", "mapview"))

library(sf)
library(dplyr)
library(igraph)

# 1. Laad je shapefile in (vervang door jouw pad)
rivieren <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"))

# Zorg ervoor dat lege WTRLICHC-codes (NA) eruit gefilterd zijn
rivieren_clean <- rivieren %>% filter(!is.na(WTRLICHC))

# 2. Functie om het aantal netwerk-componenten per WTRLICHC te berekenen
bereken_componenten <- function(sf_data) {
  # Vind welke lijnstukken elkaar snijden of raken
  adj_list <- st_intersects(sf_data, sparse = TRUE)

  # Maak er een netwerk-graaf van
  g <- graph_from_adj_list(adj_list, mode = "all")

  # Tel het aantal losse componenten (1 = alles verbonden, >1 = onderbrekingen)
  aantal_comp <- components(g)$no
  return(aantal_comp)
}

# 3. Pas de functie toe per WTRLICHC
# We splitsen de dataset per WTRLICHC, berekenen de componenten en voegen alles samen
WTRLICHC_analyse <- rivieren_clean %>%
  split(.$WTRLICHC) %>%
  lapply(function(x) {
    data.frame(
      WTRLICHC = unique(x$WTRLICHC),
      aantal_segmenten = nrow(x),
      aantal_componenten = bereken_componenten(x)
    )
  }) %>%
  bind_rows()

# 4. Bekijk de probleemgevallen (WTRLICHC's met meer dan 1 component)
probleem_WTRLICHCs <- WTRLICHC_analyse %>%
  filter(aantal_componenten > 1) %>%
  arrange(desc(aantal_componenten))

print(probleem_WTRLICHCs)

library(mapview)

# Kies één van de probleem-WTRLICHC's uit de vorige stap
test_WTRLICHC_code <- probleem_WTRLICHCs$WTRLICHC[20]

# Filter de originele spatiale data op deze ene WTRLICHC
test_data <- rivieren_clean %>% filter(WTRLICHC == test_WTRLICHC_code)

# Bereken voor visualisatie nog even tot welke "component" elk segment behoort
# zodat we elk eilandje een andere kleur kunnen geven
adj_list <- st_intersects(test_data, sparse = TRUE)
g <- graph_from_adj_list(adj_list, mode = "all")
test_data$component_id <- as.factor(components(g)$membership)

# Visualiseer interactief!
# Elk losgekoppeld deel krijgt nu een andere kleur, labels tonen de vhag code.
mapview(test_data,
        zcol = "component_id",
        label = "vhag",
        lwd = 3,
        layer.name = paste("Componenten van WTRLICHC:", test_WTRLICHC_code))
