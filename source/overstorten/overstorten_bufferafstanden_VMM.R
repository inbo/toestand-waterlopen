library(mapview)
library(sf)
library(tidyverse)
library(purrr)
library(glue)
library(here)
library(glmmTMB)
library(sjPlot)
library(DHARMa)

load(here("data", "verwerkt", "mi_data.rdata"))

locations <- read_sf(here("data", "verwerkt", "hydrologisch",
                          "mi_meetpunten_snapped_to_streams.shp"))

# inlezen afstroomgebieden met verschillende buffers
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)

bestanden <- paste0("mi_meetpunten_watersheds_buffered_", buffer_afstanden, "m.gpkg")
object_namen <- paste0("afstroomgebied_buffered_", buffer_afstanden, "m")

walk2(bestanden, object_namen, ~ {
  pad <- here("data", "verwerkt", "hydrologisch", .x)
  assign(.y, st_read(pad, quiet = TRUE), envir = .GlobalEnv)
})

# inlezen overstorten
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp")) # overstorten die uitlaten op waterloop
overstorten_uitlaat_vha_prio_score <- st_read(here("data", "verwerkt", "overstorten", "overstorten_uitlaat_vha_prio_score.gpkg"))

# selecteren meetplaatsen uit locations waar binnen het afstroomgebied van 100m minstens 1 overstort aanwezig is

# Controleer dat CRS hetzelfde is
overstorten_uitlaat_vha <- st_transform(overstorten_uitlaat_vha, st_crs(afstroomgebied_buffered_100m))

# Zoek intersecties tussen afstroomgebieden (100 m) en overstorten
intersecties <- st_intersects(afstroomgebied_buffered_100m, overstorten_uitlaat_vha)

# Voeg een kolom toe: heeft overstort (ja/nee)
afstroomgebied_buffered_100m <- afstroomgebied_buffered_100m %>%
  mutate(heeft_overstort = lengths(intersecties) > 0)

# Filter enkel meetplaatsen waar minstens één overstort voorkomt
locations_met_overstort <- locations %>%
  filter(meetplaats %in%
           afstroomgebied_buffered_100m$meetplaats[afstroomgebied_buffered_100m$heeft_overstort])

# Controle
nrow(locations_met_overstort)

#visualisatie

mapview(afstroomgebied_buffered_100m, zcol = "heeft_overstort") +
  # mapview(overstorten_uitlaat_vha, cex = 2, col.region = "red") +
  mapview(locations_met_overstort, cex = 3, col.region = "green")

######
# Aantal overstorten per meetplaats en bufferafstanden
#########

# Zorg dat alle lagen hetzelfde CRS hebben
overstorten_uitlaat_vha <- st_transform(overstorten_uitlaat_vha, st_crs(afstroomgebied_buffered_100m))

# Definieer bufferafstanden en bijhorende objectnamen
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)
buffer_obj_namen <- paste0("afstroomgebied_buffered_", buffer_afstanden, "m")

# Helperfunctie die overstorten koppelt aan afstroomgebieden en meetplaatsen selecteert
bepaal_meetplaatsen_met_overstort <- function(buffer_obj, afstand) {

  afstroom <- get(buffer_obj, envir = .GlobalEnv)

  intersecties <- st_intersects(afstroom, overstorten_uitlaat_vha)

  afstroom <- afstroom %>%
    mutate(heeft_overstort = lengths(intersecties) > 0)

  # Selecteer de corresponderende meetplaatsen
  locations_met_overstort <- locations %>%
    filter(meetplaats %in%
             afstroom$meetplaats[afstroom$heeft_overstort]) %>%
    mutate(buffer_m = afstand)

  # Resultaat teruggeven als lijst
  list(
    afstand = afstand,
    afstroom = afstroom,
    locations_met_overstort = locations_met_overstort,
    aantal_meetplaatsen = nrow(locations_met_overstort)
  )
}

# Pas de functie toe op alle bufferafstanden
resultaten_buffers <- map2(buffer_obj_namen, buffer_afstanden, bepaal_meetplaatsen_met_overstort)

# =====================================================================
# Samenvattende tabel: aantal meetplaatsen met overstort per bufferafstand
# =====================================================================
samenvatting <- map_dfr(resultaten_buffers, ~ tibble(
  buffer_m = .x$afstand,
  n_meetplaatsen_met_overstort = .x$aantal_meetplaatsen
))

print(samenvatting)

# =====================================================================
# Dataframe: per meetplaats aantal overstort per bufferafstand
# =====================================================================

# Zorg dat overstorten in hetzelfde CRS staan
overstorten_uitlaat_vha__score <- st_transform(overstorten_uitlaat_vha__score, st_crs(afstroomgebied_buffered_100m))

# Definieer buffers en bijhorende objectnamen
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)
buffer_obj_namen <- paste0("afstroomgebied_buffered_", buffer_afstanden, "m")

# Helperfunctie: tel aantal overstorten per meetplaats voor een bepaalde bufferafstand
tel_overstorten_per_buffer <- function(buffer_obj, afstand) {

  # Haal de bufferlaag op uit de environment
  afstroom <- get(buffer_obj, envir = .GlobalEnv)

  # Voer de ruimtelijke kruising uit
  # Dit geeft een lijst terug. Element 1 bevat de rij-nummers van overstorten in buffer 1, etc.
  intersecties <- st_intersects(afstroom, overstorten_uitlaat_vha__score)

  # Berekening 1: Aantal (Telling)
  aantallen <- lengths(intersecties)

  # Berekening 2: Score (Som van Blootstellingsfactor)
  # We loopen door de lijst met intersecties en sommeren de factor voor de gevonden indices
  scores <- map_dbl(intersecties, function(indices) {
    if (length(indices) == 0) {
      return(0)
    } else {
      # Haal de specifieke rijen op en sommeer de kolom
      return(sum(overstorten_uitlaat_vha__score$overstort_risk_score[indices], na.rm = TRUE))
    }
  })

  # Voeg de kolommen toe aan het dataframe
  afstroom %>%
    st_drop_geometry() %>%
    mutate(
      !!glue("aantal_overstorten_{afstand}m") := aantallen,
      !!glue("score_overstorten_{afstand}m") := scores
    ) %>%
    select(meetplaats, starts_with("aantal_"), starts_with("score_"))
}

# Bereken aantallen voor alle bufferafstanden
overstort_tellingen <- map2(buffer_obj_namen, buffer_afstanden, tel_overstorten_per_buffer)

# Combineer alle resultaten per meetplaats
overstort_tellingen_df <- reduce(overstort_tellingen, full_join, by = "meetplaats")

# Voeg eventueel info over locaties toe (optioneel)
overstort_tellingen_df <- locations %>%
  st_drop_geometry() %>%
  select(meetplaats) %>%
  left_join(overstort_tellingen_df, by = "meetplaats")

# Resultaat bekijken
print(overstort_tellingen_df)

save(overstort_tellingen_df, file = here("data", "verwerkt", "overstorten", "overstort_tellingen_df.rdata"))
load(file = here("data", "verwerkt", "overstorten", "overstort_tellingen_df.rdata"))


# =====================================================================
# Visualisatie
# =====================================================================

# Zet brede tabel om naar lange vorm
overstort_tellingen_long <- overstort_tellingen_df %>%
  pivot_longer(
    cols = starts_with("aantal_overstorten_"),
    names_to = "buffer_m",
    names_prefix = "aantal_overstorten_",
    names_transform = list(buffer_m = readr::parse_number),
    values_to = "aantal_overstorten"
  )

# Gemiddeld aantal overstorten per bufferafstand
overstort_samenvatting <- overstort_tellingen_long %>%
  group_by(buffer_m) %>%
  summarise(gemiddeld_aantal = mean(aantal_overstorten, na.rm = TRUE))

# Plots

ggplot(samenvatting, aes(x = buffer_m, y = n_meetplaatsen_met_overstort)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Aantal meetplaatsen met een overstort per bufferafstand",
    x = "Bufferafstand (m)",
    y = "Aantal meetplaatsen met minstens één overstort"
  )

ggplot(overstort_samenvatting, aes(x = buffer_m, y = gemiddeld_aantal)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Gemiddeld aantal overstorten per bufferafstand",
    x = "Bufferafstand (m)",
    y = "Gemiddeld aantal overstorten per meetplaats"
  )

ggplot(overstort_tellingen_long, aes(x = buffer_m, y = aantal_overstorten, group = meetplaats)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = 1), color = "red", se = FALSE, linewidth = 1.2) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Toename van aantal overstorten per meetplaats met bufferafstand",
    x = "Bufferafstand (m)",
    y = "Aantal overstorten"
  )
