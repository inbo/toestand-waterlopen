source(here::here("source", "inladen_packages.R"))

load(here("data", "verwerkt", "mi_data.rdata"))


mi_meetpunten_fabricio <- mi_data %>%
  select(meetplaats, monsternamedatum, waterlichaam, waterloop, owl, statuut, type, waterlichaamcategorie, geometry) %>%
  st_as_sf()

st_write(mi_meetpunten_fabricio, "mi_meetpunten_fabricio.gpkg")

