library(tidyverse)
library(here)
library(sf)
library(mapview)

load(here("data", "verwerkt", "mi_data.rdata"))

mi <- mi_data %>%
  st_as_sf() %>%
  filter(jaar > 2006) %>%
  select(meetplaats, waterlichaam, owl) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)) %>%
  unique() %>%
  group_by(waterlichaam) %>%
  summarise(aantal_meeplaatsen = n())


mapview(mi, cex = 1)

mi_data %>%
  st_as_sf() %>%
  filter(jaar > 2006) %>%
  select(meetplaats, waterlichaam, owl, jaar) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)) %>%
  unique() %>%
  group_by(waterlichaam, orde) %>%
  summarise(
    aantal_meetplaatsen = n_distinct(meetplaats),
    jaren = paste(sort(unique(jaar)), collapse = ", ")
  ) %>%
  st_drop_geometry() %>%
  write.table(file = "meetpunten_krw_mi_per_orde.csv", sep = ";", row.names = FALSE)



load(here("data", "verwerkt", "mafy_data.rdata"))
mafy_data %>%
  st_as_sf() %>%
  filter(jaar > 2006) %>%
  select(meetplaats, waterlichaam, owl, jaar) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)) %>%
  unique() %>%
  group_by(waterlichaam, orde) %>%
  summarise(
    aantal_meetplaatsen = n_distinct(meetplaats),
    jaren = paste(sort(unique(jaar)), collapse = ", ")
  ) %>%
  st_drop_geometry() %>%
  write.table(file = "meetpunten_krw_mafy_per_orde.csv", sep = ";", row.names = FALSE)
