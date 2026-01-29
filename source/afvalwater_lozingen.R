source(here::here("source", "inladen_packages.R"))

meetpunten_lozingen <- st_read(here("data", "ruw", "lozingen", "afvalwater", "Lozmtput.shp"))

meetpunten_lozingen %>% str

say(what = "Code klaar met lopen'.", by = "squirrel", type = "warning")


verdachte_lozingen <- st_read("data/ruw/lozingen/Verdachte uitlaten.shp")

mapview(verdachte_lozingen) + mapview(fd)
