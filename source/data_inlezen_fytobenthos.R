source(here::here("source", "inladen_packages.R"))

# data macro-invertebraten inlezen ----
locatie <- here("data", "ruw", "fytobenthos", "fytobenthos 2010-2023.xlsx")
sheetnames <- excel_sheets(locatie)
vmm_fybe <- lapply(sheetnames[1:5], read_excel,
                   path = locatie)
waterlopen_groep <- read.csv(here("data", "ruw", "type_waterlichamen.csv"),
                             sep = ";") %>%
  select(type, groep)

vmm_fybe <- setNames(vmm_fybe, sheetnames[1:5] %>% janitor::make_clean_names())
vmm_fybe <- lapply(vmm_fybe, janitor::clean_names)

# main dataframe aanmaken ----
fybe_deelmaatlatten0 <- vmm_fybe$indexen %>%
  arrange(meetplaats) %>%
  mutate(monsternamedatum = as.Date(monsternamedatum, "%Y-%m-%d"),
         preparaat = as.character(preparaat)) %>%
  left_join(
    vmm_fybe$meetplaatsen %>%
      select(-owl,
             -waterlichaam
      ),
    by = "meetplaats"
  ) %>%
  st_as_sf(coords = c("x", "y"),
           crs = 31370)

# toevoegen groep (rivier, beek, kempen, polder)
fybe_data0 <- fybe_deelmaatlatten0 %>%
  # filter(waterlooptype != "Geïsoleerd water" &
  #          !(waterlichaamcategorie %in% c("meer", "overgangswater"))) %>%
  left_join(waterlopen_groep, by = "type")
fybe_data <- janitor::clean_names(fybe_data0)
save(fybe_data, file = here("data", "verwerkt", "fybe_data.rdata"))

fybe_meetpunten_datum <- fybe_data %>%
  select(meetplaats, monsternamedatum, geometry) %>%
  unique
st_write(fybe_meetpunten_datum, dsn = here("data", "ruw", "macrofyten", "fybe_meetpunten_datum.gpkg"), delete_dsn = T)

fybe_meetpunten <- fybe_data %>%
  select(meetplaats, geometry) %>%
  unique
st_write(fybe_meetpunten, dsn = here("data", "ruw", "macrofyten", "fybe_meetpunten.gpkg"), delete_dsn = T)
