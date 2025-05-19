source(here::here("source", "inladen_packages.R"))

# data macro-invertebraten inlezen ----
locatie <- here("data", "ruw", "macrofyten", "macrofyten tot 2023.xlsx")
sheetnames <- excel_sheets(locatie)
vmm_mafy <- lapply(sheetnames[1:7], read_excel,
                 path = locatie)
waterlopen_groep <- read.csv(here("data", "ruw", "type_waterlichamen.csv"),
                             sep = ";") %>%
  select(type, groep)

vmm_mafy <- setNames(vmm_mafy, sheetnames[1:7] %>% janitor::make_clean_names())
vmm_mafy <- lapply(vmm_mafy, janitor::clean_names)

# main dataframe aanmaken ----
mafy_deelmaatlatten0 <- vmm_mafy$ekc_maf %>%
  select(
    -deelmonster_geen_organisme,
    -bevestigingscode_mf
  ) %>%
  arrange(meetplaats, jaar) %>%
  mutate(monsternamedatum = as.Date(monsternamedatum, "%Y-%m-%d"),
         deelmonster_id = as.character(deelmonster_id)) %>%
  left_join(
    vmm_mafy$meetplaatsen %>%
      select(-owl,
             -waterlichaam
             ),
    by = "meetplaats"
  ) %>%
  left_join(
    vmm_mafy$veldmetingen_fc %>%
      mutate(
        monsternamedatum =  as.Date(monsternamedatum, format = "%Y-%m-%d"),
        deelmonster_id = as.character(deelmonster_id_mow)
      ) %>%
      select(-teken, -eenh, - secchischijf_op_bodem) %>%
      filter(par %in% c("T", "pH", "O2", "O2 verz", "EC 20", "Secchi")) %>%
      pivot_wider(names_from = par, values_from = resultaat),
    by = c("meetplaats", "monsternamedatum", "deelmonster_id")
  ) %>%
  left_join(
    vmm_mafy$deelmaatlatten %>%
      mutate(
        deelmonster_id = as.character(deelmonster_id),
        monsternamedatum =  as.Date(monsternamedatum, format = "%Y-%m-%d")) %>%
      select(-owl, -status, -sterk_veranderd)
  ) %>%
  st_as_sf(coords = c("x", "y"),
           crs = 31370)

# toevoegen groep (rivier, beek, kempen, polder)
mafy_data0 <- mafy_deelmaatlatten0 %>%
  # filter(waterlooptype != "Geïsoleerd water" &
  #          !(waterlichaamcategorie %in% c("meer", "overgangswater"))) %>%
  left_join(waterlopen_groep, by = "type")
mafy_data <- janitor::clean_names(mafy_data0)
save(mafy_data, file = here("data", "verwerkt", "mafy_data.rdata"))

mafy_meetpunten <- mafy_data %>%
  select(meetplaats, monsternamedatum, geometry) %>%
  unique
st_write(mafy_meetpunten, dsn = here("data", "ruw", "macrofyten", "mafy_meetpunten.gpkg"), delete_dsn = T)
