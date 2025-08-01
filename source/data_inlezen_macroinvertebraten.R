source(here::here("source", "inladen_packages.R"))

# data macro-invertebraten inlezen ----
locatie <- here("data", "ruw", "macroinvertebraten", "macroinvertebraten OW tot 2023.xlsx")
sheetnames <- excel_sheets(locatie)
vmm_mi <- lapply(sheetnames[1:5], read_excel,
                 path = locatie)
waterlopen_groep <- read.csv(here("data", "ruw", "type_waterlichamen.csv"),
                             sep = ";") %>%
  select(type, groep)

vmm_mi <- setNames(vmm_mi, sheetnames[1:5] %>% janitor::make_clean_names())
vmm_mi <- lapply(vmm_mi, janitor::clean_names)

# main dataframe aanmaken ----
mi_deelmaatlatten0 <- vmm_mi$bbi_en_mmif_deelmaatlatten %>%
  select(
    deelmonster_id,
    meetplaats,
    jaar,
    datum_monstername,
    bbi,
    mmif,
    ept,
    ep_tw,
    mts,
    mt_sw,
    tax,
    ta_xw,
    swd,
    sw_dw,
    nst,
    ns_tw
  ) %>%
  arrange(meetplaats, jaar) %>%
  mutate(datum_monstername = as.Date(datum_monstername, "%Y-%m-%d")) %>%
  left_join(
    vmm_mi$meetplaatsen %>%
      select(
        meetplaats,
        lambert72_x,
        lambert72_y,
        waterlichaam,
        bekken,
        vhag,
        waterloop,
        categorie,
        statuut,
        type,
        waterlooptype,
        waterlichaamcategorie,
        categorie,
        bekken,
        stroming
      ),
    by = "meetplaats"
  ) %>%
  left_join(
    vmm_mi$veldmetingen %>%
      mutate(
        monsternamedatum =  as.Date(monsternamedatum, format = "%Y-%m-%d"),
        deelmonster_id = as.character(deelmonster_id)
      ) %>%
      select(-teken, -eenheid, -meetnet) %>%
      filter(parameter %in% c("T", "pH", "O2", "O2 verz", "EC 20", "EC 25", "Secchi")) %>%
      pivot_wider(names_from = parameter, values_from = resultaat) %>%
      rename(datum_monstername = monsternamedatum),
    by = c("meetplaats", "datum_monstername", "deelmonster_id")
  ) %>%
  st_as_sf(coords = c("lambert72_x", "lambert72_y"),
           crs = 31370) %>%
  mutate(jaar = parse_number(jaar),
         bbi = parse_number(bbi),
         mmif = parse_number(mmif),
         meetplaats = paste0("OW", meetplaats)) %>%
  rename(monsternamedatum = datum_monstername)

# toevoegen groep (rivier, beek, kempen, polder)
mi_data0 <- mi_deelmaatlatten0 %>%
  # filter(waterlooptype != "Geïsoleerd water" &
  #          !(waterlichaamcategorie %in% c("meer", "overgangswater"))) %>%
  left_join(waterlopen_groep, by = "type")
mi_data <- janitor::clean_names(mi_data0)
  save(mi_data, file = here("data", "verwerkt", "mi_data.rdata"))

mi_meetpunten <- mi_data %>%
  select(meetplaats, monsternamedatum, geometry)
st_write(mi_meetpunten, dsn = here("data", "ruw", "macroinvertebraten", "mi_meetpunten.gpkg"), delete_dsn = T)


# soortendata ----
mi_soorten0 <- vmm_mi$bemonsteringen %>%
    # mutate(datum_monstername = as.Date(datum_monstername, "%Y-%m-%d")) %>%
    arrange(meetplaats, datum_monstername) %>%
    left_join(
      vmm_mi$meetplaatsen %>%
        select(
          meetplaats,
          lambert72_x,
          lambert72_y,
          waterlichaam,
          bekken,
          vhag,
          waterloop,
          categorie,
          statuut,
          type,
          waterlooptype,
          waterlichaamcategorie,
          categorie,
          bekken,
          stroming
        ),
      by = "meetplaats"
    )
mi_soorten <- mi_soorten0 %>%
    # filter(waterlooptype != "Geïsoleerd water" &
    #          !(waterlichaamcategorie %in% c("meer", "overgangswater"))) %>%
  mutate(meetplaats = paste0("OW", deelmonster_id)) %>%
    left_join(waterlopen_groep, by = "type") %>%
  mutate(datum_monstername = as.Date(datum_monstername, "%Y-%m-%d")) %>%
  rename(monsternamedatum = datum_monstername)
save(mi_soorten, file = here("data", "verwerkt", "mi_soorten.rdata"))
