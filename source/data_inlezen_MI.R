# packages inlezen ----
library(tidyverse)
library(openxlsx2)
library(readxl)
library(inbodb)
library(here)
library(sf)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("read_xlsx", "readxl")
conflicted::conflicts_prefer(dplyr::filter)

# data macro-invertebraten inlezen ----
sheetnames <- excel_sheets(here("data", "macroinvertebraten 2010-2023.xlsx"))
vmm_mi <- lapply(sheetnames[1:5], read_excel,
                 path = here("data", "macroinvertebraten 2010-2023.xlsx"))

vmm_mi <- setNames(vmm_mi, sheetnames[1:5] %>% janitor::make_clean_names())
vmm_mi <- lapply(vmm_mi, janitor::clean_names)

# main dataframe aanmaken ----
mi_deelmaatlatten <- vmm_mi$bbi_en_mmif_deelmaatlatten %>%
  select(deelmonster_id, meetplaats, jaar, datum_monstername, bbi, mmif, ept, ep_tw, mts,
         mt_sw, tax, ta_xw, swd, sw_dw, nst, ns_tw) %>%
  arrange(meetplaats, jaar) %>%
  mutate(datum_monstername = as.Date(datum_monstername, "%Y-%m-%d")) %>%
  left_join(
    vmm_mi$meetplaatsen %>%
      select(meetplaats, lambert72_x, lambert72_y, waterlichaam, bekken,
             vhag, waterloop, categorie, statuut, type, waterlooptype,
             waterlichaamcategorie),
    by = "meetplaats") %>%
  left_join(vmm_mi$veldmetingen %>%
              mutate(monsternamedatum =  as.Date(monsternamedatum, format = "%Y-%m-%d")) %>%
              select(-teken, -eenheid, -meetnet) %>%
              filter(parameter %in% c("T", "pH", "O2", "O2 verz")) %>%
              pivot_wider(names_from = parameter, values_from = resultaat) %>%
              rename(datum_monstername = monsternamedatum),
            by = c("meetplaats", "datum_monstername", "deelmonster_id"))

  st_as_sf(coords = c("lambert72_x", "lambert72_y"), crs = 31370) %>%
  mutate(jaar = parse_number(jaar),
         bbi = parse_number(bbi),
         mmif = parse_number(mmif))

vmm_mi$veldmetingen



mi_deelmaatlatten %>%
  filter(jaar >= 2019) %>%
  select(meetplaats) %>%
  unique() %>%
  plot()


