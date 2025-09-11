# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
source(here::here("source", "inladen_packages.R"))

# Laad de benodigde data
load(here("data", "verwerkt", "mi_data.rdata"))

# Weghalen van vijvers, meren, geïsoleerd water en punten buiten Vlaanderen
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

# Selecteer de RtNt meetpunten die gekoppeld moeten worden
meetpunten_rtnt <- mi_data_analyse %>%
  filter(type == "RtNt") %>%
  select(meetplaats, type, categorie, waterlooptype, vhag) %>%
  unique()
# %>%
#   filter(meetplaats %in% c("OW10005", "OW150500" , "OW115200", "OW196000", "OW695120", "OW897005", "OW190260")) # testje met twee NA en twee handmatig uit rtnt2script

# Laad de morfologielaag en zorg dat de vhag-codes karakters zijn
morfotype <- st_read(dsn = here("data", "ruw", "waterlopen", "morfotype", "ecoltypwatl.shp")) %>%
  st_transform(., crs = st_crs(meetpunten_rtnt)) %>%
  mutate(vhag = as.character(VHAG)) %>%
  select(-VHAG)

#---------------------------------------------------------------------------------------------------
# STAP 1: Koppel op basis van VHAG en dichtstbijzijnde geometrie (primaire, robuuste methode)
#---------------------------------------------------------------------------------------------------

resultaat_vhag <- meetpunten_rtnt %>%
  # Gebruik `nest_by` om een geneste dataset te maken per meetplaats
  nest_by(meetplaats) %>%
  # Itereer over elke geneste dataset
  mutate(gekoppeld = list(
    data %>%
      mutate(
        # Filter de morfologielaag op de vhag van het meetpunt
        vhag_morfotype = list(morfotype %>%
                                filter(vhag == data$vhag)),

        # Zoek het dichtstbijzijnde morfotype binnen deze vhag-subset
        nearest_index = ifelse(nrow(vhag_morfotype[[1]]) > 0,
                               st_nearest_feature(., vhag_morfotype[[1]]),
                               NA_integer_),

        # Voeg de morfonaam van het dichtstbijzijnde morfotype toe
        morfotype = ifelse(!is.na(nearest_index),
                           vhag_morfotype[[1]]$MORFONAAM[nearest_index],
                           NA_character_)
      )
  )) %>%
  # Unnest om terug te keren naar een enkel dataframe
  unnest(gekoppeld) %>%
  ungroup() %>%
  # Selecteer de uiteindelijke kolommen
  select(meetplaats, vhag, morfotype)


#---------------------------------------------------------------------------------------------------
# STAP 2: Vertaal de "mismatch"-logica uit jouw script
#---------------------------------------------------------------------------------------------------

# Identificeer de meetpunten die niet gekoppeld konden worden via vhag
punten_zonder_vhag_match <- resultaat_vhag %>%
  filter(is.na(morfotype)) %>%
  select(meetplaats)

# Voer de handmatige koppelingen in (indien van toepassing); punten die niet op waterloop liggen werden handmatig toegewezen aan type indien mogelijk op basis van omliggende waterlopen
morfotype_handmatig <- read.csv(file = here("data", "ruw", "waterlopen", "morfotype", "handmatige_toewijzing_morfotype.csv"), sep = ";")

# Identificeer de resterende punten die nog steeds zonder match zitten -> vhag van punten matcht niet met vhag waterloop waar ze op liggen
resterende_punten <- punten_zonder_vhag_match %>%
  anti_join(morfotype_handmatig, by = "meetplaats") %>%
  inner_join(meetpunten_rtnt, by = "meetplaats") %>%
  st_as_sf()

# Koppel deze resterende punten aan de geografisch dichtstbijzijnde rivier in de gehele morfologielaag
nearest_river_index_fallback <- st_nearest_feature(resterende_punten, morfotype)

fallback_koppeling <- resterende_punten %>%
  mutate(morfotype = morfotype$MORFONAAM[nearest_river_index_fallback]) %>%
  select(meetplaats, vhag, morfotype) %>%
  st_drop_geometry()

#---------------------------------------------------------------------------------------------------
# STAP 3: Combineer alle resultaten
#---------------------------------------------------------------------------------------------------

# Combineer de resultaten van de vhag-koppeling, handmatige koppeling en de fallback
meetpunten_rtnt_morfotype_definitief <- resultaat_vhag %>%
  st_drop_geometry() %>%
  filter(!is.na(morfotype)) %>%
  bind_rows(morfotype_handmatig) %>%
  bind_rows(fallback_koppeling) %>%
  unique() %>%
  mutate(type_nieuw = case_when(
    morfotype %in% c("kunstmatige waterloop (brak)", "kunstmatige waterloop (zoet)") ~ "P",
    morfotype %in% c("kleine beek", "bronbeken") ~ "Bk",
    morfotype == "kleine beek Kempen" ~ "BkK",
    morfotype == "grote beek" ~ "Bg",
    morfotype == "grote beek Kempen" ~ "BgK",
    morfotype == "bronbeken" ~ "Bk",
    morfotype == "rivier (< 20m)" ~ "Rk",
    morfotype == "rivier (> 20m)" ~ "Rg",
    morfotype == "Maas" ~ "Rzg",
    TRUE ~ NA
  ))

#---------------------------------------------------------------------------------------------------
# STAP 4: bereken nieuwe MMIF
#---------------------------------------------------------------------------------------------------

meetpunten_morfotype_mmif_herberekend <- mi_data_analyse %>%
  right_join(., meetpunten_rtnt_morfotype_definitief %>%
               select(-vhag), by = "meetplaats") %>%
  mutate(tax_nieuw = case_when(
    type_nieuw == "P" ~ case_when(
      ta_xw <= 5 ~ 0,
      ta_xw <= 13 ~ 1,
      ta_xw <= 21 ~ 2,
      ta_xw <= 29 ~ 3,
      ta_xw > 29 ~ 4),
    type_nieuw == "BgK" | type_nieuw == "Bg" ~ case_when(
      ta_xw <= 5 ~ 0,
      ta_xw <= 13.25 ~ 1,
      ta_xw <= 21.5 ~ 2,
      ta_xw <= 29.75 ~ 3,
      ta_xw > 29.75 ~ 4),
    type_nieuw == "BkK" | type_nieuw == "Bk" ~ case_when(
      ta_xw <= 5 ~ 0,
      ta_xw <= 12.25 ~ 1,
      ta_xw <= 19.5 ~ 2,
      ta_xw <= 26.75 ~ 3,
      ta_xw > 26.75 ~ 4),
    type_nieuw == "Rk" ~ case_when(
      ta_xw <= 5 ~ 0,
      ta_xw <= 13.75 ~ 1,
      ta_xw <= 22.5 ~ 2,
      ta_xw <= 31.25 ~ 3,
      ta_xw > 31.25 ~ 4),
    type_nieuw == "Rg" ~ case_when(
      ta_xw <= 5 ~ 0,
      ta_xw <= 14.25 ~ 1,
      ta_xw <= 23.5 ~ 2,
      ta_xw <= 32.25 ~ 3,
      ta_xw > 32.25 ~ 4),
    type_nieuw == "Rzg" ~ case_when(
      ta_xw <= 5 ~ 0,
      ta_xw <= 14.75 ~ 1,
      ta_xw <= 24.5 ~ 2,
      ta_xw <= 34.25 ~ 3,
      ta_xw > 34.25 ~ 4)),
    ept_nieuw = case_when(
      type_nieuw == "P" ~ case_when(
        ep_tw == 0 ~ 0,
        ep_tw <= 2 ~ 1,
        ep_tw <= 4 ~ 2,
        ep_tw <= 6 ~ 3,
        ep_tw > 6 ~ 4),
      type_nieuw %in% c("BgK", "Rk", "Rg") ~ case_when(
        ep_tw == 0 ~ 0,
        ep_tw <= 2.25 ~ 1,
        ep_tw <= 4.5 ~ 2,
        ep_tw <= 6.75 ~ 3,
        ep_tw > 6.75 ~ 4),
      type_nieuw == "Bg" ~ case_when(
        ep_tw == 0 ~ 0,
        ep_tw <= 2 ~ 1,
        ep_tw <= 4 ~ 2,
        ep_tw <= 6 ~ 3,
        ep_tw > 6 ~ 4),
      type_nieuw == "BkK" ~ case_when(
        ep_tw == 0 ~ 0,
        ep_tw <= 2 ~ 1,
        ep_tw <= 4 ~ 2,
        ep_tw <= 6 ~ 3,
        ep_tw > 6 ~ 4),
      type_nieuw == "Bk" ~ case_when(
        ep_tw == 0 ~ 0,
        ep_tw <= 1.75 ~ 1,
        ep_tw <= 3.5 ~ 2,
        ep_tw <= 5.25 ~ 3,
        ep_tw > 5.25 ~ 4),
      type_nieuw == "Rzg" ~ case_when(
        ep_tw == 0 ~ 0,
        ep_tw <= 2.5 ~ 1,
        ep_tw <= 5 ~ 2,
        ep_tw <= 7.5 ~ 3,
        ep_tw > 7.5 ~ 4)),
    nst_nieuw = case_when(
      type_nieuw == "P" ~ case_when(
        ns_tw == 0 ~ 0,
        ns_tw <= 2.5 ~ 1,
        ns_tw <= 5 ~ 2,
        ns_tw <= 7.5 ~ 3,
        ns_tw > 7.5 ~ 4),
      type_nieuw == "BgK" | type_nieuw == "Bg" ~ case_when(
        ns_tw == 0 ~ 0,
        ns_tw <= 2.5 ~ 1,
        ns_tw <= 5 ~ 2,
        ns_tw <= 7.5 ~ 3,
        ns_tw > 7.5 ~ 4),
      type_nieuw == "BkK" | type_nieuw == "Bk" ~ case_when(
        ns_tw == 0 ~ 0,
        ns_tw <= 2.25 ~ 1,
        ns_tw <= 4.5 ~ 2,
        ns_tw <= 6.75 ~ 3,
        ns_tw > 6.75 ~ 4),
      type_nieuw %in% c("Rk", "Rg", "Rzg") ~ case_when(
        ns_tw == 0 ~ 0,
        ns_tw <= 3 ~ 1,
        ns_tw <= 6 ~ 2,
        ns_tw <= 9 ~ 3,
        ns_tw > 9 ~ 4)),
    swd_nieuw = case_when(
      sw_dw <= 0.2 ~ 0,
      sw_dw <= 1.025 ~ 1,
      sw_dw <= 1.85 ~ 2,
      sw_dw <= 2.675 ~ 3,
      sw_dw > 2.675 ~ 4),
    mts_nieuw = case_when(
      type_nieuw == "P" ~ case_when(
        mt_sw <= 2 ~ 0,
        mt_sw <= 3.075 ~ 1,
        mt_sw <= 4.15 ~ 2,
        mt_sw <= 5.225 ~ 3,
        mt_sw > 5.225 ~ 4),
      type_nieuw %in% c("BgK", "Bg", "BkK", "Bk", "Rk", "Rg", "Rzg") ~ case_when(
        mt_sw <= 2 ~ 0,
        mt_sw <= 3.125 ~ 1,
        mt_sw <= 4.25 ~ 2,
        mt_sw <= 5.375 ~ 3,
        mt_sw > 5.375 ~ 4)
    ),
    mmif_nieuw = (tax_nieuw + ept_nieuw + nst_nieuw + swd_nieuw + mts_nieuw)/20) %>%
  select(-tax, -swd, -nst, -mts, -ept, -mmif, -morfotype, -type) %>%
  rename(mmif = mmif_nieuw,
         tax = tax_nieuw,
         ept = ept_nieuw,
         nst = nst_nieuw,
         mts = mts_nieuw,
         swd = swd_nieuw,
         type = type_nieuw)

#---------------------------------------------------------------------------------------------------
# STAP 5: statuut aanpassen waar mogelijk en nieuwe waarden toevoegen aan mi_data
#---------------------------------------------------------------------------------------------------

kunstmatige_vhag <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(statuut == "Kunstmatig") %>%
  select(vhag) %>%
  unique()

mi_data_rtnt <- meetpunten_morfotype_mmif_herberekend %>%
  mutate(statuut_nieuw = case_when(
    waterlooptype == "Kunstmatige waterloop" ~ "Kunstmatig",
    waterlooptype == "Natuurlijke waterloop" ~ "Natuurlijk",
    vhag %in% kunstmatige_vhag$vhag ~ "Kunstmatig",
    TRUE ~ "Natuurlijk"
  )) %>%
  select(-statuut) %>%
  rename(statuut = statuut_nieuw)

# rtnt data toevoegen aan mi_data_analyse
waterlopen_groep <- read.csv(here("data", "ruw", "type_waterlichamen.csv"),
                             sep = ";") %>%
  select(type, groep)

mi_data_analyse_rtnt_update <- mi_data_analyse %>%
  filter(type != "RtNt") %>%
  bind_rows(., mi_data_rtnt) %>%
  select(-groep) %>%
  left_join(waterlopen_groep, by = "type")

save(mi_data_analyse_rtnt_update, file = here("data", "verwerkt", "mi_data_analyse_rtnt_update.rdata"))
