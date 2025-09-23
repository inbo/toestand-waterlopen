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

mi_meetpunten <- mi_data_analyse %>%
  select(meetplaats, vhag, owl) %>%
  unique()

# Laad de nieuwe hydromorfologielagen (trajecten + waterloopniveau)
hydromorf_nieuw_traject <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail_afgerond.shp"))

hydromorf_nieuw_waterloop <- st_read(here("data", "ruw", "hydromorfologie", "hymo_wlniveau.shp"))

# Laad de stroomsnelheid en diepte data
stroomsnelheid_breedte_diepte <- read_excel(here("data", "ruw",
                                                 "hydromorfologie", "stroomsnelheid_traject.xlsx")) %>%
  select(traj_code, avg_depth, width_used, stroomsnelheid_kmu)

# Laad de ruwe data voor aanvullende variabelen
hydromorf_nieuw_ruw <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_vmm_29jan2025_ruw.shp")) %>%
  select(traj_code, rec_width, bd_depth)

#---------------------------------------------------------------------------------------------------
# STAP 1: Combinatie alle hydromorfologische data in één dataset + datavoorbereiding
#---------------------------------------------------------------------------------------------------

hydmo_variabelen <- hydromorf_nieuw_traject %>%
  full_join(stroomsnelheid_breedte_diepte, by = "traj_code") %>%
  full_join(st_drop_geometry(hydromorf_nieuw_ruw), by = "traj_code") %>%
  mutate(vhag = as.character(vhag))

meetpunten_prep <- mi_meetpunten %>%
  mutate(vhag = as.character(vhag))

#---------------------------------------------------------------------------------------------------
# Stap 2: Koppeling meetpunten aan alle mogelijke trajecten via vhag
#---------------------------------------------------------------------------------------------------

meetpunten_met_trajecten <- meetpunten_prep %>%
  st_drop_geometry() %>%
  left_join(hydmo_variabelen %>% select(vhag, traj_code) %>%
              st_drop_geometry() %>%
              mutate(vhag = as.character(vhag)), by = "vhag")

#---------------------------------------------------------------------------------------------------
# Stap 3: Vind het dichtstbijzijnde traject per meetpunt binnen dezelfde vhag
# Belangrijk!
# De code groepeert de data per meetpunt en selecteert het dichtstbijzijnde traject.
#---------------------------------------------------------------------------------------------------

hydromorf_traject_vhag <- meetpunten_prep %>%
  # Gebruik `nest_by` om een geneste dataset te maken per meetplaats
  nest_by(meetplaats) %>%
  # Itereren over elke geneste dataset
  mutate(gekoppeld = list(
    data %>%
      mutate(
        # Filter de hydmolaag op de vhag van het meetpunt
        vhag_hydmo = list(hydmo_variabelen %>%
                                filter(vhag == data$vhag)),
        # Zoek het dichtstbijzijnde morfotype binnen deze vhag-subset
        nearest_index = ifelse(nrow(vhag_hydmo[[1]]) > 0,
                               st_nearest_feature(., vhag_hydmo[[1]]),
                               NA_integer_)
      ) %>%
      # Voeg de variabelen van het dichtstbijzijnde traject toe
      bind_cols(hydmo_variabelen[.$nearest_index, ] %>%
                  st_drop_geometry() %>%
                  select(traj_code, ekc_r, sin_s3, opst_sco_t, stroomsnelheid_kmu, rec_width, bd_depth, bd_wd_rat)) %>%
      # Selecteer de relevante kolommen en maak de output 'plat'
      ungroup()
  )) %>%
  # Unnest om terug te keren naar een enkel dataframe
  unnest(gekoppeld) %>%
  ungroup() %>%
  # Selecteer de uiteindelijke kolommen
  select(meetplaats, owl, traj_code, ekc2_traject = ekc_r,
         sinuositeit = sin_s3,
         verstuwing = opst_sco_t, stroomsnelheid = stroomsnelheid_kmu,
         breedte = rec_width, diepte = bd_depth, breedte_diepte_ratio = bd_wd_rat)

#---------------------------------------------------------------------------------------------------
# Stap 4: Koppelen ekc2 op waterlichaamniveau
#---------------------------------------------------------------------------------------------------
# deze zou beter beeld kunnen geven dan lokale toestand

hm_data0 <- hydromorf_traject_vhag %>%
  left_join(., hydromorf_nieuw_waterloop %>%
              select(owl_code, ekc_r_owl2),
            by = c("owl" = "owl_code")) %>%
  rename(ekc2_waterlichaam = ekc_r_owl2)
#
# #---------------------------------------------------------------------------------------------------
# # Stap 5: Meetplaatsen met NA (traj_code) via vhag koppelen op afstand 20m
# #---------------------------------------------------------------------------------------------------
# #controleren op kaart en lijken goed gekoppelde punten op afstand 20m
#
# missing_values <- hm_data0 %>%
#   select(-geometry) %>%
#   left_join(., mi_meetpunten) %>%
#   st_as_sf %>%
#   filter(is.na(traj_code)) %>%
#   select(meetplaats)
# mapview(missing_values) + mapview(hydromorf_nieuw)
#
# nearest_river_index <- st_nearest_feature(missing_values, hydromorf_nieuw)
# distances <- st_distance(missing_values, hydromorf_nieuw[nearest_river_index, ], by_element = TRUE)
#
# # Assign NA for points further than tolerance
# tolerance <- 25
# nearest_river_index[as.numeric(distances) > tolerance] <- NA
#
# missing_values$traj_code <- as.character(hydromorf_nieuw$traj_code[nearest_river_index])
#
# hydmo_gekoppeld_afstand <- missing_values %>%
#   left_join(., hydmo_variabelen %>%
#               st_drop_geometry() %>%
#               select(traj_code, ekc_r, sin_s3, opst_sco_t, stroomsnelheid_kmu, rec_width, bd_depth, bd_wd_rat))
#
# hydmo_gekoppeld_afstand$traj_code %>% is.na() %>% sum
#
#
# dot <- hydmo_gekoppeld_afstand %>% filter(!is.na(traj_code))
# mapview(dot) + mapview(hydromorf_nieuw_traject)

#---------------------------------------------------------------------------------------------------
# Stap 6: Opslaan data
#---------------------------------------------------------------------------------------------------

hm_data <- hm_data0 %>%
  st_drop_geometry()

save(hm_data, file = here("data", "verwerkt", "hm_data.rdata"))

# welk type-waterlopen zijn de meetplaatsen NA voor hydmo? Bovenstroomse RtNt

na_data <- mi_data_analyse %>%
  st_drop_geometry() %>%
  select(meetplaats, type, categorie) %>%
  unique() %>%
  left_join(., hm_data %>%
              select(-traj_code, -owl)) %>%
  filter(is.na(ekc2_traject))

na_data %>%
  group_by(type) %>%
  summarise(n_type = n())

na_data %>%
  group_by(categorie) %>%
  summarise(n_type = n())

na_data %>%
  filter(type != "RtNt") %>%
  group_by(categorie) %>%
  summarise(n_type = n())

