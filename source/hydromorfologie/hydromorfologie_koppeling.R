# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
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
hydromorf_nieuw_traject <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail_afgerond.shp"), quiet = T) %>% # oude data maar gebruik trajecten en lijnen
  select(traj_code) %>%
  left_join(read_xlsx(here("data", "ruw", "hydromorfologie", "hydromorfologie_meest_recent.xlsx")),
            by = "traj_code") # meest recente data linken aan gislaag

# hydromorf_nieuw_waterloop <- st_read(here("data", "ruw", "hydromorfologie", "hymo_wlniveau.shp"))

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

st_crs(mi_meetpunten) == st_crs(hydmo_variabelen)

#---------------------------------------------------------------------------------------------------
# Stap 2: Koppeling meetpunten aan alle mogelijke trajecten via vhag
#---------------------------------------------------------------------------------------------------

meetpunten_met_trajecten <- meetpunten_prep %>%
  st_drop_geometry() %>%
  left_join(hydmo_variabelen %>% select(vhag, traj_code) %>%
              st_drop_geometry() %>%
              mutate(vhag = as.character(vhag)), by = "vhag")

#---------------------------------------------------------------------------------------------------
# STAP 3: Hybride koppeling (VHAG eerst, daarna Afstand)
#---------------------------------------------------------------------------------------------------

# 1. Voorbereiding: zorg voor SF objecten en consistent CRS
meetpunten_sf <- meetpunten_prep %>% st_as_sf()
hydmo_sf <- hydmo_variabelen %>% st_as_sf()

# Controleer of CRS gelijk is (cruciaal voor st_distance)
if(st_crs(meetpunten_sf) != st_crs(hydmo_sf)) {
  meetpunten_sf <- st_transform(meetpunten_sf, st_crs(hydmo_sf))
}

# 2. De koppelingsfunctie
hm_gekoppeld_totaal <- meetpunten_sf %>%
  split(.$meetplaats) %>%
  purrr::map_dfr(function(punt) {

    # --- POGING 1: Match op basis van VHAG ---
    doel_vhag <- as.character(punt$vhag)
    trajecten_vhag_subset <- hydmo_sf %>% filter(as.character(vhag) == doel_vhag)

    match_gevonden <- FALSE
    res <- NULL

    if (nrow(trajecten_vhag_subset) > 0) {
      idx <- st_nearest_feature(punt, trajecten_vhag_subset)
      res <- bind_cols(st_drop_geometry(punt),
                       st_drop_geometry(trajecten_vhag_subset[idx, ]) %>% select(-vhag))
      match_gevonden <- TRUE
    }

    # --- POGING 2: Match op basis van afstand (indien poging 1 mislukte) ---
    if (!match_gevonden) {
      # Zoek het absoluut dichtstbijzijnde traject in de GEHELE hydmo-laag
      idx_nearest <- st_nearest_feature(punt, hydmo_sf)
      afstand <- st_distance(punt, hydmo_sf[idx_nearest, ])

      if (as.numeric(afstand) <= 25) {
        res <- bind_cols(st_drop_geometry(punt),
                         st_drop_geometry(hydmo_sf[idx_nearest, ]) %>% select(-vhag))
        # Optioneel: vlag toevoegen dat dit via afstand is gebeurd
        res$koppelmethode <- "afstand_25m"
      } else {
        # Echt geen match gevonden binnen 25m
        res <- st_drop_geometry(punt)
        res$koppelmethode <- "geen_match"
      }
    } else {
      res$koppelmethode <- "vhag_match"
    }

    return(res)
  })

# 3. Opschonen en hernoemen naar jouw format
hm_data0 <- hm_gekoppeld_totaal %>%
  rename(
    ekc2_traject = ekc_r,
    ekc2_waterlichaam = ekc_r_owl2_sgbp4,
    profiel = pt_sco_r,
    bodemsub = bs_sco_r,
    sinuositeit = sin_s3,
    doodhout = dh_sco_r,
    verstuwing = opst_sco_t,
    stroomsnelheid = stroomsnelheid_kmu,
    breedte = rec_width,
    diepte = bd_depth,
    breedte_diepte_ratio = bd_wd_rat
  )

#---------------------------------------------------------------------------------------------------
# Stap 4: Opslaan data
#---------------------------------------------------------------------------------------------------

hm_data <- hm_data0 %>%
  st_drop_geometry() %>%
  select(meetplaats, breedte_diepte_ratio, breedte, diepte, sinuositeit, doodhout, profiel, bodemsub,
         ekc2_waterlichaam, ekc2_traject, verstuwing, stroomsnelheid)

save(hm_data, file = here("data", "verwerkt", "hm_data.rdata"))


##################
# welk type-waterlopen zijn de meetplaatsen NA voor hydmo? Bovenstroomse RtNt
###################"
na_data <- mi_data_analyse %>%
  st_drop_geometry() %>%
  select(meetplaats, type, categorie, owl) %>%
  unique() %>%
  left_join(., hm_data) %>%
  filter(is.na(ekc2_waterlichaam))

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

