# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
load(here("data", "verwerkt", "mafy_data.rdata"))

load(here("data", "verwerkt", "fc_selectie.rdata")) # fyschem

load(file = here("data", "verwerkt", "hm_data_mafy.rdata"))

load(file = "data/verwerkt/koppeling/koppeling_mafy_nutrient_aggregate_wide.rdata")
load(file = "data/verwerkt/koppeling/koppeling_mafy_nutrient_aggregate.rdata")

# load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata")) # LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))# LU
# load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer_100m_jaren.Rdata")) # LU cirkelbuffer
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_mafy_oeverzones_jaren.rdata"))
load(file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones_mafy.rdata"))
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_afstroomgebieden.rdata"))# LU
# load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_buffer_100m.rdata"))# LU
# load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones.rdata"))# LU

load(file = here("data", "verwerkt", "hydro_data.rdata")) # hydrologie (neerslag)

load(file = here("data", "verwerkt", "lozingen_data.rdata")) # lozingen

load(file = here("data", "verwerkt", "mafy_schaduw_data.rdata"))


mafy_data0 <- koppeling_mafy_nutrient_aggregate %>%
  filter(!groep %in% c("meer"))
# %>%
#   st_drop_geometry()

mafy_data1 <- mafy_data0 %>%
  left_join(hm_data_mafy,
            by = "meetplaats")

landgebruik_data <- landgebruik_afstroomgebied_jaren %>%
  select(-oppervlakte, -landgebruiksjaar) %>%
  left_join(.,
            intensiteit_landbouw_afstroomgebieden_scores,
            by = c("meetplaats", "monsternamedatum")) %>%
  rename_with(~paste0(., "_afstr"), -c(1, 2)) %>%
  full_join(.,
            landgebruik_mafy_oevergebied_jaren %>%
              select(-landgebruiksjaar, -oppervlakte) %>%
              rename_with(~paste0(., "_oever"), -c(1, 2)),
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(.,
            intensiteit_landbouw_oeverzones_mafy %>%
              select(-jaar, -meetnet, -oppervlakte_oeverzone, -vhas) %>%
              rename_with(~ paste0(., "_oever"), !all_of(c("meetplaats", "monsternamedatum"))),
            by = c("meetplaats", "monsternamedatum")) %>%
  distinct()

mafy_data2 <- mafy_data1 %>%
  left_join(landgebruik_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(hydro_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(lozingen_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mafy_schaduw_data,
            by = c("meetplaats", "monsternamedatum"))

k <- 8
mafy_data3 <- mafy_data2 %>%
  mutate(
    # Stap 1: Bereken tijdelijke gewichten (1 - score).
    # Als een originele waarde NA is, wordt het gewicht automatisch ook NA.
    w_v  = 1 - v_zonder_gep,
    w_vo = 1 - vo_zonder_gep,
    w_gv = 1 - gv_zonder_gep,
    w_ts = 1 - ts_zonder_gep,

    # Stap 2: Bereken de teller (som van score * gewicht per rij)
    # na.rm = TRUE negeert elke kolom waarvoor de waarde NA is.
    teller = rowSums(cbind(
      v_zonder_gep * w_v,
      vo_zonder_gep * w_vo,
      gv_zonder_gep * w_gv,
      ts_zonder_gep * w_ts
    ), na.rm = TRUE),

    # Stap 3: Bereken de noemer (som van de gewichten per rij)
    noemer = rowSums(cbind(w_v, w_vo, w_gv, w_ts), na.rm = TRUE),

    # Stap 4: Bereken het uiteindelijke gemiddelde
    # Let op: als in een rij álle (niet-NA) scores een 1 zijn, worden alle gewichten 0.
    # Delen door 0 geeft een fout (NaN). De if_else voorkomt dit.
    index_nieuw = if_else(noemer == 0, 1, teller / noemer)
  ) %>%
  # Stap 5 (Optioneel): Ruim de tijdelijke reken-kolommen weer op
  select(-w_v, -w_vo, -w_gv, -w_ts, -teller, -noemer) %>%
  mutate(
    # ==========================================
    # 1. Voorbereiding 4 parameters (incl. VO)
    # ==========================================
    n_4 = rowSums(!is.na(cbind(vo_zonder_gep, gv_zonder_gep, ts_zonder_gep, v_zonder_gep))),
    som_machten_4 = rowSums(1 / cbind(vo_zonder_gep, gv_zonder_gep, ts_zonder_gep, v_zonder_gep)^k, na.rm = TRUE),
    som_scores_4 = rowSums(cbind(vo_zonder_gep, gv_zonder_gep, ts_zonder_gep, v_zonder_gep), na.rm = TRUE),
    EKC_4 = if_else(som_scores_4 == 0 | n_4 == 0, 0, (n_4 / som_machten_4)^(1/k)),

    # ==========================================
    # 2. Voorbereiding 3 parameters (excl. VO)
    # ==========================================
    n_3 = rowSums(!is.na(cbind(gv_zonder_gep, ts_zonder_gep, v_zonder_gep))),
    som_machten_3 = rowSums(1 / cbind(gv_zonder_gep, ts_zonder_gep, v_zonder_gep)^k, na.rm = TRUE),
    som_scores_3 = rowSums(cbind(gv_zonder_gep, ts_zonder_gep, v_zonder_gep), na.rm = TRUE),
    EKC_3 = if_else(som_scores_3 == 0 | n_3 == 0, 0, (n_3 / som_machten_3)^(1/k)),

    # ==========================================
    # 3. Toewijzing op basis van de kolom 'groep'
    # ==========================================
    ekc_mafy = case_when(
      groep %in% c("beek", "kempen", "polder", "beek_niet_toegekend") ~ EKC_4,
      groep == "rivier" ~ EKC_3,
      TRUE ~ NA_real_ # Veiligheidsmarge voor als er een onbekende of lege groep in zit
    )
  ) %>%
  # Ruim de tijdelijke rekenkolommen op, zodat je alleen 'ekc_mafy' overhoudt
  select(-n_4, -som_machten_4, -som_scores_4, -EKC_4,
         -n_3, -som_machten_3, -som_scores_3, -EKC_3)

################################################################################
# Data subsets maken ####
################################################################################
# 1. natuurlijk en sterk veranderd
# 1.1 beek
mafy_nat_sv_beek <- mafy_data3 %>%
  filter(status == "NAT") %>%
  filter(groep == "beek")

# 1.2 kempen
mafy_nat_sv_kempen <- mafy_data3 %>%
  filter(status == "NAT") %>%
  filter(groep == "kempen")

# 1.3 polder
mafy_nat_sv_polder <- mafy_data3 %>%
  filter(status == "NAT") %>%
  filter(groep == "polder")

# 1.4 rivier
mafy_nat_sv_rivier <- mafy_data3 %>%
  filter(status == "NAT") %>%
  filter(groep == "rivier")

# 2. kunstmatig
mafy_kunstmatig <- mafy_data3 %>%
  filter(status == "KUNST")

# 3. RtNt - bovenlopen -> NVT????
mafy_rtnt <- mafy_data3 %>%
  filter(waterlichaamcategorietype == "RtNt")

