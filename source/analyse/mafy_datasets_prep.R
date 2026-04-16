# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
load(here("data", "verwerkt", "mafy_data.rdata"))

load(here("data", "verwerkt", "fc_selectie.rdata")) # fyschem

load(file = here("data", "verwerkt", "hm_data_mafy.rdata"))

load(file = "data/verwerkt/koppeling/koppeling_mafy_nutrient_aggregate_wide.rdata")

# load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata")) # LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))# LU
# load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer_100m_jaren.Rdata")) # LU cirkelbuffer
# load(file = here("data", "verwerkt", "landgebruik", "landgebruik_oever_jaren.rdata"))# LU

load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_afstroomgebieden.rdata"))# LU
# load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_buffer_100m.rdata"))# LU
# load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones.rdata"))# LU

load(file = here("data", "verwerkt", "hydro_data.rdata")) # hydrologie (neerslag)

load(file = here("data", "verwerkt", "lozingen_data.rdata")) # lozingen


mafy_data0 <- koppeling_mafy_nutrient_aggregate_wide %>%
  filter(!groep %in% c("meer")) %>%
  st_drop_geometry()

mafy_data1 <- mafy_data0 %>%
  left_join(hm_data_mafy,
            by = "meetplaats")

landgebruik_data <- landgebruik_afstroomgebied_jaren %>%
  select(-oppervlakte, -landgebruiksjaar) %>%
  # filter(meetplaats %in% afstroomgebieden_binnen_vlaanderen$meetplaats) %>% # afstroomgebiedne moeten voor meer dan 80% binnen vlaanderen liggen
  # full_join(.,
  #           landgebruik_oever_jaren %>%
  #             select(-landgebruiksjaar),
  #           by = c("meetplaats", "monsternamedatum"), suffix = c("_afstr", "_oever")) %>%
  # left_join(.,
  #           landgebruik_buffer_100m_jaren %>%
  #             select(-landgebruiksjaar) %>%
  #             rename_with(~ paste0(., "_buffer"), !all_of(c("meetplaats", "monsternamedatum"))),
  #           by = c("meetplaats", "monsternamedatum"), suffix = c("", "_buffer")) %>%
  left_join(.,
            intensiteit_landbouw_afstroomgebieden_scores,
            by = c("meetplaats", "monsternamedatum")) %>%
  rename_with(~paste0(., "_afstr"), -c(1, 2))

# %>%
  # left_join(.,
  #           intensiteit_landbouw_buffer_100m_jaren %>%
  #             select(-jaar, -oppervlakte_buffer),
  #           by = c("meetplaats", "monsternamedatum"),
  #           suffix = c("_afstr", "_buffer")) %>%
  # left_join(.,
  #           intensiteit_landbouw_oeverzones_jaren %>%
  #             select(-jaar, -oppervlakte_oeverzone) %>%
  #             rename_with(~ paste0(., "_oeverzone"), !all_of(c("meetplaats", "monsternamedatum"))),
  #           by = c("meetplaats", "monsternamedatum"))

mafy_data2 <- mafy_data1 %>%
  left_join(landgebruik_data,
            by = c("meetplaats", "monsternamedatum"))

mafy_data3 <- mafy_data2 %>%
  left_join(hydro_data,
            by = c("meetplaats", "monsternamedatum"))

mafy_data4 <- mafy_data3 %>%
  left_join(lozingen_data,
            by = c("meetplaats", "monsternamedatum"))

################################################################################
# Data subsets maken ####
################################################################################
# 1. natuurlijk en sterk veranderd
# 1.1 beek
mafy_nat_sv_beek <- mafy_data4 %>%
  filter(status == "NAT") %>%
  filter(groep == "beek")

# 1.2 kempen
mafy_nat_sv_kempen <- mafy_data4 %>%
  filter(status == "NAT") %>%
  filter(groep == "kempen")

# 1.3 polder
mafy_nat_sv_polder <- mafy_data4 %>%
  filter(status == "NAT") %>%
  filter(groep == "polder")

# 1.4 rivier
mafy_nat_sv_rivier <- mafy_data4 %>%
  filter(status == "NAT") %>%
  filter(groep == "rivier")

# 2. kunstmatig
mafy_kunstmatig <- mafy_data4 %>%
  filter(status == "NAT")

# 3. RtNt - bovenlopen -> NVT????
rtnt <- mafy_data4 %>%
  filter(waterlichaamcategorietype == "RtNt")

