
# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
load(file = here("data", "verwerkt", "fybe_data.rdata"))

load(here("data", "verwerkt", "fc_selectie.rdata")) # fyschem

load(file = here("data", "verwerkt", "hm_data_fybe.rdata"))

load(file = "data/verwerkt/koppeling/koppeling_fybe_nutrient_aggregate.rdata")
#
# # load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata")) # LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))# LU
# # load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer_100m_jaren.Rdata")) # LU cirkelbuffer
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_mafy_oeverzones_jaren.rdata"))
load(file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones_mafy.rdata"))
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_afstroomgebieden.rdata"))# LU
# # load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_buffer_100m.rdata"))# LU
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones.rdata"))# LU
#
load(file = here("data", "verwerkt", "hydro_data.rdata")) # hydrologie (neerslag)
#
load(file = here("data", "verwerkt", "lozingen_data.rdata")) # lozingen
#
load(file = here("data", "verwerkt", "mafy_schaduw_data.rdata"))


fybe_data0 <- koppeling_fybe_nutrient_aggregate %>%
  filter(!groep %in% c("meer")) %>%
  mutate(jaar = lubridate::year(monsternamedatum))
# %>%
#   st_drop_geometry()

fybe_data1 <- fybe_data0 %>%
  left_join(hm_data_fybe,
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
            by = c("meetplaats", "monsternamedatum"))
#
#
fybe_data2 <- fybe_data1 %>%
  left_join(landgebruik_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(hydro_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(lozingen_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mafy_schaduw_data,
            by = c("meetplaats", "monsternamedatum"))


################################################################################
# Data subsets maken ####
################################################################################
# 1. natuurlijk en sterk veranderd
# 1.1 beek
fybe_nat_sv_beek <- fybe_data2 %>%
  filter(status == "NAT") %>%
  filter(groep == "beek")

# 1.2 kempen
fybe_nat_sv_kempen <- fybe_data1 %>%
  filter(status == "NAT") %>%
  filter(groep == "kempen")

# 1.3 polder
fybe_nat_sv_polder <- fybe_data1 %>%
  filter(status == "NAT") %>%
  filter(groep == "polder")

# 1.4 rivier
fybe_nat_sv_rivier <- fybe_data1 %>%
  filter(status == "NAT") %>%
  filter(groep == "rivier")

# 2. kunstmatig
fybe_kunstmatig <- fybe_data1 %>%
  filter(status == "KUNST")

# 3. RtNt - bovenlopen -> NVT????
fybe_rtnt <- fybe_data1 %>%
  filter(waterlichaamcategorietype == "RtNt")

