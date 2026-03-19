# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

load(here("data", "verwerkt", "mi_data.rdata")) # macroinvertebraten

load(here("data", "verwerkt", "fc_selectie.rdata")) # fyschem

load(here("data", "verwerkt", "hm_data.rdata")) # hydmo

load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata")) # LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))# LU
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer_100m_jaren.Rdata")) # LU cirkelbuffer
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_oever_jaren.rdata"))# LU

load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_afstroomgebieden.rdata"))# LU
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_buffer_100m.rdata"))# LU
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_oeverzones.rdata"))# LU

load(file = here("data", "verwerkt", "hydro_data.rdata")) # hydrologie (neerslag)

load(file = here("data", "verwerkt", "lozingen_data.rdata")) # lozingen

load(file = "data/verwerkt/koppeling/koppeling_mi_nutrient.rdata") # koppeling fyschem

# load(file = here("data", "verwerkt", "spear_data.rdata")) # spear pesticides
# load(file = here("data", "verwerkt", "tu_resultaten.rdata")) # tu score pesticiden

# weglaten vijvers, meren, geisoleerd water en punten buiten Vlaanderen
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

# koppeling fyschem
koppeling_sleutel <-
koppeling_mi_nutrient %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mi_nutrient <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(fc_selectie,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_fc"))

#### vergelijking met oude koppeling ####
#
# fc_mi_old <- mi_data_analyse %>%
#   st_drop_geometry() %>%
#   filter(jaar > 2009) %>%
#   select(meetplaats, monsternamedatum, groep, jaar) %>%
#   left_join(.,
#             fc_selectie,
#             by = c("meetplaats"), suffix = c("", "_fc")) %>%
#   filter(
#
#     {
#       days_before <- as.numeric(difftime(monsternamedatum, monsternamedatum_fc, units = "days"))
#       days_before < 90 &
#         days_before > -14
#
#     }
#   ) %>%
#   group_by(meetplaats, monsternamedatum) %>% #dubbele samples uitmiddelen
#   summarise(
#     across(
#       where(is.numeric), \(x) mean(x, na.rm = TRUE) # enkel numerische kolommen om de mean te pakken
#     ), # voor niet numerische waarden gewoon de eerste string nemen om te behouden
#     across(
#       where(is.factor) | where(is.character),
#       ~ dplyr::first(.)
#     ),
#     .groups = "drop" # Drop the grouping at the end
#   )
# fc_mi_old$n_t %>% is.na() %>% sum
gekoppelde_data_mi_nutrient %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "beek") %>%
  filter(jaar > 2009) %>%
  filter(!is.na(n_t))
#
#   pull(n_t) %>%
#   is.na() %>% sum
#
# fc_mi_old$czv %>% is.na() %>% sum
#
# fc_mi_old %>%
#   filter(groep == "beek") %>%
#   filter(jaar > 2009) %>%
#   filter(!is.na(n_t))
# gekoppelde_data_mi_nutrient$czv %>% is.na() %>% sum

#### koppeling overige data ####
data0 <- gekoppelde_data_mi_nutrient %>%
  left_join(hm_data,
            by = c("meetplaats"))

landgebruik_data <- landgebruik_afstroomgebied_jaren %>%
  select(-oppervlakte, -landgebruiksjaar) %>%
  filter(meetplaats %in% afstroomgebieden_binnen_vlaanderen$meetplaats) %>% # afstroomgebiedne moeten voor meer dan 80% binnen vlaanderen liggen
  full_join(.,
            landgebruik_oever_jaren %>%
              select(-landgebruiksjaar),
            by = c("meetplaats", "monsternamedatum"), suffix = c("_afstr", "_oever")) %>%
  left_join(.,
            landgebruik_buffer_100m_jaren %>%
              select(-landgebruiksjaar) %>%
              rename_with(~ paste0(., "_buffer"), !all_of(c("meetplaats", "monsternamedatum"))),
            by = c("meetplaats", "monsternamedatum"), suffix = c("", "_buffer")) %>%
  left_join(.,
            intensiteit_landbouw_afstroomgebieden_scores,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(.,
            intensiteit_landbouw_buffer_100m_jaren %>%
              select(-jaar, -oppervlakte_buffer),
            by = c("meetplaats", "monsternamedatum"),
            suffix = c("_afstr", "_buffer")) %>%
  left_join(.,
            intensiteit_landbouw_oeverzones_jaren %>%
              select(-jaar, -oppervlakte_oeverzone) %>%
              rename_with(~ paste0(., "_oeverzone"), !all_of(c("meetplaats", "monsternamedatum"))),
            by = c("meetplaats", "monsternamedatum"))


data1 <- data0 %>%
  left_join(landgebruik_data,
            by = c("meetplaats", "monsternamedatum"))

data2 <- data1 %>%
  left_join(hydro_data,
            by = c("meetplaats", "monsternamedatum"))

data3 <- data2 %>%
  left_join(lozingen_data,
            by = c("meetplaats", "monsternamedatum"))

#### Data subsets maken ####
# 1. natuurlijk en sterk veranderd
# 1.1 beek
mi_nat_sv_beek <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "beek")

# 1.2 kempen
mi_nat_sv_kempen <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "kempen")

# 1.3 polder
mi_nat_sv_polder <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "polder")

# 1.4 rivier
mi_nat_sv_rivier <- data3 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "rivier")

# 2. kunstmatig
mi_kunstmatig <- data3 %>%
  filter(statuut %in% c("Kunstmatig"))

# 3. RtNt - bovenlopen -> NVT????
rtnt <- data3 %>%
  filter(type == "RtNt")
load(file = here("data", "verwerkt", "mi_data_analyse_rtnt_update.rdata"))

