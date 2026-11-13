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
load(file = here("data", "verwerkt", "mi_data_analyse_rtnt_update.rdata"))

load(file = here("data", "verwerkt", "spear_data.rdata")) # spear pesticides
load(file = here("data", "verwerkt", "tu_resultaten.rdata")) # tu score pesticiden

load(file = here("data", "verwerkt", "mi_fd_multiset.rdata"))

deelbekken <- st_read("data/ruw/bekkens/Wsdeelbek.shp") %>%
  st_transform(., crs = st_crs(mi_data))

# weglaten vijvers, meren, geisoleerd water en punten buiten Vlaanderen
mi_data_analyse1 <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200")) %>%
  st_join(.,
          deelbekken %>%
            select(DEELBID),
          join = st_intersects) %>%
  rename(deelbekken = DEELBID) %>%
  mutate(deelbekken = replace_na(as.character(deelbekken), "onbekend"))

mi_data_analyse <- mi_data_analyse1 %>%
  # 1. Koppel de update data aan de originele dataset
  left_join(
    mi_data_analyse_rtnt_update %>%
      rename(type_rtnt = type) %>%
      st_drop_geometry() %>%
      # Selecteer enkel de sleutels en de te updaten kolommen, en geef ze een tijdelijke naam
      select(meetplaats, monsternamedatum, type_rtnt,
             mmif_upd = mmif, ept_upd = ept, swd_upd = swd,
             nst_upd = nst, mts_upd = mts, tax_upd = tax) %>%
      # Voeg een hulpkolom toe zodat we zeker weten welke rijen gevonden zijn
      mutate(is_match = TRUE),
    by = c("meetplaats", "monsternamedatum")
  ) %>%
  # 2. Vervang de waarden in de originele kolommen alléén als type == "RtNt" én er een match is
  mutate(
    mmif = if_else(type == "RtNt" & !is.na(is_match), mmif_upd, mmif),
    ept  = if_else(type == "RtNt" & !is.na(is_match), ept_upd, ept),
    swd  = if_else(type == "RtNt" & !is.na(is_match), swd_upd, swd),
    nst  = if_else(type == "RtNt" & !is.na(is_match), nst_upd, nst),
    mts  = if_else(type == "RtNt" & !is.na(is_match), mts_upd, mts),
    tax  = if_else(type == "RtNt" & !is.na(is_match), tax_upd, tax)
  ) %>%
  # 3. Verwijder de tijdelijke kolommen (is_match en alles dat eindigt op _upd)
  select(-ends_with("_upd"), -is_match)

mi_data_verrijkt <- st_join(mi_data_analyse, deelbekken["DEELBID"], join = st_intersects)

# koppeling fyschem
koppeling_sleutel_nutrient <-
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
  left_join(koppeling_sleutel_nutrient, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(fc_selectie,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_fc"))


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

fd_mi_filtered <- fd_mi %>%
  filter(trait_coverage_percentage >= 80)

data4 <- data3 %>%
  left_join(., fd_mi_filtered,
                         by = c("meetplaats", "monsternamedatum"))

data5 <- data4 %>%
  left_join(., spear_data %>%
              select(meetplaats, monsternamedatum, spear_pesticides, spear_tu_estimated),
                         by = c("meetplaats", "monsternamedatum"))
#
# mi_subsets_plot <- data4 %>%
#   mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
#   mutate(
#     subset = case_when(
#       # 1. Natuurlijk en Sterk Veranderd per groep
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") &
#         groep == "beek"   ~ "nat_sv_beek",
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") &
#         groep == "kempen" ~ "nat_sv_kempen",
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") &
#         groep == "polder" ~ "nat_sv_polder",
#       statuut %in% c("Natuurlijk", "Sterk Veranderd") &
#         groep == "rivier" ~ "nat_sv_rivier",
#
#       # 2. Kunstmatig (onafhankelijk van groep)
#       statuut == "Kunstmatig"                                            ~ "kunstmatig",
#
#       # 3. Specifieke types zoals RtNt
#       type == "RtNt"                                                     ~ "rtnt",
#
#       # Restgroep (optioneel, voor alles wat niet in bovenstaande valt)
#       TRUE                                                               ~ "overig"
#     )
#   )
#
#
# # 1. Voorbereiden: Data in long format en nesten
# mi_nested <- mi_subsets_plot %>%
#   filter(subset != "overig") %>%
#   pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
#                names_to = "index",
#                values_to = "waarde") %>%
#   mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
#          subset = as.factor(subset),
#          index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept"))) %>%
#   # Nest de data: elke rij is nu een unieke combinatie van subset en index
#   group_nest(subset, index)
#
# # 2. Modellen fitten en trends extraheren met purrr::map
# mi_models <- mi_nested %>%
#   mutate(
#     # Fit het model voor elke subset/index combinatie
#    # Probeer k te verhogen naar bijvoorbeeld 4 of 5
#     # Probeer k te verhogen naar bijvoorbeeld 4 of 5
#     model = map(data, ~ glmmTMB(waarde ~ s(jaar_num, k = 4) + (1|meetplaats) + (1|bekken),
#                                 data = .x)),
#     # Extraheer de trends (predictions)
#     predictions = map(model, ~ as.data.frame(ggpredict(.x, terms = "jaar_num [all]")))
#   )
#
# # 3. Resultaten uitpakken voor visualisatie
# mi_trends <- mi_models %>%
#   select(subset, index, predictions) %>%
#   unnest(predictions)
#
# # 4. De Facet Plot
# ggplot(mi_trends, aes(x = x, y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset), alpha = 0.2) +
#   geom_line(aes(color = subset), size = 1) +
#   # Gebruik facet_grid voor de matrix: subset (rijen) vs index (kolommen)
#   facet_grid(index ~ subset, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "GAM-trends per Subset en Index",
#        x = "Jaar", y = "Voorspelde waarde",
#        color = "Subset", fill = "Subset") +
#   theme(legend.position = "none",
#         strip.text = element_text(face = "bold"),
#         axis.text.x = element_text(angle = 45, hjust = 1))
#
# mi_subsets_plot %>%
#   ggplot(aes(jaar, mmif)) +
#   geom_smooth(method = "gam")

################################################################################
# Data subsets maken ####
################################################################################
# 1. natuurlijk en sterk veranderd
# 1.1 beek
mi_nat_sv_beek <- data5 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "beek")
save(mi_nat_sv_beek, file = here("data", "verwerkt", "mi_subsets", "mi_nat_sv_beek.rdata"))

# 1.2 kempen
mi_nat_sv_kempen <- data5 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "kempen")
save(mi_nat_sv_kempen, file = here("data", "verwerkt", "mi_subsets", "mi_nat_sv_kempen.rdata"))

# 1.3 polder
mi_nat_sv_polder <- data5 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "polder")

# 1.4 rivier
mi_nat_sv_rivier <- data5 %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep == "rivier")

# 2. kunstmatig
mi_kunstmatig <- data5 %>%
  filter(statuut %in% c("Kunstmatig"))

# 3. RtNt - bovenlopen -> NVT????
mi_rtnt <- data5 %>%
  filter(type == "RtNt")
