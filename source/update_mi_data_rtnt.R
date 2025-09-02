source(here::here("source", "inladen_packages.R"))

load(here("data", "verwerkt", "mi_data.rdata"))

# weglaten vijvers, meren, geisoleerd water en punten buiten Vlaanderen
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))


# wlas <- st_read(here("data", "ruw", "Wlas", "Wlas.shp"))
# unique_rtnt <- mi_data_analyse %>% filter(jaar > 2006) %>% select(meetplaats, type) %>% filter(type == "RtNt") %>% unique()
# beek <- mi_data_analyse %>% select(meetplaats, groep) %>%
#   filter(groep == "beek") %>% unique()
# polder <- mi_data_analyse %>% select(meetplaats, groep) %>%
#   filter(groep == "polder") %>% unique()
# mapview(unique_rtnt) +
#   mapview(wlas)



#kunsmatige afsplitsen -> zelfde protocol als niet-kunstmatig
mi_data_analyse %>% filter(type == "RtNt") %>% group_by(waterlooptype) %>% summarize(n()) #op het einde kan statuut worden gegroepeerd op basis hiervan

# ecoregio's koppelen aan meetpunten
meetpunten_rtnt <- mi_data_analyse %>%
  filter(type == "RtNt") %>%
  select(meetplaats, type, categorie, waterlooptype, vhag) %>% unique()
#
# # ecoregio's
# ecoregios <- st_read(here("data", "ruw", "ecoregios", "EcoregiosBelgium.shp")) %>% st_transform(., crs = crs(meetpunten_rtnt))
# mapview(beek) + mapview(ecoregios)
# mapview(polder) + mapview(ecoregios)
#
# meetpunten_ecoregio <- st_join(meetpunten_rtnt, ecoregios %>% select(REGION), join = st_intersects) %>%
#   mutate(ecoregio = case_when(
#     REGION == "kempen" ~ "kempen",
#     REGION == "polder" ~ "polder",
#     TRUE ~ "beek")) %>%
#   mutate(type_nieuw = case_when(
#     ecoregio == "polder" ~ "P",
#     categorie == "Bevaarbaar" & ecoregio == "kempen" ~ "BgK",
#     categorie == "Bevaarbaar" & ecoregio == "beek" ~ "Bg",
#     categorie == "Onbevaarbaar cat. 1" & ecoregio == "kempen" ~ "BgK",
#     categorie == "Onbevaarbaar cat. 1" & ecoregio == "beek" ~ "Bg",
#     categorie == "Onbevaarbaar cat. 2" & ecoregio == "kempen" ~ "BkK",
#     categorie == "Onbevaarbaar cat. 2" & ecoregio == "beek" ~ "Bk",
#     categorie == "Onbevaarbaar cat. 3" & ecoregio == "kempen" ~ "BkK",
#     categorie == "Onbevaarbaar cat. 3" & ecoregio == "beek" ~ "Bk",
#     TRUE ~ "Bk"
#   )) %>%
#   mutate(statuut_nieuw = case_when(
#     waterlooptype == "Kunstmatige waterloop" ~ "Kunstmatig",
#     waterlooptype == "Natuurlijke waterloop" ~ "Natuurlijk",
#     waterlooptype == "Onbekend" ~ "Natuurlijk"
#   )) %>%
#   select(meetplaats, ecoregio, type_nieuw, statuut_nieuw) %>%
#   st_drop_geometry()
#
# # mmif herberekenen
# meetpunten_ecoregio_mmif_herberekend <- mi_data_analyse %>%
#   select(meetplaats, monsternamedatum, mmif:ns_tw) %>%
#   st_drop_geometry() %>%
#   right_join(., meetpunten_ecoregio, by = "meetplaats") %>%
#   mutate(tax_nieuw = case_when(
#     type_nieuw == "P" ~ case_when(
#       ta_xw <= 5 ~ 0,
#       ta_xw <= 13 ~ 1,
#       ta_xw <= 21 ~ 2,
#       ta_xw <= 29 ~ 3,
#       ta_xw > 29 ~ 4),
#     type_nieuw == "BgK" | type_nieuw == "Bg" ~ case_when(
#       ta_xw <= 5 ~ 0,
#       ta_xw <= 13.25 ~ 1,
#       ta_xw <= 21.5 ~ 2,
#       ta_xw <= 29.75 ~ 3,
#       ta_xw > 29.75 ~ 4),
#     type_nieuw == "BkK" | type_nieuw == "Bk" ~ case_when(
#       ta_xw <= 5 ~ 0,
#       ta_xw <= 12.25 ~ 1,
#       ta_xw <= 19.5 ~ 2,
#       ta_xw <= 26.75 ~ 3,
#       ta_xw > 26.75 ~ 4)),
#     ept_nieuw = case_when(
#       type_nieuw == "P" ~ case_when(
#         ep_tw == 0 ~ 0,
#         ep_tw <= 2 ~ 1,
#         ep_tw <= 4 ~ 2,
#         ep_tw <= 6 ~ 3,
#         ep_tw > 6 ~ 4),
#       type_nieuw == "BgK" ~ case_when(
#         ep_tw == 0 ~ 0,
#         ep_tw <= 2.25 ~ 1,
#         ep_tw <= 4.5 ~ 2,
#         ep_tw <= 6.75 ~ 3,
#         ep_tw > 6.75 ~ 4),
#       type_nieuw == "Bg" ~ case_when(
#         ep_tw == 0 ~ 0,
#         ep_tw <= 2 ~ 1,
#         ep_tw <= 4 ~ 2,
#         ep_tw <= 6 ~ 3,
#         ep_tw > 6 ~ 4),
#       type_nieuw == "BkK" ~ case_when(
#         ep_tw == 0 ~ 0,
#         ep_tw <= 2 ~ 1,
#         ep_tw <= 4 ~ 2,
#         ep_tw <= 6 ~ 3,
#         ep_tw > 6 ~ 4),
#       type_nieuw == "Bk" ~ case_when(
#         ep_tw == 0 ~ 0,
#         ep_tw <= 1.75 ~ 1,
#         ep_tw <= 3.5 ~ 2,
#         ep_tw <= 5.25 ~ 3,
#         ep_tw > 5.25 ~ 4)),
#     nst_nieuw = case_when(
#       type_nieuw == "P" ~ case_when(
#         ns_tw == 0 ~ 0,
#         ns_tw <= 2.5 ~ 1,
#         ns_tw <= 5 ~ 2,
#         ns_tw <= 7.5 ~ 3,
#         ns_tw > 7.5 ~ 4),
#       type_nieuw == "BgK" | type_nieuw == "Bg" ~ case_when(
#         ns_tw == 0 ~ 0,
#         ns_tw <= 2.5 ~ 1,
#         ns_tw <= 5 ~ 2,
#         ns_tw <= 7.5 ~ 3,
#         ns_tw > 7.5 ~ 4),
#       type_nieuw == "BkK" | type_nieuw == "Bk" ~ case_when(
#         ns_tw == 0 ~ 0,
#         ns_tw <= 2.25 ~ 1,
#         ns_tw <= 4.5 ~ 2,
#         ns_tw <= 6.75 ~ 3,
#         ns_tw > 6.75 ~ 4)),
#     swd_nieuw = case_when(
#       sw_dw <= 0.2 ~ 0,
#       sw_dw <= 1.025 ~ 1,
#       sw_dw <= 1.85 ~ 2,
#       sw_dw <= 2.675 ~ 3,
#       sw_dw > 2.675 ~ 4
#     ),
#     mts_nieuw = case_when(
#       type_nieuw == "P" ~ case_when(
#         mt_sw <= 2 ~ 0,
#         mt_sw <= 3.075 ~ 1,
#         mt_sw <= 4.15 ~ 2,
#         mt_sw <= 5.225 ~ 3,
#         mt_sw > 5.225 ~ 4),
#       type_nieuw == "BgK" | type_nieuw == "Bg" | type_nieuw == "BkK" | type_nieuw == "Bk" ~ case_when(
#         mt_sw <= 2 ~ 0,
#         mt_sw <= 3.125 ~ 1,
#         mt_sw <= 4.25 ~ 2,
#         mt_sw <= 5.375 ~ 3,
#         mt_sw > 5.375 ~ 4)
#     ),
#     mmif_nieuw = (tax_nieuw + ept_nieuw + nst_nieuw + swd_nieuw + mts_nieuw)/20)
#
# #checken of mmif en mmif_nieuw gelijk zijn voor Bk
#
# meetpunten_ecoregio_mmif_herberekend %>% filter(type_nieuw == "Bk") %>% select(mmif, mmif_nieuw) %>% View
#
# meetpunten_ecoregio_mmif_herberekend %>% select(mmif, mmif_nieuw) %>% View
#
# meetpunten_ecoregio_mmif_lang <- meetpunten_ecoregio_mmif_herberekend %>%
#   pivot_longer(., cols = c("mmif", "mmif_nieuw"), names_to = "mmif_oudofnieuw", values_to = "mmif_waarden") %>%
#   select(meetplaats, mmif_oudofnieuw, mmif_waarden, type_nieuw)
#
#
#
# ggplot(data = meetpunten_ecoregio_mmif_lang, aes(x = mmif_waarden)) +
#   geom_histogram(
#     aes(fill = mmif_oudofnieuw),
#     position = "identity",
#     alpha = 0.5, # Maakt de kleuren transparant
#     bins = 30 # Stel het aantal bins in
#   ) +
#   scale_fill_manual(
#     values = c("mmif" = "#1F78B4", "mmif_nieuw" = "#E31A1C"),
#     name = "Variabele" # Pas de naam van de legende aan
#   ) +
#   labs(
#     title = "Verdeling mmif en mmif_nieuw",
#     x = "Waarde",
#     y = "Frequentie"
#   ) +
#   theme_minimal()

# tweede methode op basis van morfologiekaart ----

morfotype <- st_read(dsn = here("data", "ruw", "waterlopen", "morfotype", "ecoltypwatl.shp")) %>%
  st_transform(., crs = crs(meetpunten_rtnt))
st_crs(morfotype)
nearest_river_index <- st_nearest_feature(meetpunten_rtnt, morfotype)
meetpunten_morfotype <- meetpunten_rtnt %>%
  mutate(morfotype = morfotype$MORFONAAM[nearest_river_index])

morfotype_vhag_unique <- morfotype %>%
  distinct(VHAG, MORFONAAM) %>%
  mutate(vhag = as.character(VHAG)) %>%
  select(-VHAG)
koppeling_meetplaatsen_vhag <- meetpunten_rtnt %>%
  left_join(., morfotype_vhag_unique) %>%
  select(meetplaats, vhag, MORFONAAM) %>%
  group_by(meetplaats) %>%
  mutate(n_vhag = n())



meetplaatsen_toomany <- koppeling_meetplaatsen_vhag %>% filter(n_vhag > 1)
unique(meetplaatsen_toomany$meetplaats) %>% length()




meetpunten_morfotype %>% st_drop_geometry() %>% group_by(morfotype) %>% summarize(n())

meetpunten_ecoregio %>%
  left_join(., meetpunten_morfotype) %>%
  select(meetplaats, type_nieuw, morfotype) %>%
  View

distances <- st_distance(meetpunten_rtnt, morfotype[nearest_river_index, ], by_element = TRUE)
meetpunten_morfotype$distance <- as.numeric(distances)

dichte_meetpunten <- meetpunten_morfotype %>%
  filter(distance < 25) #1794

verre_meetpunten <- meetpunten_morfotype %>%
  filter(distance > 25)
verre_meetpunten %>% st_drop_geometry() %>% group_by(morfotype) %>% summarise(n())

koppeling_verre_punten <- verre_meetpunten %>%
  left_join(., morfotype_vhag_unique) %>%
  select(meetplaats, vhag, MORFONAAM) %>%
  group_by(meetplaats) %>%
  mutate(n_vhag = n())
  # verre meetpunten koppelen via vhag aan morfotype

verre_meetplaatsen_toomany <- koppeling_verre_punten %>% filter(n_vhag > 1)
verre_meetplaatsen_toomany$meetplaats %>% unique()

plot_punten1 <- verre_meetplaatsen_toomany$meetplaats %>% unique()
plot_punten <- meetpunten_rtnt  %>% filter(meetplaats %in% plot_punten1)
plot_waterlopen1 <- verre_meetplaatsen_toomany$vhag %>% unique()
plot_waterlopen <- morfotype %>% filter(VHAG %in% plot_waterlopen1)

# mapview(plot_punten) + mapview(plot_waterlopen, zcol = "MORFONAAM")

nearest_river_index2 <- st_nearest_feature(plot_punten, plot_waterlopen) #verre punten met dubbele VHAG koppeling snappen naar dichtste
verre_punten_toomany_nearest <- plot_punten %>%
  mutate(morfotype = morfotype$MORFONAAM[nearest_river_index2])



verre_meetplaatsen_unique <- koppeling_verre_punten %>% filter(n_vhag == 1) #203

meetpunten_rtnt_morfotype1 <- bind_rows(dichte_meetpunten %>%
                                         select(meetplaats, vhag, morfotype),
                                       verre_meetplaatsen_unique %>%
                                         rename(morfotype = MORFONAAM) %>%
                                         select(meetplaats, vhag, morfotype),
                                       verre_punten_toomany_nearest %>%
                                         select(meetplaats, vhag, morfotype))

punten_zonder <- meetpunten_rtnt_morfotype1 %>% filter(is.na(morfotype))
# punten_zonder_vhag <- punten_zonder$vhag
# wlas <- st_read(here("data", "ruw", "wlas", "Wlas.shp"))
# wlas %>% filter(VHAG %in% punten_zonder_vhag) %>% View #verre punten waarvan geen vhag kan gelinkt worden aan morfotype want ontbreekt in die laag. -> deze terug plotten en bekijken op kaart -> handmatig aangvuld

morfotype_handmatig <- read.csv(file = here("data", "ruw", "waterlopen", "morfotype", "handmatige_toewijzing_morfotype.csv"), sep = ";")

# hier verderdoen -> waarom zoveel NA voor morfotype -> check verre punten_unique en toomany!!!

onduidelijke_punten <- morfotype_handmatig %>%
  filter(!morfotype %in% c("kleine beek", "kleine beek Kempen", "grote beek", "grote beek Kempen",
                           "rivier (< 20m)", "grote rivier (> 20m)", "Maas", "kanaal", "kunstmatige waterloop (zoet)",
                           "kunstmatige waterloop (brak)")) %>%
  left_join(., mi_data_analyse %>%
               select(meetplaats)) %>%
  st_as_sf()
#
# kaartje_maarten <- mapview(onduidelijke_punten) + mapview(morfotype, zcol = "MORFONAAM")
# mapshot(kaartje_maarten, url = here("output", "kaartje_onduidelijke_punten.html"))

meetpunten_rtnt_morfotype <- meetpunten_rtnt_morfotype1 %>%
  st_drop_geometry() %>%
  select(-vhag) %>%
  drop_na(morfotype) %>%
  bind_rows(., morfotype_handmatig) %>%
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

# mmif herberekenen
meetpunten_morfotype_mmif_herberekend <- mi_data_analyse %>%
  # select(meetplaats, monsternamedatum, mmif:ns_tw) %>%
  # st_drop_geometry() %>%
  right_join(., meetpunten_rtnt_morfotype, by = "meetplaats") %>%
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


#checken of mmif en mmif_nieuw gelijk zijn voor Bk
#
# meetpunten_morfotype_mmif_herberekend %>% filter(type_nieuw == "Bk") %>% select(mmif, mmif_nieuw) %>% View
#
# meetpunten_morfotype_mmif_herberekend %>% select(mmif, mmif_nieuw) %>% View
#
# meetpunten_morfotype_mmif_lang <- meetpunten_morfotype_mmif_herberekend %>%
#   pivot_longer(., cols = c("mmif", "mmif_nieuw"), names_to = "mmif_oudofnieuw", values_to = "mmif_waarden") %>%
#   select(meetplaats, mmif_oudofnieuw, mmif_waarden, type_nieuw)
#
#
# ggplot(data = meetpunten_morfotype_mmif_lang, aes(x = mmif_waarden)) +
#   geom_histogram(
#     aes(fill = mmif_oudofnieuw),
#     position = "identity",
#     alpha = 0.5, # Maakt de kleuren transparant
#     bins = 30 # Stel het aantal bins in
#   ) +
#   scale_fill_manual(
#     values = c("mmif" = "#1F78B4", "mmif_nieuw" = "#E31A1C"),
#     name = "Variabele" # Pas de naam van de legende aan
#   ) +
#   labs(
#     title = "Verdeling mmif en mmif_nieuw",
#     x = "Waarde",
#     y = "Frequentie"
#   ) +
#   theme_minimal()
#

# statuut aanpassen ----

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
