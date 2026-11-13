# Laad de nodige packages
library(dplyr)
library(factoextra) # Voor prachtige PCA plots

####################
#data klaarmaken #
#####################
data_subset <- mi_nat_sv_beek %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl, geom, vhag,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw, fdisp_full, fdisp_habitat, fdisp_waterkwaliteit,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs, cl, spear_pesticides, spear_tu_estimated,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone, water_afstr, water_oever,
         spei6, n_extreme_3m, p_sum_7d,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                fdisp_2 = fdisp_full^2,
                fdisp_s = fdisp_full / max(fdisp_full, na.rm = TRUE),
                fdisp_waterkwaliteit_s = fdisp_waterkwaliteit / (max(fdisp_waterkwaliteit, na.rm = TRUE) + 0.01),
                bekken = as.factor(bekken),
                maand = as.factor(lubridate::month(monsternamedatum)),
                vhag = as.factor(vhag),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                zs_log = log(zs),
                ec_20_log = log(ec_20),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1))

dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, vhag,
    mmif, ept_prop, ep_tw, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, ec_20, o2_verz_s, cl_s, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(raw_landuse),
    water_afstr_s, water_oever_s,
    all_of(clean_fysico),
  ) %>%
  na.omit %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  )


data_landgebruik_beek <- dredge_data %>%
  select(meetplaats, monsternamedatum, all_of(raw_landuse), water_afstr_s, water_oever_s)


########
# PCA
##########

# 1. Selecteer enkel de numerieke landgebruiksvariabelen
landgebruik_matrix <- data_landgebruik_beek %>%
  select(ends_with("_s"))

# 2. Voer de PCA uit
# Omdat jouw data al geschaald is (de "_s" op het eind), hoef je in theorie
# niet nog eens te schalen, maar scale. = TRUE is de veiligste standaard.
pca_result <- prcomp(landgebruik_matrix, center = TRUE, scale. = TRUE)

# Bekijk snel hoeveel variantie je eerste assen verklaren
summary(pca_result)

# Teken de biplot
fviz_pca_biplot(pca_result,
                # Opmaak van de metingen (puntjes)
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = "lightblue",
                col.ind = "black",
                alpha.ind = 0.6,

                # Opmaak van de variabelen (pijlen)
                col.var = "darkred",
                repel = TRUE, # Zorgt dat de tekstlabels elkaar niet overlappen

                # Titels en assen
                title = "PCA Biplot: Gradiënten in Landgebruik (Beek)",
                xlab = paste0("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)"),
                ylab = paste0("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)")) +
  theme_minimal()

#####
## biplot tijd
####

# Extra package nodig om makkelijk het jaar uit je datum te trekken
library(lubridate)
library(dplyr)
library(factoextra)

# 1. Voeg een 'jaar' kolom toe aan je originele dataset
data_landgebruik <- data_landgebruik_beek %>%
  mutate(jaar = lubridate::year(monsternamedatum))

# 2. Selecteer enkel de numerieke variabelen voor de PCA
landgebruik_matrix <- data_landgebruik_beek %>%
  select(ends_with("_s"))

# 3. Voer de PCA uit
pca_result <- prcomp(landgebruik_matrix, center = TRUE, scale. = TRUE)

# 4. Teken de biplot met tijd-gradiënt
fviz_pca_biplot(pca_result,
                geom.ind = "point",
                pointsize = 2.5,
                alpha.ind = 0.8,

                # Kleur de puntjes in op basis van het jaar!
                col.ind = data_landgebruik$jaar,
                # Kies een duidelijk kleurenpalet: bijv. van koel (blauw/oud) naar warm (rood/nieuw)
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = "Jaar",

                # Maak de pijlen (variabelen) zwart en duidelijk zodat ze opvallen
                col.var = "black",
                repel = TRUE,

                title = "PCA Biplot: Evolutie van Landgebruik in de tijd",
                xlab = paste0("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)"),
                ylab = paste0("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)")) +
  theme_minimal()

#########
# Kempen
########

#kempen
data_subset <- mi_nat_sv_kempen %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl, geom, vhag,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw, fdisp_full, fdisp_habitat, fdisp_waterkwaliteit,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs, cl, spear_pesticides, spear_tu_estimated,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone, water_afstr, water_oever,
         spei6, n_extreme_3m, p_sum_7d,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                fdisp_2 = fdisp_full^2,
                fdisp_s = fdisp_full / max(fdisp_full, na.rm = TRUE),
                fdisp_waterkwaliteit_s = fdisp_waterkwaliteit / (max(fdisp_waterkwaliteit, na.rm = TRUE) + 0.01),
                bekken = as.factor(bekken),
                maand = as.factor(lubridate::month(monsternamedatum)),
                vhag = as.factor(vhag),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                zs_log = log(zs),
                ec_20_log = log(ec_20),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1))

dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, vhag,
    mmif, ept_prop, ep_tw, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, ec_20, o2_verz_s, cl_s, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(raw_landuse),
    water_afstr_s, water_oever_s,
    all_of(clean_fysico),
  ) %>%
  na.omit %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  )


data_landgebruik_kempen <- dredge_data %>%
  select(meetplaats, monsternamedatum, all_of(raw_landuse), water_afstr_s, water_oever_s)

########
# PCA
##########

# 1. Selecteer enkel de numerieke landgebruiksvariabelen
landgebruik_matrix <- data_landgebruik_kempen %>%
  select(ends_with("_s"))

# 2. Voer de PCA uit
# Omdat jouw data al geschaald is (de "_s" op het eind), hoef je in theorie
# niet nog eens te schalen, maar scale. = TRUE is de veiligste standaard.
pca_result <- prcomp(landgebruik_matrix, center = TRUE, scale. = TRUE)

# Bekijk snel hoeveel variantie je eerste assen verklaren
summary(pca_result)

# Teken de biplot
fviz_pca_biplot(pca_result,
                # Opmaak van de metingen (puntjes)
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = "lightblue",
                col.ind = "black",
                alpha.ind = 0.6,

                # Opmaak van de variabelen (pijlen)
                col.var = "darkred",
                repel = TRUE, # Zorgt dat de tekstlabels elkaar niet overlappen

                # Titels en assen
                title = "PCA Biplot: Gradiënten in Landgebruik (kempen)",
                xlab = paste0("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)"),
                ylab = paste0("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)")) +
  theme_minimal()

#####
## biplot tijd
####

# Extra package nodig om makkelijk het jaar uit je datum te trekken
library(lubridate)
library(dplyr)
library(factoextra)

# 1. Voeg een 'jaar' kolom toe aan je originele dataset
data_landgebruik <- data_landgebruik_kempen %>%
  mutate(jaar = lubridate::year(monsternamedatum))

# 2. Selecteer enkel de numerieke variabelen voor de PCA
landgebruik_matrix <- data_landgebruik_kempen %>%
  select(ends_with("_s"))

# 3. Voer de PCA uit
pca_result <- prcomp(landgebruik_matrix, center = TRUE, scale. = TRUE)

# 4. Teken de biplot met tijd-gradiënt
fviz_pca_biplot(pca_result,
                geom.ind = "point",
                pointsize = 2.5,
                alpha.ind = 0.8,

                # Kleur de puntjes in op basis van het jaar!
                col.ind = data_landgebruik$jaar,
                # Kies een duidelijk kleurenpalet: bijv. van koel (blauw/oud) naar warm (rood/nieuw)
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = "Jaar",

                # Maak de pijlen (variabelen) zwart en duidelijk zodat ze opvallen
                col.var = "black",
                repel = TRUE,

                title = "PCA Biplot: Evolutie van Landgebruik in de tijd",
                xlab = paste0("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)"),
                ylab = paste0("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)")) +
  theme_minimal()

#########
# rivier
########

#rivier
data_subset <- mi_nat_sv_rivier %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, owl, geom, vhag,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw, fdisp_full, fdisp_habitat, fdisp_waterkwaliteit,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs, cl, spear_pesticides, spear_tu_estimated,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone, water_afstr, water_oever,
         spei6, n_extreme_3m, p_sum_7d,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                fdisp_2 = fdisp_full^2,
                fdisp_s = fdisp_full / max(fdisp_full, na.rm = TRUE),
                fdisp_waterkwaliteit_s = fdisp_waterkwaliteit / (max(fdisp_waterkwaliteit, na.rm = TRUE) + 0.01),
                bekken = as.factor(bekken),
                maand = as.factor(lubridate::month(monsternamedatum)),
                vhag = as.factor(vhag),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                zs_log = log(zs),
                ec_20_log = log(ec_20),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1))

dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, vhag,
    mmif, ept_prop, ep_tw, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, ec_20, o2_verz_s, cl_s, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(raw_landuse),
    water_afstr_s, water_oever_s,
    all_of(clean_fysico),
  ) %>%
  na.omit %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  )


data_landgebruik_rivier <- dredge_data %>%
  select(meetplaats, monsternamedatum, all_of(raw_landuse), water_afstr_s, water_oever_s)

########
# PCA
##########

# 1. Selecteer enkel de numerieke landgebruiksvariabelen
landgebruik_matrix <- data_landgebruik_rivier %>%
  select(ends_with("_s"))

# 2. Voer de PCA uit
# Omdat jouw data al geschaald is (de "_s" op het eind), hoef je in theorie
# niet nog eens te schalen, maar scale. = TRUE is de veiligste standaard.
pca_result <- prcomp(landgebruik_matrix, center = TRUE, scale. = TRUE)

# Bekijk snel hoeveel variantie je eerste assen verklaren
summary(pca_result)

# Teken de biplot
fviz_pca_biplot(pca_result,
                # Opmaak van de metingen (puntjes)
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = "lightblue",
                col.ind = "black",
                alpha.ind = 0.6,

                # Opmaak van de variabelen (pijlen)
                col.var = "darkred",
                repel = TRUE, # Zorgt dat de tekstlabels elkaar niet overlappen

                # Titels en assen
                title = "PCA Biplot: Gradiënten in Landgebruik (rivier)",
                xlab = paste0("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)"),
                ylab = paste0("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)")) +
  theme_minimal()

#####
## biplot tijd
####

# Extra package nodig om makkelijk het jaar uit je datum te trekken
library(lubridate)
library(dplyr)
library(factoextra)

# 1. Voeg een 'jaar' kolom toe aan je originele dataset
data_landgebruik <- data_landgebruik_rivier %>%
  mutate(jaar = lubridate::year(monsternamedatum))

# 2. Selecteer enkel de numerieke variabelen voor de PCA
landgebruik_matrix <- data_landgebruik_rivier %>%
  select(ends_with("_s"))

# 3. Voer de PCA uit
pca_result <- prcomp(landgebruik_matrix, center = TRUE, scale. = TRUE)

# 4. Teken de biplot met tijd-gradiënt
fviz_pca_biplot(pca_result,
                geom.ind = "point",
                pointsize = 2.5,
                alpha.ind = 0.8,

                # Kleur de puntjes in op basis van het jaar!
                col.ind = data_landgebruik$jaar,
                # Kies een duidelijk kleurenpalet: bijv. van koel (blauw/oud) naar warm (rood/nieuw)
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = "Jaar",

                # Maak de pijlen (variabelen) zwart en duidelijk zodat ze opvallen
                col.var = "black",
                repel = TRUE,

                title = "PCA Biplot: Evolutie van Landgebruik in de tijd",
                xlab = paste0("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)"),
                ylab = paste0("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)")) +
  theme_minimal()
