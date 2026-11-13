dredge_data <- data_subset %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, jaar, owl, geom, maand, vhag,
    mmif, ept_prop, ep_tw, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log, ec_20, o2_verz_s, cl_s, spear_pesticides_s, spear_pesticides, spear_tu_estimated_s, spear_tu_estimated,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse),
    all_of(clean_fysico),
  ) %>%
  na.omit %>%
  st_as_sf

mapview(dredge_data, zcol = "spear_pesticides")

# Laad mapview (als dat nog niet gebeurd is)
library(mapview)

# Maak de kaart met een aangepast kleurenpalet en een nette legende-titel
mapview(dredge_data,
        zcol = "spear_pesticides",
        col.regions = colorRampPalette(c("red", "orange", "yellow", "green"))(100),
        layer.name = "SPEAR (Pesticiden)",
        alpha.regions = 0.8) # alpha maakt de bolletjes een beetje doorzichtig

test <- glmmTMB(spear_pesticides ~ intensiteit_combo_oeverzone_s + (1|owl) + (1|meetplaats),
                data= dredge_data,
                family = ordbeta)
summary(test)

spear_pesticides
