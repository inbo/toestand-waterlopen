
# Laad de vegan package (dé standaard voor ecologische multivariate analyse)
library(vegan)

# Stap 1: Definieer je blokken (matrices van je variabelen)
# Zorg dat er geen NAs in deze specifieke data zitten
chemie    <- test2[, c("ec_20_s", "p_h_s", "o2_s", "p_t_log")]
morfologie<- test2[, c("profiel_s", "verharding_oever_s")]
landgebruik <- test2[, c("verharding_afstr_s", "lozingen_riool_ie_log", "intensiteit_combo_afstr_s")]
klimaat   <- test2[, c("spei6_s", "n_extreme_3m_s", "jaar_s")]

# Stap 2: Run de variantiepartitionering op je respons (mmif)
vp_mmif <- varpart(test2$mmif, chemie, landgebruik, klimaat)

# Stap 3: Bekijk de wiskundige output (fracties van R-kwadraat)
print(vp_mmif)

# Stap 4: Teken het beroemde Venn-diagram!
plot(vp_mmif,
     bg = c("hotpink", "skyblue", "lightgreen", "orange"),
     Xnames = c("Chemie", "Landgebruik", "Klimaat"))
