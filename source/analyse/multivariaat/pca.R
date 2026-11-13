# Installeer factoextra als je deze nog niet hebt: install.packages("factoextra")
library(dplyr)
library(tidyr)
library(factoextra)

# 1. Definieer jouw lijst met variabelen
# pca_vars <- c("t_s", "ec_20_log", "zs_log", "n_t_log", "p_t_log",
#               "no2_log", "no3_log", "nh4_log", "intensiteit_combo_afstr_s",
#               "verharding_afstr_s", "breedte_diepte_ratio_s", "sinuositeit_s",
#               "bodemsub_s", "doodhout_s", "profiel_s", "ekc2_waterlichaam_s",
#               "ekc2_traject_s", "stroomsnelheid_s", "lozingen_rwzi_ie_log",
#               "lozingen_riool_ie_log", "lozingen_industrie_ie_log",
#               "overstorten_blootstelling_index_log", "spei6_s",
#               "n_extreme_3m_s", "p_sum_7d_s")
pca_vars <- c("t_s", "ec_20_log", "zs_log", "n_t_log", "p_t_log", "intensiteit_combo_afstr",
              "verharding_afstr", "ekc2_waterlichaam", "lozingen_rwzi_ie_log", "o2",
              "lozingen_riool_ie_log", "lozingen_industrie_ie_log", "verharding_oever", "intensiteit_combo_oeverzone", "natuur_oever", "natuur_afstr", "mmif",
              "overstorten_blootstelling_index_log", "spei6",
              "n_extreme_3m", "p_sum_7d")

# 2. Filter de data en verwijder rijen met NA's in deze kolommen
pca_data <- data_subset %>%
  select(all_of(pca_vars)) %>%
  drop_na()

# 3. Voer de PCA uit
# Belangrijk: scale. = TRUE is cruciaal! Je _s variabelen zijn al geschaald,
# maar je _log variabelen nog niet. Dit trekt alles wiskundig gelijk.
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Scree plot
fviz_eig(pca_result,
         addlabels = TRUE,
         ylim = c(0, 50),
         main = "Variantie verklaard per Principal Component")

# Variabelen plot
fviz_pca_var(pca_result,
             col.var = "contrib", # Kleur de pijlen op basis van hoe belangrijk ze zijn
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Van blauw (zwak) naar rood (sterk)
             repel = TRUE,        # Zorgt dat de tekstlabels elkaar niet overlappen
             title = "PCA van Omgevingsvariabelen (Dimensie 1 & 2)")
