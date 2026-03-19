# pakketten en data inladen

if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
load(here("data", "verwerkt", "mi_data.rdata"))
load(here("data", "verwerkt", "hm_data.rdata"))
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

#

hydmo_data <- mi_data_analyse %>%
  filter(jaar > 2009) %>%
  left_join(hm_data,
            by = c("meetplaats"))


NA_test <- data0 %>%
  filter(statuut != "Default") %>%
  # filter(is.na(ekc2_waterlichaam)) %>%
  select(meetplaats, owl.x, vhag.x, ekc2_waterlichaam, doodhout) %>%
  filter(is.na(ekc2_waterlichaam))

na_ekc <- hydromorf_nieuw_traject %>%
  filter(is.na(ekc_r_owl2_sgbp4))

mapview(na_ekc)


ggplot(data0, aes(log(sinuositeit + 1), ekc2_waterlichaam)) +
  geom_point()


hm_top <- hydromorf_nieuw_traject %>%
  filter(ekc_r_owl2_sgbp4 > 0.45)

mi_data_sf <- mi_data_analyse %>%
  filter(!statuut %in% c("Default")) %>%
  select(meetplaats, vhag) %>%
  unique()

mapview(hm_top) + mapview(mi_data_sf)


# 1. Selecteer de variabelen uit je model
# We pakken de namen direct uit je formule om fouten te voorkomen
model_vars <- c("n_t_log", "intensiteit_combo_s", "spei6_s", "n_extreme_3m_s",
                "verharding_afstr_s", "ekc2_waterlichaam_s", "o2_verz_fc_s",
                "jaar_s", "p_t_log", "overstorten_blootstelling_index_log",
                "lozingen_industrie_ie_log", "lozingen_rwzi_ie_log",
                "czv_s", "lozingen_riool_ie_log")

# 2. Maak een subset van de data en bereken de correlatiematrix
# We gebruiken 'pairwise.complete.obs' voor het geval er NA's zijn
cor_matrix <- cor(data_sem_clean[, model_vars], use = "pairwise.complete.obs")

# 3. Plot de correlatiematrix
library(corrplot)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust", # Groepeert variabelen die op elkaar lijken
         addCoef.col = "black", # Voeg de getallen toe voor precisie
         tl.col = "black",
         tl.srt = 45, # Draai de labels voor leesbaarheid
         diag = FALSE,
         number.cex = 0.7, # Grootte van de getallen
         title = "Correlatieplot Model Variabelen",
         mar = c(0,0,1,0))
library(performance)
check_collinearity(m2)
