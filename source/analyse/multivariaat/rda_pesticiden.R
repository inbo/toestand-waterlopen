
load(file = "data/verwerkt/koppeling/koppeling_mi_pesticides.rdata")
load(file = here("data", "verwerkt", "polluenten", "tu_specific_groups_ec50_macroinvertebrates.rdata"))
koppeling_sleutel_pesticides <-
  koppeling_mi_pesticides %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )
# --- 1. KOPPELEN VAN DE DATA ---
# We vertrekken van je data_wide (voordat je de species_matrix loskoppelde)
data_wide_pest <- data_wide %>%
  # Koppel de sleutel (gebruik st_drop_geometry indien het ruimtelijke data is)
  left_join(koppeling_sleutel_pesticides %>% st_drop_geometry(),
            by = c("meetplaats", "monsternamedatum")) %>%
  # Koppel de effectieve pesticidendata
  left_join(tu_specific_groups_mi,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum")) %>%
  # LET OP: Verwijder de rijen waar we geen pesticidendata voor hebben gevonden
  # Vervang 'naam_van_een_pesticide_kolom' door een effectieve kolomnaam (bv. TU_totaal)
  drop_na(TU_max)

# --- 2. DATA OPSPLITSEN ---
# De metadata en omgevingsvariabelen
env_data <- data_wide_pest %>%
  select(deelmonster_id, monsternamedatum, meetplaats, statuut, groep,
         starts_with("TU_"), concentratie_pesticiden_sum, # Of de specifieke namen van je pesticidenkolommen
         qual_meetplaats, qual_monsternamedatum)

# Enkel de soortendata overhouden
species_matrix_rda <- data_wide_pest %>%
  select(-any_of(colnames(env_data)))

# --- 3. TRANSFORMATIE VAN SOORTENDATA (Cruciaal voor RDA!) ---
library(vegan)
species_hellinger <- decostand(species_matrix_rda, method = "hellinger")

# --- 4. RDA MODEL BOUWEN ---
# Vervang 'pesticide_var1' door jouw werkelijke kolomnamen (bv. TU_insecticiden, TU_herbiciden)
rda_model <- rda(species_hellinger ~ TU_insecticide + TU_max, data = env_data)

# Bekijk de proportie verklaarde variantie
summary(rda_model)

# --- 5. STATISTISCH TESTEN ---
# Is het volledige model significant?
anova(rda_model, permutations = 999)

# Welke specifieke pesticidenvariabelen zijn significant?
anova(rda_model, by = "margin", permutations = 999)

# --- 6. VISUALISATIE ---
# Basis RDA plot
plot(rda_model, scaling = 2, main = "RDA: Macroinvertebraten ~ Pesticiden")
