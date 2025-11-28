
load(here("data", "verwerkt", "pesticiden_ec50.rdata"))

load(here("data", "verwerkt", "fc_data.rdata"))

# -------------------------------------------------------------------------
# STAP 1 & 2: KOPPELEN EN TU BEREKENEN (Vereenvoudigd)
# -------------------------------------------------------------------------

ec50_clean <- pesticiden_ec50 %>%
  select(stof_symbool, cas, stof_naam, final_ec50_ug_L, sensitive_group, sensitive_species) %>%
  distinct(stof_symbool, .keep_all = TRUE)

tu_dataset <- fc_data %>%
  # Selecteer relevante kolommen uit meetdata
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%

  # Koppel aan toxiciteitsdata
  # Alles uit fc_data dat niet in ec50_clean staat (zoals DDT t, Metalen, Nitraat)
  # verdwijnt hier automatisch.
  inner_join(ec50_clean, by = c("parameter_symbool" = "stof_symbool")) %>%

  mutate(
    TU_individual = resultaat_ug_L / final_ec50_ug_L
  )

# -------------------------------------------------------------------------
# STAP 3: AGGREGEREN (SOMMEREN PER STAAL)
# -------------------------------------------------------------------------

tu_per_sample <- tu_dataset %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    # 1. De TU Som (msPAF benadering via Concentration Addition)
    TU_sum = sum(TU_individual, na.rm = TRUE),

    # 2. Metadata (Handig voor analyse achteraf)
    aantal_pesticiden_gemeten = n(),
    aantal_pesticiden_met_TU = sum(!is.na(TU_individual)),

    # Welke stof draagt het meeste bij aan de toxiciteit in dit staal?
    driver_stof = stof_naam[which.max(TU_individual)],
    driver_TU = max(TU_individual, na.rm = TRUE),
    driver_group = sensitive_group[which.max(TU_individual)],
    driver_species = sensitive_species[which.max(TU_individual)],

    .groups = "drop"
  )

# -------------------------------------------------------------------------
# STAP 4: RESULTAAT BEKIJKEN & OPSLAAN
# -------------------------------------------------------------------------

cat("✅ TU Berekening Voltooid.\n")
cat("Aantal berekende samples:", nrow(tu_per_sample), "\n")
cat("Gemiddelde TU_sum:", mean(tu_per_sample$TU_sum, na.rm = T), "\n")
cat("Maximum TU_sum:", max(tu_per_sample$TU_sum, na.rm = T), "\n")
cat("Gemiddelde aantal_pesticiden_gemeten:", mean(tu_per_sample$aantal_pesticiden_gemeten, na.rm = T), "\n")

# Top 10 meest toxische stalen bekijken
print(tu_per_sample %>% arrange(desc(TU_sum)) %>% head(10))

# Opslaan
save(tu_per_sample, tu_dataset, file = here("data", "verwerkt", "tu_resultaten.rdata"))


# Check de EC50 waarden van de boosdoeners
check_drivers <- pesticiden_ec50 %>%
  filter(stof_naam %in% c("Malathion", "Diuron", "Imidacloprid")) %>%
  select(stof_naam, cas, final_ec50_ug_L, sensitive_group, sensitive_species)

print(check_drivers)
