data_EC50_final %>%
  select(latin_name, ecotox_group) %>%
  distinct %>%
  print



# ==============================================================================
# STAP 7 & 8 (HERZIEN): MI-SPECIFIEKE AGGREGATIE OP BASIS VAN DATA GROEPEN
# ==============================================================================

# 1. Definieer de hiërarchie op basis van jouw unieke groepen
# Prio 1: De directe "drivers" voor SPEAR (Geleedpotigen)
prio_1_arthropoda <- c(
  "Crustaceans",
  "Crustaceans;Standard Test Species",
  "Insects/Spiders",
  "Insects/Spiders;Standard Test Species"
)

# Prio 2: Algemene ongewervelden (als prio 1 niet beschikbaar is)
prio_2_invertebrates <- c(
  "Invertebrates",
  "Invertebrates;Standard Test Species",
  "Molluscs",
  "Molluscs;Standard Test Species",
  "Molluscs;U.S. Invasive Species",
  "Worms",
  "Worms;Standard Test Species"
)

ec50_aggregated_mi <- data_EC50_final %>%
  # A. Filter: Alleen ongewervelden (dus GEEN Fish, Algae, Flowers, Fungi, Reptiles)
  filter(ecotox_group %in% c(prio_1_arthropoda, prio_2_invertebrates)) %>%

  # B. Ken prioriteit toe aan de groepen
  mutate(
    mi_priority = case_when(
      ecotox_group %in% prio_1_arthropoda ~ 1,
      ecotox_group %in% prio_2_invertebrates ~ 2,
      TRUE ~ 3
    )
  ) %>%

  # C. Filter per stof op Freshwater prioriteit (jouw bestaande logica)
  group_by(cas) %>%
  mutate(has_freshwater_data = any(priority_score == 1)) %>%
  filter(priority_score == 1 | !has_freshwater_data) %>%
  ungroup() %>%

  # D. Taxonomische filter: Als er data is voor Insects/Crustaceans, negeer dan de rest
  group_by(cas) %>%
  filter(mi_priority == min(mi_priority)) %>%
  ungroup() %>%

  # E. Kwaliteitsfilter: Prioriteer 'Standard Test Species' binnen de gekozen groep
  mutate(is_standard = grepl("Standard Test Species", ecotox_group, ignore.case = TRUE)) %>%
  group_by(cas) %>%
  filter(is_standard | !any(is_standard)) %>%
  ungroup() %>%

  # F. Bereken Geometrisch Gemiddelde per soort (bijv. alle Daphnia magna studies samen)
  group_by(cas, latin_name, ecotox_group) %>%
  summarise(
    ec50_species_geomean = geomean(ec50_ug_L),
    .groups = "drop"
  ) %>%

  # G. Finale stap: Kies de meest gevoelige soortwaarde per stof
  group_by(cas) %>%
  summarise(
    final_ec50_ug_L = min(ec50_species_geomean),
    sensitive_species = latin_name[which.min(ec50_species_geomean)],
    sensitive_group = ecotox_group[which.min(ec50_species_geomean)],
    n_species_used = n(),
    .groups = "drop"
  )

# Koppel terug aan je originele lijst
pesticiden_ec50_mi <- pesticiden_target %>%
  left_join(ec50_aggregated_mi, by = "cas")



# -------------------------------------------------------------------------
# STAP 1 & 2: KOPPELEN EN TU BEREKENEN (Vereenvoudigd)
# -------------------------------------------------------------------------

ec50_clean_mi <- pesticiden_ec50_mi %>%
  select(stof_symbool, cas, stof_naam, final_ec50_ug_L, sensitive_group, sensitive_species) %>%
  distinct(stof_symbool, .keep_all = TRUE)

tu_dataset_ruw_mi <- fc_data %>%
  # Selecteer relevante kolommen uit meetdata
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%

  # Koppel aan toxiciteitsdata
  # Alles uit fc_data dat niet in ec50_clean staat (zoals DDT t, Metalen, Nitraat)
  # verdwijnt hier automatisch.
  inner_join(ec50_clean_mi, by = c("parameter_symbool" = "stof_symbool")) %>%

  mutate(
    TU_individual = resultaat_ug_L / final_ec50_ug_L
  )


# -------------------------------------------------------------------------
# STAP 3: AGGREGEREN (SOMMEREN PER STAAL)
# -------------------------------------------------------------------------
#
# tu_per_sample <- tu_dataset_ruw_mi %>%
#   group_by(meetplaats, monsternamedatum) %>%
#   summarise(
#     # 1. De TU Som (msPAF benadering via Concentration Addition)
#     TU_sum = sum(TU_individual, na.rm = TRUE),
#     TU_max = max(TU_individual, na.rm = TRUE),
#     # 2. Metadata (Handig voor analyse achteraf)
#     concentratie_pesticiden_sum = sum(resultaat_ug_L),
#     aantal_pesticiden_gemeten = n(),
#     aantal_pesticiden_met_TU = sum(!is.na(TU_individual)),
#
#     # Welke stof draagt het meeste bij aan de toxiciteit in dit staal?
#     driver_stof = stof_naam[which.max(TU_individual)],
#     driver_TU = max(TU_individual, na.rm = TRUE),
#     driver_group = sensitive_group[which.max(TU_individual)],
#     driver_species = sensitive_species[which.max(TU_individual)],
#
#     .groups = "drop"
#   )

#hier specifieke subtype toevoegen om verschillende TU's te bereken extra

pesticiden_subtype <- chemische_stoffen %>%
  filter(type == "pesticide")

classificatie_info <- pesticiden_subtype %>%
  select(stof_symbool, subtype) %>% # Zorg dat kolomnamen matchen met tu_dataset_ruw
  distinct()

# We kijken in tu_dataset_ruw (dus stoffen die EN pesticide zijn EN een EC50 hebben)
#core stoffen selecteren
jaren_per_stof <- tu_dataset_ruw_mi %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(
    aantal_jaren_gemeten = n_distinct(jaar),
    eerste_jaar = min(jaar),
    laatste_jaar = max(jaar),
    # Extra check: hoe vaak kwam deze stof boven de detectielimiet/droeg hij bij aan TU?
    aantal_keren_bijdraag = n()
  ) %>%
  # Filter: Stoffen die in minstens 10 van de 15 jaren (2010-2024) data hebben
  filter(aantal_jaren_gemeten >= 10)

core_stoffen <- jaren_per_stof$parameter_symbool

tu_specific_groups_mi <- tu_dataset_ruw_mi %>%
  # Koppel de classificatie (als die er nog niet in zat)
  left_join(classificatie_info, by = c("parameter_symbool" = "stof_symbool")) %>%

  # Groepeer per staal
  group_by(meetplaats, monsternamedatum) %>%

  summarise(
    # 1. De Totaalscore (zoals je al had)
    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = max(TU_individual, na.rm = TRUE),
    TU_core_sum = sum(TU_individual[parameter_symbool %in% core_stoffen], na.rm = TRUE),

    concentratie_pesticiden_sum = sum(resultaat_ug_L),
    # 2. Specifieke scores (Mode of Action)
    # Let op: check in je data of 'insecticide' met hoofdletter is of niet
    TU_insecticide = sum(TU_individual[subtype == "insecticide"], na.rm = TRUE),
    TU_insecticide_max = if (any(subtype == "insecticide", na.rm = TRUE)) {
      max(TU_individual[subtype == "insecticide"], na.rm = TRUE)
    } else {
      0  # Gebruik 0 (geen toxiciteit) of NA (geen data)
    },
    TU_herbicide   = sum(TU_individual[subtype == "herbicide"], na.rm = TRUE),
    TU_fungicide   = sum(TU_individual[subtype == "fungicide"], na.rm = TRUE),
    concentratie_insecticide = sum(resultaat_ug_L[subtype == "insecticide"], na.rm = TRUE),
    TU_core_insecticide_max = if (any(subtype == "insecticide" & parameter_symbool %in% core_stoffen, na.rm = TRUE)) {
      max(TU_individual[subtype == "insecticide" & parameter_symbool %in% core_stoffen], na.rm = TRUE)
    } else {
      0
    },

    # 3. Neonicotinoïden specifiek (Optioneel, vaak interessant!)
    # Imidacloprid, Thiacloprid, Acetamiprid, etc.
    TU_neonicotinoids = sum(TU_individual[stof_naam %in% c("Imidacloprid", "Thiacloprid", "Acetamiprid", "Clothianidin", "Thiamethoxam")], na.rm = TRUE),

    .groups = "drop"
  )




#
#
#
#
# # ==============================================================================
# # STAP 9: KOPPELING AAN MEETDATA & TU BEREKENING
# # ==============================================================================
#
# tu_pesticiden_mi <- fc_data %>%
#   select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
#   # Koppel aan de MI-specifieke EC50 lijst
#   inner_join(pesticiden_ec50_mi, by = c("parameter_symbool" = "stof_symbool")) %>%
#   mutate(TU_mi = resultaat_ug_L / final_ec50_ug_L)
#
# tu_per_sample_mi <- tu_pesticiden_mi %>%
#   group_by(meetplaats, monsternamedatum) %>%
#   summarise(
#     TU_sum_mi = sum(TU_mi, na.rm = TRUE),
#     TU_max_mi = max(TU_mi, na.rm = TRUE), # Vaak de beste voorspeller voor SPEAR
#     n_pesticiden = n(),
#     driver_stof = sensitive_species[which.max(TU_mi)],
#     .groups = "drop"
#   )
