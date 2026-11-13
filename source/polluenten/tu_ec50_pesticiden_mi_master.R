# ==============================================================================
# STAP 0: SETUP, BIBLIOTHEKEN & HULPFUNCTIES
# ==============================================================================
pacman::p_load(ECOTOXr, dplyr, tidyr, webchem, here, readr, lubridate)

# 1. Hulpfunctie voor Geometrisch Gemiddelde
geomean <- function(x) {
  x_clean <- x[!is.na(x) & x > 0]
  if(length(x_clean) == 0) return(NA)
  exp(mean(log(x_clean)))
}

# 2. Hulpfunctie voor veilige maxima (vermijdt -Inf bij lege groepen)
safe_max <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(0)
  max(x_clean, na.rm = TRUE)
}

# Check database beschikbaarheid
if (!check_ecotox_availability()) {
  cat("ECOTOX data niet gevonden. Start download...\n")
  download_ecotox_data(ask = FALSE, verify_ssl = FALSE)
  build_ecotox_sqlite(source = "data/ruw/pesticiden/ecotox_ascii") # Pas pad aan indien nodig
}

# Laad de benodigde lokale bestanden (Zorg dat deze objecten in je environment zitten)
load(here("data", "verwerkt", "fc_data.rdata"))
chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"),
                                header = TRUE, sep = ";", na.strings = "nvt")

pesticiden_target <- chemische_stoffen %>%
  filter(type == "pesticide", subtype != "mengsel")
target_cas_list <- pesticiden_target %>% pull(cas)

# ==============================================================================
# STAP 1 & 2: DATA EXTRACTIE & OPSCHONING (Basis)
# ==============================================================================
cat("⏳ Data ophalen en basis filteren...\n")

output_columns <- c(
  "results.endpoint", "results.conc1_mean_op", "results.conc1_mean", "results.conc1_unit",
  "chemicals.chemical_name", "chemicals.cas_number",
  "species.latin_name", "species.ecotox_group", "species.class",
  "results.result_id", "tests.organism_habitat", "tests.media_type"
)

data_EC50_scope <- search_ecotox(list(endpoint = list(terms = "EC50", method = "contains")), output_columns) %>%
  filter(
    conc1_mean != "NR" & conc1_mean != "0" & conc1_mean != "",
    conc1_mean_op %in% c("", "~"),
    organism_habitat == "Water"
  ) %>%
  mutate(
    conc1_value_num = as.numeric(gsub("[^0-9.-]", "", conc1_mean)),
    cas = as.character(ECOTOXr::as.cas(test_cas)),
    media_clean = trimws(gsub("/", "", media_type)),
    media_final = case_when(media_clean %in% c("NC", "NR", "") ~ "FW", TRUE ~ media_clean),
    priority_score = case_when(
      media_final %in% c("FW", "CUL") ~ 1,
      media_final == "BW" ~ 2,
      media_final == "SW" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  filter(priority_score < 99, cas %in% target_cas_list)

# ==============================================================================
# STAP 3: MOLECUULGEWICHTEN VIA WEBCHEM (Met Caching)
# ==============================================================================
# Tip: We slaan dit lokaal op om API-limieten en netwerkfouten te voorkomen.
mw_cache_file <- here("data", "verwerkt", "polluenten", "mol_weights_cache.rds")

if (file.exists(mw_cache_file)) {
  cat("✅ Molecuulgewichten geladen vanuit lokale cache.\n")
  mol_weight_lookup <- readRDS(mw_cache_file)
} else {
  cat("⏳ Ophalen molecuulgewichten via WebChem (eerste keer duurt even)...\n")
  unieke_cas <- unique(data_EC50_scope$cas[!is.na(data_EC50_scope$cas)])
  cids <- get_cid(unieke_cas, from = "cas", domain = "compound", match = "first") %>% filter(!is.na(cid))
  props <- pc_prop(cids$cid, properties = "MolecularWeight") %>% rename(cid = CID)

  mol_weight_lookup <- cids %>%
    left_join(props, by = "cid") %>%
    select(cas = query, mol_weight = MolecularWeight) %>%
    mutate(mol_weight = as.numeric(mol_weight)) %>%
    distinct(cas, .keep_all = TRUE)

  saveRDS(mol_weight_lookup, file = mw_cache_file)
}

# Eenheden converteren naar ug/L
data_EC50_final <- data_EC50_scope %>%
  left_join(mol_weight_lookup, by = "cas") %>%
  mutate(
    is_log = grepl("\\(log\\)", endpoint, ignore.case = TRUE),
    conc1_value_corr = ifelse(is_log, 10^conc1_value_num, conc1_value_num),
    unit_clean = tolower(trimws(gsub("ai |ae |\\(ai\\)| media", "", conc1_unit))),
    factor_to_ug_L = case_when(
      unit_clean %in% c("ug/l", "ppb", "ug/dm3", "um", "umol/l") ~ 1,
      unit_clean %in% c("mg/l", "ppm", "g/m3", "ug/ml", "mm", "mmol/l") ~ 1000,
      unit_clean %in% c("ng/l", "ppt", "nm", "nmol/l") ~ 0.001,
      unit_clean %in% c("m", "mol/l", "mol") ~ 1e6,
      unit_clean == "ppm" & media_final == "SW" ~ 1025,
      TRUE ~ NA_real_
    ),
    ec50_ug_L = case_when(
      grepl("mol|m$|mm$|um$|nm$", unit_clean) ~ conc1_value_corr * factor_to_ug_L * mol_weight,
      TRUE ~ conc1_value_corr * factor_to_ug_L
    )
  ) %>%
  filter(!is.na(ec50_ug_L))

# ==============================================================================
# STAP 4: MI-SPECIFIEKE AGGREGATIE (Arthropoden Prioriteit)
# ==============================================================================
cat("⏳ Macroinvertebraat-specifieke EC50 berekenen...\n")

prio_1_arthropoda <- c("Crustaceans", "Crustaceans;Standard Test Species", "Insects/Spiders", "Insects/Spiders;Standard Test Species")
prio_2_invertebrates <- c("Invertebrates", "Invertebrates;Standard Test Species", "Molluscs", "Molluscs;Standard Test Species", "Molluscs;U.S. Invasive Species", "Worms", "Worms;Standard Test Species")

ec50_mi_clean <- data_EC50_final %>%
  filter(ecotox_group %in% c(prio_1_arthropoda, prio_2_invertebrates)) %>%
  mutate(mi_priority = ifelse(ecotox_group %in% prio_1_arthropoda, 1, 2)) %>%

  # A. Freshwater filter per stof
  group_by(cas) %>%
  mutate(has_freshwater_data = any(priority_score == 1)) %>%
  filter(priority_score == 1 | !has_freshwater_data) %>%

  # B. Arthropoda prioriteit per stof
  filter(mi_priority == min(mi_priority)) %>%

  # C. Standard Species prioriteit per stof
  mutate(is_standard = grepl("Standard Test Species", ecotox_group, ignore.case = TRUE)) %>%
  filter(is_standard | !any(is_standard)) %>%

  # D. Geomean per soort
  group_by(cas, latin_name, ecotox_group) %>%
  summarise(ec50_species_geomean = geomean(ec50_ug_L), .groups = "drop") %>%

  # E. Gevoeligste soort per stof
  group_by(cas) %>%
  summarise(
    final_ec50_ug_L = min(ec50_species_geomean),
    sensitive_species = latin_name[which.min(ec50_species_geomean)],
    sensitive_group = ecotox_group[which.min(ec50_species_geomean)],
    .groups = "drop"
  )

# ==============================================================================
# STAP 5: TU BEREKENING & AGGREGATIE PER STAAL (Master Dataset)
# ==============================================================================
cat("⏳ Master TU dataset opbouwen...\n")

# Koppel EC50 lijst aan de veldmetingen
tu_dataset_ruw_mi <- fc_data %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
  inner_join(
    pesticiden_target %>% left_join(ec50_mi_clean, by = "cas") %>% select(stof_symbool, final_ec50_ug_L, stof_naam),
    by = c("parameter_symbool" = "stof_symbool")
  ) %>%
  filter(!is.na(final_ec50_ug_L)) %>%
  mutate(TU_individual = resultaat_ug_L / final_ec50_ug_L)

# Dynamisch core-stoffen bepalen (>50% van de actieve meetjaren)
totaal_aantal_jaren <- n_distinct(lubridate::year(tu_dataset_ruw_mi$monsternamedatum))
drempel_jaren <- ceiling(totaal_aantal_jaren * 0.5)

core_stoffen <- tu_dataset_ruw_mi %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(jaren_gemeten = n_distinct(jaar)) %>%
  filter(jaren_gemeten >= drempel_jaren) %>%
  pull(parameter_symbool)

# De grote aggregatie: Eén rij per staal met alle relevante indexen
tu_master_mi <- tu_dataset_ruw_mi %>%
  left_join(chemische_stoffen %>% select(stof_symbool, subtype) %>% distinct(), by = c("parameter_symbool" = "stof_symbool")) %>%
  mutate(is_core = parameter_symbool %in% core_stoffen) %>%

  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    # Algemeen
    aantal_detecties = n(),
    concentratie_som = sum(resultaat_ug_L, na.rm = TRUE),

    # Totalen
    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = safe_max(TU_individual),

    # Core stoffen
    TU_core_sum = sum(TU_individual[is_core], na.rm = TRUE),
    TU_core_max = safe_max(TU_individual[is_core]),

    # Modus of Action (Belangrijk voor MI)
    TU_insecticide_sum = sum(TU_individual[subtype == "insecticide"], na.rm = TRUE),
    TU_insecticide_max = safe_max(TU_individual[subtype == "insecticide"]),
    TU_herbicide_sum   = sum(TU_individual[subtype == "herbicide"], na.rm = TRUE),
    TU_fungicide_sum   = sum(TU_individual[subtype == "fungicide"], na.rm = TRUE),

    # Specifieke groepen (Neonicotinoïden)
    TU_neonics_sum = sum(TU_individual[stof_naam %in% c("Imidacloprid", "Thiacloprid", "Acetamiprid", "Clothianidin", "Thiamethoxam")], na.rm = TRUE),

    # Identificatie van de piek
    driver_stof = stof_naam[which.max(TU_individual)],
    driver_TU = safe_max(TU_individual),

    .groups = "drop"
  )

# Opslaan
save(tu_master_mi, tu_dataset_ruw_mi, ec50_mi_clean, file = here("data", "verwerkt", "polluenten", "tu_master_macroinvertebrates.rdata"))
cat("✅ Script succesvol afgerond. Data opgeslagen in 'tu_master_macroinvertebrates.rdata'.\n")

# ==============================================================================
# STAP 4b: MACROFYT-SPECIFIEKE AGGREGATIE (Planten Prioriteit)
# ==============================================================================
cat("⏳ Macrofyt-specifieke EC50 berekenen...\n")

# Prio 1: Echte waterplanten (Vaatplanten en mossen)
# (Inclusief de standaard testsoorten zoals Lemna minor en Myriophyllum spicatum)
prio_1_macrophytes <- c(
  "Flowers, Trees, Shrubs, Ferns",
  "Flowers, Trees, Shrubs, Ferns;Standard Test Species",
  "Mosses",
  "Mosses;Standard Test Species"
)

# Prio 2: Algen (Als ecologische proxy indien Prio 1 ontbreekt)
prio_2_algae <- c(
  "Algae",
  "Algae;Standard Test Species"
)

ec50_mafy_clean <- data_EC50_final %>%
  # A. Filter op planten en algen (dieren gaan eruit)
  filter(ecotox_group %in% c(prio_1_macrophytes, prio_2_algae)) %>%

  # B. Ken prioriteit toe aan de groepen
  mutate(mafy_priority = case_when(
    ecotox_group %in% prio_1_macrophytes ~ 1,
    ecotox_group %in% prio_2_algae ~ 2,
    TRUE ~ 3
  )) %>%

  # C. Freshwater filter per stof
  group_by(cas) %>%
  mutate(has_freshwater_data = any(priority_score == 1)) %>%
  filter(priority_score == 1 | !has_freshwater_data) %>%

  # D. Taxonomische prioriteit: Vaatplanten > Algen
  filter(mafy_priority == min(mafy_priority)) %>%

  # E. Standard Species prioriteit per stof
  mutate(is_standard = grepl("Standard Test Species", ecotox_group, ignore.case = TRUE)) %>%
  filter(is_standard | !any(is_standard)) %>%

  # F. Geomean per soort berekenen (voor het geval er 20 Lemna studies zijn)
  group_by(cas, latin_name, ecotox_group) %>%
  summarise(ec50_species_geomean = geomean(ec50_ug_L), .groups = "drop") %>%

  # G. Gevoeligste plant/alg soort per stof selecteren
  group_by(cas) %>%
  summarise(
    final_ec50_ug_L = min(ec50_species_geomean),
    sensitive_species = latin_name[which.min(ec50_species_geomean)],
    sensitive_group = ecotox_group[which.min(ec50_species_geomean)],
    .groups = "drop"
  )

# ==============================================================================
# STAP 5b: TU BEREKENING & AGGREGATIE PER STAAL (Master Dataset)
# ==============================================================================
cat("⏳ Master TU dataset opbouwen...\n")

# Koppel EC50 lijst aan de veldmetingen
tu_dataset_ruw_mafy <- fc_data %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
  inner_join(
    pesticiden_target %>% left_join(ec50_mafy_clean, by = "cas") %>% select(stof_symbool, final_ec50_ug_L, stof_naam),
    by = c("parameter_symbool" = "stof_symbool")
  ) %>%
  filter(!is.na(final_ec50_ug_L)) %>%
  mutate(TU_individual = resultaat_ug_L / final_ec50_ug_L)

# Dynamafysch core-stoffen bepalen (>50% van de actieve meetjaren)
totaal_aantal_jaren <- n_distinct(lubridate::year(tu_dataset_ruw_mafy$monsternamedatum))
drempel_jaren <- ceiling(totaal_aantal_jaren * 0.5)

core_stoffen <- tu_dataset_ruw_mafy %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(jaren_gemeten = n_distinct(jaar)) %>%
  filter(jaren_gemeten >= drempel_jaren) %>%
  pull(parameter_symbool)

# De grote aggregatie: Eén rij per staal met alle relevante indexen
tu_master_mafy <- tu_dataset_ruw_mafy %>%
  left_join(chemische_stoffen %>% select(stof_symbool, subtype) %>% distinct(), by = c("parameter_symbool" = "stof_symbool")) %>%
  mutate(is_core = parameter_symbool %in% core_stoffen) %>%

  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    # Algemeen
    aantal_detecties = n(),
    concentratie_som = sum(resultaat_ug_L, na.rm = TRUE),

    # Totalen
    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = safe_max(TU_individual),

    # Core stoffen
    TU_core_sum = sum(TU_individual[is_core], na.rm = TRUE),
    TU_core_max = safe_max(TU_individual[is_core]),

    # Modus of Action (Belangrijk voor mafy)
    TU_insecticide_sum = sum(TU_individual[subtype == "insecticide"], na.rm = TRUE),
    TU_insecticide_max = safe_max(TU_individual[subtype == "insecticide"]),
    TU_herbicide_sum   = sum(TU_individual[subtype == "herbicide"], na.rm = TRUE),
    TU_fungicide_sum   = sum(TU_individual[subtype == "fungicide"], na.rm = TRUE),

    # Specifieke groepen (Neonicotinoïden)
    TU_neonics_sum = sum(TU_individual[stof_naam %in% c("Imafydacloprid", "Thiacloprid", "Acetamafyprid", "Clothianidin", "Thiamethoxam")], na.rm = TRUE),

    # Identificatie van de piek
    driver_stof = stof_naam[which.max(TU_individual)],
    driver_TU = safe_max(TU_individual),

    .groups = "drop"
  )

# Opslaan
save(tu_master_mafy, tu_dataset_ruw_mafy, ec50_mafy_clean, file = here("data", "verwerkt", "polluenten", "tu_master_macrophytes.rdata"))
cat("✅ Script succesvol afgerond. Data opgeslagen in 'tu_master_macroinvertebrates.rdata'.\n")
