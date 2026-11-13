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

# AANPASSING 1: Selecteer PAKs uit de classificatie
pak_target <- chemische_stoffen %>%
  filter(type == "PAK", subtype != "mengsel", !is.na(cas))
target_cas_list <- unique(pak_target$cas)

# ==============================================================================
# STAP 1 & 2: DATA EXTRACTIE & OPSCHONING (Basis)
# ==============================================================================
cat("⏳ Data ophalen en basis filteren voor PAKs...\n")

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
mw_cache_file <- here("data", "verwerkt", "polluenten", "mol_weights_paks_cache.rds")

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

ec50_mi_clean_pak <- data_EC50_final %>%
  filter(ecotox_group %in% c(prio_1_arthropoda, prio_2_invertebrates)) %>%
  mutate(mi_priority = ifelse(ecotox_group %in% prio_1_arthropoda, 1, 2)) %>%
  group_by(cas) %>%
  mutate(has_freshwater_data = any(priority_score == 1)) %>%
  filter(priority_score == 1 | !has_freshwater_data) %>%
  filter(mi_priority == min(mi_priority)) %>%
  mutate(is_standard = grepl("Standard Test Species", ecotox_group, ignore.case = TRUE)) %>%
  filter(is_standard | !any(is_standard)) %>%
  group_by(cas, latin_name, ecotox_group) %>%
  summarise(ec50_species_geomean = geomean(ec50_ug_L), .groups = "drop") %>%
  group_by(cas) %>%
  summarise(
    final_ec50_ug_L = min(ec50_species_geomean),
    sensitive_species = latin_name[which.min(ec50_species_geomean)],
    sensitive_group = ecotox_group[which.min(ec50_species_geomean)],
    .groups = "drop"
  )

# ==============================================================================
# STAP 5: TU BEREKENING & AGGREGATIE PER STAAL (Master Dataset MI)
# ==============================================================================
cat("⏳ Master TU dataset opbouwen (Macroinvertebraten)...\n")

tu_dataset_ruw_mi_pak <- fc_data %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
  inner_join(
    pak_target %>% select(stof_symbool, cas, stof_naam),
    by = c("parameter_symbool" = "stof_symbool")
  ) %>%
  inner_join(ec50_mi_clean_pak, by = "cas") %>%
  filter(!is.na(final_ec50_ug_L)) %>%
  mutate(TU_individual = resultaat_ug_L / final_ec50_ug_L)

# Dynamisch core-stoffen bepalen
totaal_aantal_jaren <- n_distinct(lubridate::year(tu_dataset_ruw_mi_pak$monsternamedatum))
drempel_jaren <- ceiling(totaal_aantal_jaren * 0.5)

core_stoffen <- tu_dataset_ruw_mi_pak %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(jaren_gemeten = n_distinct(jaar)) %>%
  filter(jaren_gemeten >= drempel_jaren) %>%
  pull(parameter_symbool)

target_paks_cas <- c(
  "120-12-7", # Anthraceen
  "50-32-8",  # Benzo(a)pyreen
  "205-99-2", # Benzo(b)fluorantheen
  "207-08-9", # Benzo(k)fluorantheen
  "191-24-2", # Benzo(g,h,i)peryleen
  "193-39-5", # Indeno(1,2,3-cd)pyreen
  "206-44-0", # Fluorantheen
  "91-20-3"   # Naftaleen
)

tu_master_mi_pak <- tu_dataset_ruw_mi_pak %>%
  left_join(chemische_stoffen %>% select(stof_symbool, subtype) %>% distinct(), by = c("parameter_symbool" = "stof_symbool")) %>%
  mutate(is_core = parameter_symbool %in% core_stoffen,
         is_target_pak = cas %in% target_paks_cas) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    aantal_detecties = n(),
    concentratie_som = sum(resultaat_ug_L, na.rm = TRUE),

    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = safe_max(TU_individual),

    TU_core_sum = sum(TU_individual[is_core], na.rm = TRUE),
    TU_core_max = safe_max(TU_individual[is_core]),

    # PAK specifieke groepen: Licht en Zwaar
    TU_licht_sum = sum(TU_individual[subtype == "licht"], na.rm = TRUE),
    TU_licht_max = safe_max(TU_individual[subtype == "licht"]),
    TU_zwaar_sum = sum(TU_individual[subtype == "zwaar"], na.rm = TRUE),
    TU_zwaar_max = safe_max(TU_individual[subtype == "zwaar"]),
    TU_target_paks_sum = sum(TU_individual[is_target_pak], na.rm = TRUE),
    TU_target_paks_max = safe_max(TU_individual[is_target_pak]),
    driver_stof = stof_naam[which.max(TU_individual)],
    driver_TU = safe_max(TU_individual),

    .groups = "drop"
  )

save(tu_master_mi_pak, tu_dataset_ruw_mi_pak, ec50_mi_clean_pak, file = here("data", "verwerkt", "polluenten", "tu_master_paks_macroinvertebrates.rdata"))
cat("✅ Script succesvol afgerond. Data opgeslagen in 'tu_master_paks_macroinvertebrates.rdata'.\n")

# ==============================================================================
# STAP 4b: MACROFYT-SPECIFIEKE AGGREGATIE (Planten Prioriteit)
# ==============================================================================
cat("⏳ Macrofyt-specifieke EC50 berekenen...\n")

prio_1_macrophytes <- c(
  "Flowers, Trees, Shrubs, Ferns",
  "Flowers, Trees, Shrubs, Ferns;Standard Test Species",
  "Mosses",
  "Mosses;Standard Test Species"
)

prio_2_algae <- c(
  "Algae",
  "Algae;Standard Test Species"
)

ec50_mafy_clean_pak <- data_EC50_final %>%
  filter(ecotox_group %in% c(prio_1_macrophytes, prio_2_algae)) %>%
  mutate(mafy_priority = case_when(
    ecotox_group %in% prio_1_macrophytes ~ 1,
    ecotox_group %in% prio_2_algae ~ 2,
    TRUE ~ 3
  )) %>%
  group_by(cas) %>%
  mutate(has_freshwater_data = any(priority_score == 1)) %>%
  filter(priority_score == 1 | !has_freshwater_data) %>%
  filter(mafy_priority == min(mafy_priority)) %>%
  mutate(is_standard = grepl("Standard Test Species", ecotox_group, ignore.case = TRUE)) %>%
  filter(is_standard | !any(is_standard)) %>%
  group_by(cas, latin_name, ecotox_group) %>%
  summarise(ec50_species_geomean = geomean(ec50_ug_L), .groups = "drop") %>%
  group_by(cas) %>%
  summarise(
    final_ec50_ug_L = min(ec50_species_geomean),
    sensitive_species = latin_name[which.min(ec50_species_geomean)],
    sensitive_group = ecotox_group[which.min(ec50_species_geomean)],
    .groups = "drop"
  )

# ==============================================================================
# STAP 5b: TU BEREKENING & AGGREGATIE PER STAAL (Master Dataset Mafy)
# ==============================================================================
cat("⏳ Master TU dataset opbouwen (Macrophyten)...\n")

tu_dataset_ruw_mafy_pak <- fc_data %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
  inner_join(
    pak_target %>% select(stof_symbool, cas, stof_naam),
    by = c("parameter_symbool" = "stof_symbool")
  ) %>%
  inner_join(ec50_mafy_clean_pak, by = "cas") %>%
  filter(!is.na(final_ec50_ug_L)) %>%
  mutate(TU_individual = resultaat_ug_L / final_ec50_ug_L)

totaal_aantal_jaren <- n_distinct(lubridate::year(tu_dataset_ruw_mafy_pak$monsternamedatum))
drempel_jaren <- ceiling(totaal_aantal_jaren * 0.5)

core_stoffen <- tu_dataset_ruw_mafy_pak %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(jaren_gemeten = n_distinct(jaar)) %>%
  filter(jaren_gemeten >= drempel_jaren) %>%
  pull(parameter_symbool)

tu_master_mafy_pak <- tu_dataset_ruw_mafy_pak %>%
  left_join(chemische_stoffen %>% select(stof_symbool, subtype) %>% distinct(), by = c("parameter_symbool" = "stof_symbool")) %>%
  mutate(is_core = parameter_symbool %in% core_stoffen,
         is_target_pak = cas %in% target_paks_cas) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    aantal_detecties = n(),
    concentratie_som = sum(resultaat_ug_L, na.rm = TRUE),

    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = safe_max(TU_individual),

    TU_core_sum = sum(TU_individual[is_core], na.rm = TRUE),
    TU_core_max = safe_max(TU_individual[is_core]),

    # PAK specifieke groepen: Licht en Zwaar
    TU_licht_sum = sum(TU_individual[subtype == "licht"], na.rm = TRUE),
    TU_licht_max = safe_max(TU_individual[subtype == "licht"]),
    TU_zwaar_sum = sum(TU_individual[subtype == "zwaar"], na.rm = TRUE),
    TU_zwaar_max = safe_max(TU_individual[subtype == "zwaar"]),
    TU_target_paks_sum = sum(TU_individual[is_target_pak], na.rm = TRUE),
    TU_target_paks_max = safe_max(TU_individual[is_target_pak]),
    driver_stof = stof_naam[which.max(TU_individual)],
    driver_TU = safe_max(TU_individual),

    .groups = "drop"
  )

save(tu_master_mafy_pak, tu_dataset_ruw_mafy_pak, ec50_mafy_clean_pak, file = here("data", "verwerkt", "polluenten", "tu_master_paks_macrophytes.rdata"))
cat("✅ Script succesvol afgerond. Data opgeslagen in 'tu_master_paks_macrophytes.rdata'.\n")
