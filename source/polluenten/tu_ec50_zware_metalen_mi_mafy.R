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
  build_ecotox_sqlite(source = "data/ruw/pesticiden/ecotox_ascii")
}

# Laad de benodigde lokale bestanden
load(here("data", "verwerkt", "fc_data.rdata"))
chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"),
                                header = TRUE, sep = ";", na.strings = "nvt")

# AANPASSING 1: Selecteer zware metalen uit de classificatie
metalen_target <- chemische_stoffen %>%
  filter(type == "metaal", subtype == "zwaar metaal", !is.na(cas))
target_cas_list <- unique(metalen_target$cas)

# ==============================================================================
# STAP 1 & 2: DATA EXTRACTIE & OPSCHONING (Basis)
# ==============================================================================
cat("⏳ Data ophalen en basis filteren voor zware metalen...\n")

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

# # ==============================================================================
# # STAP 3: MOLECUULGEWICHTEN VIA WEBCHEM (Met Caching)
# # ==============================================================================
# mw_cache_file <- here("data", "verwerkt", "polluenten", "mol_weights_metals_cache.rds")
#
# if (file.exists(mw_cache_file)) {
#   cat("✅ Molecuulgewichten geladen vanuit lokale cache.\n")
#   mol_weight_lookup <- readRDS(mw_cache_file)
# } else {
#   cat("⏳ Ophalen atoom/molecuulgewichten via WebChem...\n")
#   unieke_cas <- unique(data_EC50_scope$cas[!is.na(data_EC50_scope$cas)])
#   cids <- get_cid(unieke_cas, from = "cas", domain = "compound", match = "first") %>% filter(!is.na(cid))
#   props <- pc_prop(cids$cid, properties = "MolecularWeight") %>% rename(cid = CID)
#
#   mol_weight_lookup <- cids %>%
#     left_join(props, by = "cid") %>%
#     select(cas = query, mol_weight = MolecularWeight) %>%
#     mutate(mol_weight = as.numeric(mol_weight)) %>%
#     distinct(cas, .keep_all = TRUE)
#
#   saveRDS(mol_weight_lookup, file = mw_cache_file)
# }
#
# # Eenheden converteren naar ug/L
# data_EC50_final <- data_EC50_scope %>%
#   left_join(mol_weight_lookup, by = "cas") %>%
#   mutate(
#     is_log = grepl("\\(log\\)", endpoint, ignore.case = TRUE),
#     conc1_value_corr = ifelse(is_log, 10^conc1_value_num, conc1_value_num),
#     unit_clean = tolower(trimws(gsub("ai |ae |\\(ai\\)| media", "", conc1_unit))),
#     factor_to_ug_L = case_when(
#       unit_clean %in% c("ug/l", "ppb", "ug/dm3", "um", "umol/l") ~ 1,
#       unit_clean %in% c("mg/l", "ppm", "g/m3", "ug/ml", "mm", "mmol/l") ~ 1000,
#       unit_clean %in% c("ng/l", "ppt", "nm", "nmol/l") ~ 0.001,
#       unit_clean %in% c("m", "mol/l", "mol") ~ 1e6,
#       unit_clean == "ppm" & media_final == "SW" ~ 1025,
#       TRUE ~ NA_real_
#     ),
#     ec50_ug_L = case_when(
#       grepl("mol|m$|mm$|um$|nm$", unit_clean) ~ conc1_value_corr * factor_to_ug_L * mol_weight,
#       TRUE ~ conc1_value_corr * factor_to_ug_L
#     )
#   ) %>%
#   filter(!is.na(ec50_ug_L))

# ==============================================================================
# STAP 3: ATOOMGEWICHTEN ZWARE METALEN (Hardcoded Lookup)
# ==============================================================================
cat("✅ Atoomgewichten zware metalen inladen via interne lijst...\n")

# Maak een vaste opzoektabel met CAS-nummers en hun atoomgewicht (in g/mol)
mol_weight_lookup <- tibble::tribble(
  ~cas,         ~mol_weight, ~symbool,
  "7440-22-4",  107.868,     "Ag", # Zilver
  "7440-38-2",  74.922,      "As", # Arseen
  "7440-41-7",  9.012,       "Be", # Beryllium
  "7440-43-9",  112.411,     "Cd", # Cadmium
  "7440-48-4",  58.933,      "Co", # Kobalt
  "7440-47-3",  51.996,      "Cr", # Chroom
  "7440-50-8",  63.546,      "Cu", # Koper
  "7439-97-6",  200.592,     "Hg", # Kwik
  "7439-96-5",  54.938,      "Mn", # Mangaan
  "7439-98-7",  95.95,       "Mo", # Molybdeen
  "7440-02-0",  58.693,      "Ni", # Nikkel
  "7439-92-1",  207.2,       "Pb", # Lood
  "7440-31-5",  118.71,      "Sn", # Tin
  "7440-32-6",  47.867,      "Ti", # Titaan
  "7440-28-0",  204.38,      "Tl", # Thallium
  "7440-61-1",  238.029,     "U",  # Uranium
  "7440-62-2",  50.942,      "V",  # Vanadium
  "7440-66-6",  65.38,       "Zn", # Zink
  "7439-89-6",  55.845,      "Fe", # Ijzer

  # Extra's voor als je de dataset later verbreedt (metalloïden/lichte metalen):
  "7440-42-8",  10.81,       "B",  # Boor
  "7440-39-3",  137.327,     "Ba", # Barium
  "7440-36-0",  121.76,      "Sb", # Antimoon
  "7782-49-2",  78.971,      "Se", # Seleen
  "13494-80-9", 127.6,       "Te", # Telluur
  "7429-90-5",  26.982,      "Al", # Aluminium
  "7440-70-2",  40.078,      "Ca", # Calcium
  "7439-95-4",  24.305,      "Mg", # Magnesium
  "7440-09-7",  39.098,      "K",  # Kalium
  "7440-23-5",  22.990,      "Na"  # Natrium
) %>%
  select(cas, mol_weight) # Symbool droppen we, was puur voor de leesbaarheid

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

ec50_mi_clean_metalen <- data_EC50_final %>%
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
cat("⏳ Master TU dataset opbouwen...\n")

# AANPASSING 2: Join via CAS-nummer in plaats van exact parametersymbool.
# Metalen hebben meerdere symbolen per CAS (bijv. Cu o en Cu t).
tu_dataset_ruw_mi_metalen <- fc_data %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
  inner_join(
    metalen_target %>% select(stof_symbool, cas, stof_naam),
    by = c("parameter_symbool" = "stof_symbool")
  ) %>%
  inner_join(ec50_mi_clean_metalen, by = "cas") %>%
  filter(!is.na(final_ec50_ug_L)) %>%
  mutate(TU_individual = resultaat_ug_L / final_ec50_ug_L)

# Dynamisch core-stoffen bepalen
totaal_aantal_jaren <- n_distinct(lubridate::year(tu_dataset_ruw_mi_metalen$monsternamedatum))
drempel_jaren <- ceiling(totaal_aantal_jaren * 0.5)

core_stoffen <- tu_dataset_ruw_mi_metalen %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(jaren_gemeten = n_distinct(jaar)) %>%
  filter(jaren_gemeten >= drempel_jaren) %>%
  pull(parameter_symbool)

target_9_metals_cas <- c(
  "7440-38-2", # Arseen (As)
  "7440-43-9", # Cadmium (Cd)
  "7440-48-4", # Kobalt (Co)
  "7440-47-3", # Chroom (Cr)
  "7440-50-8", # Koper (Cu)
  "7439-97-6", # Kwik (Hg)
  "7440-02-0", # Nikkel (Ni)
  "7439-92-1", # Lood (Pb)
  "7440-66-6"  # Zink (Zn)
)
# AANPASSING 3: Aggregatie focust op "opgelost" vs "totaal" i.p.v. insecticiden/herbiciden
tu_master_mi_metalen <- tu_dataset_ruw_mi_metalen %>%
  mutate(
    is_core = parameter_symbool %in% core_stoffen,
    is_opgelost = grepl(" o$| mo$", parameter_symbool),
    is_totaal = grepl(" t$| mt$", parameter_symbool),
    is_target9 = cas %in% target_9_metals_cas
  ) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    aantal_detecties = n(),
    concentratie_som = sum(resultaat_ug_L, na.rm = TRUE),

    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = safe_max(TU_individual),

    TU_core_sum = sum(TU_individual[is_core], na.rm = TRUE),
    TU_core_max = safe_max(TU_individual[is_core]),

    # Metaal specifieke groepen
    TU_opgelost_sum = sum(TU_individual[is_opgelost], na.rm = TRUE),
    TU_opgelost_max = safe_max(TU_individual[is_opgelost]),
    TU_totaal_sum   = sum(TU_individual[is_totaal], na.rm = TRUE),
    TU_totaal_max   = safe_max(TU_individual[is_totaal]),
    TU_target9_sum = sum(TU_individual[is_target9], na.rm = TRUE),
    TU_target9_max = safe_max(TU_individual[is_target9]),
    driver_stof = parameter_symbool[which.max(TU_individual)],
    driver_TU = safe_max(TU_individual),

    .groups = "drop"
  )

save(tu_master_mi_metalen, tu_dataset_ruw_mi_metalen, ec50_mi_clean_metalen, file = here("data", "verwerkt", "polluenten", "tu_master_metals_macroinvertebrates.rdata"))
cat("✅ Script succesvol afgerond. Data opgeslagen in 'tu_master_metals_macroinvertebrates.rdata'.\n")

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

ec50_mafy_clean_metalen <- data_EC50_final %>%
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

tu_dataset_ruw_mafy_metalen <- fc_data %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>%
  inner_join(
    metalen_target %>% select(stof_symbool, cas, stof_naam),
    by = c("parameter_symbool" = "stof_symbool")
  ) %>%
  inner_join(ec50_mafy_clean_metalen, by = "cas") %>%
  filter(!is.na(final_ec50_ug_L)) %>%
  mutate(TU_individual = resultaat_ug_L / final_ec50_ug_L)

totaal_aantal_jaren <- n_distinct(lubridate::year(tu_dataset_ruw_mafy_metalen$monsternamedatum))
drempel_jaren <- ceiling(totaal_aantal_jaren * 0.5)

core_stoffen <- tu_dataset_ruw_mafy_metalen %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(jaren_gemeten = n_distinct(jaar)) %>%
  filter(jaren_gemeten >= drempel_jaren) %>%
  pull(parameter_symbool)



tu_master_mafy_metalen <- tu_dataset_ruw_mafy_metalen %>%
  mutate(
    is_core = parameter_symbool %in% core_stoffen,
    is_opgelost = grepl(" o$| mo$", parameter_symbool),
    is_totaal = grepl(" t$| mt$", parameter_symbool),
    is_target9 = cas %in% target_9_metals_cas
  ) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    aantal_detecties = n(),
    concentratie_som = sum(resultaat_ug_L, na.rm = TRUE),

    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = safe_max(TU_individual),

    TU_core_sum = sum(TU_individual[is_core], na.rm = TRUE),
    TU_core_max = safe_max(TU_individual[is_core]),

    TU_opgelost_sum = sum(TU_individual[is_opgelost], na.rm = TRUE),
    TU_opgelost_max = safe_max(TU_individual[is_opgelost]),
    TU_totaal_sum   = sum(TU_individual[is_totaal], na.rm = TRUE),
    TU_totaal_max   = safe_max(TU_individual[is_totaal]),
    TU_target9_sum = sum(TU_individual[is_target9], na.rm = TRUE),
    TU_target9_max = safe_max(TU_individual[is_target9]),
    driver_stof = parameter_symbool[which.max(TU_individual)],
    driver_TU = safe_max(TU_individual),

    .groups = "drop"
  )

save(tu_master_mafy_metalen, tu_dataset_ruw_mafy_metalen, ec50_mafy_clean_metalen, file = here("data", "verwerkt", "polluenten", "tu_master_metals_macrophytes.rdata"))
cat("✅ Script succesvol afgerond. Data opgeslagen in 'tu_master_metals_macrophytes.rdata'.\n")
