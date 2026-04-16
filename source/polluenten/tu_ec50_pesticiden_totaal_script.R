# ==============================================================================
# STAP 0: SETUP & BIBLIOTHEKEN
# ==============================================================================
pacman::p_load(ECOTOXr, dplyr, tidyr, webchem, here, readr)

# Check database beschikbaarheid
if (!check_ecotox_availability()) {
  cat("ECOTOX data niet gevonden. Start download...\n")
  download_ecotox_data(ask = FALSE, verify_ssl = FALSE)
  # Pas het pad aan naar waar jij de database wilt/hebt staan
  build_ecotox_sqlite(source = "data/ruw/pesticiden/ecotox_ascii_09_11_2025")
}

# Hulpfunctie voor Geometrisch Gemiddelde
geomean <- function(x) {
  x_clean <- x[!is.na(x) & x > 0]
  if(length(x_clean) == 0) return(NA)
  exp(mean(log(x_clean)))
}

# ==============================================================================
# STAP 1: DATA EXTRACTIE (Alleen EC50)
# ==============================================================================

# Output kolommen definiëren
output_columns <- c(
  "results.endpoint", "results.conc1_mean_op", "results.conc1_mean", "results.conc1_unit",
  "chemicals.chemical_name", "chemicals.cas_number",
  "species.latin_name", "species.ecotox_group", "species.class",
  "results.result_id", "tests.organism_habitat", "tests.media_type"
)

data_EC50_raw <- search_ecotox(
  list(endpoint = list(terms = "EC50", method = "contains")),
  output_columns
)

cat("✅ Ruwe data binnen. Totaal records:", nrow(data_EC50_raw), "\n")

# ==============================================================================
# STAP 2: EERSTE OPSCHONING (Habitat = Water)
# ==============================================================================

data_EC50_clean_step1 <- data_EC50_raw %>%
  filter(
    # A. Verwijder lege/ongeldige concentraties
    conc1_mean != "NR" & conc1_mean != "0" & conc1_mean != "",
    # B. Alleen exacte waarden of 'ongeveer' (geen < of >)
    conc1_mean_op %in% c("", "~"),
    # C. HABITAT FILTER: Alleen waterdieren (verwijdert ratten, vogels, wormen)
    organism_habitat == "Water"
  ) %>%
  mutate(
    conc1_value_num = as.numeric(gsub("[^0-9.-]", "", conc1_mean)),
    cas = as.character(ECOTOXr::as.cas(test_cas))
  )


# ==============================================================================
# STAP 3: JOUW SCOPE BEPALEN (Stoffen & Groepen)
# ==============================================================================

# 3.1 Laad jouw stoffenlijst
chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"),
                                header = T, sep = ";", na.strings = "nvt")

# Selecteer pesticiden (GEEN mengsels)
pesticiden_target <- chemische_stoffen %>%
  filter(type == "pesticide", subtype != "mengsel")

target_cas_list <- pesticiden_target %>% pull(cas)

# 3.2 Extra filter op organismegroepen (Amfibieën eruit voor KRW focus)
niet_relevante_groepen <- c("Amphibians", "Amphibians;Standard Test Species",
                            "Amphibians;U.S. Invasive Species", "Miscellaneous")

data_EC50_scope <- data_EC50_clean_step1 %>%
  filter(
    cas %in% target_cas_list,
    !ecotox_group %in% niet_relevante_groepen
  )

# ==============================================================================
# STAP 4: MEDIA PRIORITERING
# ==============================================================================

data_EC50_media <- data_EC50_scope %>%
  mutate(
    media_clean = trimws(gsub("/", "", media_type)),
    # Vul NC aan als FW omdat Habitat=Water
    media_final = case_when(
      media_clean %in% c("NC", "NR", "") ~ "FW",
      TRUE ~ media_clean
    ),
    # Prioriteit: 1=Zoetwater, 2=Brak, 3=Zout
    priority_score = case_when(
      media_final %in% c("FW", "CUL") ~ 1,
      media_final == "BW" ~ 2,
      media_final == "SW" ~ 3,
      TRUE ~ 99 # Bodem/Dieet/Lucht die door de filter glipte
    )
  ) %>%
  filter(priority_score < 99)

cat("✅ Gefilterd op Scope & Media. Resterende records:", nrow(data_EC50_media), "\n")

# ==============================================================================
# STAP 5: MOLECUULGEWICHTEN (WebChem)
# ==============================================================================

cat("⏳ Ophalen molecuulgewichten via WebChem...\n")
unieke_cas <- unique(data_EC50_media$cas)
unieke_cas <- unieke_cas[!is.na(unieke_cas)]

cids <- get_cid(unieke_cas, from = "cas", domain = "compound", match = "first")
cids_found <- cids %>% filter(!is.na(cid))
props <- pc_prop(cids_found$cid, properties = "MolecularWeight") %>%
  rename(cid = CID)

mol_weight_lookup <- cids_found %>%
  left_join(props, by = "cid") %>%
  select(cas = query, mol_weight = MolecularWeight) %>%
  mutate(mol_weight = as.numeric(mol_weight)) %>%
  distinct(cas, .keep_all = TRUE)

data_EC50_mw <- data_EC50_media %>%
  left_join(mol_weight_lookup, by = "cas")

# ==============================================================================
# STAP 6: HARMONISATIE (Log, Dichtheid, Eenheden naar ug/L)
# ==============================================================================

data_EC50_final <- data_EC50_mw %>%
  mutate(
    # A. Log terugrekenen
    is_log = grepl("\\(log\\)", endpoint, ignore.case = TRUE),
    conc1_value_corr = ifelse(is_log, 10^conc1_value_num, conc1_value_num),

    # B. Dichtheid (SW=1.025)
    water_density = ifelse(media_final == "SW", 1.025, 1.000),

    # C. Eenheden opschonen
    unit_clean = tolower(trimws(conc1_unit)),
    unit_clean = gsub("ai |ae |\\(ai\\)| media", "", unit_clean),

    # D. Factor naar ug/L
    factor_to_ug_L = case_when(
      unit_clean %in% c("ug/l", "ppb", "ug/dm3") ~ 1,
      unit_clean %in% c("mg/l", "ppm", "g/m3", "ug/ml") ~ 1000,
      unit_clean %in% c("ng/l", "ppt") ~ 0.001,
      unit_clean %in% c("m", "mol/l", "mol") ~ 1e6,
      unit_clean %in% c("mm", "mmol/l") ~ 1e3,
      unit_clean %in% c("um", "umol/l") ~ 1,
      unit_clean %in% c("nm", "nmol/l") ~ 0.001,
      unit_clean == "ppm" & media_final == "SW" ~ 1025,
      TRUE ~ NA_real_
    ),

    # E. Berekening
    ec50_ug_L = case_when(
      grepl("mol|m$|mm$|um$|nm$", unit_clean) ~ conc1_value_corr * factor_to_ug_L * mol_weight,
      TRUE ~ conc1_value_corr * factor_to_ug_L
    )
  ) %>%
  filter(!is.na(ec50_ug_L))

# ==============================================================================
# STAP 7 & 8 (AANGEPAST): AGGREGATIE MET STRICTE ZOETWATER VOORRANG
# ==============================================================================

ec50_aggregated <- data_EC50_final %>%
  # -----------------------------------------------------------------------
# NIEUW: Filter per CAS-nummer op Freshwater beschikbaarheid
# -----------------------------------------------------------------------
group_by(cas) %>%
  mutate(
    # Check of deze stof minstens één keer in Zoetwater (Score 1) is getest
    has_freshwater_data = any(priority_score == 1)
  ) %>%
  ungroup() %>%

  filter(
    # BEHOUD DE RIJ ALS:
    # 1. Het een zoetwater test is (Score 1)
    # OF
    # 2. Er voor deze hele stof GEEN zoetwater data is (dan mag je SW/BW gebruiken)
    priority_score == 1 | !has_freshwater_data
  ) %>%
  # -----------------------------------------------------------------------

# Vanaf hier is de logica hetzelfde als voorheen:

# 1. Detecteer standaardsoorten (Standard Test Species)
mutate(
  is_standard_species = grepl("Standard Test Species", ecotox_group, ignore.case = TRUE)
) %>%

  # 2. Prioriteer Standaardsoorten per stof (indien beschikbaar)
  group_by(cas) %>%
  filter(is_standard_species | !any(is_standard_species)) %>%
  ungroup() %>%

  # 3. Bereken Geometrisch Gemiddelde per soort
  group_by(cas, latin_name, ecotox_group) %>%
  summarise(
    ec50_species_geomean = geomean(ec50_ug_L),
    n_studies = n(),
    # Even checken of we nu FW of SW hebben
    used_media = paste(unique(media_final), collapse="|"),
    .groups = "drop"
  ) %>%

  # 4. Kies de gevoeligste soort per groep
  group_by(cas, ecotox_group) %>%
  summarise(
    ec50_group_min = min(ec50_species_geomean),
    most_sensitive_species = latin_name[which.min(ec50_species_geomean)],
    # Metadata meenemen
    n_species_in_group = n(),
    .groups = "drop"
  )

# ==============================================================================
# STAP 9: DEFINITIEVE KOPPELING AAN STOFFENLIJST
# ==============================================================================

final_ec50_per_cas <- ec50_aggregated %>%
  group_by(cas) %>%
  summarise(
    # De absolute worst-case EC50 over alle groepen (Algen, Daphnia, Vis) heen
    final_ec50_ug_L = min(ec50_group_min),
    sensitive_group = ecotox_group[which.min(ec50_group_min)],
    sensitive_species = most_sensitive_species[which.min(ec50_group_min)]
  )

# Koppel terug aan je originele lijst
pesticiden_ec50 <- pesticiden_target %>%
  left_join(final_ec50_per_cas, by = "cas")

# --- Resultaat Bekijken ---
cat("\n📊 ANALYSE VOLTOOID\n")
cat("Totaal aantal pesticiden in lijst:", nrow(pesticiden_target), "\n")
cat("Aantal pesticiden met gevonden EC50 data:", sum(!is.na(pesticiden_ec50$final_ec50_ug_L)), "\n")
cat("Dekking:", round(sum(!is.na(pesticiden_ec50$final_ec50_ug_L)) / nrow(pesticiden_target) * 100, 1), "%\n")

# Opslaan
save(pesticiden_ec50, file = here("data", "verwerkt", "polluenten", "pesticiden_ec50.rdata"))



load(here("data", "verwerkt", "polluenten", "pesticiden_ec50.rdata"))

load(here("data", "verwerkt", "fc_data.rdata"))

chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"),
                                header = T, sep = ";", na.strings = "nvt")


# -------------------------------------------------------------------------
# STAP 1 & 2: KOPPELEN EN TU BEREKENEN (Vereenvoudigd)
# -------------------------------------------------------------------------

ec50_clean <- pesticiden_ec50 %>%
  select(stof_symbool, cas, stof_naam, final_ec50_ug_L, sensitive_group, sensitive_species) %>%
  distinct(stof_symbool, .keep_all = TRUE)

tu_dataset_ruw <- fc_data %>%
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

tu_per_sample <- tu_dataset_ruw %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    # 1. De TU Som (msPAF benadering via Concentration Addition)
    TU_sum = sum(TU_individual, na.rm = TRUE),
    TU_max = max(TU_individual, na.rm = TRUE),
    # 2. Metadata (Handig voor analyse achteraf)
    concentratie_pesticiden_sum = sum(resultaat_ug_L),
    aantal_pesticiden_gemeten = n(),
    aantal_pesticiden_met_TU = sum(!is.na(TU_individual)),

    # Welke stof draagt het meeste bij aan de toxiciteit in dit staal?
    driver_stof = stof_naam[which.max(TU_individual)],
    driver_TU = max(TU_individual, na.rm = TRUE),
    driver_group = sensitive_group[which.max(TU_individual)],
    driver_species = sensitive_species[which.max(TU_individual)],

    .groups = "drop"
  )

#hier specifieke subtype toevoegen om verschillende TU's te bereken extra

pesticiden_subtype <- chemische_stoffen %>%
  filter(type == "pesticide")

classificatie_info <- pesticiden_subtype %>%
  select(stof_symbool, subtype) %>% # Zorg dat kolomnamen matchen met tu_dataset_ruw
  distinct()

# We kijken in tu_dataset_ruw (dus stoffen die EN pesticide zijn EN een EC50 hebben)
#core stoffen selecteren
jaren_per_stof <- tu_dataset_ruw %>%
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

tu_specific_groups <- tu_dataset_ruw %>%
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


# -------------------------------------------------------------------------
# STAP 4: RESULTAAT BEKIJKEN & OPSLAAN
# -------------------------------------------------------------------------

cat("✅ TU Berekening Voltooid.\n")
cat("Aantal berekende samples:", nrow(tu_per_sample), "\n")
cat("Gemiddelde TU_sum:", mean(tu_per_sample$TU_sum, na.rm = T), "\n")
cat("Maximum TU_sum:", max(tu_per_sample$TU_sum, na.rm = T), "\n")
cat("Gemiddelde aantal_pesticiden_gemeten:", mean(tu_per_sample$aantal_pesticiden_gemeten, na.rm = T), "\n")
summary(tu_per_sample$aantal_pesticiden_gemeten, na.rm = T)

# Top 10 meest toxische stalen bekijken
print(tu_per_sample %>% arrange(desc(TU_sum)) %>% head(10))

# Opslaan
tu_pesticiden_per_sample <- tu_per_sample
tu_pesticiden_ruw <- tu_dataset_ruw
save(tu_pesticiden_per_sample, tu_pesticiden_ruw, tu_specific_groups, file = here("data", "verwerkt", "polluenten", "tu_ec50_pesticiden.rdata"))
