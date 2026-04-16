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
# STAP 1: DATA EXTRACTIE (Alleen LC50)
# ==============================================================================

# Output kolommen definiëren
output_columns <- c(
  "results.endpoint", "results.conc1_mean_op", "results.conc1_mean", "results.conc1_unit",
  "chemicals.chemical_name", "chemicals.cas_number",
  "species.latin_name", "species.ecotox_group", "species.class",
  "results.result_id", "tests.organism_habitat", "tests.media_type"
)

data_LC50_raw <- search_ecotox(
  list(endpoint = list(terms = "LC50", method = "contains")),
  output_columns
)

cat("✅ Ruwe data binnen. Totaal records:", nrow(data_LC50_raw), "\n")

# ==============================================================================
# STAP 2: EERSTE OPSCHONING (Habitat = Water)
# ==============================================================================

data_LC50_clean_step1 <- data_LC50_raw %>%
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

data_LC50_scope <- data_LC50_clean_step1 %>%
  filter(
    cas %in% target_cas_list,
    !ecotox_group %in% niet_relevante_groepen
  )

# ==============================================================================
# STAP 4: MEDIA PRIORITERING
# ==============================================================================

data_LC50_media <- data_LC50_scope %>%
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

cat("✅ Gefilterd op Scope & Media. Resterende records:", nrow(data_LC50_media), "\n")

# ==============================================================================
# STAP 5: MOLECUULGEWICHTEN (WebChem)
# ==============================================================================

cat("⏳ Ophalen molecuulgewichten via WebChem...\n")
unieke_cas <- unique(data_LC50_media$cas)
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

data_LC50_mw <- data_LC50_media %>%
  left_join(mol_weight_lookup, by = "cas")

# ==============================================================================
# STAP 6: HARMONISATIE (Log, Dichtheid, Eenheden naar ug/L)
# ==============================================================================

data_LC50_final <- data_LC50_mw %>%
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
    LC50_ug_L = case_when(
      grepl("mol|m$|mm$|um$|nm$", unit_clean) ~ conc1_value_corr * factor_to_ug_L * mol_weight,
      TRUE ~ conc1_value_corr * factor_to_ug_L
    )
  ) %>%
  filter(!is.na(LC50_ug_L))
