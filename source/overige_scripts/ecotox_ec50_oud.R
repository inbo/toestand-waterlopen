# R-code om uit te voeren
library(ECOTOXr)
library(dplyr)
library(tidyr) # Nodig voor eventuele latere data opschoning
library(webchem)

# Zorg ervoor dat de database beschikbaar is (zoals in de originele code)
# Als deze stap faalt, controleer dan uw internetverbinding of de handmatige download/build
if (!check_ecotox_availability()) {
  cat("ECOTOX data is nog niet beschikbaar. Start download/build...\n")
  download_ecotox_data(ask = FALSE, verify_ssl = FALSE)
  build_ecotox_sqlite(source = "data/ruw/pesticiden/ecotox_ascii_09_11_2025")

}

list_all_ecotox_fields <- data.frame(output_fields = list_ecotox_fields('all'))

# --- 1. Definieer de Output Kolommen ---
# We gebruiken een brede set kolommen die nuttig zijn voor filtering en conversie.
output_columns <-
  c("results.endpoint", "results.conc1_mean_op", "results.conc1_mean", "results.conc1_unit",
    "chemicals.chemical_name", "chemicals.cas_number", # Zeer belangrijk voor koppeling
    "species.latin_name", "species.ecotox_group", "species.class", # Belangrijk voor filtering op groepen
    "results.result_id", "tests.reference_number", "references.publication_year", "tests.organism_habitat",
    "tests.media_type", "tests.media_type_comments")

# --- 2. Voer de Brede Zoekopdracht uit ---
# We filteren ALLEEN op de eindpunt: EC50. Alle andere filters worden verwijderd.
data_EC50_master0 <- search_ecotox(
  list(
    # Haal ALLE EC50 records op
    endpoint = list(terms = "EC50", method = "contains")
    # GEEN filters op conc1_mean_op (incl. <, >, ~)
    # GEEN filters op conc1_unit (alle eenheden meenemen)
  ),
  output_columns
)

cat("✅ MASTER EC50 dataset succesvol opgehaald. Aantal records:", nrow(data_EC50_master), "\n\n")

# --- 3. Opschoning en Typeconversie ---
# Verwijder records waar de concentratiewaarde niet beschikbaar (not reported; NR) is of nul is
data_EC50_master <- subset(data_EC50_master0, conc1_mean != "NR" & conc1_mean != "0" & conc1_mean != "")
data_EC50_master1 <- data_EC50_master0 %>%
  filter(conc1_mean != "NR" & conc1_mean != "0" & conc1_mean != "" & conc1_mean_op %in% c("", "~")) %>%
  filter(organism_habitat == "Water")

# Maak een numerieke versie van de concentratie (verwijder operators zoals '>' of '<')
data_EC50_master <- data_EC50_master1 %>%
  mutate(
    # Gebruik gsub om non-numerieke tekens (behalve punt en min) te verwijderen
    conc1_value_num = as.numeric(gsub("[^0-9.-]", "", conc1_mean)),
    # Maak de soorten class kolom schoon
    species_class_clean = ifelse(class == "", "Unspecified", class),
   cas = as.character(ECOTOXr::as.cas(test_cas))
  )

# ----------------------------------------------------------------------
# 4. FILTEREN OP UW DATA (STOFFEN EN MACRO-INVERTEBRATEN)
# ----------------------------------------------------------------------

# **AANPASSEN:** Definieer hier uw lijsten met chemische stoffen en organismegroepen

chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"), header = T, sep = ";", na.strings = "nvt")

pesticiden <- chemische_stoffen %>%
  filter(type == "pesticide") %>%
  filter(subtype != "mengsel")

uw_CAS_nummers <- pesticiden %>% pull(cas) # Vervang met uw CAS-nummers
niet_relevante_ecotox_groepen <- c(
  "Mammals", "Mammals;Standard Test Species",
  "Birds", "Birds;Standard Test Species",
  "Reptiles", "Reptiles;U.S. Threatened and Endangered Species",
  "Fungi", "Fungi;U.S. Invasive Species",
  "Miscellaneous",
  "Amphibians", "Amphibians;Standard Test Species", "Amphibians;U.S. Invasive Species", "Amphibians;U.S. Threatened and Endangered Species"
)

data_EC50_uw_selectie <- data_EC50_master %>%
  filter(
    # Filter op chemische stoffen (gebruik CAS-nummer voor betrouwbaarheid)
    cas %in% uw_CAS_nummers,

    # Filter op organismegroepen (gebruik de class kolom)
    !ecotox_group %in% niet_relevante_ecotox_groepen
  )

cat("\n✅ Selectie voor uw stoffen en Macro-invertebraten voltooid. Aantal records:", nrow(data_EC50_uw_selectie), "\n")

# Toon de structuur van de uiteindelijke selectie
print(head(data_EC50_uw_selectie))

# ----------------------------------------------------------------------
# 5. EENHEDEN HARMONISEREN (Conversie naar ug/L)
# ----------------------------------------------------------------------

# STAP 5.1: Molecuulgewichten ophalen (Nodig voor conversie van mol naar gram)

# 1. Selecteer unieke CAS-nummers om de API niet te overbelasten
unieke_cas_lijst <- unique(data_EC50_uw_selectie$cas)

# Verwijder eventuele NA's
unieke_cas_lijst <- unieke_cas_lijst[!is.na(unieke_cas_lijst)]

# 2. Stap A: Haal PubChem CID's op (Identifier translation)
# We vragen PubChem: "Welk ID hoort bij dit CAS-nummer?"
cids <- get_cid(unieke_cas_lijst, from = "cas", domain = "compound", match = "first")

# Filter de gevonden resultaten
cids_found <- cids %>%
  filter(!is.na(cid))

# 3. Stap B: Haal eigenschappen op (MolecularWeight) op basis van CID
# We doen dit in blokjes (chunks) omdat de API soms stopt bij te grote aanvragen
# pc_prop haalt eigenschappen op van PubChem
props <- pc_prop(cids_found$cid, properties = "MolecularWeight") %>%
  group_by(CID) %>%
  summarise(MolecularWeight = mean(as.numeric(MolecularWeight)))

# 4. Samenvoegen: Koppel gewicht terug aan CAS
mol_weight_lookup <- cids_found %>%
  # Koppel de eigenschappen aan de CID's
  left_join(props, by = c("cid" = "CID")) %>%
  # Selecteer en hernoem de kolommen die we nodig hebben
  select(cas = query, mol_weight = MolecularWeight, cid) %>%
  # Zorg dat het numeriek is
  mutate(mol_weight = as.numeric(mol_weight))

dubbele_cids <- mol_weight_lookup %>%
  group_by(cid) %>%
  summarise(aantal_cas = n(), welke_cas = paste(cas, collapse = ", ")) %>%
  filter(aantal_cas > 1)

if(nrow(dubbele_cids) > 0) {
  cat("\nℹ️ De volgende CIDs worden gedeeld door meerdere CAS-nummers en zijn optische isomeren:\n")
  print(dubbele_cids)
}

# 5. Integreer in je hoofd-dataset
data_EC50_met_molweight0 <- data_EC50_uw_selectie %>%
  left_join(mol_weight_lookup, by = "cas")

data_EC50_met_molweight <- data_EC50_met_molweight0 %>% # logEC weglaten (2observaties)
 filter(endpoint == "(log)EC50")



data_EC50_clean <- data_EC50_met_molweight %>%
  # 1. Eerst de string opschonen: alles kleine letters, spaties weg, 'ai'/'ae' weg
  mutate(
    # Originele eenheid bewaren voor controle
    unit_orig = conc1_unit,

    # Schoonmaak: kleine letters, trimmen
    unit_clean = tolower(trimws(conc1_unit)),

    # Verwijder voorvoegsels als 'ai ' (active ingredient), 'ae ' (acid equivalent)
    # Verwijder ook suffixen zoals ' media' (mg/l media -> mg/l)
    unit_clean = gsub("ai |ae |\\(ai\\)", "", unit_clean),
    unit_clean = gsub(" media", "", unit_clean)
  ) %>%

  # 2. De grote conversie logica
  mutate(
    factor_to_ug_L = case_when(
      # --- DIRECTE MASSA CONVERSIES (Doel = ug/L) ---

      # ug/L familie (Factor 1)
      unit_clean %in% c("ug/l", "ppb", "ug/dm3") ~ 1,
      # Noot: 'ug/l diet' is twijfelachtig (vloeibaar voedsel?), maar qua eenheid klopt het.
      # Veiligheidshalve vaak beter te filteren op matrix, maar wiskundig is het x1.

      # mg/L familie (Factor 1000)
      unit_clean %in% c("mg/l", "ppm", "g/m3") ~ 1000,

      # ng/L familie (Factor 0.001)
      unit_clean %in% c("ng/l", "ppt") ~ 0.001,

      # g/L familie (Factor 1.000.000)
      unit_clean == "g/l" ~ 1e6, # Staat niet in je lijst, maar voor zekerheid

      # ml/L en afgeleiden (dichtheid water aangenomen = 1 g/ml)
      unit_clean %in% c("ug/ml") ~ 1000,   # ug/ml = mg/l = 1000 ug/l
      unit_clean %in% c("ng/ml") ~ 1,      # ng/ml = ug/l

      # Percentages (w/v = weight/volume). 1% = 1g/100ml = 10g/L = 10.000.000 ug/L
      unit_clean == "% w/v" ~ 1e7,

      # --- MOLAIRE CONVERSIES (Hebben mol_weight nodig) ---
      # Formule: Waarde * Factor * Molgewicht

      # M (Mol/L) -> ug/L:  1 mol/L * MW (g/mol) * 1e6 (ug/g)
      unit_clean %in% c("m", "mol/l", "mol") ~ 1e6,

      # mM (mmol/L) -> ug/L: 1 mmol/L = 1e-3 mol/L.  Factor = 1e3
      unit_clean %in% c("mm", "mmol/l") ~ 1e3,

      # uM (umol/L) -> ug/L: 1 umol/L = 1e-6 mol/L. Factor = 1
      # (Want: 1 umol * MW (ug/umol) = MW ug)
      unit_clean %in% c("um", "umol/l", "ai um", "umol/dm3") ~ 1,

      # nM (nmol/L) -> ug/L: 1 nmol/L = 1e-9 mol/L. Factor = 1e-3
      unit_clean %in% c("nm", "nmol/l", "nm/l") ~ 0.001,

      # --- ALLES WAT WE WEGGOOIEN (Geef NA) ---
      # Bodem/Sediment (kg soil, dry soil, ug/g)
      grepl("soil", unit_clean) ~ NA_real_,
      grepl("sediment", unit_clean) ~ NA_real_,
      grepl("ug/g", unit_clean) ~ NA_real_,  # Vaak weefsel of bodem
      grepl("mg/kg", unit_clean) ~ NA_real_, # Vaak bodem of voedsel

      # Dieet (diet)
      grepl("diet", unit_clean) ~ NA_real_,

      # Oppervlakte (ha, acre, cm2)
      grepl("ha", unit_clean) ~ NA_real_,
      grepl("acre", unit_clean) ~ NA_real_,
      grepl("cm2", unit_clean) ~ NA_real_,

      # Per organisme/ei
      grepl("/org", unit_clean) ~ NA_real_,
      grepl("/egg", unit_clean) ~ NA_real_,
      grepl("/cell", unit_clean) ~ NA_real_,

      # Gasvormig
      unit_clean == "ppmv" ~ NA_real_,

      # % v/v (Volume/Volume - vereist dichtheid van de stof zelf, onbetrouwbaar)
      unit_clean == "% v/v" ~ NA_real_,

      TRUE ~ NA_real_ # Vangnet voor de rest
    ),

    # 3. Voer de berekening uit
    ec50_ug_L = case_when(
      # Als factor NA is, blijft resultaat NA
      is.na(factor_to_ug_L) ~ NA_real_,

      # Molaire berekening (check of mol_weight beschikbaar is!)
      unit_clean %in% c("m", "mol/l", "mol", "mm", "mmol/l", "um", "umol/l", "ai um", "umol/dm3", "nm", "nmol/l", "nm/l") ~
        conc1_value_num * factor_to_ug_L * mol_weight,

      # Massa berekening (mol_weight niet nodig)
      TRUE ~ conc1_value_num * factor_to_ug_L
    )
  ) %>%
  # 4. Filter de onbruikbare data weg
  filter(!is.na(ec50_ug_L))

# --- Validatie ---
cat("✅ Conversie gereed.\n")
cat("Aantal behouden records:", nrow(data_EC50_clean), "\n")
cat("Verwijderde records (bodem/dieet/etc):", nrow(data_EC50_met_molweight) - nrow(data_EC50_clean), "\n")

# Check even de "gevaarlijke" omzettingen (zoals molaire eenheden)
data_EC50_clean %>%
  filter(grepl("mol", unit_clean) | grepl("M", conc1_unit)) %>%
  select(chemical_name, conc1_mean, conc1_unit, mol_weight, ec50_ug_L) %>%
  head(10) %>%
  print()
