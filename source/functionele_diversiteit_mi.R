source(here::here("source", "inladen_packages.R"))

# --- 1. Gegevens inlezen ---
load(file = here("data", "verwerkt", "mi_soorten.rdata"))

comm_data_raw <- mi_soorten %>%
  select(meetplaats, monsternamedatum, macroinvertebraat, aantal) %>%
  pivot_wider(., names_from = macroinvertebraat, values_from = aantal, values_fill = 0)
# write.csv(comm_data, file = here("data", "ruw", "macroinvertebraten", "traits", "comm_data.csv"))

trait_data_raw <- read_excel(here("data", "ruw", "macroinvertebraten", "traits", "tachet_traits_mi.xlsx")) %>%
  janitor::clean_names()
# write.csv(trait_data_raw, file = here("data", "ruw", "macroinvertebraten", "traits", "trait_data.csv"))


# Lees de eerder gegenereerde taxonlijst met taxonomisch niveau in
taxon_levels_df <- read_csv(here("data", "ruw", "macroinvertebraten", "traits", "taxonlist_with_taxonomic_level.csv"))

# --- 2. Trait Data voorbereiden en consolideren voor koppeling ---

trait_data0 <- trait_data_raw

# Stap A: Maak een 'Taxon_Consolidated_Key' die 'sp.', '[Fam:X]', ' Ad.', ' Lv.', ' gen.' verwijdert
# Deze key zal gebruikt worden om rijen te groeperen die tot hetzelfde taxon behoren.
trait_data1 <- trait_data0 %>%
  mutate(
    # Aangepast om ' gen.' te verwijderen in Taxon_Consolidated_Key
    Taxon_Consolidated_Key = tolower(str_replace_all(taxon, " sp\\.| Ad\\.| Lv\\.| Gen\\.", "")),
    # Aangepast om '[Ord:.*?] ' te verwijderen in Family_Clean
    Family_Clean = tolower(str_replace_all(family, "\\[Ord:|\\]", "")),
    Taxagroup_Clean = tolower(taxagroup)
  )

selected_trait_groups <- c("saprobity_", "dispersal_", "reproduction_", "locomotion_",
                           "substrate_", "current_velocity_", "trophic_status_", "temperature_")


trait_data <- trait_data1 %>%
  select(taxon, family, taxagroup, Taxon_Consolidated_Key, Family_Clean, Taxagroup_Clean, starts_with(selected_trait_groups))

# Identificeer de kolommen die de daadwerkelijke traits bevatten
# Dit zijn alle kolommen na de eerste 4 (Taxagroup, Family, Subfamily, Taxon)
# En exclusief de nieuw aangemaakte 'Clean' en 'Consolidated_Key' kolommen
trait_cols <- names(trait_data)[7:(ncol(trait_data))] # Past zich aan als er meer 'clean' kolommen komen

# Stap B: Consolidateer trait data door te groeperen op Taxon_Consolidated_Key
# Als er meerdere rijen zijn per geconsolideerde taxon (b.v. Ad. en Lv.), dan gemiddelde traits.
# We consolideren ook Family_Clean en Taxagroup_Clean, en kiezen de eerste voor andere info.
consolidated_trait_data <- trait_data %>%
  group_by(Taxon_Consolidated_Key) %>%
  summarise(
    # Neem de eerste unieke Family_Clean en Taxagroup_Clean voor de geconsolideerde entry
    Family_Clean = first(Family_Clean),
    Taxagroup_Clean = first(Taxagroup_Clean),
    # Bereken het gemiddelde voor alle numerieke trait kolommen
    across(all_of(trait_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = 'drop' # Verwijder de groepering na summarize
  )

# Komt niet voor met tachet. telkens alle traits voor alle taxa
# Vervang eventuele NaN waarden die kunnen ontstaan zijn door mean(NA) met NA
consolidated_trait_data[is.nan(as.matrix(consolidated_trait_data))] <- NA

# Maak de geconsolideerde trait data frame klaar voor matching door Taxon_Consolidated_Key
# als rijnamen in te stellen.
matched_traits_df_for_fd <- consolidated_trait_data %>%
  column_to_rownames(var = "Taxon_Consolidated_Key") %>%
  select(all_of(trait_cols)) # Selecteer alleen de trait kolommen

# --- 3. Taxa uit Community Data extraheren en filteren ---

# De taxa namen zijn de kolomnamen van de community data, exclusief de eerste drie (index, meetplaats, monsternamedatum)
comm_taxa_list_raw <- names(comm_data_raw)[4:ncol(comm_data_raw)]

# Filter de taxa op basis van de Taxonomic_Level kolom in taxon_levels_df
# We behouden alleen taxa die NIET 'Vis' of 'Other' zijn
comm_taxa_list_filtered <- taxon_levels_df %>%
  filter(!Taxonomic_Level %in% c("Vis", "Other")) %>%
  pull(macroinvertebraat) %>%
  base::intersect(comm_taxa_list_raw) # Zorg ervoor dat ze ook daadwerkelijk in de community data voorkomen

# --- 4. Trait Matching (Hiërarchisch en Specifieke Regels) ---

# Maak een leeg dataframe om de gematchte traits op te slaan
matched_traits_output_df <- as.data.frame(matrix(NA,
                                                 nrow = length(comm_taxa_list_filtered),
                                                 ncol = length(trait_cols)))
colnames(matched_traits_output_df) <- trait_cols
rownames(matched_traits_output_df) <- comm_taxa_list_filtered

# Houd een lijst bij van niet-gematchte taxa na filtering
unmatched_taxa <- character()

for (comm_taxon in comm_taxa_list_filtered) {
  comm_taxon_lower <- tolower(trimws(comm_taxon)) # Opschonen van spaties

  # Poging 1: Match op de geconsolideerde Taxon_Consolidated_Key
  match_consolidated_taxon <- matched_traits_df_for_fd %>%
    rownames_to_column(var = "Taxon_Consolidated_Key") %>% # Tijdelijk weer kolom maken voor filter
    filter(Taxon_Consolidated_Key == comm_taxon_lower)

  if (nrow(match_consolidated_taxon) > 0) {
    matched_traits_output_df[comm_taxon, ] <- match_consolidated_taxon[1, trait_cols]
    next
  }

  # Specifieke regel voor Chironomidae groepen
  if (grepl("chironomidae", comm_taxon_lower) && grepl("thummi", comm_taxon_lower)) {
    match_chironomidae <- consolidated_trait_data %>%
      filter(Family_Clean == "chironomidae")
    if (nrow(match_chironomidae) > 0) {
      matched_traits_output_df[comm_taxon, ] <- colMeans(match_chironomidae[, trait_cols], na.rm = TRUE)
      next
    }
  }

  # Specifieke regel voor Syrphidae-Eristalinae
  if (comm_taxon_lower == "syrphidae-eristalinae") {
    match_syrphidae <- consolidated_trait_data %>%
      filter(Family_Clean == "syrphidae")
    if (nrow(match_syrphidae) > 0) {
      matched_traits_output_df[comm_taxon, ] <- colMeans(match_syrphidae[, trait_cols], na.rm = TRUE)
      next
    }
  }

  # Poging 2: Match op schoongemaakte Family (nu van de geconsolideerde data)
  match_family <- consolidated_trait_data %>%
    filter(Family_Clean == comm_taxon_lower)

  if (nrow(match_family) > 0) {
    matched_traits_output_df[comm_taxon, ] <- colMeans(match_family[, trait_cols], na.rm = TRUE)
    next
  }

  # Poging 3: Match op schoongemaakte Taxagroup (nu van de geconsolideerde data)
  match_taxagroup <- consolidated_trait_data %>%
    filter(Taxagroup_Clean == comm_taxon_lower)

  if (nrow(match_taxagroup) > 0) {
    matched_traits_output_df[comm_taxon, ] <- colMeans(match_taxagroup[, trait_cols], na.rm = TRUE)
    next
  }

  # Als er op geen enkel niveau een match is gevonden na filtering
  unmatched_taxa <- c(unmatched_taxa, comm_taxon)
}

# --- 5. Data voorbereiden voor functionele diversiteitsberekening (vervolg) ---

# Filter de gematchte_traits_output_df om rijen met alleen NA's te verwijderen
# Dit zijn taxa waarvoor geen enkele trait is gevonden na de matching, zelfs na de specifieke regels
matched_traits_df_final <- matched_traits_output_df[rowSums(is.na(matched_traits_output_df)) == 0, ]

# Identificeer de taxa die we daadwerkelijk gaan gebruiken (die met complete, gematchte trait data)
taxa_for_fd <- rownames(matched_traits_df_final)

# --- NIEUWE STAP: Normaliseer de fuzzy scores per traitcategorie ---

# Definieer de trait categorieën (prefixes van de kolommen)

trait_categories <- c(
  "longitudinal_distribution", "transversal_distribution", "altitude",
  "substrate", "current_velocity", "temperature", "pH", "salinity",
  "saprobity", "trophic_status", "food", "feeding_habits", "locomotion", # <-- HIER AANGEPAST
  "respiration", "resistance_forms", "maximal_potential_size",
  "dispersal", "life_cycle_duration", "potential_number_of_cycles_per_year",
  "reproduction", "aquatic_stages"
)

# Maak een kopie om te normaliseren
normalized_traits_df <- matched_traits_df_final

for (category in trait_categories) {
  # Vind alle kolommen die tot deze categorie behoren
  # De speciale spatiebehandeling is hier niet meer nodig
  category_pattern <- paste0(category, "_")

  category_cols <- grep(paste0("^", category_pattern),
                        colnames(normalized_traits_df), value = TRUE)

  if (length(category_cols) > 0) {
    # Bereken de rij-som voor deze categorie voor elk taxon
    row_sums <- rowSums(normalized_traits_df[, category_cols], na.rm = TRUE)

    # Normaliseer alleen rijen waar de som > 0 is
    for (i in seq_along(row_sums)) {
      if (row_sums[i] > 0) {
        normalized_traits_df[i, category_cols] <- normalized_traits_df[i, category_cols] / row_sums[i]
      }
      # Als row_sums[i] 0 is, blijven de waarden 0 (geen deling door nul)
      # Als er NA's waren die de sum 0 maakten, blijven ze NA (na.rm = TRUE)
    }
  }
}

# Controleer op eventuele resterende NaN's na normalisatie (door division by zero als na.rm=FALSE was geweest,
# maar met na.rm=TRUE zouden ze NA moeten blijven als de originele som NA was)
normalized_traits_df[is.nan(as.matrix(normalized_traits_df))] <- NA


# Bereid de community matrix voor per meetplaats en monsternamedatum
comm_matrix <- comm_data_raw %>%
  mutate(
    monsternamedatum_str = format(as.Date(monsternamedatum), "%Y-%m-%d"),
    unique_id = paste(meetplaats, monsternamedatum_str, sep = "_")
  ) %>%
  select(unique_id, all_of(taxa_for_fd)) %>% # Selecteer alleen taxa die uiteindelijk gematcht zijn
  column_to_rownames(var = "unique_id") %>%
  as.matrix()

# Vervang eventuele NA's in de community matrix door 0 (als NA's afwezigheid betekenen)
comm_matrix[is.na(comm_matrix)] <- 0

# --- NIEUWE STAP: Verwijder communities (rijen) met nul-som abundanties ---
# Bereken de som van abundanties voor elke rij
row_sums_comm_matrix <- rowSums(comm_matrix, na.rm = TRUE)

# Identificeer rijen waar de som nul is
empty_communities <- names(row_sums_comm_matrix[row_sums_comm_matrix == 0])

if (length(empty_communities) > 0) {
  message(paste("Waarschuwing: De volgende meetplaatsen/data combinaties hebben nul-som abundanties en worden verwijderd (aantal tussen haakjes):",
                paste(empty_communities, collapse = ", "), "(", paste(length(empty_communities), ")")))
  comm_matrix <- comm_matrix[row_sums_comm_matrix > 0, ]
}

# Zorg ervoor dat de kolomnamen van de community matrix en de rijnamen van de trait matrix
# in dezelfde volgorde staan en dezelfde taxa bevatten.
comm_matrix <- comm_matrix[, order(colnames(comm_matrix))]
normalized_traits_df <- normalized_traits_df[order(rownames(normalized_traits_df)), ] # Gebruik de genormaliseerde df

# Controleer nogmaals of de taxa in beide matrices overeenkomen
if (!all(colnames(comm_matrix) == rownames(normalized_traits_df))) {
  stop("Taxa in community matrix en genormaliseerde trait matrix komen niet overeen of staan niet in dezelfde volgorde voor FD berekening!")
}

# --- 6. Functionele Diversiteit berekenen ---

fd_results <- dbFD(
  x = normalized_traits_df, # Gebruik de genormaliseerde traits matrix
  a = comm_matrix,          # Community matrix (rijen zijn meetplaats_monsternamedatum)
  calc.FRic = TRUE,         # Deze blijft wel behouden
  calc.FDiv = TRUE,         # Deze blijft wel behouden
  w.abun = TRUE,            # Gebruik abundanties
  stand.FRic = TRUE,        # Standardiseer functionele rijkdom
  corr = "cailliez",        # Correctie voor negatieve eigenvalues
  m = "min"                 # Minimale aantal PCoA dimensies
)

# Bereken de Trait Coverage per sampling event ---

# Bereid een dataframe voor met de totale abundanties van de oorspronkelijke data
total_raw_abundances_df <- comm_data_raw %>%
  mutate(
    monsternamedatum_str = format(as.Date(monsternamedatum), "%Y-%m-%d"),
    unique_id = paste(meetplaats, monsternamedatum_str, sep = "_")
  ) %>%
  # Selecteer alle taxa-kolommen en de unieke ID
  select(-meetplaats, -monsternamedatum, -monsternamedatum_str) %>%
  # Bereken de rij-som voor elke unieke ID
  mutate(total_raw_abundance = rowSums(select(., c(-unique_id)))) %>%
  select(unique_id, total_raw_abundance)

# Haal de unieke sampling IDs uit de comm_matrix
sampling_ids <- rownames(comm_matrix)

# Maak een dataframe met de totale abundanties van de gematchte taxa
total_matched_abundances_df <- tibble(
  unique_id = sampling_ids,
  total_matched_abundance = rowSums(comm_matrix, na.rm = TRUE)
)

# Bereken de trait coverage door de twee dataframes samen te voegen
trait_coverage_df <- total_matched_abundances_df %>%
  left_join(total_raw_abundances_df, by = "unique_id") %>%
  mutate(
    trait_coverage_percentage = (total_matched_abundance / total_raw_abundance) * 100
  ) %>%
  # Vervang NaN's (deling door nul) en oneindige waarden met 0
  mutate(trait_coverage_percentage = replace_na(trait_coverage_percentage, 0)) %>%
  mutate(trait_coverage_percentage = ifelse(is.infinite(trait_coverage_percentage), 0, trait_coverage_percentage)) %>%
  select(unique_id, trait_coverage_percentage)

# filter samples met trait coverage lager dan x%
trait_coverage_df %>%
  filter(trait_coverage_percentage > 80) %>%
  nrow

fdisp_mi <- fd_results$FDis %>%
  as.data.frame %>%
  rownames_to_column(var = "unique_id") %>%
  rename(fdisp = ".") %>%
  left_join(.,
            trait_coverage_df,
            by = "unique_id") %>%
  left_join(.,
            comm_data_raw %>%
              mutate(
                monsternamedatum_str = format(as.Date(monsternamedatum), "%Y-%m-%d"),
                unique_id = paste(meetplaats, monsternamedatum_str, sep = "_")) %>%
              select(monsternamedatum, meetplaats, unique_id),
            by = "unique_id")
save(fdisp_mi, file = here("data", "verwerkt", "mi_fd.rdata"))

ggplot(fdisp_mi
       %>%
         filter(monsternamedatum > '2007/12/31'), aes(monsternamedatum, fdisp)) +
  # geom_point() +
  geom_smooth()



