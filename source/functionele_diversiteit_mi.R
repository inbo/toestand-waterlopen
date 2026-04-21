# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(lubridate::year)
#---------------------------------------------------------------------------------------------------
# --- 1. Gegevens inlezen ---
#---------------------------------------------------------------------------------------------------

load(file = here("data", "verwerkt", "mi_soorten.rdata"))

comm_data_raw <- mi_soorten %>%
  select(meetplaats, monsternamedatum, deelmonster_id, macroinvertebraat, aantal) %>%
  pivot_wider(., names_from = macroinvertebraat, values_from = aantal, values_fill = list(aantal = 0))

trait_data_raw <- read_excel(here("data", "ruw", "macroinvertebraten", "traits", "tachet_traits_mi.xlsx")) %>%
  janitor::clean_names()

taxon_levels_df <- read_csv(here("data", "ruw", "macroinvertebraten", "traits", "taxonlist_with_taxonomic_level.csv"))

#---------------------------------------------------------------------------------------------------
# --- 2. Trait Data voorbereiden (Alle beschikbare traits) ---
#---------------------------------------------------------------------------------------------------

# Stap A: Namen opschonen
trait_data_all <- trait_data_raw %>%
  mutate(
    Taxon_Consolidated_Key = tolower(str_replace_all(taxon, " sp\\.| Ad\\.| Lv\\.| Gen\\.", "")),
    Family_Clean = tolower(str_replace_all(family, "\\[Ord:|\\]", "")),
    Taxagroup_Clean = tolower(taxagroup)
  )

# Identificeer ALLE numerieke trait kolommen (alles wat begint met een bekende categorie)
# We selecteren hier alles behalve de taxonomische ID-kolommen
trait_cols <- names(trait_data_all)[which(sapply(trait_data_all, is.numeric))]

# Stap B: Consolidateer trait data (gemiddelde over Ad./Lv. etc)
consolidated_trait_data <- trait_data_all %>%
  group_by(Taxon_Consolidated_Key) %>%
  summarise(
    Family_Clean = first(Family_Clean),
    Taxagroup_Clean = first(Taxagroup_Clean),
    across(all_of(trait_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = 'drop'
  )

# NaN handling
consolidated_trait_data[is.nan(as.matrix(consolidated_trait_data))] <- NA

# Maak basis matrix voor matching
matched_traits_df_for_fd <- consolidated_trait_data %>%
  column_to_rownames(var = "Taxon_Consolidated_Key") %>%
  select(all_of(trait_cols))

#---------------------------------------------------------------------------------------------------
# --- 3. Taxa Filteren en Matching ---
#---------------------------------------------------------------------------------------------------

comm_taxa_list_raw <- names(comm_data_raw)[4:ncol(comm_data_raw)]
comm_taxa_list_filtered <- taxon_levels_df %>%
  filter(!Taxonomic_Level %in% c("Vis", "Other")) %>%
  pull(macroinvertebraat) %>%
  base::intersect(comm_taxa_list_raw)

matched_traits_output_df <- as.data.frame(matrix(NA,
                                                 nrow = length(comm_taxa_list_filtered),
                                                 ncol = length(trait_cols)))
colnames(matched_traits_output_df) <- trait_cols
rownames(matched_traits_output_df) <- comm_taxa_list_filtered

for (comm_taxon in comm_taxa_list_filtered) {
  comm_taxon_lower <- tolower(trimws(comm_taxon))

  # 1. Directe match
  if (comm_taxon_lower %in% rownames(matched_traits_df_for_fd)) {
    matched_traits_output_df[comm_taxon, ] <- matched_traits_df_for_fd[comm_taxon_lower, trait_cols]
    next
  }

  # 2. Specifieke regels (Chironomidae/Syrphidae)
  if (grepl("chironomidae", comm_taxon_lower)) {
    match_fam <- consolidated_trait_data %>% filter(Family_Clean == "chironomidae")
    if (nrow(match_fam) > 0) { matched_traits_output_df[comm_taxon, ] <- colMeans(match_fam[, trait_cols], na.rm = TRUE); next }
  }

  if (grepl("syrphidae", comm_taxon_lower)) {
    match_fam <- consolidated_trait_data %>% filter(Family_Clean == "syrphidae")
    if (nrow(match_fam) > 0) { matched_traits_output_df[comm_taxon, ] <- colMeans(match_fam[, trait_cols], na.rm = TRUE); next }
  }

  # 3. Familie/Groep match
  match_fam <- consolidated_trait_data %>% filter(Family_Clean == comm_taxon_lower)
  if (nrow(match_fam) > 0) { matched_traits_output_df[comm_taxon, ] <- colMeans(match_fam[, trait_cols], na.rm = TRUE); next }

  match_group <- consolidated_trait_data %>% filter(Taxagroup_Clean == comm_taxon_lower)
  if (nrow(match_group) > 0) { matched_traits_output_df[comm_taxon, ] <- colMeans(match_group[, trait_cols], na.rm = TRUE); next }
}

#---------------------------------------------------------------------------------------------------
# --- 4. Volledige Normalisatie ---
#---------------------------------------------------------------------------------------------------

matched_traits_df_final <- matched_traits_output_df[rowSums(is.na(matched_traits_output_df)) == 0, ]
taxa_for_fd <- rownames(matched_traits_df_final)

trait_categories <- c(
  "longitudinal_distribution", "transversal_distribution", "altitude",
  "substrate", "current_velocity", "temperature", "ph", "salinity",
  "saprobity", "trophic_status", "food", "feeding_habits", "locomotion",
  "respiration", "resistance_forms", "maximal_potential_size",
  "dispersal", "life_cycle_duration", "potential_number_of_cycles_per_year",
  "reproduction", "aquatic_stages"
)

normalized_traits_all <- matched_traits_df_final

for (category in trait_categories) {
  category_pattern <- paste0("^", category, "_")
  category_cols <- grep(category_pattern, colnames(normalized_traits_all), value = TRUE)

  if (length(category_cols) > 0) {
    row_sums <- rowSums(normalized_traits_all[, category_cols], na.rm = TRUE)
    for (i in seq_along(row_sums)) {
      if (row_sums[i] > 0) {
        normalized_traits_all[i, category_cols] <- normalized_traits_all[i, category_cols] / row_sums[i]
      }
    }
  }
}

#---------------------------------------------------------------------------------------------------
# --- 5. Community Matrix Voorbereiden ---
#---------------------------------------------------------------------------------------------------

comm_matrix_full <- comm_data_raw %>%
  mutate(unique_id = as.character(deelmonster_id)) %>%
  select(unique_id, all_of(taxa_for_fd)) %>%
  column_to_rownames(var = "unique_id") %>%
  as.matrix()

comm_matrix_full[is.na(comm_matrix_full)] <- 0
comm_matrix_full <- comm_matrix_full[rowSums(comm_matrix_full) > 0, ]
comm_matrix_full <- comm_matrix_full[, order(colnames(comm_matrix_full))]
normalized_traits_all <- normalized_traits_all[order(rownames(normalized_traits_all)), ]

#---------------------------------------------------------------------------------------------------
# --- 6. FD BEREKENEN (Loop over Subsets) ---
#---------------------------------------------------------------------------------------------------

# Definieer de subsets
subsets <- list(
  waterkwaliteit = c("saprobity_", "trophic_status_", "respiration_", "ph_"),
  habitat        = c("substrate_", "current_velocity_", "locomotion_"),
  full           = c("saprobity_", "dispersal_", "reproduction_", "locomotion_",
                     "substrate_", "current_velocity_", "trophic_status_", "temperature_",
                     "respiration_")
)

# Maak een lijst om de resultaten per subset op te slaan
subset_results_list <- list()

for (subset_name in names(subsets)) {
  message(paste("Berekenen van FD voor subset:", subset_name))

  # Selecteer de relevante traits
  traits_subset <- normalized_traits_all %>%
    select(starts_with(subsets[[subset_name]]))

  # Bereken FD
  res <- dbFD(
    x = traits_subset,
    a = comm_matrix_full,
    calc.FRic = TRUE, calc.FDiv = TRUE,
    w.abun = TRUE, stand.FRic = TRUE,
    corr = "cailliez", m = "min"
  )

  # Zet om naar dataframe en voeg suffix toe aan kolomnamen
  subset_df <- tibble(
    unique_id = rownames(comm_matrix_full),
    fdisp = res$FDis,
    fric  = res$FRic,
    feve  = res$FEve,
    fdiv  = res$FDiv
  ) %>%
    rename_with(~ paste0(., "_", subset_name), -unique_id)

  subset_results_list[[subset_name]] <- subset_df
}

# Combineer alle subsets in één breed dataframe
fd_combined_df <- subset_results_list %>%
  reduce(left_join, by = "unique_id")

#---------------------------------------------------------------------------------------------------
# --- 7. Trait Coverage & Finale Export ---
#---------------------------------------------------------------------------------------------------

# Bereken de totale ruwe abundantie per staal (voor alle taxa, ook de niet-gematchte)
total_raw_abundances <- rowSums(comm_data_raw[, 4:ncol(comm_data_raw)], na.rm = TRUE)
names(total_raw_abundances) <- as.character(comm_data_raw$deelmonster_id)

# Bereid de coverage data voor
trait_coverage_df <- tibble(
  unique_id = rownames(comm_matrix_full),
  total_matched_abundance = rowSums(comm_matrix_full),
  total_raw_abundance = total_raw_abundances[rownames(comm_matrix_full)],
  trait_coverage_percentage = (total_matched_abundance / total_raw_abundance) * 100
)

# Combineer alles en aggregeer per meetplaats/datum (indien er meerdere deelmonsters zijn)
fd_mi <- fd_combined_df %>%
  left_join(trait_coverage_df, by = "unique_id") %>%
  left_join(
    comm_data_raw %>%
      mutate(unique_id = as.character(deelmonster_id)) %>%
      select(unique_id, meetplaats, monsternamedatum),
    by = "unique_id"
  ) %>%
  select(-unique_id, -total_matched_abundance, -total_raw_abundance) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Opslaan
save(fd_mi, file = here("data", "verwerkt", "mi_fd_multiset.rdata"))
load(file = here("data", "verwerkt", "mi_fd_multiset.rdata"))

#---------------------------------------------------------------------------------------------------
# --- 8. Bonus: Snelle Check/Visualisatie ---
#---------------------------------------------------------------------------------------------------

# Vergelijk bijvoorbeeld FDis Waterkwaliteit met Habitat
ggplot(fd_mi, aes(x = fdisp_waterkwaliteit, y = fdisp_habitat)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Vergelijking FDis: Waterkwaliteit vs Habitat")

fd_mi_long <- fd_mi %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  pivot_longer(fdisp_waterkwaliteit:fdiv_full, names_to = "fd_variabele", values_to = "fd_waarde") %>%
  separate_wider_delim(
    cols = fd_variabele,
    delim = "_",
    names = c("fd_maat", "type_fd")
  )

fd_mi_long %>%
  ggplot(aes(jaar, fd_waarde, color = type_fd)) +
  geom_smooth(method = "gam") +
  facet_grid(~fd_maat)

# link met mi data
fd_mi_plot_data <- fd_mi_long %>%
left_join(
  mi_data %>%
    select(meetplaats, monsternamedatum, groep, statuut, type, bekken),
  by = c("meetplaats", "monsternamedatum")
) %>%
  mutate(
    subset = case_when(
      # 1. Natuurlijk en Sterk Veranderd per groep
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "rivier" ~ "nat_sv_rivier",

      # 2. Kunstmatig (onafhankelijk van groep)
      statuut == "Kunstmatig"                                            ~ "kunstmatig",

      # 3. Specifieke types zoals RtNt
      type == "RtNt"                                                     ~ "rtnt",

      # Restgroep (optioneel, voor alles wat niet in bovenstaande valt)
      TRUE                                                               ~ "overig"
    )
  )

fd_mi_plot_data %>%
  filter(fd_maat == "fdisp") %>%
  filter(jaar > 2009) %>%
  ggplot(aes(jaar, fd_waarde, color = type_fd)) +
  geom_smooth(method = "gam") +
  facet_grid(~subset)

### Figuur op bais van GAMs

library(tidyverse)
library(mgcv)
library(ggeffects)

fd_nested <- fd_mi_plot_data %>%
  filter(fd_maat == "fdisp") %>%
  filter(jaar > 2009) %>%
  filter(subset != "overig") %>%
  mutate(
    subset = as.factor(subset),
    type_fd = as.factor(type_fd),
    meetplaats = as.factor(meetplaats),
    bekken = as.factor(bekken),
    jaar_num = as.numeric(jaar)
  ) %>%
  drop_na(fd_waarde, jaar_num, meetplaats, bekken) %>%
  group_nest(subset, type_fd)

fd_models <- fd_nested %>%
  mutate(
    model = map(data, ~ gam(fd_waarde ~ s(jaar_num, k = 4) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re"),
                            data = .x,
                            method = "REML")),

    predictions = map(model, ~ {
      if (is.null(.x)) return(NULL)
      # ggaverage berekent het gemiddelde effect, corrigerend voor de random effects
      res <- ggaverage(.x, terms = "jaar_num [all]")
      as.data.frame(res)
    })
  )

fd_trends <- fd_models %>%
  select(subset, type_fd, predictions) %>%
  unnest(predictions)

plot_fd_gam <- ggplot(fd_trends, aes(x = x, y = predicted, group = type_fd)) +
  # Betrouwbaarheidsintervallen
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = type_fd), alpha = 0.2) +
  # De trendlijn
  geom_line(aes(color = type_fd), size = 1) +
  # Faceting op basis van je subset (natuurlijk, kunstmatig, etc.)
  facet_wrap(~ subset, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Functionele Diversiteit (FDisp) Trends",
    subtitle = "Gefit met Mixed Effect GAM (Random Effects: Meetplaats & Bekken)",
    x = "Jaar",
    y = "Voorspelde FDisp waarde",
    color = "Type FD",
    fill = "Type FD"
  )

print(plot_fd_gam)
ggsave(
  filename =  here("output", "figuren", "trend_fdisp_subsets.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)
################################################################################
# CWM exploratie
################################################################################

# Haal de CWM resultaten specifiek van jouw 'full' definitie
# (Zorg dat 'res_full' de output is van de dbFD met jouw 8 geselecteerde trait-groepen)
cwm_full_data <- as.data.frame(res$CWM) %>% #laatste res uit loop -> die van _full hier!!!!
  rownames_to_column("unique_id") %>%
  left_join(
    comm_data_raw %>%
      mutate(unique_id = as.character(deelmonster_id)) %>%
      select(unique_id, meetplaats, monsternamedatum),
    by = "unique_id"
  )

save(cwm_full_data, file = here("data", "verwerkt", "mi_cwm.rdata"))
load(file = here("data", "verwerkt", "mi_cwm.rdata"))

load(file = here("data", "verwerkt", "mi_data.rdata"))

cwm_mi_plot_data <- cwm_full_data %>%
  left_join(
    mi_data %>%
      select(meetplaats, monsternamedatum, groep, statuut, type),
    by = c("meetplaats", "monsternamedatum")
  ) %>%
  mutate(
    subset = case_when(
      # 1. Natuurlijk en Sterk Veranderd per groep
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") &
        groep == "rivier" ~ "nat_sv_rivier",

      # 2. Kunstmatig (onafhankelijk van groep)
      statuut == "Kunstmatig"                                            ~ "kunstmatig",

      # 3. Specifieke types zoals RtNt
      type == "RtNt"                                                     ~ "rtnt",

      # Restgroep (optioneel, voor alles wat niet in bovenstaande valt)
      TRUE                                                               ~ "overig"
    )
  )

library(scales) # Voor nette percentage formatting

plot_cwm_subsets_jaar <- function(data, category_prefix, selected_subsets, start_jaar) {

  # 1. Filter data en bereid jaarlijkse gemiddelden voor PER SUBSET
  plot_df <- data %>%
    filter(subset %in% selected_subsets) %>%
    mutate(jaar = lubridate::year(monsternamedatum)) %>%
    # Groepeer op jaar EN subset
    group_by(jaar, subset) %>%
    summarise(across(starts_with(category_prefix), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    # Omzetten naar lang formaat voor de stacks
    pivot_longer(cols = starts_with(category_prefix),
                 names_to = "Modaliteit",
                 values_to = "Waarde") %>%
    mutate(Modaliteit = str_replace(Modaliteit, category_prefix, ""),
           subset = factor(subset, levels = selected_subsets)) %>%
    # Procentueel label (alleen voor segmenten > 5% voor leesbaarheid)
    mutate(label = ifelse(Waarde > 0.05, percent(Waarde, accuracy = 1), "")) %>%
    filter(jaar > (start_jaar - 1))

  # 2. De Plot met Faceting
  ggplot(plot_df, aes(x = factor(jaar), y = Waarde, fill = Modaliteit)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 2.5, color = "white", fontface = "bold") +
    # HIER GEBEURT HET: splits de plot in kolommen op basis van subset
    facet_wrap(~ subset, scales = "free_x") +
    scale_fill_viridis_d(option = "viridis") +
    theme_minimal() +
    labs(title = paste("Trendvergelijking:", str_remove(category_prefix, "_")),
         subtitle = "Jaarlijkse CWM profielen per geselecteerde subset",
         x = "Jaar", y = "Proportie", fill = "Kenmerk") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
          legend.position = "bottom",
          strip.background = element_rect(fill = "gray90"), # subset koppen accentueren
          strip.text = element_text(face = "bold"))
}

# Voorbeeld: Vergelijk saprobiteit tussen alle 'natuurlijke' regio's
plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "saprobity_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)

plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "current_velocity_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)

plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "respiration_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)

plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "temperature_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)

plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "trophic_status_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)

plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "substrate_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)
plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "reproduction_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)

plot_cwm_subsets_jaar(
  data = cwm_mi_plot_data,
  category_prefix = "dispersal_",
  selected_subsets = c("nat_sv_beek", "nat_sv_kempen", "nat_sv_rivier", "nat_sv_polder", "kunstmatig", "rtnt"),
  start_jaar = 2010
)
