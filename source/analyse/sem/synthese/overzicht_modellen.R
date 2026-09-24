# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
library(dplyr)
library(stringr)
library(purrr)

# 1. Definieer de map (Let op: in R gebruik je forward slashes '/')
map_pad <- "C:/Users/emiel_delombaerde/Documents/R/toestand-waterlopen/source/analyse/sem/synthese/sem_output"

# 2. Zoek alle .rds bestanden in deze map
bestanden <- list.files(path = map_pad, pattern = "\\.rds$", full.names = TRUE)

if (length(bestanden) == 0) {
  message("⚠️ Geen .rds bestanden gevonden in de opgegeven map.")
} else {
  message(sprintf("🔄 %d .rds bestanden gevonden. Bezig met samenvoegen...", length(bestanden)))

  # 3. Loop over elk bestand, lees in, extraheer naam-info, en bind alles samen
  master_df <- purrr::map_dfr(bestanden, function(pad) {

    # Lees het .rds dataframe in
    df <- readRDS(pad)

    # Haal de pure bestandsnaam op (bijv. "mi_rivier_tax.rds")
    bestandsnaam <- basename(pad)

    # Verwijder de ".rds" extensie (bijv. "mi_rivier_tax")
    naam_zonder_ext <- str_remove(bestandsnaam, "\\.rds$")

    # Splits de string op de underscores
    naam_delen <- str_split(naam_zonder_ext, "_")[[1]]

    # Haal de onderdelen eruit (met een fallback voor als een naam afwijkt)
    # Als 'maatlat' zelf underscores bevat, plakken we de rest weer netjes aan elkaar.
    groep_val     <- ifelse(length(naam_delen) >= 1, naam_delen[1], NA)
    typologie_val <- ifelse(length(naam_delen) >= 2, naam_delen[2], NA)
    maatlat_val   <- ifelse(length(naam_delen) >= 3, paste(naam_delen[3:length(naam_delen)], collapse = "_"), NA)

    # Voeg de kolommen vooraan toe aan het dataframe
    df <- df %>%
      mutate(
        groep     = groep_val,
        typologie = typologie_val,
        maatlat   = maatlat_val,
        .before   = 1
      )

    return(df)
  })

  message("✅ master_df is succesvol aangemaakt!")
}


########
# check de upstream paths voor verschillen #
########

# 1. Definieer jouw biologische eindpunten.
# Dit zijn de vars waarnaar we NU even niet willen kijken,
# omdat deze per definitie uniek zijn per deelmaatlat-model.
biologische_vars <- c("mmif", "ept_prop", "sw_dw", "ta_xw", "mt_sw_prop", "nst_prop", "index_nieuw", "gv_zonder_gep", "v_zonder_gep", "vo_zonder_gep", "ts_zonder_gep")

# 2. Bouw de check
check_verschillen <- master_df %>%
  # Filter de biologische pijlen eruit, we willen alleen "Milieu -> Milieu" paden
  filter(!Response %in% biologische_vars) %>%

  # groepeer per uniek pad, per groep en typologie
  group_by(groep, typologie, Predictor, Response) %>%

  # Bereken de verschillen voor dit specifieke pad over de verschillende maatlat-modellen
  summarise(
    aantal_modellen = n(), # In hoeveel maatlat-modellen komt dit pad voor?
    welke_maatlatten = paste(maatlat, collapse = ", "),

    # Zoek de maximale en minimale Estimate (effectgrootte) en P-waarde
    min_est = min(Std.Estimate, na.rm = TRUE),
    max_est = max(Std.Estimate, na.rm = TRUE),
    verschil_estimate = max_est - min_est,

    min_p = min(P.Value, na.rm = TRUE),
    max_p = max(P.Value, na.rm = TRUE),
    verschil_p = max_p - min_p,

    .groups = "drop"
  ) %>%

  # Filter: Laat alleen de paden zien waar daadwerkelijk een verschil in zit!
  # (We gebruiken > 1e-4 (0.0001) om te voorkomen dat R puur op
  # irrelevante wiskundige afrondingsfoutjes achter de komma triggert).
  filter(verschil_estimate > 1e-4 | verschil_p > 1e-4) %>%

  # Sorteer zodat de meest afwijkende paden bovenaan staan
  arrange(desc(verschil_estimate))

# Bekijk het resultaat
head(check_verschillen, 10) # verschillen in kempen door probleem met dsep test?



# ==============================================================================
# HULPSTAP: Bereken de theoretische maxima per groep
# ==============================================================================
# Maximaal aantal unieke typologieën per groep (voor de abiotische tabel)
max_typo_df <- master_df %>%
  group_by(groep) %>%
  summarise(max_typo = n_distinct(typologie), .groups = "drop")

# Maximaal aantal biologische respons-modellen per groep (voor Tabel 2)
# (Dit telt alle unieke combinaties van typologie + Maatlat)
max_biomod_df <- master_df %>%
  filter(Response %in% biologische_vars) %>%
  group_by(groep) %>%
  summarise(max_biomod = n_distinct(paste(typologie, Response)), .groups = "drop")


# ==============================================================================
# TABEL 1: DE ABIOTISCHE 'RUGGENGRAAT' (Nu opgesplitst per MI / MAFY)
# ==============================================================================
abiotische_synthese <- master_df %>%
  # Selecteer alleen de milieu -> milieu paden
  filter(!Response %in% biologische_vars) %>%

  # Ontdubbelen (één abiotische realiteit per groep + typologie)
  distinct(groep, typologie, Predictor, Response, .keep_all = TRUE) %>%

  # groepeer nu mét 'groep' erbij
  group_by(groep, Predictor, Response) %>%
  summarise(
    aantal_keren_significant = n(),
    in_welke_typologieen = paste(unique(typologie), collapse = ", "),
    gem_effect = round(mean(Estimate, na.rm = TRUE), 3),
    min_effect = round(min(Estimate, na.rm = TRUE), 3),
    max_effect = round(max(Estimate, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%

  # Koppel het maximum aantal typologieën en bereken het percentage
  left_join(max_typo_df, by = "groep") %>%
  mutate(
    percentage_significant = paste0(round((aantal_keren_significant / max_typo) * 100, 0), "%"),
    tegenstelling = ifelse(min_effect < 0 & max_effect > 0, "⚠️ JA", "Nee")
  ) %>%
  # Sorteer op groep en op hoe vaak het mechanisme voorkomt
  select(-max_typo) %>% # Ruim de hulpkolom op
  arrange(groep, desc(aantal_keren_significant))


# ==============================================================================
# TABEL 2: DE BIOLOGISCHE EFFECTEN (Eindpunten)
# ==============================================================================
biologische_synthese <- master_df %>%
  # Selecteer alleen de directe paden naar de biologie
  filter(Response %in% biologische_vars) %>%

  # groepeer per biologische groep en stressor
  group_by(groep, Predictor) %>%
  summarise(
    aantal_modellen_geraakt = n(),
    welke_maatlatten = paste(unique(Response), collapse = ", "),
    in_welke_typologieen = paste(unique(typologie), collapse = ", "),
    gem_effect = round(mean(Estimate, na.rm = TRUE), 3),
    min_effect = round(min(Estimate, na.rm = TRUE), 3),
    max_effect = round(max(Estimate, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%

  # Koppel het maximum aantal bio-modellen en bereken het percentage
  left_join(max_biomod_df, by = "groep") %>%
  mutate(
    percentage_significant = paste0(round((aantal_modellen_geraakt / max_biomod) * 100, 0), "%"),
    tegenstelling = ifelse(min_effect < 0 & max_effect > 0, "⚠️ JA", "Nee")
  ) %>%
  select(-max_biomod) %>%
  arrange(groep, desc(aantal_modellen_geraakt))

# Bekijk de resultaten
head(abiotische_synthese, 15)
head(biologische_synthese, 15)

