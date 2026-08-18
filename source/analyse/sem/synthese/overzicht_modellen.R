# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))

model_overzicht <- read.csv("source/analyse/sem/synthese/model_overzicht.csv", sep = ";")

# Maak een lege lijst om alle gefilterde dataframes in op te slaan
alle_significante_paden <- list()

# Loop over elke rij van je overzichtstabel
for (i in 1:nrow(model_overzicht)) {

  # 1. Haal de metadata en het pad op voor de huidige iteratie
  huidige_groep <- model_overzicht$groep[i]
  huidige_typo <- model_overzicht$typologie[i]
  huidige_maatlat <- model_overzicht$maatlat[i]
  huidig_pad <- model_overzicht$bestandspad[i]

  message(sprintf("Bezig met inlezen model %d/%d: %s - %s - %s",
                  i, nrow(model_overzicht), huidige_groep, huidige_typo, huidige_maatlat))

  # 2. Laad het .rdata bestand veilig in een tijdelijke environment
  # (zodat het geen andere objecten overschrijft en we de naam kunnen vangen)
  temp_env <- new.env()
  tryCatch({
    load(huidig_pad, envir = temp_env)
  }, error = function(e) {
    message("⚠️ Fout bij inlezen van: ", huidig_pad)
    return(NULL)
  })

  # 3. Pak het pSEM object (het eerste en hopelijk enige object in de .rdata)
  obj_naam <- ls(temp_env)[1]
  psem_obj <- temp_env[[obj_naam]]

  # 4. Pas jouw standaardisatie functie toe
  # TryCatch voorkomt dat de hele loop crasht als één model een foutmelding geeft
  coefs_berekend <- tryCatch({
    standardize_psem(psem_obj)[, -9] # Jouw aanroep, we laten kolom 9 vallen
  }, error = function(e) {
    message("⚠️ Fout bij standaardiseren van: ", huidig_pad)
    return(NULL) # Skip dit model bij een error
  })

  # Ga door naar het volgende model als dit model leeg of mislukt is
  if (is.null(coefs_berekend)) next

  # 5. Filter op Significante paden (Meestal P.Value < 0.05 in piecewiseSEM)
  # (Controleer even of de kolom exact "P.Value" heet in jouw output!)
  significante_paden <- coefs_berekend %>%
    filter(P.Value < 0.05) %>%
    # Voeg de metadata kolommen toe aan het begin van het dataframe
    mutate(
      groep = huidige_groep,
      typologie = huidige_typo,
      maatlat = huidige_maatlat,
      .before = 1
    )

  # 6. Sla het resultaat op in de lijst
  alle_significante_paden[[i]] <- significante_paden
}

# 7. Plak alle individuele tabellen uit de lijst samen tot één Master Dataframe
master_df <- bind_rows(alle_significante_paden)

message("✅ Oogsten voltooid! Bekijk master_df.")

########
# check de upstream paths voor verschillen #
########

# 1. Definieer jouw biologische eindpunten.
# Dit zijn de vars waarnaar we NU even niet willen kijken,
# omdat deze per definitie uniek zijn per deelmaatlat-model.
biologische_vars <- c("mmif", "ept_prop", "sw_dw", "ta_xw", "index_nieuw", "gv_zonder_gep", "v_zonder_gep", "vo_zonder_gep", "ts_zonder_gep")

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
    min_est = min(Estimate, na.rm = TRUE),
    max_est = max(Estimate, na.rm = TRUE),
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
head(check_verschillen, 10)



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

