
load(here("data", "verwerkt", "pesticiden_ec50.rdata"))

load(here("data", "verwerkt", "fc_data.rdata"))

chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"),
                                header = T, sep = ";", na.strings = "nvt")


# -------------------------------------------------------------------------
# STAP 1 & 2: KOPPELEN EN TU BEREKENEN (Vereenvoudigd)
# -------------------------------------------------------------------------

ec50_clean <- pesticiden_ec50 %>%
  select(stof_symbool, cas, stof_naam, final_ec50_ug_L, sensitive_group, sensitive_species) %>%
  distinct(stof_symbool, .keep_all = TRUE)

tu_dataset <- fc_data %>%
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

tu_per_sample <- tu_dataset %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    # 1. De TU Som (msPAF benadering via Concentration Addition)
    TU_sum = sum(TU_individual, na.rm = TRUE),

    # 2. Metadata (Handig voor analyse achteraf)
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
  select(stof_symbool, subtype) %>% # Zorg dat kolomnamen matchen met tu_dataset
  distinct()

tu_specific_groups <- tu_dataset %>%
  # Koppel de classificatie (als die er nog niet in zat)
  left_join(classificatie_info, by = c("parameter_symbool" = "stof_symbool")) %>%

  # Groepeer per staal
  group_by(meetplaats, monsternamedatum) %>%

  summarise(
    # 1. De Totaalscore (zoals je al had)
    TU_sum = sum(TU_individual, na.rm = TRUE),

    # 2. Specifieke scores (Mode of Action)
    # Let op: check in je data of 'insecticide' met hoofdletter is of niet
    TU_insecticide = sum(TU_individual[subtype == "insecticide"], na.rm = TRUE),
    TU_herbicide   = sum(TU_individual[subtype == "herbicide"], na.rm = TRUE),
    TU_fungicide   = sum(TU_individual[subtype == "fungicide"], na.rm = TRUE),

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
save(tu_per_sample, tu_dataset, file = here("data", "verwerkt", "tu_resultaten.rdata"))


# Check de EC50 waarden van de boosdoeners
check_drivers <- pesticiden_ec50 %>%
  filter(stof_naam %in% c("Malathion", "Diuron", "Imidacloprid")) %>%
  select(stof_naam, cas, final_ec50_ug_L, sensitive_group, sensitive_species)

print(check_drivers)

#### Aggregatie TU pesticiden per jaar ####
# -------------------------------------------------------------------------
# STAP 1: CHEMIE AGGREGEREN NAAR JAAR-NIVEAU
# -------------------------------------------------------------------------

tu_jaarlijks <- tu_per_sample %>%
  mutate(jaar = year(monsternamedatum)) %>%
  group_by(meetplaats, jaar) %>%
  summarise(
    # 1. Toxische Druk parameters
    # Mean: Wat is de gemiddelde druk gedurende het jaar?
    TU_jaar_gem = mean(TU_sum, na.rm = TRUE),

    # Max: Wat was de ergste piek dat jaar? (Vaak belangrijker voor sterfte)
    TU_jaar_max = max(TU_sum, na.rm = TRUE),

    # P90: De 90-percentiel (standaard in KRW toetsing)
    TU_jaar_P90 = quantile(TU_sum, 0.90, na.rm = TRUE),

    # 2. Kwaliteit van de data
    aantal_metingen = n(),

    # 3. Wie was de grootste boosdoener dit jaar?
    # We pakken de driver van het staal met de hoogste TU
    driver_van_het_jaar = driver_stof[which.max(TU_sum)],
    driver_groep_van_het_jaar = driver_group[which.max(TU_sum)],

    .groups = "drop"
  )

# -------------------------------------------------------------------------
# STAP 2: BIOLOGIE AGGREGEREN NAAR JAAR-NIVEAU
# -------------------------------------------------------------------------

mi_jaarlijks <- mi_nat_sv %>%
  # Verwijder geometrie voor snellere verwerking (voeg later toe indien nodig)
  st_drop_geometry() %>%
  mutate(jaar = year(monsternamedatum)) %>%
  filter(jaar > 2009) %>%

  # Als er meerdere biologiesamples in 1 jaar zijn op 1 plek, middelen we ze uit
  group_by(meetplaats, jaar) %>%
  summarise(
    # Voor numerieke kolommen (EQR scores): gemiddelde
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),

    # Voor tekstkolommen (watertype e.d.): eerste waarde behouden
    across(where(is.character) | where(is.factor), ~ dplyr::first(.)),

    .groups = "drop"
  )

# -------------------------------------------------------------------------
# STAP 3: DE KOPPELING (JOIN)
# -------------------------------------------------------------------------

# OPTIE A: STRICTE KOPPELING (Zelfde Jaar)
# Biologie 2015 wordt gekoppeld aan Chemie 2015
data_matched_same_year <- mi_jaarlijks %>%
  inner_join(tu_jaarlijks, by = c("meetplaats", "jaar"))

# OPTIE B: LAG KOPPELING (Huidig Jaar + Vorig Jaar) - AANBEVOLEN
# Biologie wordt beïnvloed door chemie van dit jaar én vorig jaar.
# We koppelen chemie data van jaar X aan biologie jaar X.
# Als dat niet lukt, of als aanvulling, kijken we naar jaar X-1.

# We maken een kopie van chemie, maar zetten het jaar eentje vooruit
# Zodat Chemie 2014 matcht met Biologie 2015
tu_vorig_jaar <- tu_jaarlijks %>%
  mutate(match_jaar = jaar + 1) %>%
  select(meetplaats, match_jaar, TU_prev_mean = TU_jaar_gem, TU_prev_max = TU_jaar_max)

data_matched_robust <- mi_jaarlijks %>%
  # 1. Koppel met Chemie van HETZELFDE jaar
  left_join(tu_jaarlijks, by = c("meetplaats", "jaar")) %>%

  # 2. Koppel met Chemie van VORIG jaar
  left_join(tu_vorig_jaar, by = c("meetplaats", "jaar" = "match_jaar")) %>%

  # 3. Filter: We willen biologie waar we TENMINSTE chemie data van hebben
  # (van dit jaar OF vorig jaar)
  filter(!is.na(TU_jaar_gem) | !is.na(TU_prev_mean)) %>%

  mutate(
    # Maak een 'Beste Schatting' TU
    # Als we data van dit jaar hebben, gebruik die. Anders vorig jaar.
    # Of neem het gemiddelde van beide als je wilt.
    TU_final_max = pmax(TU_jaar_max, TU_prev_max, na.rm = TRUE),
    TU_source = case_when(
      !is.na(TU_jaar_max) & !is.na(TU_prev_max) ~ "Both years",
      !is.na(TU_jaar_max) ~ "Current year",
      !is.na(TU_prev_max) ~ "Previous year"
    )
  )

# -------------------------------------------------------------------------
# STAP 4: EVALUATIE VAN HET SUCCES
# -------------------------------------------------------------------------

cat("📊 MATCHING RESULTATEN:\n")
cat("Totaal aantal Biologie (MI) observaties (jaren):", nrow(mi_jaarlijks), "\n")
cat("Aantal matches (Zelfde jaar):", nrow(data_matched_same_year), "\n")
cat("Aantal matches (Robust: Dit jaar OF vorig jaar):", nrow(data_matched_robust), "\n")
cat("Succespercentage:", round(nrow(data_matched_robust)/nrow(mi_jaarlijks)*100, 1), "%\n")

# Bekijk de eerste rijen van de succesvolle dataset
head(data_matched_robust %>% select(meetplaats, jaar, TU_final_max, TU_source, driver_van_het_jaar))

length(base::intersect(unique(mi_nat_sv$meetplaats), unique(tu_per_sample$meetplaats))) ##spatiaal match probleem???
