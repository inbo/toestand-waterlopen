# ---
# ## Inlezen van de landgebruiksdata van de oevers

# Inlezen van de txt-bestanden (zoals door u aangeleverd)
landuse_oever_2013 <- read.table(here("data", "ruw", "landgebruik", "rebufferslandgebruik", "mi_meetpunten_lu_2013.txt"), sep = ";", header = T)
landuse_oever_2016 <- read.table(here("data", "ruw", "landgebruik", "rebufferslandgebruik", "mi_meetpunten_lu_2016.txt"), sep = ";", header = T)
landuse_oever_2019 <- read.table(here("data", "ruw", "landgebruik", "rebufferslandgebruik", "mi_meetpunten_lu_2019.txt"), sep = ";", header = T)
landuse_oever_2022 <- read.table(here("data", "ruw", "landgebruik", "rebufferslandgebruik", "mi_meetpunten_lu_2022.txt"), sep = ";", header = T)

# Maak een lijst van de ingelezen dataframes, met de jaren als namen
oever_landgebruik_lijst <- list(
  `2013` = landuse_oever_2013,
  `2016` = landuse_oever_2016,
  `2019` = landuse_oever_2019,
  `2022` = landuse_oever_2022
)

oever_landgebruik_lijst <- map(oever_landgebruik_lijst, .f = function(df){
  df %>%
    rename(meetplaats = MEETPLAATS)
})

kolommen_toevoegen <- c("VALUE_8", "VALUE_12", "VALUE_13")

oever_landgebruik_lijst_aangevuld <- map(oever_landgebruik_lijst, function(df) {
  for (kolom in kolommen_toevoegen) {
    if (!(kolom %in% names(df))) {
      df <- df %>%
        mutate(!!sym(kolom) := 0)
    }
  }

  return(df)
})


# Overschrijf de originele lijst met de aangevulde lijst
oever_landgebruik_lijst <- oever_landgebruik_lijst_aangevuld

# ---
# ## Verwerking van de oever-landgebruiksdata

# Functie om de oever-landgebruiksdata per jaar te verwerken
verwerk_oever_landgebruik <- function(data, jaar) {
  # # Zorg ervoor dat de kolomnamen overeenkomen met die in de functies:
  # # 'id' en 'meetplaats' zijn aanwezig, en de 'Value_X' kolommen.
  # # We selecteren enkel de relevante kolommen en hernoemen ze indien nodig
  # data <- data %>%
  #   select(meetplaats, starts_with("Value_")) %>%
  #   rename_with(~ str_replace(., "Value_", "VALUE_"), starts_with("Value_")) # Pas namen aan naar 'VALUE_X'

  # 1. Omzetten van pixelwaarden naar percentages
  oever_landuse_pct <- convert_pixels_to_percentages(data = data)

  # 2. Hergroeperen van de landgebruiksklassen
  reclassified_data <- landuse_reclass(oever_landuse_pct, suffix = as.character(jaar))

  return(reclassified_data)
}

# Voer de functie uit voor alle jaren
resultaten_oever_lijst <- map2(oever_landgebruik_lijst, names(oever_landgebruik_lijst), verwerk_oever_landgebruik)

# Voeg de resultaten samen in één dataframe
finale_oever_landgebruik <- resultaten_oever_lijst[[1]] %>%
  select(meetplaats) # Start met de meetplaatsen

# Voeg de resultaten per jaar samen
for (i in seq_along(resultaten_oever_lijst)) {
  # Gebruik full_join om ervoor te zorgen dat alle meetplaatsen behouden blijven
  # (hoewel inner_join ook zou kunnen als alle meetplaatsen in alle jaren voorkomen)
  finale_oever_landgebruik <- finale_oever_landgebruik %>%
    full_join(resultaten_oever_lijst[[i]], by = "meetplaats")
}

# Sla het uiteindelijke resultaat op
save(finale_oever_landgebruik, file = here("data", "verwerkt", "landgebruik", "landgebruik_oever_tijdreeks.Rdata"))

# ---
# ## Koppeling met mi_data op basis van monsternamejaar

# De koppeling is gelijkaardig aan die van de afstroomgebieden.

# 1. Gebruik de eerder bepaalde landgebruiksjaren uit 'landgebruik_koppeling0'
#    (die afkomstig is van mi_data)

# Als 'landgebruik_koppeling0' niet meer in de omgeving staat, voer deze stap opnieuw uit:
if (!exists("landgebruik_koppeling0")) {
  landgebruik_koppeling0 <- mi_data %>%
    select(meetplaats, monsternamedatum) %>%
    mutate(
      landgebruiksjaar = as.character(case_when(
        monsternamedatum < ymd("2014-07-01") ~ 2013,
        monsternamedatum < ymd("2017-07-01") ~ 2016,
        monsternamedatum < ymd("2020-07-01") ~ 2019,
        TRUE ~ 2022
      ))
    ) %>%
    st_drop_geometry()
}

# 2. Herschik de oever-landgebruiksdata van breed naar lang formaat
landgebruik_oever_koppeling1 <- finale_oever_landgebruik %>%
  pivot_longer(
    cols = -c(meetplaats), # Kies alle kolommen behalve deze
    names_to = c("landgebruik", "landgebruiksjaar"), # Maak twee tijdelijke kolommen
    names_pattern = "(.*)_([0-9]{4})$" # Splits op het laatste _ gevolgd door 4 cijfers
  ) %>%
  pivot_wider(
    names_from = landgebruik, # Maak nieuwe kolommen op basis van de 'landgebruik' waarden
    values_from = value # Vul de nieuwe kolommen met de waarden uit 'value'
  )

# 3. Voer de koppeling uit
landgebruik_oever_jaren <- landgebruik_koppeling0 %>% # bevat meetplaats, monsternamedatum, landgebruiksjaar
  left_join(landgebruik_oever_koppeling1, by = c("meetplaats", "landgebruiksjaar"))

# Sla het uiteindelijke resultaat op
save(landgebruik_oever_jaren, file = here("data", "verwerkt", "landgebruik", "landgebruik_oever_jaren.rdata"))
