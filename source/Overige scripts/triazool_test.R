alle_stoffen <- read.csv("alle_stoffen.csv")

alle_stoffen %>%
  filter(grepl("azol|azool", parameter_omschrijving, ignore.case = TRUE))

triazool_fungiciden <- c(
  "Tebuconazole",
  "Epoxiconazole",
  "Clotrimazol",
  "Fluconazol",
  "Ipconazool",
  "Metconazool",
  "Penconazole",
  "Tetraconazool"
)

load(file = here("data", "verwerkt", "fc_data.rdata"))

concentratie_triazolen <- fc_data %>%
  filter(parameter_omschrijving %in% triazool_fungiciden) %>%
  select(meetplaats, monsternamedatum, bekken, parameter_omschrijving, resultaat_detectielimiet, lambert_x, lambert_y) %>%
  st_as_sf(coords = c("lambert_x", "lambert_y"), crs = 31370)

%>%
  mutate(jaar = year(monsternamedatum)) %>%
  group_by(meetplaats) %>%
  filter(jaar == max(jaar)) %>%
  group_by(meetplaats, monsternamedatum, geometry) %>%
  summarise(som_triazolen = sum(resultaat_detectielimiet))



mapview(concentratie_triazolen, zcol = "resultaat_detectielimiet")


# 1. Bibliotheken laden
library(dplyr)
library(ggplot2)

# GA ERVAN UIT DAT UW DATAFRAME 'df' HEET
df <- concentratie_triazolen

# 2. Data aggregeren: Bereken de totale triazool-concentratie per sample
# Het resultaat wordt opgeslagen in een nieuwe kolom: 'Totale_Triazool_ng_L'
geaggregeerde_data <- df %>%
  # Groeperen op de unieke combinatie van meetplaats en datum
  group_by(meetplaats, monsternamedatum, bekken) %>%
  # De som van de concentraties berekenen voor elke groep
  summarise(
    Totale_Triazool_ng_L = sum(resultaat_detectielimiet, na.rm = TRUE),
    .groups = 'drop' # Haal de groepering weg na het samenvatten
  )

# 3. Visualisatie: Concentratie over tijd per meetplaats
# Een lijngrafiek is ideaal om de trend over tijd voor elke meetplaats te zien.
plot_totaal_triazool <- geaggregeerde_data %>%
  ggplot(aes(x = monsternamedatum, y = Totale_Triazool_ng_L, group = meetplaats, color = meetplaats)) +

  # Lijnen en punten toevoegen
  geom_line(linewidth = 1) +
  geom_point(size = 3) +

  # Titels en labels instellen
  labs(
    title = "Totale Triazool Concentratie per Meetplaats over Tijd",
    x = "Monsternamedatum",
    y = expression("Totale Concentratie ("*ng/L*")"), # LaTeX notatie voor ng/L
    color = "Meetplaats ID"
  ) +

  # Thema voor betere leesbaarheid
  theme_minimal(base_size = 14) +

  # Optioneel: Facetteren per meetplaats als er veel datapunten zijn
  facet_wrap(~ bekken, scales = "free_y") +

  # Optioneel: Een horizontale lijn voor een grenswaarde (bijv. 100 ng/L)
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  theme(legend.position = "none")

  # Toon de plot
  print(plot_totaal_triazool)


  # 1. Bibliotheken laden
  library(dplyr)
  library(ggplot2)
  library(mapview)
  library(sf) # Nodig om met de geometry kolom (sf object) te werken

  # GA ERVAN UIT DAT UW DATAFRAME 'df' HEET
  df <- concentratie_triazolen

  # 2. Data voor recente jaren aggregeren
  # We filteren eerst de data op 1 januari 2018 en later.
  datum_filter <- as.Date("2018-01-01")

  geaggregeerde_data_recent <- df %>%
    # Filteren op datum
    filter(monsternamedatum >= datum_filter) %>%

    # Groeperen op unieke sample (meetplaats, datum) en geometry
    # Het is cruciaal om de geometry te behouden als we willen plotten met mapview
    group_by(meetplaats, monsternamedatum, geometry) %>%

    # De som van de concentraties berekenen
    summarise(
      Totale_Triazool_ng_L = sum(resultaat_detectielimiet, na.rm = TRUE),
      .groups = 'drop'
    )

  # 3. Visualisatie van Recente Concentratie over Tijd
  # Dit is de gevraagde lijngrafiek, maar dan enkel vanaf 2018.
  plot_totaal_triazool_recent <- geaggregeerde_data_recent %>%
    ggplot(aes(x = monsternamedatum, y = Totale_Triazool_ng_L, group = meetplaats, color = meetplaats)) +

    geom_line(linewidth = 1) +
    geom_point(size = 3) +

    labs(
      title = paste("Totale Triazool Concentratie per Meetplaats (vanaf", format(datum_filter, "%Y"), ")"),
      x = "Monsternamedatum",
      y = expression("Totale Concentratie ("*ng/L*")"),
      color = "Meetplaats ID"
    ) +

    theme_minimal(base_size = 14) +

    # Facetteren om trends per locatie te zien
    facet_wrap(~ meetplaats, scales = "free_y", ncol = 3) +

    # Horizontale lijn voor grenswaarde
    geom_hline(yintercept = 100, linetype = "dashed", color = "red") +

    # Legenda behouden, want de facetten zijn nu de focus.
    theme(legend.position = "none")

  # Toon de plot
  print(plot_totaal_triazool_recent)

  # 4. Kaartvisualisatie met Mapview (Recente Data)

  # Om de kaart overzichtelijk te houden, selecteren we enkel de meest recente meting
  # per meetplaats in de gefilterde set.

  laatste_meting_per_locatie <- geaggregeerde_data_recent %>%
    # Sorteer op meetplaats en dan op datum (aflopend)
    arrange(meetplaats, desc(monsternamedatum)) %>%
    group_by(meetplaats) %>%
    # Hou enkel de eerste rij (de meest recente datum) per meetplaats
    slice(1)

  # Maak de mapview plot
  kaart_triazool_recent <- mapview(
    laatste_meting_per_locatie,
    # Kleur de punten op basis van de totale concentratie
    zcol = "Totale_Triazool_ng_L"
    # # Naam van de legenda
    # layer.name = "Conc. Triazolen (ng per L)",
    # # Gebruik een aangepaste kleurenschaal (bijv. van groen naar rood)
    # legend = TRUE,
    # # Optioneel: pas de grootte van de punten aan op basis van de concentratie
    # cex = "Totale_Triazool_ng_L",
    # burst = TRUE
  )

  # Toon de interactieve kaart
  kaart_triazool_recent


####  regressie ####
  load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata"))
  # load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer.Rdata"))
  load(file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))
  load(file = here("data", "verwerkt", "landgebruik", "landgebruik_oever_jaren.rdata"))

  landgebruik_data <- landgebruik_afstroomgebied_jaren %>%
    select(-oppervlakte, -landgebruiksjaar) %>%
    filter(meetplaats %in% afstroomgebieden_binnen_vlaanderen$meetplaats) %>%
    full_join(.,
              landgebruik_oever_jaren %>%
                select(-landgebruiksjaar),
              by = c("meetplaats", "monsternamedatum"), suffix = c("_afstr", "_oever"))

  landgebruik_afstroomgebied_jaren %>%
    filter(landgebruiksjaar == 2022)


triazool_landgebruik <- geaggregeerde_data %>%
  mutate(jaar = year(monsternamedatum)) %>%
  left_join(landgebruik_afstroomgebied_jaren %>%
              filter(landgebruiksjaar == 2022) %>%
                select(-monsternamedatum) %>%
                distinct(meetplaats, .keep_all = TRUE),
            by = c("meetplaats")) %>%
  drop_na()

mod_triazool <- glmmTMB(data = triazool_landgebruik,
        log(Totale_Triazool_ng_L) ~ scale(landbouw_intens) + bekken + (1 | jaar) + (1 | meetplaats))
diagnose(mod_triazool)
summary(mod_triazool)
plot_model(mod_triazool, "pred")
