library(tidyverse) # Loads both dplyr and ggplot2

mi_nat_sv %>%
  ggplot(aes(x = groep, y = monsternamedatum)) +
  geom_boxplot() + # Shows the median, quartiles, and outliers
  geom_jitter(width = 0.2, alpha = 0.5) + # Adds individual points to see the density
  facet_wrap(~factor(jaar)) +
  labs(
    title = "Spreiding van de Monstermamedatum (DATE) per Groep in 2021",
    x = "Groep",
    y = "Monsternamedatum"
  )

library(tidyverse) # Zorgt voor dplyr en ggplot2

mi_nat_sv %>%
  filter(!groep %in% c("overgangswater", "zeer_grote_rivier")) %>%
  # GEEN filter meer, zodat alle jaren beschikbaar zijn
  # filter(jaar %in% c(2017:2023)) # Optioneel: als je de jaren wilt beperken
  ggplot(aes(x = groep, y = monsternamedatum)) +
  geom_boxplot() + # Boxplot voor de spreiding
  geom_jitter(width = 0.2, alpha = 0.5) + # Individuele punten voor dichtheid

  # FACET WRAP: Verdeelt de plot per jaar (kolom 'jaar')
  facet_wrap(~ jaar, scales = "free_y") +

  labs(
    title = "Spreiding van de Monstermamedatum per Groep, uitgesplitst per Jaar",
    x = "Groep",
    y = "Monsternamedatum"
  )

library(tidyverse) # Zorgt voor dplyr en ggplot2

# 1. Bereken de aantallen (N) per groep en jaar
n_data <- mi_nat_sv %>%
  filter(!groep %in% c("overgangswater", "zeer_grote_rivier")) %>%
  # Gebruik mutate om de DATUM om te zetten naar een numerieke waarde (Julian Day)
  # Dit is nodig om een geschikte y-positie te bepalen voor het N-label
  mutate(datum_num = as.numeric(monsternamedatum)) %>%
  group_by(jaar, groep) %>%
  summarise(
    n_count = n(),
    # Bepaal de laagste (minimale) y-positie voor de tekst (het vroegste monster)
    # zodat het label onder de boxplot staat.
    label_y = min(monsternamedatum, na.rm = TRUE)
  ) %>%
  ungroup()

# 2. Plot de data en voeg de tekstlabels toe
mi_nat_sv %>%
  filter(!groep %in% c("overgangswater", "zeer_grote_rivier")) %>%
  ggplot(aes(x = groep, y = monsternamedatum)) +

  # Boxplot en Jitter
  geom_boxplot(outlier.shape = NA) + # outlier.shape = NA voorkomt dubbele outliers met jitter
  geom_jitter(width = 0.2, alpha = 0.5) +

  # Tekstlabel toevoegen met de berekende N
  # De data = n_data zorgt ervoor dat deze geom de geaggregeerde dataset gebruikt
  geom_text(
    data = n_data,
    aes(x = groep, y = label_y, label = paste0("N=", n_count)),
    vjust = 1.5, # Plaats het label iets onder de laagste datum
    size = 3.5,
    fontface = "bold"
  ) +

  # Facet Wrap
  facet_wrap(~ jaar, scales = "free_y") +

  labs(
    title = "Spreiding van de Monstermamedatum per Groep, uitgesplitst per Jaar",
    subtitle = "Aantal waarnemingen (N) weergegeven onder elke boxplot",
    x = "Groep",
    y = "Monsternamedatum"
  ) +
  # Verklein de marges op de datum-as om ruimte te maken voor de N-labels
  scale_y_date(expand = expansion(mult = c(0.1, 0.05)))

####gelijke opnameperiode over jaren.?
library(tidyverse) # Zorgt voor dplyr en ggplot2

mi_nat_sv %>%
  # 1. Filter de gewenste groep
  filter(groep == "kempen") %>%

  # 2. Bepaal de dag van het jaar (dag 1 t/m 366)
  mutate(
    dag_van_jaar = as.numeric(format(monsternamedatum, "%j")), # "%j" geeft de dag van het jaar (Julian Day)
    meetplaats_id = as.factor(meetplaats)
  ) %>%

  # 3. Plot de data: kleur wordt nu grijs (of een andere enkele kleur)
  ggplot(aes(x = jaar, y = dag_van_jaar, group = meetplaats_id)) + # 'group' blijft nodig voor de lijnen

  # Lijnen om de herhalingen per meetplaats te verbinden (allemaal grijs)
  geom_line(color = "gray60", alpha = 0.4) +

  # Punten om elke meting weer te geven (allemaal zwart)
  geom_point(color = "black", alpha = 0.7, size = 1.5) +

  # Labels en thema
  labs(
    title = "Monsternamedagen 'kempen': Consistentie over de Jaren heen",
    subtitle = "Elke lijn verbindt metingen van hetzelfde meetplaats (locatie).",
    x = "Jaar",
    y = "Dag van het Jaar (1 = 1 jan, 365/366 = 31 dec)"
  ) +

  # VERWIJDER DE LEGENDE VOOR KLEUR
  guides(color = "none") +

  # Verplaats de jaren naar de x-as als discrete waarden
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +

  # Handige labels voor de y-as (dag van het jaar)
  scale_y_continuous(
    breaks = c(1, 60, 121, 182, 244, 305, 365), # Ongeveer begin jan/mrt/mei/jul/sep/nov
    labels = c("Jan 1", "Mrt 1", "Mei 1", "Jul 1", "Sep 1", "Nov 1", "Dec 31")
  ) +
  theme_minimal() +
  # Verwijder de legende definitief uit het thema
  theme(legend.position = "none")


#######"""

# Installeer indien nodig: install.packages("ggridges")
library(ggridges)
library(tidyverse)

mi_nat_sv %>%
  filter(groep == "kempen") %>%
  mutate(
    dag_van_jaar = as.numeric(format(monsternamedatum, "%j")),
    # Zorg dat 'jaar' een factor is voor de y-as
    jaar_factor = as.factor(jaar)
  ) %>%

  ggplot(aes(x = dag_van_jaar, y = jaar_factor, fill = jaar_factor)) +
  # Gebruik geom_density_ridges om de verdeling weer te geven
  geom_density_ridges(
    alpha = 0.7,
    rel_min_height = 0.005, # Verberg kleine artefacten
    scale = 3 # Hoe ver de dichtheidscurves elkaar overlappen
  ) +

  # Labels en schalen
  labs(
    title = "Jaarlijkse Dichtheid van Monstermomenten in 'kempen'",
    subtitle = "De verdeling van de bemonsteringsdag over de jaren heen.",
    x = "Dag van het Jaar",
    y = "Jaar"
  ) +
  scale_x_continuous(
    breaks = c(1, 60, 121, 182, 244, 305, 365),
    labels = c("Jan 1", "Mrt 1", "Mei 1", "Jul 1", "Sep 1", "Nov 1", "Dec 31")
  ) +
  theme_ridges() + # Een netter thema voor dit soort plots
  theme(legend.position = "none")



######verschiuving

library(tidyverse)
library(broom) # Nodig voor tidy()

gemiddelde_verschuiving <- mi_nat_sv %>%
  # 1. Initiële filtering en conversie
  filter(groep == "beek") %>%
  mutate(
    # Zorg ervoor dat 'jaar' numeriek is, zodat regressie goed werkt.
    # We gebruiken hier 'as.character' als tussenstap om problemen met factoren te vermijden.
    jaar_num = as.numeric(as.character(jaar)),
    dag_van_jaar = as.numeric(format(monsternamedatum, "%j"))
  ) %>%

  # 2. Groepeer en filter op minstens 2 unieke jaren
  group_by(meetplaats) %>%
  filter(n_distinct(jaar_num) >= 2) %>%
  ungroup() %>% # Ontgroepeer voordat je gaat nesten

  # 3. Nesten van de data (moderne vervanging voor 'do()')
  nest(data = -meetplaats) %>%

  # 4. Fit de lineaire regressie in elke geneste dataframe
  mutate(
    model = map(data, ~ lm(dag_van_jaar ~ jaar_num, data = .)),
    # Haal de coëfficiënten uit elk model met tidy()
    tidy = map(model, tidy)
  ) %>%

  # 5. Haal de coëfficiënten terug uit de lijst
  unnest(tidy) %>%

  # 6. Filter en hernoem de resultaten
  filter(term == "jaar_num") %>%
  rename(jaarlijkse_verschuiving_dagen = estimate) %>%

  # Selecteer alleen de relevante kolommen
  select(meetplaats, jaarlijkse_verschuiving_dagen, statistic, p.value)

# Bekijk de resultaten
head(gemiddelde_verschuiving)


gemiddelde_verschuiving %>%
  ggplot(aes(x = jaarlijkse_verschuiving_dagen)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue4", color = "white") +

  # Voeg een verticale lijn toe op 0 (geen verschuiving)
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +

  labs(
    title = "Distributie van de Gemiddelde Jaarlijkse Verschuiving in Bemonsteringsdatum ('kempen')",
    subtitle = "De helling van de lineaire trend (dagen/jaar) voor elk meetpunt.",
    x = "Gemiddelde Jaarlijkse Verschuiving (Dagen)",
    y = "Aantal Meetpunten"
  ) +
  scale_x_continuous(breaks = seq(floor(min(gemiddelde_verschuiving$jaarlijkse_verschuiving_dagen)),
                                  ceiling(max(gemiddelde_verschuiving$jaarlijkse_verschuiving_dagen)),
                                  by = 2)) +
  theme_minimal()
