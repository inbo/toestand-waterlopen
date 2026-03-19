


chemische_stoffen <- read.table(file = here("data", "ruw", "fys_chem", "classificatie_chemische_stoffen.txt"),
                                header = T, sep = ";", na.strings = "nvt")
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

overschrijdingen <- read_excel(here("data", "ruw", "fys_chem", "250326_Beoordeling GS per SP met norm_2007_2024.xlsx"), sheet = "BeoordelingParMPToetswijze") %>%
  janitor::clean_names() %>%
  rename(meetplaats = sample_point_naam) %>%
  select(-x, -y) %>%
  left_join(chemische_stoffen,
            by = c("parameter_symbool" = "stof_symbool")) %>%
  left_join(meetnetten,
            by = c("meetplaats" = "nummer")) %>%
  filter(type == "pesticide") %>%
  select(meetplaats, jaar, parameter_omschrijving, parameter_symbool, toetswijze_jaimfcp_code, owl_code, beoordeling)

head(overschrijdingen)


library(ggplot2)
library(tidyr)

# A. Check de ruimtelijke en temporele dekking
dekking_matrix <- overschrijdingen %>%
  group_by(owl_code, jaar) %>%
  summarise(aantal_parameters = n_distinct(parameter_symbool), .groups = "drop") %>%
  filter(!str_detect(owl_code, "^L2")) %>%
  right_join(data_sem_clean %>% select(owl.x) %>% unique(),
             by = c("owl_code" = "owl.x"))

# Visualiseer de gaten in de tijd/ruimte
ggplot(dekking_matrix, aes(x = jaar, y = owl_code, fill = aantal_parameters)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Meetfrequentie: Aantal unieke pesticiden per OWL per jaar",
       subtitle = "Witte vlakken duiden op ontbrekende jaren voor een specifiek waterlichaam",
       x = "Jaar", y = "OWL Code", fill = "Aantal stoffen")

# B. Check of bepaalde OWL codes structureel onderbemonsterd zijn
owl_samenvatting <- dekking_matrix %>%
  group_by(owl_code) %>%
  summarise(
    jaren_gemeten = n(),
    gemiddeld_aantal_stoffen = mean(aantal_parameters),
    min_jaar = min(jaar),
    max_jaar = max(jaar)
  ) %>%
  arrange(jaren_gemeten)

print(owl_samenvatting)
