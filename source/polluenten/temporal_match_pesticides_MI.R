library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(tidyr)
library(data.table)
load(file = here("data", "verwerkt", "polluenten", "tu_master_macroinvertebrates.rdata"))
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")
tu_owl <- tu_master_mi %>% left_join(meetnetten %>% select(owl_code, nummer), by = c("meetplaats" = "nummer"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# ==== 1. HOEVEEL PESTICIDEN GEDETECTEERD PER STAAL? ====

# Scatterplot met loess trend
tu_owl %>%
  ggplot(aes(x = monsternamedatum, y = aantal_detecties)) +
  geom_jitter(alpha = 0.3, width = 0, height = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Evolutie van Detecties per Staal",
    subtitle = "Hoeveel pesticiden werden er effectief gedetecteerd doorheen de tijd?",
    x = "Datum",
    y = "Aantal detecties per staal"
  )

# Boxplot verdeling per jaar
tu_owl %>%
  mutate(jaar = factor(year(monsternamedatum))) %>%
  ggplot(aes(x = jaar, y = aantal_detecties)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Verdeling aantal detecties per jaar",
    x = "Jaar",
    y = "Aantal stoffen (detecties)"
  )

# Relatie met TU_sum
tu_owl %>%
  filter(TU_sum < 10) %>% # Uitschieters filteren
  ggplot(aes(x = aantal_detecties, y = TU_sum)) +
  geom_point(aes(color = monsternamedatum), alpha = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_viridis_c(trans = "date", name = "Datum") +
  theme_minimal() +
  labs(
    title = "Correlatie: Aantal Detecties vs. Gevonden Toxiciteit",
    subtitle = "Leidt een breder detectieprofiel tot een hogere berekende TU?",
    x = "Aantal detecties",
    y = "Toxic Units (TU Som)"
  )


# ==== 2. BIAS CHECK & DRIVER ANALYSE ====

# Bias check: TU_sum vs TU_core_sum
# Omdat tu_owl deze al bevat, kunnen we direct overgaan naar de plot
tu_long <- tu_owl %>%
  select(meetplaats, monsternamedatum, TU_sum, TU_core_sum) %>%
  pivot_longer(cols = c(TU_sum, TU_core_sum), names_to = "Type_TU", values_to = "TU_waarde")

ggplot(tu_long, aes(x = monsternamedatum, y = TU_waarde, color = Type_TU)) +
  geom_smooth(method = "gam", se = TRUE) +
  coord_cartesian(ylim = c(0, 2)) +
  labs(
    title = "Monitoring Bias Check: Totale TU vs. Core TU",
    subtitle = "Verschil toont invloed van nieuwe/andere stoffen buiten de core set",
    x = "Tijd",
    y = "Gemiddelde Toxic Units",
    color = "TU Type"
  ) +
  theme_minimal()

# Evolutie van de top drivers
# We groeperen op het nieuwe veld 'driver_stof'
driver_plot_data <- tu_owl %>%
  mutate(jaar = year(monsternamedatum)) %>%
  filter(!is.na(driver_stof)) %>%
  # Bepaal de top 8 drivers over de hele dataset
  mutate(is_top_driver = driver_stof %in% (
    count(., driver_stof, sort = TRUE) %>% slice(1:8) %>% pull(driver_stof)
  )) %>%
  mutate(plot_label = ifelse(is_top_driver, driver_stof, "Andere (Rest)")) %>%
  group_by(jaar, plot_label) %>%
  summarise(
    avg_TU_contribution = mean(driver_TU, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(driver_plot_data, aes(x = jaar, y = avg_TU_contribution, fill = forcats::fct_reorder(plot_label, avg_TU_contribution, .desc = TRUE))) +
  geom_area(alpha = 0.8, size = 0.5, color = "white") +
  scale_x_continuous(breaks = 2010:2024) +
  scale_fill_viridis_d(option = "turbo", name = "Stofnaam") +
  labs(
    title = "Driver Analyse: Welke stoffen sturen de Toxiciteit (Max TU)?",
    subtitle = "Gestapelde bijdrage aan de gemiddelde Driver-TU per jaar",
    x = "Jaar",
    y = "Gemiddelde Toxic Units"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


# ==== 3. TEMPORELE EN RUIMTELIJKE MATCH (TU vs MI) ====

# 1. Zorg dat datums Date objecten zijn
tu_dates <- tu_owl %>%
  select(meetplaats, date_tu = monsternamedatum) %>%
  distinct()

mi_dates <- data5 %>%
  st_drop_geometry() %>% # Omdat data5 een 'geom' kolom heeft
  select(meetplaats, date_mi = monsternamedatum) %>%
  distinct()

# 2. De Join (Cartesiaans per meetplaats)
mismatch_analysis <- mi_dates %>%
  inner_join(tu_dates, by = "meetplaats") %>%
  mutate(diff_days = as.numeric(date_mi - date_tu)) %>%
  group_by(meetplaats, date_mi) %>%
  summarise(
    days_to_nearest_tu = diff_days[which.min(abs(diff_days))],
    days_to_nearest_prev_tu = ifelse(any(diff_days > 0), min(diff_days[diff_days > 0]), NA),
    .groups = "drop"
  )

# Histogram van het tijdsgat
ggplot(mismatch_analysis, aes(x = days_to_nearest_tu)) +
  geom_histogram(binwidth = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Tijdsgat tussen MI staalname en dichtstbijzijnde Pesticide meting",
    subtitle = "Negatief = TU ná biologie | Positief = TU vóór biologie",
    x = "Dagen verschil (MI datum - TU datum)",
    y = "Aantal MI stalen"
  ) +
  theme_minimal()

# Overlap in locaties bekijken
locs_mi <- unique(data5$meetplaats)
locs_tu <- unique(tu_owl$meetplaats)

venn_overlap <- list(
  Alleen_Bio = setdiff(locs_mi, locs_tu),
  Alleen_Chem = setdiff(locs_tu, locs_mi),
  Overlap = base::intersect(locs_mi, locs_tu)
)
cat("\nAantal unieke locaties in Bio, Chemie, en de overlap ertussen:\n")
print(lengths(venn_overlap))


# ==== 4. ROLLING JOIN (Data.table aanpak) ====

# 1. Bereid de tabellen voor
dt_mi <- as.data.table(
  data5 %>%
    st_drop_geometry() %>%
    mutate(ID_biologie = row_number()) %>%
    select(meetplaats, monsternamedatum, ID_biologie)
)

dt_tu <- as.data.table(tu_owl)

# Cruciale kopie van de TU datum
dt_tu[, datum_chemie := monsternamedatum]

# Zet sleutels
setkey(dt_mi, meetplaats, monsternamedatum)
setkey(dt_tu, meetplaats, monsternamedatum)

# 2. Rolling join (Zoek de laatste TU meting vóór de MI meting)
matches <- dt_tu[dt_mi, roll = Inf, on = .(meetplaats, monsternamedatum)]

# 3. Bereken het tijdsverschil
matches[, diff_days := as.numeric(monsternamedatum - datum_chemie)]

# 4. Filter op maximale ouderdom (bvb 1 jaar)
matches_clean <- matches[diff_days >= 0 & diff_days < 365]

cat("\nAantal MI stalen succesvol gekoppeld aan een chemische meting uit het afgelopen jaar:", nrow(matches_clean), "\n")

