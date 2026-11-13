library(dplyr)
library(sf)

# 1. Isoleer de unieke meetplaatsen (één punt per meetplaats)
fybe_uniek <- fybe_data %>%
  distinct(meetplaats, .keep_all = TRUE) %>%
  select(meetplaats_fybe = meetplaats)

mafy_uniek <- mafy_data %>%
  distinct(meetplaats, .keep_all = TRUE) %>%
  select(meetplaats_mafy = meetplaats, vhas_mafy = vhas)

meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")
mi_uniek <- mi_data %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer")) %>%
  distinct(meetplaats, .keep_all = TRUE) %>%
  select(meetplaats_mi = meetplaats, vhas_mi = vhas)

# 2. Ruimtelijke join op exacte coördinaten
# join = st_equals zorgt ervoor dat de geometrieën 100% identiek moeten zijn
fybe_gekoppeld <- fybe_uniek %>%
  st_join(mafy_uniek, join = st_equals) %>%
  st_join(mi_uniek, join = st_equals)

# 3. Bekijk het resultaat: hoeveel fybe punten komen overeen?
# Dit geeft een tabel die toont of het punt uit mafy, mi, beide of geen van beide komt
tabel_overzicht <- table(
  In_MAFY = !is.na(fybe_gekoppeld$meetplaats_mafy),
  In_MI = !is.na(fybe_gekoppeld$meetplaats_mi)
)
print("Overzicht van de matches:")
print(tabel_overzicht)

# 4. Koppel de gevonden MAFY en MI namen terug aan je originele fybe dataset
fybe_data_volledig <- fybe_data %>%
  left_join(
    fybe_gekoppeld %>% st_drop_geometry(),
    by = c("meetplaats" = "meetplaats_fybe")
  ) %>%
  mutate(
    oorsprong = case_when(
      !is.na(meetplaats_mafy) & is.na(meetplaats_mi) ~ "Enkel MAFY",
      is.na(meetplaats_mafy) & !is.na(meetplaats_mi) ~ "Enkel MI",
      !is.na(meetplaats_mafy) & !is.na(meetplaats_mi) ~ "Beide (MAFY & MI)",
      TRUE ~ "Geen match"
    )
  )

# Bekijk de eerste rijen
head(fybe_data_volledig)
