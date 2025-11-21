library(tidyverse)
library(here)

# tabel voor input in tool klaarmaken (https://www.systemecology.de/indicate/)
comm_data_raw <- mi_soorten %>%
  select(meetplaats, monsternamedatum, deelmonster_id, macroinvertebraat, aantal) %>%
  filter(monsternamedatum > '2006-12-31') %>%
  mutate(id = paste0(meetplaats, "_", monsternamedatum)) %>%
  select(id, macroinvertebraat, aantal)
openxlsx::write.xlsx(comm_data_raw, file = "data/temp/comm_data.xlsx")

# outputtabel van de tool inlezen

spear_data <- read.csv("data/ruw/macroinvertebraten/traits/spear_calculation.csv") %>%
  select(-Name_1, -Name_2) %>%
  separate(col = Name_3,
           into = c("meetplaats", "monsternamedatum"),
           sep = "_",
           remove = T) %>%
  janitor::clean_names() %>%
  mutate(monsternamedatum = as.Date(monsternamedatum))

# opslaan
save(spear_data, file = here("data", "verwerkt", "spear_data.rdata"))
