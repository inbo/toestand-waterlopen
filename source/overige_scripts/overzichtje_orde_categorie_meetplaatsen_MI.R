cat <- mi_data_analyse %>% filter(type != "RtNt") %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)
           ) %>% st_drop_geometry()
cat %>% group_by(orde, categorie) %>% summarise(n = n()) %>% View
cat2 <- mi_data_analyse %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)
  )%>% st_drop_geometry()
cat2 %>% group_by(orde, categorie, type) %>% summarise(n = n()) %>% filter(orde == "L2")

mi_data_analyse %>%
  filter(jaar > 2006) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)
  ) %>% st_drop_geometry() %>%
  filter(orde != "L2")

mi_data_analyse %>% # aantal unieke meetplaatsen van VL en L1 en NG
  filter(jaar > 2006) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)
  ) %>% st_drop_geometry() %>%
  filter(orde != "L2") %>%
  pull(meetplaats) %>%
  unique() %>%
  length

mi_full$jaar %>% summary

mi_full %>% filter(!statuut %in% c("Default")) %>% pull(meetplaats) %>% unique %>% length
mi_full$meetplaats %>% unique() %>% length()

mi_full %>% filter(statuut %in% c("Default")) %>%
  filter(!is.na(ekc2_traject)) %>%
  pull(meetplaats) %>%
  unique() %>%
  length

mi_full %>% filter(statuut %in% c("Default")) %>%
  pull(meetplaats) %>%
  unique() %>%
  length
# mafy

mafy_data %>%
  filter(!type %in% c("Awe", "RtNt")) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)) %>%
  filter(orde != "L2") %>%
  pull(meetplaats) %>%
  unique() %>%
  length()

mafy_data %>%
  filter(!type %in% c("Awe", "RtNt")) %>%
  mutate(orde = str_sub(owl, start = 1, end = 2)) %>%
  filter(orde != "L2") %>%
  st_drop_geometry() %>%
  group_by(jaar) %>%
  summarise(aantal_meetplaatsen = n()) %>%
  ggplot(aes(jaar, aantal_meetplaatsen)) +
  geom_col()

hydromorf_nieuw_traject %>%
  mutate(orde = str_sub(owl_code, start = 1, end = 2)) %>%
  pull(orde) %>%
  unique()
