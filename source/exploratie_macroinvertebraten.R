# inladen packages en data ----
source(here::here("source", "inladen_packages.R"))
load(here("source", "mi_data.rdata"))

# aantal uniek meetplaatsen per statuut (onafh van jaar)
mi_data %>%
  distinct(statuut, meetplaats) %>% # Rem. duplicate meetplaats within statuut
  group_by(statuut) %>%
  summarise(unique_meetplaats_count = n())

# recentste jaar telkens per meetplaats

mi_data %>%
  group_by(meetplaats) %>%
  filter(jaar == max(jaar))

# vroegste jaar telkens per meetplaats
mi_data %>%
  group_by(meetplaats) %>%
  filter(jaar == min(jaar))

# meetplaatsen na 2019
mi_data %>%
  filter(jaar >= 2019) %>%
  select(meetplaats) %>%
  unique() %>%
  plot()

# plot trend mmif per statuut

mi_data %>%
  group_by(meetplaats) %>%
  ggplot(aes(jaar, mmif)) +
  geom_smooth(method = "gam") +
  facet_wrap(~statuut)

mi_data %>%
  ggplot() +
  geom_line(aes(jaar, mmif, group = meetplaats), alpha = 0.25) +
  geom_smooth(aes(jaar, mmif), method = "gam") +
  facet_grid(statuut~groep)

mi_data %>%
  filter(statuut == "Natuurlijk") %>%
  pivot_longer(cols = c("mmif", "ept", "swd", "nst", "tax", "mts"), names_to = "deelmaatlatten", values_to = "deelmaatlatten_score") %>%
  ggplot() +
  geom_line(aes(jaar, deelmaatlatten_score, group = interaction(meetplaats, deelmaatlatten)), alpha = 0.25) +
  geom_smooth(aes(jaar, deelmaatlatten_score), method = "gam") +
  facet_grid(deelmaatlatten~groep)

lmer(data = mi_data, mmif ~ o2 + jaar + groep + (1 | meetplaats))
