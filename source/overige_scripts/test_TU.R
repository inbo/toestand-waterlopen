load(here("data", "verwerkt", "mi_nat_sv.rdata"))
load(file = here("data", "verwerkt", "tu_resultaten.rdata"))

tu_data <- tu_specific_groups %>%
  select(meetplaats, monsternamedatum, TU_sum, TU_insecticide)
koppeling_mi_tu <- mi_nat_sv %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  select(meetplaats, monsternamedatum) %>%
  left_join(.,
            tu_data,
            by = c("meetplaats"), suffix = c("", "_tu")) %>%
  filter(

    {
      days_before <- as.numeric(difftime(monsternamedatum, monsternamedatum_tu, units = "days"))
      days_before < 365 &
        days_before > -14

    }
  ) %>%
  group_by(meetplaats, monsternamedatum) %>% #dubbele samples uitmiddelen
  summarise(
    across(
      where(is.numeric), \(x) max(x, na.rm = TRUE) # enkel numerische kolommen om de mean te pakken
    ), # voor niet numerische waarden gewoon de eerste string nemen om te behouden
    across(
      where(is.factor) | where(is.character),
      ~ first(.)
    ),
    .groups = "drop" # Drop the grouping at the end
  )

mi_data_met_tu <- mi_nat_sv %>%
  left_join(.,
            koppeling_mi_tu,
            by = c("meetplaats", "monsternamedatum")) %>%
  drop_na(TU_sum) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                owl = as.factor(owl.x),
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw)
mi_data_met_tu %>% filter(!is.na(TU_sum))


m <- glmmTMB(data = mi_data_met_tu %>% filter(TU_sum < 5),
             mmif ~ poly(TU_sum, 2) * scale(jaar) + (1 | meetplaats),
             # weights = ta_xw,
             family = gaussian)
summary(m)
plot_model(m, "pred", terms = c("TU_sum", "jaar"))
ggplot(mi_data_met_tu %>% filter(TU_insecticide < 5), aes(TU_insecticide, ep_tw)) + geom_point() + geom_smooth()
locations <- read_sf(here("data", "verwerkt", "hydrologisch",
                          "mi_meetpunten_snapped_to_streams.shp"))
spatial <-
mi_nat_sv %>%
  left_join(locations, by = "meetplaats")

spatial_plot <- spatial %>% select(meetplaats, geometry) %>%
  unique() %>%
  st_as_sf()
mapview(spatial_plot)

#####landuse


source(here::here("source", "inladen_packages.R"))
load(file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_tu.rdata"))

tu_lu <- tu_per_sample %>%
  left_join(intensiteit_landbouw_scores_tu,
            by = c("meetplaats", "monsternamedatum")) %>%
  filter(TU_sum < 10) %>%
  mutate(jaar = year(monsternamedatum)) %>%
  na.omit()

ggplot(data = tu_lu %>% filter(TU_sum < 10), aes(intensiteit_gewasbescherming, TU_sum)) +
  geom_point() +
  geom_smooth()

model <- glmmTMB(data = tu_lu %>% filter(TU_sum < 10),
                 TU_sum ~ poly(intensiteit_gewasbescherming, 2) * scale(jaar) + (1 | meetplaats))

summary(model)
plot_model(model, "pred", terms = c("intensiteit_gewasbescherming", "jaar"))
