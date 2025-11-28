tu_data <- tu_per_sample %>%
  select(meetplaats, monsternamedatum, TU_sum)
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


m <- glmmTMB(data = mi_data_met_tu,
             nst_prop ~ log(TU_sum) + scale(jaar) + (1 | meetplaats),
             weights = ta_xw,
             family = binomial)
summary(m)
plot_model(m, "pred")
ggplot(mi_data_met_tu, aes(TU_sum, ep_tw)) + geom_point() + geom_smooth()
locations <- read_sf(here("data", "verwerkt", "hydrologisch",
                          "mi_meetpunten_snapped_to_streams.shp"))
spatial <-
mi_nat_sv %>%
  left_join(locations, by = "meetplaats")

spatial_plot <- spatial %>% select(meetplaats, geometry) %>%
  unique() %>%
  st_as_sf()
mapview(spatial_plot)


