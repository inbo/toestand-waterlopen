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
  geom_smooth(aes(jaar, mmif), method = "gam",
              formula = y ~ s(x, k = 3)) +
  facet_grid(statuut ~ groep)

# welk type waterlopen zijn de defaults
mi_data %>%
  st_drop_geometry() %>%
  filter(statuut == "Default") %>%
  group_by(categorie, waterlooptype) %>%
  summarise(n())

# plots van de mmif en deelmaatlatten de verschillende types ----

plot_waterlopen_statuut <- function(statuut_input, titel_input) {

  data_filtered <- mi_data %>%
    filter(statuut == statuut_input) %>%
    mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
    pivot_longer(
      cols = c("mmif", "ept", "swd", "nst", "tax", "mts"),
      names_to = "deelmaatlatten",
      values_to = "deelmaatlatten_score"
    ) %>%
    mutate(deelmaatlatten = factor(
      deelmaatlatten,
      levels = c("mmif", "ept", "swd", "nst", "tax", "mts")
    ))

  # Compute unique counts of 'meetplaats' for each facet
  counts <- data_filtered %>%
    group_by(deelmaatlatten, groep) %>%
    summarise(n_obs = n_distinct(meetplaats), .groups = "drop")
  counts2 <- data_filtered %>%
    group_by(deelmaatlatten, groep) %>%
    summarise(n_obs = n(), .groups = "drop")

  # Create the plot with counts annotated in two corners
  p <- ggplot(data_filtered) +
    geom_smooth(
      aes(x = jaar, y = deelmaatlatten_score),
      method = "gam",
      formula = y ~ s(x, k = 3)
    ) +
    facet_grid(deelmaatlatten ~ groep) +
    ggtitle(titel_input) +
    # Annotation in the top-left corner of each panel
    geom_text(
      data = counts,
      aes(x = -Inf, y = Inf, label = paste("#mtpl =", n_obs)),
      hjust = -0.1, vjust = 1.1, size = 3,
      inherit.aes = FALSE
    ) +
    # Annotation in the bottom-right corner of each panel
    geom_text(
      data = counts2,
      aes(x = Inf, y = -Inf, label = paste("#obs =", n_obs)),
      hjust = 1.1, vjust = -0.1, size = 3,
      inherit.aes = FALSE
    )

  return(p)
}

plot_waterlopen_statuut("Natuurlijk", "Natuurlijke waterlopen")

plot_waterlopen_statuut("Sterk Veranderd", "Sterk veranderde waterlopen")

plot_waterlopen_statuut("Kunstmatig", "Kunstmatige waterlopen")

plot_waterlopen_statuut("Default", "Niet toegewezen")

lmer(data = mi_data, mmif ~ o2 + jaar + groep + (1 | meetplaats))

# Overzichtkaartjes ----

mapviewdata <- mi_data %>%
  filter(statuut == "Natuurlijk" | statuut == "Sterk Veranderd") %>%
  group_by(bekken, meetplaats) %>%
  summarize(jaren = paste(jaar, collapse = ", "),
            lengte_reeks = max(jaar) - min(jaar) + 1,
            aantal_bemonsteringen = n(),
            recentste_jaar = max(jaar),
            .groups = "drop")

bekkens_sf <- read_sf(
  here("data", "bekkens", "Wsbekken.shp")
)

vha_bekkens <- bekkens_sf %>%
  st_cast("GEOMETRYCOLLECTION") %>%
  st_collection_extract()

mapviewdata %>%
  mapview(zcol = "lengte_reeks") +
  mapview(vha_bekkens, alpha.regions = 0)

mapviewdata %>%
  mapview(zcol = "aantal_bemonsteringen") +
  mapview(vha_bekkens, alpha.regions = 0)

mapviewdata %>%
  mapview(zcol = "recentste_jaar")  +
  mapview(vha_bekkens, alpha.regions = 0)
