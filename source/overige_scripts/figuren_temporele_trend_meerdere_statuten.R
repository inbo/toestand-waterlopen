plot_waterlopen_statuut2 <- function(input_data, titel_input, aantal_knots) {

  # Voorbereiden van de data
  data_filtered <- input_data %>%
    st_drop_geometry() %>%
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

  # Aantal meetplaatsen en observaties
  counts_meetplaatsen <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_meetplaatsen = n_distinct(meetplaats), .groups = "drop")

  counts_observaties <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_observaties = n(), .groups = "drop")

  counts_df <- counts_meetplaatsen %>%
    left_join(counts_observaties, by = c("deelmaatlatten", "groep", "statuut")) %>%
    mutate(
      hoek_x = ifelse(as.numeric(as.factor(statuut)) == 1, -Inf, Inf),   # links of rechts
      hoek_y = ifelse(as.numeric(as.factor(statuut)) == 1, Inf, -Inf),  # boven of onder
      hjust_pos = ifelse(as.numeric(as.factor(statuut)) == 1, -0.1, 1.1),
      vjust_mtpl = ifelse(as.numeric(as.factor(statuut)) == 1, 1.1, -0.5),
      vjust_obs  = ifelse(as.numeric(as.factor(statuut)) == 1, 2.5, -1.9)
    )

  # Plot
  p <- ggplot(data_filtered) +
    geom_smooth(
      aes(x = jaar, y = deelmaatlatten_score, color = statuut),
      method = "gam",
      formula = y ~ s(x, k = aantal_knots),
      se = FALSE
    ) +
    facet_grid(deelmaatlatten ~ groep) +
    ggtitle(titel_input) +

    # Tekst meetplaatsen
    geom_text(
      data = counts_df,
      aes(
        x = hoek_x, y = hoek_y,
        label = paste("#mtpl =", n_meetplaatsen),
        color = statuut,
        hjust = hjust_pos,
        vjust = vjust_mtpl
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +

    # Tekst observaties
    geom_text(
      data = counts_df,
      aes(
        x = hoek_x, y = hoek_y,
        label = paste("#obs =", n_observaties),
        color = statuut,
        hjust = hjust_pos,
        vjust = vjust_obs
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +

    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8)
    )

  return(p)
}


mi_data_nat_sv <- mi_data_analyse %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep != "overgangswater") %>%
  filter(jaar > 2006)
plot_waterlopen_statuut2(mi_data_nat_sv, "natuurlijk+sterkveranderd", 3)

plot_waterlopen_statuut3 <- function(input_data, titel_input, aantal_knots) {

  # Voorbereiden van de data
  data_filtered <- input_data %>%
    st_drop_geometry() %>%
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

  # Aantal meetplaatsen en observaties
  counts_meetplaatsen <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_meetplaatsen = n_distinct(meetplaats), .groups = "drop")

  counts_observaties <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_observaties = n(), .groups = "drop")

  counts_df <- counts_meetplaatsen %>%
    left_join(counts_observaties, by = c("deelmaatlatten", "groep", "statuut")) %>%
    mutate(
      stat_nr = as.numeric(as.factor(statuut)),
      hoek_x = case_when(
        stat_nr == 1 ~ -Inf,   # links
        stat_nr == 2 ~ Inf,    # rechts
        stat_nr == 3 ~ Inf     # rechts
      ),
      hoek_y = case_when(
        stat_nr == 1 ~ Inf,    # boven
        stat_nr == 2 ~ -Inf,   # onder
        stat_nr == 3 ~ Inf     # boven
      ),
      hjust_pos = case_when(
        stat_nr == 1 ~ -0.1,   # links
        stat_nr == 2 ~ 1.1,    # rechts
        stat_nr == 3 ~ 1.1     # rechts
      ),
      vjust_mtpl = case_when(
        stat_nr == 1 ~ 1.1,    # iets onder bovenkant
        stat_nr == 2 ~ -0.5,   # iets boven onderkant
        stat_nr == 3 ~ 1.1     # iets onder bovenkant
      ),
      vjust_obs = case_when(
        stat_nr == 1 ~ 2.5,    # nog iets lager
        stat_nr == 2 ~ -1.9,   # nog iets hoger
        stat_nr == 3 ~ 2.5     # nog iets lager
      )
    )

  # Plot
  p <- ggplot(data_filtered) +
    geom_smooth(
      aes(x = jaar, y = deelmaatlatten_score, color = statuut),
      method = "gam",
      formula = y ~ s(x, k = aantal_knots),
      se = FALSE
    ) +
    facet_grid(deelmaatlatten ~ groep) +
    ggtitle(titel_input) +

    # Tekst meetplaatsen
    geom_text(
      data = counts_df,
      aes(
        x = hoek_x, y = hoek_y,
        label = paste("#mtpl =", n_meetplaatsen),
        color = statuut,
        hjust = hjust_pos,
        vjust = vjust_mtpl
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +

    # Tekst observaties
    geom_text(
      data = counts_df,
      aes(
        x = hoek_x, y = hoek_y,
        label = paste("#obs =", n_observaties),
        color = statuut,
        hjust = hjust_pos,
        vjust = vjust_obs
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +

    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8)
    )

  return(p)
}



mi_data_nat_sv_km <- mi_data_analyse_rtnt_update %>% filter(statuut %in% c("Natuurlijk", "Sterk Veranderd", "Kunstmatig"))

plot_waterlopen_statuut3(mi_data_nat_sv_km, "natuurlijk+sterkveranderd+kunstmatig", 3)

plot_waterlopen_statuut4 <- function(input_data, titel_input, aantal_knots) {

  # Voorbereiden van de data
  data_filtered <- input_data %>%
    st_drop_geometry() %>%
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

  # Aantal meetplaatsen en observaties
  counts_meetplaatsen <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_meetplaatsen = n_distinct(meetplaats), .groups = "drop")

  counts_observaties <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_observaties = n(), .groups = "drop")

  counts_df <- counts_meetplaatsen %>%
    left_join(counts_observaties, by = c("deelmaatlatten", "groep", "statuut")) %>%
    mutate(
      stat_nr = as.numeric(as.factor(statuut)),
      hoek_x = case_when(
        stat_nr == 1 ~ -Inf,   # linksboven
        stat_nr == 2 ~ Inf,    # rechtsonder
        stat_nr == 3 ~ Inf,    # rechtsboven
        stat_nr == 4 ~ -Inf    # linksonder
      ),
      hoek_y = case_when(
        stat_nr == 1 ~ Inf,    # linksboven
        stat_nr == 2 ~ -Inf,   # rechtsonder
        stat_nr == 3 ~ Inf,    # rechtsboven
        stat_nr == 4 ~ -Inf    # linksonder
      ),
      hjust_pos = case_when(
        stat_nr == 1 ~ -0.1,   # links
        stat_nr == 2 ~ 1.1,    # rechts
        stat_nr == 3 ~ 1.1,    # rechts
        stat_nr == 4 ~ -0.1    # links
      ),
      vjust_mtpl = case_when(
        stat_nr == 1 ~ 1.1,    # iets onder bovenkant
        stat_nr == 2 ~ -0.5,   # iets boven onderkant
        stat_nr == 3 ~ 1.1,    # iets onder bovenkant
        stat_nr == 4 ~ -0.5    # iets boven onderkant
      ),
      vjust_obs = case_when(
        stat_nr == 1 ~ 2.5,    # nog iets lager
        stat_nr == 2 ~ -1.9,   # nog iets hoger
        stat_nr == 3 ~ 2.5,    # nog iets lager
        stat_nr == 4 ~ -1.9    # nog iets hoger
      )
    )

  # Plot
  p <- ggplot(data_filtered) +
    geom_smooth(
      aes(x = jaar, y = deelmaatlatten_score, color = statuut),
      method = "gam",
      formula = y ~ s(x, k = aantal_knots),
      se = FALSE
    ) +
    facet_grid(deelmaatlatten ~ groep) +
    ggtitle(titel_input) +

    # Tekst meetplaatsen
    geom_text(
      data = counts_df,
      aes(
        x = hoek_x, y = hoek_y,
        label = paste("#mtpl =", n_meetplaatsen),
        color = statuut,
        hjust = hjust_pos,
        vjust = vjust_mtpl
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +

    # Tekst observaties
    geom_text(
      data = counts_df,
      aes(
        x = hoek_x, y = hoek_y,
        label = paste("#obs =", n_observaties),
        color = statuut,
        hjust = hjust_pos,
        vjust = vjust_obs
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +

    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8)
    )

  return(p)
}
mi_data_nat_sv_km_df <- mi_data_analyse %>% filter(statuut %in% c("Natuurlijk", "Sterk Veranderd", "Kunstmatig", "Default"))

plot_waterlopen_statuut4(mi_data_nat_sv_km_df, "natuurlijk+sterkveranderd+kunstmatig+default", 3)


#### aangepaste functie voor stuurgroep ####

plot_waterlopen_statuut_sg <- function(input_data, titel_input, aantal_knots) {

  # Voorbereiden van de data
  data_filtered <- input_data %>%
    st_drop_geometry() %>%
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

  # Aantal meetplaatsen en observaties
  counts_meetplaatsen <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_meetplaatsen = n_distinct(meetplaats), .groups = "drop")

  counts_observaties <- data_filtered %>%
    group_by(deelmaatlatten, groep, statuut) %>%
    summarise(n_observaties = n(), .groups = "drop")

  counts_df <- counts_meetplaatsen %>%
    left_join(counts_observaties, by = c("deelmaatlatten", "groep", "statuut")) %>%
    mutate(
      hoek_x = ifelse(as.numeric(as.factor(statuut)) == 1, -Inf, Inf),   # links of rechts
      hoek_y = ifelse(as.numeric(as.factor(statuut)) == 1, Inf, -Inf),  # boven of onder
      hjust_pos = ifelse(as.numeric(as.factor(statuut)) == 1, -0.1, 1.1),
      vjust_mtpl = ifelse(as.numeric(as.factor(statuut)) == 1, 1.1, -0.5),
      vjust_obs  = ifelse(as.numeric(as.factor(statuut)) == 1, 2.5, -1.9)
    )

  # Plot
  p <- ggplot(data_filtered) +
    geom_smooth(
      aes(x = jaar, y = deelmaatlatten_score, color = statuut),
      method = "gam",
      formula = y ~ s(x, k = aantal_knots),
      se = FALSE
    ) +
    facet_grid(deelmaatlatten ~ groep) +
    ggtitle(titel_input) +

# Tekst meetplaatsen
geom_text(
  data = counts_df,
  aes(
    x = hoek_x, y = hoek_y,
    label = paste("#mtpl =", n_meetplaatsen),
    color = statuut,
    hjust = hjust_pos,
    vjust = vjust_mtpl
  ),
  size = 3,
  inherit.aes = FALSE,
  show.legend = FALSE
) +

# Tekst observaties
geom_text(
  data = counts_df,
  aes(
    x = hoek_x, y = hoek_y,
    label = paste("#obs =", n_observaties),
    color = statuut,
    hjust = hjust_pos,
    vjust = vjust_obs
  ),
  size = 3,
  inherit.aes = FALSE,
  show.legend = FALSE
) +

    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8)
    )

  return(p)
}

mi_data_nat_sv <- mi_data_analyse %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(!groep %in% c( "overgangswater", "zeer_grote_rivier")) %>%
  filter(jaar > 2006)
plot_waterlopen_statuut_sg(mi_data_nat_sv, "Natuurlijk en sterk veranderde waterlopen", 3)
