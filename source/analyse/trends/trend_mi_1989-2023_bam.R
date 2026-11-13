# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

load(here("data", "verwerkt", "mi_data.rdata")) # macroinvertebraten

time_mgcv <- gam(
  ept ~ 1 + s(jaar, k = 5) +                  # De vloeiende trendlijn
    s(meetplaats, bs = "re") +        # Random effect voor meetplaats in mgcv-stijl
    s(owl, bs = "re"),                # Random effect voor owl in mgcv-stijl

  data = mi_data %>%
    filter(statuut %in% c("Natuurlijk")) %>%
    filter(groep %in% c("kempen")) %>%
    # filter(jaar > 2007) %>%
    mutate(meetplaats = as.factor(meetplaats),
           owl = as.factor(owl)),

  method = "REML",
  family = gaussian           # Standaard Beta-verdeling in mgcv
)

summary(time_mgcv)

time_mgcv_o2 <- gam(
  mmif ~ 1 + s(jaar, k = 5) +                  # De vloeiende trendlijn
    s(meetplaats, bs = "re") +        # Random effect voor meetplaats in mgcv-stijl
    s(owl, bs = "re"),                # Random effect voor owl in mgcv-stijl

  data = mi_data %>%
    filter(statuut %in% c("Natuurlijk")) %>% # "Sterk Veranderd"
    filter(groep %in% c("beek")) %>%
    mutate(meetplaats = as.factor(meetplaats),
           owl = as.factor(owl)),

  method = "REML",
  family = gaussian           # Standaard Beta-verdeling in mgcv
)

# Visualisatie van de pure GAM-trend
plot(time_mgcv, select = 1, shade = TRUE, pages = 1)

pred <- as.data.frame(ggpredict(time_mgcv, terms = "jaar [all]"))
ggplot(pred, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(), size = 1) +
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  # Gebruik facet_grid(index ~ subset) zoals in je laatste voorbeeld
  # facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV GAM-trends per Subset en Index",
       subtitle = "Model: waarde ~ s(jaar, k=5) + s(meetplaats, bs='re') + s(bekken, bs='re')",
       x = "Jaar", y = "Voorspelde waarde",
       color = "Subset", fill = "Subset") +
  # scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

data_filter <- mi_data %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(groep %in% c("kempen", "rivier", "beek")) %>%
  mutate(meetplaats = as.factor(meetplaats),
         owl = as.factor(owl))
# %>%
#   drop_na(o2)


n_obs <- nrow(data_filter)
data_filter$mmif_krimp <- (data_filter$mmif * (n_obs - 1) + 0.5) / n_obs
aantal_cores <- parallel::detectCores() - 1
time_bam <- bam(
  formula = swd ~ 1 + s(jaar, k = 4) +
    # s(meetplaats, bs = "re") +
    s(owl, bs = "re"),
  data    = data_filter,
  family  = gaussian, # Bèta-verdeling voor de proportie

  # DE MAGIC SPEED-UP ARGUMENTEN:
  method   = "fREML",      # 'Fast REML', extreem veel sneller dan gewone REML
  discrete = TRUE,          # Dit is de gouden truc: het bundelt dubbele waarden in het geheugen!
  nthreads = aantal_cores,
  control = gam.control(trace = TRUE)
  )

# Bekijk de resultaten
summary(time_bam)

# Plot de vloeiende trendlijn

pred <- as.data.frame(ggpredict(time_bam, terms = "jaar [all]"))
ggplot(pred, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(), size = 1) +
  # Gebruik facet_grid(index ~ subset) zoals in je laatste voorbeeld
  # facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV GAM-trends per Subset en Index",
       subtitle = "Model: waarde ~ s(jaar, k=4) + s(meetplaats, bs='re') + s(owl, bs='re')",
       x = "Jaar", y = "Voorspelde waarde",
       color = "Subset", fill = "Subset") +
  # scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
