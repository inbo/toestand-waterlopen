source(here::here("source", "inladen_packages.R"))


meetpunten <- st_read(here("data", "meetpunten", "mi_meetpunten.shp"))
hydromorf <- st_read(here("data", "hydromorfologie", "deelmaatlatten_wlniveau_OUD.shp"))

tolerance <- 20

# Calculate distance to nearest river for each point
nearest_river_index <- st_nearest_feature(meetpunten, hydromorf)
distances <- st_distance(meetpunten, hydromorf[nearest_river_index, ], by_element = TRUE)

# Assign NA for points further than tolerance
nearest_river_index[as.numeric(distances) > tolerance] <- NA
nearest_river_index %>%
  na.omit() %>%
  length

# Extract river attribute (e.g. 'river_value')
meetpunten$EKC_hydromorf <- as.numeric(hydromorf$EKC[nearest_river_index])
meetpunten$bedding <- as.numeric(hydromorf$B[nearest_river_index])

load(here("source", "mi_data.rdata"))
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  mutate(meetplaats = paste0("OW", meetplaats)) %>%
  left_join(., meetpunten %>% st_drop_geometry(), by = "meetplaats")


conflicted::conflicts_prefer(lmerTest::lmer)
model <- lmer(data = mi_data_analyse,
              mmif ~ bedding + jaar + groep + statuut + (1 | meetplaats))

pred <- ggpredict(model, terms = "bedding")

# Plot
ggplot(pred, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Jaar", y = "MMIF", title = "Conditional Effect of Jaar")

# Testen sdmTMB----

data <- mi_data_analyse %>%
  mutate(
    X = st_coordinates(.)[,1]/1000,
    Y = st_coordinates(.)[,2]/1000
  ) %>%
  st_drop_geometry()

inv_dist <- as.matrix(dist(data[, c("X", "Y")]))
diag(inv_dist) <- 0
Moran.I(data$mmif, inv_dist, na.rm = T)

mesh <- make_mesh(data, xy_cols = c("X", "Y"), n_knots = 100)
plot(mesh)
fit <- sdmTMB(formula = mmif ~ bedding + jaar + groep + statuut + (1 | meetplaats),
       data = data %>%
         mutate(meetplaats = as.factor(meetplaats)),
       mesh = mesh)
dharma_residuals(object = fit, simu)
