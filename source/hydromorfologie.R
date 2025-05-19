source(here::here("source", "inladen_packages.R"))

mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten.gpkg"))
hydromorf <- st_read(here("data", "ruw", "hydromorfologie", "deelmaatlatten_wlniveau_OUD.shp"))
hydromorf_nieuw <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail_afgerond.shp"))
hydromorf3 <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail.shp"))
hydromorf4 <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_vmm_29jan2025_ruw.shp"))

hydromorf_velddata <- read_excel(here("data", "ruw", "hydromorfologie", "Opname Resultaten Hydormorfologie v2.0 T11_25_02.xlsx"), sheet = "Opnames Resultaten")
stroomsnelheid_breedte_diepte <- read_excel(here("data", "ruw", "hydromorfologie", "stroomsnelheid_traject.xlsx"))
sinuositeit <- read_excel(here("data", "ruw", "hydromorfologie", "Opname Resultaten Hydormorfologie v2.0 T11_25_02.xlsx"), sheet = "trajecten", skip = 1)

# waldo::compare(hydromorf2, hydromorf3)

tolerance <- 20

# Calculate distance to nearest river for each point
nearest_river_index <- st_nearest_feature(mi_meetpunten, hydromorf_nieuw)
distances <- st_distance(mi_meetpunten, hydromorf[nearest_river_index, ], by_element = TRUE)

# Assign NA for points further than tolerance
nearest_river_index[as.numeric(distances) > tolerance] <- NA
nearest_river_index %>%
  na.omit() %>%
  length

# Extract river attribute (e.g. 'river_value')
meetpunten$EKC_hydromorf <- as.numeric(hydromorf$EKC[nearest_river_index])
meetpunten$bedding <- as.numeric(hydromorf$B[nearest_river_index])
meetpunten$profiel <- as.numeric(hydromorf$P[nearest_river_index])
meetpunten$stroming <- as.numeric(hydromorf$S[nearest_river_index])
meetpunten$oever <- as.numeric(hydromorf$O[nearest_river_index])
meetpunten$latcon <- as.numeric(hydromorf$LaC[nearest_river_index])
meetpunten$loncon <- as.numeric(hydromorf$LoC[nearest_river_index])
meetpunten$alluproc <- as.numeric(hydromorf$AP[nearest_river_index])

load(here("data", "verwerkt", "mi_data.rdata"))
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  mutate(meetplaats = paste0("OW", meetplaats)) %>%
  left_join(., meetpunten %>% st_drop_geometry(), by = "meetplaats")


conflicted::conflicts_prefer(lmerTest::lmer)
model <- lmer(data = mi_data_analyse,
              mmif ~ EKC_hydromorf + bedding + stroming.y + oever + latcon + loncon + alluproc + jaar + groep + statuut + (1 | meetplaats))

pred <- ggpredict(model, terms = "alluproc")

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
fit <- sdmTMB(formula = mmif ~ s(EKC_hydromorf, k=3) + jaar + groep + statuut + (1 | meetplaats),
       data = data %>%
         mutate(meetplaats = as.factor(meetplaats)) %>%
         na.omit(),
       mesh = mesh,
       spatial = "off")
r.squaredGLMM(fit)
dharma_residuals(object = fit, simu)

# Hydromorfologie 2.0 ----
tolerance <- 20

# Calculate distance to nearest river for each point
nearest_river_index <- st_nearest_feature(meetpunten, hydromorf2)
distances <- st_distance(meetpunten, hydromorf2[nearest_river_index, ], by_element = TRUE)

# Assign NA for points further than tolerance
nearest_river_index[as.numeric(distances) > tolerance] <- NA
nearest_river_index %>%
  na.omit() %>%
  length

meetpunten$EKC_hydromorf <- as.numeric(hydromorf2$ekc_r[nearest_river_index])
hist(meetpunten$EKC_hydromorf)

model <- lmer(data = mi_data_analyse,
              sw_dw ~ EKC_hydromorf + jaar + groep + statuut + (1 | meetplaats))
summary(model)
pred <- ggpredict(model, terms = "EKC_hydromorf")
r.squaredGLMM(model)
# Plot
ggplot(pred, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Jaar", y = "MMIF", title = "Conditional Effect of Jaar")
