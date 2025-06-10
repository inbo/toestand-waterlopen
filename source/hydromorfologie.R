source(here::here("source", "inladen_packages.R"))

hm_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten.gpkg")) %>%
  select(-monsternamedatum) %>%
  unique()
hydromorf <- st_read(here("data", "ruw", "hydromorfologie", "deelmaatlatten_wlniveau_OUD.shp"))
hydromorf_nieuw <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail_afgerond.shp"))
hydromorf3 <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail.shp"))
hydromorf4 <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_vmm_29jan2025_ruw.shp"))

hydromorf_velddata <- read_excel(here("data", "ruw", "hydromorfologie", "Opname Resultaten Hydormorfologie v2.0 T11_25_02.xlsx"), sheet = "Opnames Resultaten")
stroomsnelheid_breedte_diepte <- read_excel(here("data", "ruw",
                                                 "hydromorfologie", "stroomsnelheid_traject.xlsx")) %>%
  select(traj_code, owl, avg_depth, width_used, stroomsnelheid_kmu)

hydromorf_oud_nieuw <- read_excel(here("data", "ruw", "hydromorfologie", "oude_nieuwe_trajecten_velddata.xlsx"),
                                  sheet = "Sheet1") %>%
  janitor::clean_names()

wider <- hydromorf_velddata %>%
  janitor::clean_names() %>%
  pivot_wider(., names_from = profieltype_naam, values_from = `resultaatwaarde_naam`)

oeververdediging <- hydromorf_velddata %>% filter(`Resultaatwaarde Groep Naam` == "Oeververdediging")


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

hydmo_variabelen <- hydromorf_nieuw %>%
  inner_join(., stroomsnelheid_breedte_diepte, by = "traj_code")

test2 <- hydromorf_nieuw %>%
  inner_join(., hydromorf_oud_nieuw, by = c("traj_code" = "corr_traject"))

# Extract river attribute (e.g. 'river_value')
hm_data$EKC2 <- as.numeric(hydmo_variabelen$ekc_r[nearest_river_index])
hm_data$sinuositeit <- as.numeric(hydmo_variabelen$sin_s3[nearest_river_index])
hm_data$verstuwing <- as.numeric(hydmo_variabelen$opst_sco_t[nearest_river_index])
hm_data$stroomsnelheid <- as.numeric(hydmo_variabelen$stroomsnelheid_kmu[nearest_river_index])
hm_data$diepte <- as.numeric(hydmo_variabelen$avg_depth[nearest_river_index])
hm_data$breedte <- as.numeric(hydmo_variabelen$width_used[nearest_river_index])

hm_data <- hm_data %>%
  st_drop_geometry()

save(hm_data, file = here("data", "verwerkt", "hm_data.rdata"))

load(here("data", "verwerkt", "mi_data.rdata"))
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  left_join(., hm_data %>% st_drop_geometry(), by = "meetplaats")


conflicted::conflicts_prefer(lmerTest::lmer)
model <- lmer(data = mi_data_analyse,
              mmif ~ stroomsnelheid + jaar + groep + statuut + (1 | meetplaats))
summary(model)
pred <- ggpredict(model, terms = "stroomsnelheid")

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
nearest_river_index <- st_nearest_feature(hm_data, hydromorf2)
distances <- st_distance(hm_data, hydromorf2[nearest_river_index, ], by_element = TRUE)

# Assign NA for points further than tolerance
nearest_river_index[as.numeric(distances) > tolerance] <- NA
nearest_river_index %>%
  na.omit() %>%
  length

hm_data$EKC_hydromorf <- as.numeric(hydromorf2$ekc_r[nearest_river_index])
hist(hm_data$EKC_hydromorf)

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
