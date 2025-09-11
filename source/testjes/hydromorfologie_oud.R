source(here::here("source", "inladen_packages.R"))

mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten.gpkg"))

hydromorf <- st_read(here("data", "ruw", "hydromorfologie", "deelmaatlatten_wlniveau_OUD.shp")) # de oude hydromorfologielaag met oude EKC en deelmaatlatten
hydromorf_nieuw <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail_afgerond.shp")) # de nieuwe hydromorfologielaag: de ze bevat zowel variabelen, deelmaatlatten en EKC2
hydromorf3 <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_detail.shp")) # geen idee wat het verschil is met vorige laag.
hydromorf4 <- st_read(here("data", "ruw", "hydromorfologie", "trajectenlaag_vmm_29jan2025_ruw.shp")) # ruwe data voor nieuwe hydmolaag

# hydromorf_velddata <- read_excel(here("data", "ruw", "hydromorfologie", "Opname Resultaten Hydormorfologie v2.0 T11_25_02.xlsx"), sheet = "Opnames Resultaten") # veldmetingen -> hier kan info over bvb oevers uitgehaald worden -> moeilijke structuur (zeer long)
stroomsnelheid_breedte_diepte <- read_excel(here("data", "ruw",
                                                 "hydromorfologie", "stroomsnelheid_traject.xlsx")) %>%
  select(traj_code, owl, avg_depth, width_used, stroomsnelheid_kmu) # data over stroomsnelheid en breede en diepte op basis van PEGASE model

hydromorf_oud_nieuw <- read_excel(here("data", "ruw", "hydromorfologie", "oude_nieuwe_trajecten_velddata.xlsx"),
                                  sheet = "Sheet1") %>%
  janitor::clean_names() #file met koppeling tussen nieuwe en oude trajecten?

wider <- hydromorf_velddata %>%
  janitor::clean_names() %>%
  pivot_wider(., names_from = profieltype_naam, values_from = `resultaatwaarde_naam`)

# oeververdediging <- hydromorf_velddata %>% filter(`Resultaatwaarde Groep Naam` == "Oeververdediging") # oeververdediging uit velddata halen -> met L en R oever


# linken meetpunten macroinvertebraten met trajecten hydromorfologie ----

tolerance <- 100 #varieert niet zoveel met afstand

# Calculate distance to nearest river for each point
nearest_river_index <- st_nearest_feature(mi_meetpunten, hydromorf_nieuw)
distances <- st_distance(mi_meetpunten, hydromorf[nearest_river_index, ], by_element = TRUE)

# Assign NA for points further than tolerance
nearest_river_index[as.numeric(distances) > tolerance] <- NA
nearest_river_index %>% #4095
  na.omit() %>%
  length

hydmo_variabelen <- hydromorf_nieuw %>%
  full_join(., stroomsnelheid_breedte_diepte, by = "traj_code") %>%
  full_join(., hydromorf4 %>%
              select(traj_code, rec_width, bd_depth) %>%
              st_drop_geometry())


test2 <- hydromorf_nieuw %>%
  inner_join(., hydromorf_oud_nieuw, by = c("traj_code" = "corr_traject")) # even geen idee meer wat dit bekijken, dit was om de koppeling van oude en nieuwe trajecten te checken?

hm_data <- mi_meetpunten

# Extract river attribute (e.g. 'river_value')
hm_data$EKC2 <- as.numeric(hydmo_variabelen$ekc_r[nearest_river_index]) # 3650 meetpunten
hm_data$sinuositeit <- as.numeric(hydmo_variabelen$sin_s3[nearest_river_index]) #4069
hm_data$verstuwing <- as.numeric(hydmo_variabelen$opst_sco_t[nearest_river_index]) # 2763 -> veel NA's!
hm_data$stroomsnelheid <- as.numeric(hydmo_variabelen$stroomsnelheid_kmu[nearest_river_index]) #4070
hm_data$diepte_peg <- as.numeric(hydmo_variabelen$avg_depth[nearest_river_index]) # 4074
hm_data$breedte_peg <- as.numeric(hydmo_variabelen$width_used[nearest_river_index])
hm_data$diepte_ruw <- as.numeric(hydmo_variabelen$bd_depth[nearest_river_index])
hm_data$breedte_ruw <- as.numeric(hydmo_variabelen$rec_width[nearest_river_index])

hm_data <- hm_data %>%
  st_drop_geometry()

save(hm_data, file = here("data", "verwerkt", "hm_data.rdata"))

# kijken welke NA waarden want neemt snel af met verstuwing!

#testje

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

