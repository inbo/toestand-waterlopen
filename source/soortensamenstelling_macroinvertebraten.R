# inladen packages en data ----
source(here::here("source", "inladen_packages.R"))
load(here("data", "verwerkt", "mi_data.rdata"))
load(here("data", "verwerkt", "mi_soorten.rdata"))
# soortendata NMDS ----

data_wide <- mi_soorten %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(groep != "overgangswater") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(statuut != "Default") %>% #Voorlopig RtNt enzo weglaten
  select(macroinvertebraat, ind, aantal, deelmonster_id, monsternamedatum, meetplaats, statuut, groep) %>%
  drop_na(ind) %>%
  filter(!macroinvertebraat %in% c("Vis", "Salamander", "Lege schelpen", "Lege kokers")) %>%
  group_by(meetplaats) %>%
  filter(monsternamedatum == max(monsternamedatum)) %>%
  filter(monsternamedatum > as.POSIXct("2006-12-31")) %>%
  ungroup() %>%
  select(-ind) %>%
  pivot_wider(names_from = macroinvertebraat, values_from = aantal, values_fill = 0) %>%
  na.omit()

species_matrix <- data_wide %>%
  select(-deelmonster_id, -monsternamedatum, -meetplaats, -statuut, -groep)

nmds0 <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 10)
plot(nmds0, type = "t")  # Basic NMDS plot

data_wide <- data_wide %>%
  filter(deelmonster_id != "22033877") # heel sterke outlier kunstmatige rivier uithalen voor visualisatie
species_matrix <- data_wide %>%
  select(-deelmonster_id, -monsternamedatum, -meetplaats, -statuut, -groep)
# %>%
#   select(-Heleobia, -Melitidae)

nmds <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 5)
plot(nmds, type = "t")  # Basic NMDS plot

# statuut
ordiplot(nmds, type = "n")
points(nmds, col = as.factor(data_wide$statuut), pch = 16)

adonis_result <- adonis2(species_matrix ~ statuut, data = data_wide, method = "bray")
print(adonis_result)

dispersion_test <- betadisper(vegdist(species_matrix, method = "bray"), data_wide$statuut)
anova(dispersion_test)  # p < 0.05 means dispersion varies between groups


# groep
ordiplot(nmds, type = "n")
points(nmds, col = as.factor(mi_soorten$groep), pch = 16)

adonis_result <- adonis2(species_matrix ~ groep, data = data_wide, method = "bray")
print(adonis_result)

dispersion_test <- betadisper(vegdist(species_matrix, method = "bray"), data_wide$groep)
anova(dispersion_test)  # p < 0.05 means dispersion varies between groups

# figuur met hulls

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))  # Extract NMDS coordinates
nmds_scores$statuut <- data_wide$statuut  # Add statuut column


# Compute convex hulls for each group
hull_data <- nmds_scores %>%
  group_by(statuut) %>%
  slice(chull(NMDS1, NMDS2))  # Get convex hull points

# Plot NMDS with convex hulls
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = statuut)) +
  geom_point(size = 3, alpha = 0.7) +  # NMDS points
  geom_polygon(data = hull_data, aes(fill = statuut, group = statuut), alpha = 0.3) +  # Convex hulls
  theme_minimal() +
  labs(title = "NMDS Ordination with Convex Hulls",
       x = "NMDS1", y = "NMDS2") +
  theme(legend.position = "right")

#figuur met hulls voor groep

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))  # Extract NMDS coordinates
nmds_scores$groep <- data_wide$groep  # Add groep column

# Compute convex hulls for each group
hull_data <- nmds_scores %>%
  group_by(groep) %>%
  slice(chull(NMDS1, NMDS2))  # Get convex hull points

# Plot NMDS with convex hulls
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = groep)) +
  geom_point(size = 3, alpha = 0.7) +  # NMDS points
  geom_polygon(data = hull_data, aes(fill = groep, group = groep), alpha = 0.3) +  # Convex hulls
  theme_minimal() +
  labs(title = "NMDS Ordination with Convex Hulls",
       x = "NMDS1", y = "NMDS2") +
  theme(legend.position = "right")


# met soorten bij

species_scores <- as.data.frame(scores(nmds, display = "species"))  # Extract species coordinates
species_scores$species <- rownames(species_scores)  # Add species names

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = groep)) +
  geom_point(size = 3, alpha = 0.7) +  # NMDS points
  geom_polygon(data = hull_data, aes(fill = groep, group = groep), alpha = 0.3) +  # Convex hulls
  geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
            color = "black", fontface = "italic", size = 2) +  # Add species labels
  # geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
  #              arrow = arrow(length = unit(0.2, "cm")), color = "black") +  # Arrows from origin
  theme_minimal() +
  labs(title = "NMDS Ordination with Convex Hulls and Species",
       x = "NMDS1", y = "NMDS2") +
  theme(legend.position = "right")
