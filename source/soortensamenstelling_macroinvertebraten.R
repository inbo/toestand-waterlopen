# soortendata NMDS ----


library(vegan)
data_wide <- mi_soorten %>%
  select(macroinvertebraat, aantal, deelmonster_id, datum_monstername, meetplaats, statuut, groep) %>%
  group_by(meetplaats) %>%
  filter(datum_monstername == max(datum_monstername)) %>%
  ungroup() %>%
  pivot_wider(names_from = macroinvertebraat, values_from = aantal, values_fill = 0) %>%
  filter(!is.na(groep))
data_wide <- data_wide[-535, ]
species_matrix <- data_wide %>%
  select(-deelmonster_id, -datum_monstername, -meetplaats, -statuut, -groep) %>%
  select(-Heleobia, -Melitidae, -Tanaidae)


nmds <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 10)
plot(nmds, type = "t")  # Basic NMDS plot

ordiplot(nmds, type = "n")
points(nmds, col = as.factor(data_wide$statuut), pch = 16)

adonis_result <- adonis2(species_matrix ~ statuut, data = data_wide, method = "bray")
print(adonis_result)

dispersion_test <- betadisper(vegdist(species_matrix, method = "bray"), data_wide$statuut)
anova(dispersion_test)  # p < 0.05 means dispersion varies between groups


#groep effect

ordiplot(nmds, type = "n")
points(nmds, col = as.factor(mi_soorten$groep), pch = 16)

adonis_result <- adonis2(species_matrix ~ groep, data = data_wide, method = "bray")
print(adonis_result)

dispersion_test <- betadisper(vegdist(species_matrix, method = "bray"), data_wide$statuut)
anova(dispersion_test)  # p < 0.05 means dispersion varies between groups

#figuur met hulls

library(ggplot2)
library(vegan)
library(dplyr)

species_matrix <- species_matrix[rowSums(species_matrix) > 0, ]
nmds_scores <- as.data.frame(scores(nmds))

# Compute NMDS
nmds <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 10)

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

library(ggplot2)
library(vegan)
library(dplyr)

# Compute NMDS
nmds <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 10)

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
