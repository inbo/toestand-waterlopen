# inladen packages en data ----
source(here::here("source", "inladen_packages.R"))
load(here("data", "verwerkt", "mi_data.rdata"))

# Linken LU aan macroinvertebraten ----

load(file  = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied.Rdata"))
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_oever.Rdata"))
load(file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer.Rdata"))

mi_data_analyse <- mi_data %>%
  group_by(meetplaats) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  left_join(watershed_landuse_reclass, by = "meetplaats") %>%
  left_join(oever_landuse_reclass, by = "meetplaats") %>%
  left_join(buffer_landuse_reclass, by = "meetplaats") %>%
  st_drop_geometry()

# correlations ----
library(GGally)
library(ggplot2)

pairwise_correlation_plot <- function(data, columns) {
  selected_data <- data[, columns, drop = FALSE]  # Select specified columns
  selected_data <- na.omit(selected_data)
  ggpairs(selected_data) + theme_minimal()  # Create pairwise correlation plot
}

# Example usage:
pairwise_correlation_plot(mi_data_analyse, c(34:47))
pairwise_correlation_plot(mi_data_analyse, c(41:54))


# Load necessary libraries
install.packages("corrplot")  # If not installed
library(corrplot)

# Compute the correlation matrix
cor_data <- mi_data_analyse[, c(34:54)] %>%
  na.omit()

cor_matrix <- cor(cor_data)


# Define a custom color palette
col_palette <- colorRampPalette(c("blue", "white", "red"))(200)

# Plot the correlation matrix
corrplot(cor_matrix,
         method = "color",          # Use color visualization
         col = col_palette,         # Custom color palette
         addCoef.col = "black",     # Add correlation coefficients
         tl.col = "black",          # Labels in black
         tl.srt = 45,               # Rotate labels
         number.cex = 0.7,          # Adjust coefficient text size
         diag = FALSE,              # Hide diagonal
         order = "hclust",          # Cluster similar variables
         addrect = 4)               # Add rectangles to group clusters

# Highlight strong correlations (absolute value > 0.6)
masked_cor_matrix <- cor_matrix
masked_cor_matrix[abs(masked_cor_matrix) < 0.6] <- NA

corrplot(masked_cor_matrix,
         method = "color",      # Use color visualization
         col = col_palette,     # Custom color palette
         addCoef.col = "black", # Show correlation values in black
         tl.col = "black",      # Label color
         tl.srt = 45,           # Rotate labels
         number.cex = 0.7,      # Adjust coefficient text size
         diag = FALSE,          # Hide diagonal
         order = "hclust",      # Cluster similar variables
         na.label = " ")        # Set NA values (low correlations) to appear as white

corrplot(cor_matrix,
         method = "color",
         col = col_palette,
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         diag = FALSE,
         order = "hclust",
         addrect = 4,
         p.mat = 1 - strong_cor,    # Use p.mat to mask non-strong correlations
         sig.level = 0.5)           # Only show strong correlations

# Create a significance matrix: 1 for strong correlations, 0 for weak ones
p.mat <- abs(cor_matrix) >= 0.6

# Plot the correlation matrix with weak correlations shown in white
corrplot(cor_matrix,
         method = "color",      # Use color visualization
         col = col_palette,     # Custom color palette
         addCoef.col = "black", # Show correlation values in black
         tl.col = "black",      # Label color
         tl.srt = 45,           # Rotate labels
         number.cex = 0.7,      # Adjust coefficient text size
         diag = FALSE,          # Hide diagonal
         order = "hclust",      # Cluster similar variables
         p.mat = 1 - p.mat,     # Mask weak correlations
         sig.level = 0.5,       # Only show strong correlations
         insig = "blank")       # Set insignificant correlations to white

mi_data_analyse %>%
  ggplot() +
  geom_point(aes(log(landbouw_intens_afstroomgebied), ta_xw)) +
  geom_smooth(aes(log(landbouw_intens_afstroomgebied), ta_xw), method = "lm") +
  facet_grid(groep~statuut)

mi_data_analyse %>%
  ggplot() +
  geom_point(aes(log(natte_natuur_afstroomgebied), mmif)) +
  geom_smooth(aes(log(natte_natuur_afstroomgebied), mmif), method = "lm") +
  facet_grid(~statuut)

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(landbouw_intens_afstroomgebied, mmif)) +
  geom_smooth(aes(landbouw_intens_afstroomgebied, mmif), method = "lm") +
  facet_grid(~groep)

mi_data_analyse$landbouw_intens_afstroomgebied %>% hist()
mi_data_analyse$landbouw_extensief_afstroomgebied %>%  hist()

mi_data_analyse$mmif %>%  hist()

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(landbouw_intens_afstroomgebied, mmif)) +
  geom_smooth(aes(landbouw_intens_afstroomgebied, mmif), method = "lm") +
  facet_grid(~groep)

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(landbouw_intens_oever, mmif)) +
  geom_smooth(aes(landbouw_intens_oever, mmif), method = "lm") +
  facet_grid(~groep)

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(landbouw_intens_buffer, ep_tw)) +
  geom_smooth(aes(landbouw_intens_buffer, ep_tw), method = "lm") +
  facet_grid(~groep)

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(hooggroen_afstroomgebied, mmif)) +
  geom_smooth(aes(hooggroen_afstroomgebied, mmif), method = "lm") +
  facet_grid(~groep)

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(hooggroen_oever, mmif)) +
  geom_smooth(aes(hooggroen_oever, mmif), method = "lm") +
  facet_grid(~groep)

mi_data_analyse %>%
  filter(statuut == "Natuurlijk") %>%
  ggplot() +
  geom_point(aes(hooggroen_buffer, ep_tw)) +
  geom_smooth(aes(hooggroen_buffer, ep_tw), method = "lm") +
  facet_grid(~groep)
