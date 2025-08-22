library(dplyr)
library(ggplot2)
library(tidyr)

# Create a sample dataframe with NAs to mimic your data
df_na <- mi_full
df_na <- df_na[order(df_na$monsternamedatum),]
# Calculate NA counts for each variable
na_counts <- colSums(is.na(df_na))

# Convert to a dataframe for plotting
na_df <- data.frame(
  variable = names(na_counts),
  na_count = na_counts
)

# Create a bar plot of NA counts per variable
ggplot(na_df, aes(x = reorder(variable, na_count), y = na_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Number of Missing Values (NA) per Variable",
    x = "Variable",
    y = "Number of NA values"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = na_count), vjust = -0.5)

# install.packages("naniar")
library(naniar)

# Visualize missing data with a dedicated function
vis_miss(df_na %>%
           filter(jaar > 2009) %>%
           select(-meetplaats, - jaar, -monsternamedatum, -statuut, -groep, -categorie, -type, -waterlooptype,
                  -bekken, -deelmonster_id, -landgebruiksjaar, -bbi, -vhag, -waterlichaamcategorie)
           , warn_large_data = F)
