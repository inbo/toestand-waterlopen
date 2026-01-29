library(piecewiseSEM)
library(dplyr)
library(tidygraph)
library(ggraph)

# --- Stap 1: Data voorbereiden ---
# coefs_df <- coefs(sem_resultaat)[,-9]

# Filter en maak expliciete 'from' en 'to' kolommen
edges_data <- coefs_df %>%
  filter(P.Value < 0.05) %>%
  # BELANGRIJK: Hernoem Predictor naar 'from' en Response naar 'to'
  # en zet ze vooraan. Dit lost de pijlrichting op.
  select(from = Predictor, to = Response, Std.Estimate, P.Value) %>%
  mutate(
    direction = ifelse(Std.Estimate > 0, "Positief", "Negatief"),
    weight = abs(Std.Estimate)
  )

# --- Stap 2: Nodes en Labels maken ---
# We halen alle unieke namen uit de 'from' en 'to' kolommen
node_names <- unique(c(edges_data$from, edges_data$to))

nodes_data <- data.frame(name = node_names) %>%
  mutate(label = case_when(
    name == "n_t_log" ~ "Stikstof (log)",
    name == "p_t_log" ~ "Fosfor (log)",
    name == "o2_fc_s" ~ "Zuurstof",
    name == "intensiteit_combo_s" ~ "Landbouw Int.",
    name == "mmif" ~ "MMIF (Macrofauna)",
    name == "stress_prop" ~ "Stress %",
    name == "aantal_pesticiden_met_overschrijding" ~ "Pesticiden",
    name == "ekc2_waterlichaam_s" ~ "EKC2",
    name == "Neerslag_som_1jaar_s" ~ "Neerslag (Jaar)",
    name == "Neerslag_som_10dagen_s" ~ "Neerslag (10d)",
    TRUE ~ name
  ))

# --- Stap 3: Maak het Tidygraph object ---
# tidygraph koppelt nu automatisch de namen in edges_data aan nodes_data
g <- tbl_graph(nodes = nodes_data, edges = edges_data, directed = TRUE)

# --- Stap 4: De Plot ---
set.seed(123)

ggraph(g, layout = "sugiyama") +
  # 1. De Pijlen
  geom_edge_fan(aes(color = direction,   # Kleur koppelen aan direction
                    width = weight,      # Dikte koppelen aan weight
                    label = round(Std.Estimate, 2)),
                angle_calc = 'along',
                label_dodge = unit(2.5, 'mm'),
                label_size = 3,          # Iets kleinere label tekst
                arrow = arrow(length = unit(4, 'mm'), type = "closed"),
                end_cap = circle(6, 'mm'),   # Iets meer ruimte voor de pijl
                start_cap = circle(6, 'mm')) +

  # 2. De Bollen
  geom_node_point(size = 9, color = "white", fill = "lightblue", shape = 21, stroke = 1.5) +

  # 3. De Tekst labels
  geom_node_text(aes(label = label), repel = TRUE, size = 3.5, fontface = "bold") +

  # 4. Styling & Kleuren (Hier lossen we het kleurenprobleem op)
  scale_edge_width(range = c(0.5, 2.5), guide = "none") + # guide=none verbergt dikte legende
  scale_edge_color_manual(values = c("Positief" = "darkgreen", "Negatief" = "firebrick"),
                          name = "Effectrichting") + # Naam voor de legende

  theme_graph() +
  labs(title = "Piecewise SEM Resultaten",
       subtitle = "Dikte pijl = Sterkte effect")
#
# # Gebruik een betere layout engine binnen igraph
# l <- layout_with_sugiyama(g)$layout
#
# plot(
#   g,
#   layout = l, # Gebruik de hiërarchische layout
#   vertex.size = 25,
#   vertex.color = "white",
#   vertex.frame.color = "grey60",
#   vertex.label.color = "black",
#   vertex.label.cex = 0.8,
#   vertex.label.dist = 0, # Label IN de bol
#
#   # Pijlen styling
#   edge.arrow.size = 0.5,
#   edge.curved = 0.2, # Kromme lijnen helpen tegen overlap
#   edge.label.cex = 0.8,
#   edge.label.color = "grey20",
#
#   # Zorg voor marges zodat tekst niet van het plot valt
#   rescale = TRUE
# )
#
