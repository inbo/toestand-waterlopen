library(piecewiseSEM)
library(dplyr)
library(tidygraph)
library(ggraph)
library(grid)   # voor unit()

# --- Stap 1: Edges voorbereiden ---
edges_data <- coefs_filled %>%
  filter(P.Value < 0.05) %>%
  select(from = Predictor,
         to = Response,
         Std.Estimate,
         P.Value) %>%
  mutate(
    weight = abs(Std.Estimate),
    effect_dir = ifelse(Std.Estimate > 0, "Positief", "Negatief"),
    effect_dir = factor(effect_dir, levels = c("Positief", "Negatief"))
  )


# --- Stap 2: Nodes maken ---
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

# --- Stap 3: Graph maken (BELANGRIJK: NA mutate) ---
g <- tbl_graph(nodes = nodes_data,
               edges = edges_data,
               directed = TRUE)

# --- Stap 4: Plot ---
set.seed(123)

plot <- ggraph(g, layout = "sugiyama") +

  geom_edge_fan(aes(color = effect_dir,
                    width = weight,
                    label = round(Std.Estimate, 2)),
                angle_calc = 'along',
                label_dodge = unit(2.5, 'mm'),
                label_size = 3,
                arrow = arrow(length = unit(4, 'mm'), type = "closed"),
                end_cap = circle(6, 'mm'),
                start_cap = circle(6, 'mm')) +

  geom_node_point(size = 9,
                  color = "white",
                  fill = "lightblue",
                  shape = 21,
                  stroke = 1.5) +

  geom_node_text(aes(label = label),
                 repel = TRUE,
                 size = 3.5,
                 fontface = "bold") +

  scale_edge_width(range = c(0.5, 2.5), guide = "none") +

  scale_edge_color_manual(
    values = c("Positief" = "darkgreen",
               "Negatief" = "firebrick"),
    name = "Effectrichting"
  ) +

  theme_graph(base_family = "sans") +

  labs(title = "Piecewise SEM Resultaten",
       subtitle = "Dikte pijl = Sterkte effect")

print(plot)
