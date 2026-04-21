library(piecewiseSEM)
library(dplyr)
library(tidygraph)
library(ggraph)
library(grid)   # voor unit()

# --- Stap 1: Edges voorbereiden ---
# edges_data <- coefs_filled %>%
#   filter(P.Value < 0.05) %>%
#   select(from = Predictor,
#          to = Response,
#          Std.Estimate,
#          P.Value) %>%
#   mutate(
#     weight = abs(Std.Estimate),
#     effect_dir = ifelse(Std.Estimate > 0, "Positief", "Negatief"),
#     effect_dir = factor(effect_dir, levels = c("Positief", "Negatief"))
#   )

edges_data <- coefs_filled %>%
  filter(P.Value < 0.05) %>%
  # FILTER HIER: verwijder rijen waar Predictor een correlatie is (bevat '~~')
  filter(!grepl("~~", Predictor)) %>%
  filter(Predictor != "jaar_s" & Response != "jaar_s") %>%
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
    name == "czv_log" ~ "CZV (log)",
    name == "o2_s" ~ "Zuurstof",
    name == "ec_20_s" ~ "geleidbaarheid",
    name == "intensiteit_combo_afstr_s" ~ "Landbouw Int. (afstr.)",
    name == "mmif" ~ "MMIF",
    name == "stress_prop" ~ "Stress %",
    name == "aantal_pesticiden_met_overschrijding" ~ "Pesticiden",
    name == "ekc2_waterlichaam_s" ~ "EKC Hydromorf.",
    name == "spei6_s" ~ "SPEI6maand",
    name == "p_sum_7d_s" ~ "Neerslagsom (7d)",
    name == "p_sum_7d_s" ~ "Neerslagsom (7d)",
    name == "lozingen_rwzi_ie_log" ~ "RWZI",
    name == "lozingen_industrie_ie_log" ~ "Industrie",
    name == "lozingen_riool_ie_log" ~ "Rioollozing",
    name == "t_s" ~ "Watertemperatuur",
    name == "verharding_afstr_s" ~ "Verharding (afstr.)",
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

