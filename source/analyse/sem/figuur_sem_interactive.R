library(piecewiseSEM)
library(dplyr)
library(visNetwork)
library(htmlwidgets)
library(igraph)

# --- Stap 1: Edges voorbereiden ---
edges_data <- coefs_filled %>%
  filter(P.Value < 0.05) %>%
  filter(!grepl("~~", Predictor)) %>%
  filter(Predictor != "jaar_s" & Response != "jaar_s") %>%
  select(from = Predictor, to = Response, Std.Estimate, P.Value) %>%
  mutate(
    weight = abs(Std.Estimate),
    effect_dir = ifelse(Std.Estimate > 0, "Positief", "Negatief")
  )

edges_vis <- edges_data %>%
  mutate(
    label = as.character(round(Std.Estimate, 2)),
    width = weight * 5,
    color = ifelse(effect_dir == "Positief", "darkgreen", "firebrick"),
    arrows = "to"
  )

# --- Stap 2: Nodes maken met herkenbare 4-letter afkortingen ---
node_names <- unique(c(edges_vis$from, edges_vis$to))

nodes_vis <- data.frame(name = node_names) %>%
  mutate(
    label = case_when(
      name == "n_t_log" ~ "Ntot",
      name == "p_t_log" ~ "Ptot",
      name == "czv_log" ~ "CZV",
      name == "o2_s" ~ "O2",
      name == "ec_20_log" ~ "EC",
      name == "intensiteit_combo_afstr_s" ~ "LBAF",
      name == "intensiteit_combo_oeverzone_s" ~ "LBOE",
      name == "mmif" ~ "MMIF",
      name == "stress_prop" ~ "STRS",
      name == "aantal_pesticiden_met_overschrijding" ~ "PEST",
      name == "ekc2_waterlichaam_s" ~ "HYMO",
      name == "spei6_s" ~ "SPEI",
      name == "p_sum_7d_s" ~ "PIEK",
      name == "n_extreme_3m_s" ~ "EXTR",
      name == "lozingen_rwzi_ie_log" ~ "RWZI",
      name == "lozingen_industrie_ie_log" ~ "IND",
      name == "lozingen_riool_ie_log" ~ "RIOL",
      name == "t_s" ~ "TEMP",
      name == "zs_log" ~ "ZWST",
      name == "verharding_afstr_s" ~ "VHAF",
      name == "verharding_oever_s" ~ "VHOE",
      name == "natuur_oever_s" ~ "NTOE",
      name == "overstorten_blootstelling_index_log" ~ "OS",
      name == "ekc_mafy" ~ "MAFY",
      name == "EQR" ~ "VIS",
      name == "sw_dw" ~ "SWD",
      name == "ept_prop" ~ "EPT",
      name == "ta_xw" ~ "TAX",
      TRUE ~ toupper(substr(name, 1, 4))
    ),
    title = case_when(
      name == "n_t_log" ~ "Totaal Stikstof (log)",
      name == "p_t_log" ~ "Totaal Fosfor (log)",
      name == "czv_log" ~ "Chemisch zuurstofverbruik (log)",
      name == "o2_s" ~ "Zuurstof",
      name == "ec_20_log" ~ "geleidbaarheid",
      name == "intensiteit_combo_afstr_s" ~ "Landbouw Intensiteit afstroomgebied",
      name == "intensiteit_combo_oeverzone_s" ~ "Landbouw Intensiteit oeverzone",
      name == "mmif" ~ "MMIF",
      name == "stress_prop" ~ "Stress %",
      name == "aantal_pesticiden_met_overschrijding" ~ "Pesticiden",
      name == "ekc2_waterlichaam_s" ~ "EKC Hydromorfologie",
      name == "spei6_s" ~ "SPEI (6m)",
      name == "p_sum_7d_s" ~ "Neerslagsom (7d)",
      name == "n_extreme_3m_s" ~ "Aantal extreme neerslagevents (30d)",
      name == "lozingen_rwzi_ie_log" ~ "Rioolwaterzuiveringsinstallatie",
      name == "lozingen_industrie_ie_log" ~ "Industrie",
      name == "lozingen_riool_ie_log" ~ "Rioollozing",
      name == "t_s" ~ "Watertemperatuur",
      name == "zs_log" ~ "Zwevende stof",
      name == "verharding_afstr_s" ~ "Verharding afstroomgebied",
      name == "verharding_oever_s" ~ "Verharding oeverzone",
      name == "natuur_oever_s" ~ "Natuur oeverzone",
      name == "natuur_oever_s" ~ "VHOE",
      name == "overstorten_blootstelling_index_log" ~ "Overstorten index",
      name == "ekc_mafy" ~ "MAFY index",
      name == "EQR" ~ "VIS index",
      name == "sw_dw" ~ "Shannon-Wiener diversiteit",
      name == "ept_prop" ~ "EPT soorten",
      name == "tax_prop" ~ "Aantal taxa",
      TRUE ~ name
    )
  ) %>%
  rename(id = name)

# --- Stap 2.5: Gecorrigeerde & Gebalanceerde Coördinaten ---
ig <- graph_from_data_frame(d = edges_vis, vertices = nodes_vis, directed = TRUE)
sug_layout <- layout_with_sugiyama(ig)$layout

layout_df <- data.frame(
  id = V(ig)$name,
  x = sug_layout[, 1] * 70,   # "Sweet spot" breedte (niet te wijd, voorkomt extreem uitzoomen)
  y = sug_layout[, 2] * -110  # Mooie verticale gelaagdheid
)

nodes_vis <- nodes_vis %>%
  select(-matches("^x$|^y$")) %>%
  left_join(layout_df, by = "id")

nodes_vis <- nodes_vis %>%
  mutate(
    group = case_when(
      id %in% c("spei6_s", "p_sum_7d_s", "n_extreme_3m_s") ~ "Klimaat",
      id %in% c("lozingen_rwzi_ie_log", "lozingen_industrie_ie_log", "lozingen_riool_ie_log", "overstorten_blootstelling_index_log") ~ "Lozingen en overstorten",
      id %in% c("czv_log", "o2_s", "ec_20_s", "zs_log", "o2_s", "t_s", "ec_20_log") ~ "Fysico-chemie",
      id %in% c("ekc2_waterlichaam_s") ~ "Hydromorfologie",
      id %in% c("n_t_log", "p_t_log") ~ "Nutriënten",
      id %in% c("intensiteit_combo_afstr_s", "verharding_afstr_s", "intensiteit_combo_oeverzone_s", "natuur_oever_s", "verharding_oever_s") ~ "Landgebruik",
      id %in% c("mmif", "index_zonder_gep", "ekc_mafy", "EQR", "sw_dw", "ept_prop", "ta_xw") ~ "Ecologische respons",
      TRUE ~ "Overig"
    )
  )

# --- Stap 3 & 4: Het Interactieve Netwerk met werkende Groepskleuren ---
network <- visNetwork(nodes_vis, edges_vis, width = "100%", height = "650px") %>%
  visOptions(nodesIdSelection = TRUE) %>%

  # 1. Basis-styling (ZONDER de vaste color-lijst, zodat groepen werken!)
  visNodes(
    shape = "circle",
    size = 40,
    font = list(face = "sans", size = 15, fontface = "bold", color = "black")
  ) %>%

  # 2. Definieer hier expliciet mooie, herkenbare kleuren per categorie
  visGroups(groupname = "Klimaat", color = list(background = "#d2f5d2", border = "darkgreen")) %>%
  visGroups(groupname = "Lozingen en overstorten", color = list(background = "#ffcccc", border = "firebrick")) %>%
  visGroups(groupname = "Fysico-chemie", color = list(background = "#e6f2ff", border = "steelblue")) %>%
  visGroups(groupname = "Hydromorfologie", color = list(background = "#e6ccff", border = "purple")) %>%
  visGroups(groupname = "Nutriënten", color = list(background = "#ffe6cc", border = "darkorange")) %>%
  visGroups(groupname = "Landgebruik", color = list(background = "#eaeaea", border = "#666666")) %>%
  visGroups(groupname = "Ecologische respons", color = list(background = "#fff0b3", border = "goldenrod")) %>%
  visGroups(groupname = "Overig", color = list(background = "#f9f9f9", border = "#cccccc")) %>%

  visEdges(
    smooth = list(enabled = TRUE, type = "cubicBezier"),
    font = list(align = "middle", size = 11, color = "black")
  ) %>%
  visPhysics(enabled = FALSE) %>%

  # 3. Geoptimaliseerde JavaScript (onthoudt en herstelt de groeps-kleurinstelling)
  visEvents(
    selectNode = JS("
      function(params) {
        if (params.nodes.length > 0) {
          var selectedNode = params.nodes[0];
          var allNodes = this.body.data.nodes.get();
          var allEdges = this.body.data.edges.get();
          var visitedNodes = [selectedNode];
          var visitedEdges = [];
          var queue = [selectedNode];

          while (queue.length > 0) {
            var currNode = queue.shift();
            for (var i = 0; i < allEdges.length; i++) {
              var edge = allEdges[i];
              if (edge.to === currNode) {
                if (visitedEdges.indexOf(edge.id) === -1) visitedEdges.push(edge.id);
                if (visitedNodes.indexOf(edge.from) === -1) {
                  visitedNodes.push(edge.from);
                  queue.push(edge.from);
                }
              }
            }
          }

          var nodesToUpdate = allNodes.map(function(node) {
            // Sla de originele handmatige kleur op ALS die er is, anders 'null' (zodat de groepskleur geldt)
            if (!node.hasOwnProperty('origColor')) {
              node.origColor = node.color ? JSON.parse(JSON.stringify(node.color)) : null;
            }

            if (node.id === selectedNode) {
              node.color = { background: 'orange', border: 'darkorange' };
              node.font = { color: 'black', size: 15, face: 'sans', fontface: 'bold' };
            } else if (visitedNodes.indexOf(node.id) !== -1) {
              // Gevonden in het pad: zet terug naar origColor (indien null valt vis.js terug op de groep)
              node.color = node.origColor;
              node.font = { color: 'black', size: 15, face: 'sans', fontface: 'bold' };
            } else {
              // Niet in het pad: vervagen naar transparant grijs
              node.color = { background: 'rgba(240, 240, 240, 0.15)', border: 'rgba(220, 220, 220, 0.15)' };
              node.font = { color: 'rgba(180, 180, 180, 0.25)', size: 15, face: 'sans', fontface: 'bold' };
            }
            return node;
          });

          var edgesToUpdate = allEdges.map(function(edge) {
            if (!edge.origColor) edge.origColor = edge.color;
            if (visitedEdges.indexOf(edge.id) !== -1) {
              edge.color = edge.origColor;
              edge.font = { color: 'black', align: 'middle', size: 11 };
            } else {
              edge.color = 'rgba(220, 220, 220, 0.05)';
              edge.font = { color: 'rgba(220, 220, 220, 0.05)', align: 'middle', size: 11 };
            }
            return edge;
          });

          this.body.data.nodes.update(nodesToUpdate);
          this.body.data.edges.update(edgesToUpdate);
        }
      }
    "),
    deselectNode = JS("
      function(params) {
        var allNodes = this.body.data.nodes.get();
        var allEdges = this.body.data.edges.get();

        var nodesToUpdate = [];
        allNodes.forEach(function(node) {
          if (node.hasOwnProperty('origColor')) {
            node.color = node.origColor; // wist de handmatige overschrijving -> groepskleur herstelt live
            node.font = { color: 'black', size: 15, face: 'sans', fontface: 'bold' };
            nodesToUpdate.push(node);
          }
        });

        var edgesToUpdate = [];
        allEdges.forEach(function(edge) {
          if (edge.origColor) {
            edge.color = edge.origColor;
            edge.font = { color: 'black', align: 'middle', size: 11 };
            edgesToUpdate.push(edge);
          }
        });

        this.body.data.nodes.update(nodesToUpdate);
        this.body.data.edges.update(edgesToUpdate);
      }
    ")
  ) %>%
  # De legende toont nu automatisch de juiste categorieën met grote tekst (overgenomen van visNodes font)
  visLegend(main = list(text = NULL, style = "font-size:16px; font-weight:bold;"), position = "right", width = 0.15)

# Toon het herstelde netwerk
print(network)
#'
#'
#'
#' # Sla het netwerk op als een los interactief HTML-bestand
#' saveWidget(network, file = here("output", "figuren", "psem_interactief_netwerk.html"), selfcontained = TRUE)
#'
#' #### test bewegen ####
#'
#' # --- Stap 3 & 4: Het Netwerk met XL opzet en BEWEGINGS-physics ---
#' network_moving <- visNetwork(nodes_vis, edges_vis, width = "100%", height = "700px") %>%
#'   visOptions(nodesIdSelection = TRUE) %>%
#'   visNodes(
#'     shape = "circle",
#'     size = 55,
#'     color = list(background = "lightblue", border = "steelblue"),
#'     font = list(face = "sans", size = 18, fontface = "bold", color = "black")
#'   ) %>%
#'   visEdges(
#'     smooth = list(enabled = TRUE, type = "cubicBezier"),
#'     font = list(align = "middle", size = 12, color = "black")
#'   ) %>%
#'
#'   # --- HIER staat de physics nu correct in de 'pipe'-keten ---
#'   # --- Dynamic Physics (blijft subtiel bewegen) ---
#'   visPhysics(
#'     enabled = TRUE,
#'     solver = "barnesHut",
#'     barnesHut = list(
#'       gravitationalConstant = -4000,
#'       centralGravity = 0.02,         # Iets verlaagd zodat ze meer ruimte hebben om te zweven
#'       springLength = 140,            # Iets langere lijnen geeft meer bewegingsruimte
#'       springConstant = 0.03,
#'       avoidOverlap = 1,
#'       damping = 0.2                  # MINDER wrijving = nodes blijven veel langer in beweging
#'     ),
#'     stabilization = FALSE,
#'     minVelocity = 0.005              # Extreem laag: de simulatie stopt hierdoor (bijna) nooit meer
#'   ) %>%
#'
#'   # JavaScript met de hardcoded tekstgrootte op 18 (verbindt perfect door)
#'   visEvents(
#'     selectNode = JS("
#'       function(params) {
#'         if (params.nodes.length > 0) {
#'           var selectedNode = params.nodes[0];
#'           var allNodes = this.body.data.nodes.get();
#'           var allEdges = this.body.data.edges.get();
#'           var visitedNodes = [selectedNode];
#'           var visitedEdges = [];
#'           var queue = [selectedNode];
#'
#'           while (queue.length > 0) {
#'             var currNode = queue.shift();
#'             for (var i = 0; i < allEdges.length; i++) {
#'               var edge = allEdges[i];
#'               if (edge.to === currNode) {
#'                 if (visitedEdges.indexOf(edge.id) === -1) visitedEdges.push(edge.id);
#'                 if (visitedNodes.indexOf(edge.from) === -1) {
#'                   visitedNodes.push(edge.from);
#'                   queue.push(edge.from);
#'                 }
#'               }
#'             }
#'           }
#'
#'           var nodesToUpdate = allNodes.map(function(node) {
#'             if (!node.origColor) node.origColor = node.color || { background: 'lightblue', border: 'steelblue' };
#'
#'             if (node.id === selectedNode) {
#'               node.color = { background: 'orange', border: 'darkorange' };
#'               node.font = { color: 'black', size: 18, face: 'sans', fontface: 'bold' };
#'             } else if (visitedNodes.indexOf(node.id) !== -1) {
#'               node.color = node.origColor;
#'               node.font = { color: 'black', size: 18, face: 'sans', fontface: 'bold' };
#'             } else {
#'               node.color = { background: 'rgba(230, 230, 230, 0.15)', border: 'rgba(200, 200, 200, 0.15)' };
#'               node.font = { color: 'rgba(180, 180, 180, 0.2)', size: 18, face: 'sans', fontface: 'bold' };
#'             }
#'             return node;
#'           });
#'
#'           var edgesToUpdate = allEdges.map(function(edge) {
#'             if (!edge.origColor) edge.origColor = edge.color;
#'             if (visitedEdges.indexOf(edge.id) !== -1) {
#'               edge.color = edge.origColor;
#'               edge.font = { color: 'black', align: 'middle', size: 12 };
#'             } else {
#'               edge.color = 'rgba(220, 220, 220, 0.05)';
#'               edge.font = { color: 'rgba(220, 220, 220, 0.05)', align: 'middle', size: 12 };
#'             }
#'             return edge;
#'           });
#'
#'           this.body.data.nodes.update(nodesToUpdate);
#'           this.body.data.edges.update(edgesToUpdate);
#'         }
#'       }
#'     "),
#'     deselectNode = JS("
#'       function(params) {
#'         var allNodes = this.body.data.nodes.get();
#'         var allEdges = this.body.data.edges.get();
#'
#'         var nodesToUpdate = [];
#'         allNodes.forEach(function(node) {
#'           if (node.origColor) {
#'             node.color = node.origColor;
#'             node.font = { color: 'black', size: 18, face: 'sans', fontface: 'bold' };
#'             nodesToUpdate.push(node);
#'           }
#'         });
#'
#'         var edgesToUpdate = [];
#'         allEdges.forEach(function(edge) {
#'           if (edge.origColor) {
#'             edge.color = edge.origColor;
#'             edge.font = { color: 'black', align: 'middle', size: 12 };
#'             edgesToUpdate.push(edge);
#'           }
#'         });
#'
#'         this.body.data.nodes.update(nodesToUpdate);
#'         this.body.data.edges.update(edgesToUpdate);
#'       }
#'     ")
#'   )
#'
#'
#' network_control_pad <- network_moving %>%
#'   visInteraction(
#'     navigationButtons = TRUE, # Voegt de fysieke knoppen toe op het scherm
#'     keyboard = TRUE,          # Je kunt nu scrollen met je toetsenbord!
#'     hover = TRUE              # Nodes lichten subtiel op als je er met de muis over zweeft
#'   )
#'
#' # Toon het netwerk
#' network_moving
#' print(network_moving)
#'
#' saveWidget(network_moving, file = here("output", "figuren", "psem_interactief_netwerk_moving.html"), selfcontained = TRUE)
#'
#' network_beestjes <- network_moving %>%
#'   htmlwidgets::onRender("
#'     function(el, x) {
#'       // 1. Kies hier je 5 beestjes (nu 5 garnalen, maar mag een mix zijn!)
#'       var creatures = ['🦐', '🐟', '🦟', '🦫', '🦦'];
#'
#'       var style = document.createElement('style');
#'       var dynamicStyles = '';
#'
#'       for (var i = 0; i < 5; i++) {
#'         var bug = document.createElement('div');
#'         bug.innerHTML = creatures[i];
#'
#'         bug.style.position = 'absolute';
#'         bug.style.zIndex = '9999';
#'         bug.style.pointerEvents = 'none';
#'         bug.style.fontSize = (25 + (i * 4)) + 'px';
#'
#'         var startTop = 15 + (i * 14);
#'         var midTop = startTop - 10;
#'         var endTop = startTop + 15;
#'
#'         // HIER ZIJN DE TRANSFORMS OMGEKEEERD SPECIAAL VOOR DE GARNAAL:
#'         dynamicStyles += `
#'           @keyframes swimAround${i} {
#'             0%   { left: -60px; top: ${startTop}%; transform: scaleX(-1); } /* Schuin rechtsvooruit */
#'             45%  { left: 96%;   top: ${midTop}%;   transform: scaleX(-1); }
#'             50%  { left: 96%;   top: ${midTop}%;   transform: scaleX(1); }  /* Kop naar links draaien */
#'             95%  { left: -60px; top: ${endTop}%;   transform: scaleX(1); }  /* Schuin linksvooruit */
#'             100% { left: -60px; top: ${startTop}%; transform: scaleX(-1); } /* Weer naar rechts draaien */
#'           }
#'         `;
#'
#'         var duration = 13 + (i * 3);
#'         var delay = i * 2;
#'
#'         bug.style.animation = `swimAround${i} ${duration}s infinite ${delay}s linear`;
#'         el.appendChild(bug);
#'       }
#'
#'       style.innerHTML = dynamicStyles;
#'       document.head.appendChild(style);
#'     }
#'   ")
#'
#'
#'
#' # Toon het netwerk met de 5 bewegende beestjes
#' network_beestjes
#'
#' saveWidget(network_beestjes, file = here("output", "figuren", "psem_interactief_netwerk_beestjes.html"), selfcontained = TRUE)
#'
#' network_icoontjes <- visNetwork(nodes_vis, edges_vis, width = "100%", height = "650px") %>%
#'   visOptions(nodesIdSelection = TRUE) %>%
#'
#'   # 1. Zet de basisvorm op "icon" in plaats van "circle"
#'   visNodes(
#'     shape = "icon",
#'     size = 40,
#'     font = list(face = "sans", size = 15, fontface = "bold", color = "black")
#'   ) %>%
#'
#'   # 2. Geef per groep aan welk icoon (code) en welke kleur hij moet krijgen
#'   visGroups(groupname = "Klimaat",
#'             icon = list(code = "f0c2", color = "darkgreen")) %>%          # Wolk
#'   visGroups(groupname = "Lozingen en overstorten",
#'             icon = list(code = "f275", color = "firebrick")) %>%          # Fabriek
#'   visGroups(groupname = "Fysico-chemie",
#'             icon = list(code = "f043", color = "steelblue")) %>%          # Druppel
#'   visGroups(groupname = "Hydromorfologie",
#'             icon = list(code = "f279", color = "purple")) %>%             # Thermometer
#'   visGroups(groupname = "Nutriënten",
#'             icon = list(code = "f1fc", color = "darkorange")) %>%         # Blad
#'   visGroups(groupname = "Landgebruik",
#'             icon = list(code = "f1bb", color = "#666666")) %>%            # Boom
#'   visGroups(groupname = "Ecologische respons",
#'             icon = list(code = "f188", color = "goldenrod")) %>%          # Insect/Bug
#'   visGroups(groupname = "Overig",
#'             icon = list(code = "f059", color = "#cccccc")) %>%            # Vraagteken
#'
#'   visEdges(
#'     smooth = list(enabled = TRUE, type = "cubicBezier"),
#'     font = list(align = "middle", size = 11, color = "black")
#'   ) %>%
#'   visPhysics(enabled = FALSE) %>%
#'
#'   # 3. CRUCIAAL: Activeer de FontAwesome bibliotheek die standaard in visNetwork zit
#'   addFontAwesome() %>%
#'
#'   # Je bestaande visLegend en visEvents (met de JavaScript) kunnen hier gewoon onder blijven plakken!
#'   visLegend(main = list(text = "Categorieën", style = "font-size:16px; font-weight:bold;"), position = "right", width = 0.15)
#' network_icoontjes
