source("source/analyse/mafy_datasets_prep.R")


# ==============================================================================
# 1. MAFY DATA PREPARATIE (Zonder Rtnt)
# ==============================================================================
# Plaats hier jouw ruwe MAFY datasets. We laten Rtnt bewust weg.
lijst_datasets_mafy <- list(
  "Beek"   = mafy_nat_sv_beek,
  "Kempen" = mafy_nat_sv_kempen,
  "Rivier" = mafy_nat_sv_rivier,
  "Polder" = mafy_nat_sv_polder
)

# Samenvoegen tot één master raw-dataset voor MAFY
df_master_raw_mafy <- bind_rows(lijst_datasets_mafy, .id = "groep") %>%
  rename(n_t = qual_n_t,
         p_t = qual_p_t,
         czv = qual_czv)

# ==============================================================================
# 2. DEFINIEER JOUW MAFY VARIABELEN
# ==============================================================================
# VUL HIER JOUW EXACTE KOLOMNAMEN IN (bijv. de Macrofyt index en deelmaatlatten)
# Voorbeeld: "mmf", "soorten", "abundantie", etc.
respons_lijst_mafy <- c("index_nieuw", "v_zonder_gep", "gv_zonder_gep", "vo_zonder_gep", "ts_zonder_gep")

# ==============================================================================
# 3. RUN DE GAM PIJPLEIN VOOR MAFY
# ==============================================================================
# We hergebruiken jouw bestaande, robuuste functie 'analyseer_gam_volledig'!
cat("\n[1/2] GAMs fitten voor Macrofyten...\n")
resultaten_gam_mafy <- map(respons_lijst_mafy, ~analyseer_gam_volledig(.x, df_master_raw_mafy))
names(resultaten_gam_mafy) <- respons_lijst_mafy

# ==============================================================================
# 4. SLIMME PLOTS GENEREREN EN GEPAGINEERD WEERGEVEN
# ==============================================================================
# We hergebruiken 'plot_significante_gam_effecten' voor de MAFY resultaten
cat("\n[2/2] Significante marginale effecten plotten voor Macrofyten...\n")
alle_plots_gam_mafy <- map(respons_lijst_mafy, ~plot_significante_gam_effecten(.x, resultaten_gam_mafy)) %>%
  compact() %>%
  flatten()

# Splitsen en Plotten
if (length(alle_plots_gam_mafy) > 0) {

  plots_per_pagina <- 4

  chunks_mafy <- split(alle_plots_gam_mafy, ceiling(seq_along(alle_plots_gam_mafy) / plots_per_pagina))
  totaal_paginas_mafy <- length(chunks_mafy)

  cat(paste("\nSucces! Er zijn", length(alle_plots_gam_mafy), "significante plots gevonden. Verdeeld over", totaal_paginas_mafy, "pagina's.\n"))

  walk2(chunks_mafy, seq_along(chunks_mafy), function(chunk, pagina_nummer) {

    p <- wrap_plots(chunk, ncol = 2) +
      plot_layout(guides = "collect") +
      plot_annotation(
        title = paste0("MACROFYTEN: Niet-Lineaire Effecten (Pagina ", pagina_nummer, "/", totaal_paginas_mafy, ")"),
        subtitle = "Interactieplots = 3 lijnen (startkwaliteit). Hoofdeffectplots = 1 zwarte lijn.",
        theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2ca25f"), # Subtiel groen tintje voor planten!
                      legend.position = "bottom")
      )

    print(p)

  })

} else {
  cat("Geen enkele significante variabele gevonden voor de Macrofyten!\n")
}
