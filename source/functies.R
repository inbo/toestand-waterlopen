if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

# ==============================================================================
# FUNCTIE 1: FILTER OP COLLINEARITEIT (De "Schoonmaker")
# ==============================================================================
# Doel: Als twee variabelen > 0.7 correleren, gooi degene weg met de meeste NA's.
# Input: dataset, vector met kolomnamen, drempelwaarde (default 0.7)

filter_collinear_vars <- function(data, candidate_vars, threshold = 0.7) {

  # 1. Selecteer data en check of kolommen bestaan
  valid_vars <- candidate_vars[candidate_vars %in% names(data)]
  if(length(valid_vars) < 2) return(valid_vars)

  df_subset <- data %>% select(all_of(valid_vars))

  # 2. Bereken correlatiematrix (Spearman voor robuustheid)
  # use = "pairwise.complete.obs" zorgt dat NA's niet alles crashen
  cor_mat <- correlate(df_subset, method = "spearman", quiet = TRUE) %>%
    shave() %>%
    corrr::stretch() %>%
    filter(abs(r) > threshold) %>%
    filter(x != y) %>%
    arrange(desc(abs(r)))

  # Als er geen hoge correlaties zijn, return alles
  if(nrow(cor_mat) == 0) return(valid_vars)

  # 3. Eliminatie proces
  vars_to_remove <- c()

  # Loop door de paren met hoge correlatie
  for(i in 1:nrow(cor_mat)) {
    v1 <- cor_mat$x[i]
    v2 <- cor_mat$y[i]

    # Als een van beide al verwijderd is, sla over
    if(v1 %in% vars_to_remove | v2 %in% vars_to_remove) next

    # Tel NA's in beide variabelen
    na_v1 <- sum(is.na(data[[v1]]))
    na_v2 <- sum(is.na(data[[v2]]))

    # Verwijder degene met de meeste NA's. Bij gelijke stand, verwijder v2.
    if(na_v1 <= na_v2) {
      vars_to_remove <- c(vars_to_remove, v2)
    } else {
      vars_to_remove <- c(vars_to_remove, v1)
    }
  }

  # Return de opgeschoonde lijst
  final_vars <- setdiff(valid_vars, vars_to_remove)

  message(paste("Verwijderd wegens collineariteit:", paste(vars_to_remove, collapse = ", ")))
  return(final_vars)
}

# ==============================================================================
# FUNCTIE 2: SELECTEER BESTE VOORSPELLERS (De "Ranker")
# ==============================================================================
# Doel: Fit glmmTMB voor elke variabele en rank op AIC
# Input: data, respons variabele, lijst kandidaten, familie

screen_predictors <- function(data, response_var, candidate_vars, family_distr = gaussian) {

  results <- map_dfr(candidate_vars, function(var) {

    # Formule: Y ~ X + jaar + (1|meetplaats)
    # Jaar zit erin om trends eruit te halen, meetplaats voor herhaalde metingen
    f <- as.formula(paste(response_var, "~", var, "+ jaar_s + (1|meetplaats)"))

    tryCatch({
      # Fit model (REML = FALSE want we vergelijken modellen met verschillende fixed effects via AIC)
      m <- glmmTMB(formula = f, data = data, family = family_distr, REML = FALSE)

      tibble(
        response = response_var,
        predictor = var,
        aic = AIC(m),
        p_val = summary(m)$coefficients$cond[var, "Pr(>|z|)"],
        n_obs = nobs(m), # <--- DIT IS DE MAGISCHE TOEVOEGING
        converged = m$fit$convergence == 0
      )
    }, error = function(e) {
      # Als model crasht (bv door te weinig data), return NA
      return(tibble(response = response_var, predictor = var, aic = NA, p_val = NA, converged = FALSE))
    })
  })

  # Sorteren en Delta AIC berekenen
  results %>%
    filter(converged == TRUE) %>%
    arrange(aic) %>%
    mutate(delta_aic = aic - min(aic, na.rm = TRUE)) %>%
    select(response, predictor, delta_aic, p_val, aic, n_obs)
}

# ==============================================================================
# FUNCTIE 3: plot correllogram
# ==============================================================================
plot_groep_correlogram <- function(data, vars, group_name = "Variabelen") {

  # 1. Check welke variabelen daadwerkelijk in de dataset zitten
  valid_vars <- base::intersect(vars, names(data))

  if(length(valid_vars) < 2) {
    message(paste("Niet genoeg geldige variabelen gevonden voor:", group_name))
    return(invisible(NULL))
  }

  # 2. Selecteer de data en verwijder rijen met NA's voor deze specifieke subset
  # Dit is nodig omdat statistische toetsen (p-waarden) fouten geven bij wisselende N
  df_sub <- data %>%
    dplyr::select(all_of(valid_vars)) %>%
    tidyr::drop_na()

  if(nrow(df_sub) < 10) {
    message(paste("Te weinig complete waarnemingen na het verwijderen van NA's voor:", group_name))
    return(invisible(NULL))
  }

  # 3. Bereken de correlatiematrix (Spearman is robuuster tegen uitschieters)
  cor_matrix <- cor(df_sub, method = "spearman")

  # 4. Bereken p-waarden om niet-significante correlaties door te kruisen
  p_mat <- cor.mtest(df_sub, conf.level = 0.95, method = "spearman")$p

  # 5. Maak de plot
  corrplot(cor_matrix,
           method = "color",           # Gebruik kleurschalen
           type = "upper",             # Toon alleen de bovenste helft (geen dubbele info)
           order = "hclust",           # Groepeer gecorreleerde variabelen samen
           addCoef.col = "black",      # Zet de correlatiegetallen in de vakjes
           number.cex = 0.8,           # Grootte van de getallen
           tl.col = "black",           # Kleur van de tekstlabels
           tl.srt = 45,                # Draai de tekstlabels 45 graden voor leesbaarheid
           p.mat = p_mat,              # Voeg p-waarden matrix toe
           sig.level = 0.05,           # Grens voor significantie
           insig = "pch",              # 'pch' betekent: zet een symbool bij niet-significant
           pch.col = "red",            # Kleur van het symbool (rood)
           pch.cex = 1.5,              # Grootte van het symbool
           diag = FALSE,               # Verberg de diagonaal (die is altijd 1)
           title = paste("Correlogram:", group_name),
           mar = c(0, 0, 2, 0)         # Voorkom dat de titel overlapt met de labels
  )
}

# ==============================================================================
# FUNCTIE 4: plot VIF uit model
# ==============================================================================
plot_model_vif <- function(model, plot_title = "VIF Check") {

  # 1. Bereken VIF waarden
  # suppressWarnings voorkomt ruis als het model net niet perfect is
  vif_result <- suppressWarnings(performance::check_collinearity(model))

  if (is.null(vif_result)) {
    message("⚠️ Kon geen VIF berekenen. Heeft het model wel minimaal 2 predictoren?")
    return(invisible(NULL))
  }

  # 2. Zet om naar een dataframe
  df_vif <- as.data.frame(vif_result)

  # Veiligheidscheck: repareer de kolomnaam afhankelijk van package versie
  if("Term" %in% names(df_vif)) {
    df_vif <- df_vif %>% rename(Parameter = Term)
  }

  # Als er geen VIF kolom is (bv. bij slechts 1 predictor), stop dan netjes
  if(!"VIF" %in% names(df_vif)) {
    message("⚠️ Niet genoeg variabelen om collineariteit (VIF) te berekenen.")
    return(invisible(NULL))
  }

  # Zorg dat er een fallback is als de maximale VIF erg laag is
  max_vif <- max(df_vif$VIF, 6, na.rm = TRUE)

  # 3. Bouw de plot
  p <- ggplot(df_vif, aes(x = reorder(Parameter, VIF), y = VIF)) +
    geom_col(aes(fill = VIF), color = "darkgray", width = 0.6) +
    scale_fill_gradientn(
      colors = c("#1a8828", "#ffc400", "#d7191c"),
      values = scales::rescale(c(1, 3, 5, max_vif)),
      limits = c(1, max_vif)
    ) +
    geom_hline(yintercept = 3, linetype = "dashed", color = "orange", linewidth = 1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 1) +
    coord_flip() +
    labs(
      title = plot_title,
      subtitle = "Controle op multicollineariteit in het model",
      x = "",
      y = "Variance Inflation Factor (VIF)",
      caption = "Veilig: < 3 | Oppassen: > 3 (oranje) | Problematisch: > 5 (rood)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 11, face = "bold"),
      plot.title = element_text(face = "bold")
    )

  return(p)
}

# ==============================================================================
# FUNCTIE 5: cleveland plots
# ==============================================================================
check_outliers_plot <- function(data, var_name) {

  # 1. Selecteer de variabele en gooi NA's (tijdelijk) weg voor de plot
  plot_data <- data %>%
    select(all_of(var_name)) %>%
    drop_na() %>%
    arrange(.data[[var_name]]) %>%
    mutate(rij_id = row_number())

  # 2. Haal de absolute minimumwaarde op om te checken op nullen
  min_waarde <- min(plot_data[[var_name]])

  # 3. Bepaal de juiste transformatie!
  if (min_waarde < 0) {
    warning(paste("Let op:", var_name, "bevat negatieve waarden. Log-transformatie is niet zomaar mogelijk!"))
    plot_data <- plot_data %>% mutate(trans_waarde = .data[[var_name]])
    trans_titel <- "Let op: Geen transformatie (negatieve data)"

  } else if (min_waarde == 0) {
    # Er zijn nullen! Gebruik log(x + 1)
    plot_data <- plot_data %>% mutate(trans_waarde = log(.data[[var_name]] + 1))
    trans_titel <- "log(x + 1)"

  } else {
    # Geen nullen, gewoon positieve getallen. Gebruik normale log(x)
    plot_data <- plot_data %>% mutate(trans_waarde = log(.data[[var_name]]))
    trans_titel <- "log(x)"
  }

  # 4. Maak de RAW plot
  plot_raw <- ggplot(plot_data, aes(x = .data[[var_name]], y = rij_id)) +
    geom_point(color = "steelblue", alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(
      title = paste("RAW:", var_name),
      x = "Ongetransformeerde waarde",
      y = "Rij ID (Gesorteerd)"
    )

  # 5. Maak de Transformed plot
  plot_trans <- ggplot(plot_data, aes(x = trans_waarde, y = rij_id)) +
    geom_point(color = "darkgreen", alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(
      title = paste("TRANS:", trans_titel),
      x = paste(trans_titel, "waarde"),
      y = ""
    )

  # 6. Plak ze naast elkaar en print!
  return(plot_raw + plot_trans)
}
