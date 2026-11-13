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

# ==============================================================================
# FUNCTIE 6: standardized estimates psem
# ==============================================================================
standardize_psem <- function(psem_obj) {
  # 1. Haal de coëfficiënten op (ongestandaardiseerd + gestandaardiseerd waar mogelijk)
  coef_table <- piecewiseSEM::coefs(psem_obj, standardize = "scale")

  # 2. Identificeer welke rijen de Std.Estimate missen
  # We checken op NA of op het "-" teken dat piecewiseSEM vaak gebruikt
  missing_idx <- which(is.na(coef_table$Std.Estimate) | coef_table$Std.Estimate == "-")

  if (length(missing_idx) == 0) {
    message("✅ Alle Std.Estimates zijn al aanwezig.")
    return(coef_table)
  }

  # 3. Haal unieke responses op die reparatie nodig hebben
  responses_to_fix <- unique(coef_table$Response[missing_idx])

  for (resp_name in responses_to_fix) {
    # Zoek het specifieke model in de psem lijst
    # We zoeken op basis van de naam van de afhankelijke variabele
    model_obj <- psem_obj[[which(sapply(psem_obj, function(x)
      tryCatch(insight::find_response(x) == resp_name, error = function(e) FALSE)))]]

    if (is.null(model_obj)) next

    message("🔧 Berekenen gestandaardiseerde waarden voor: ", resp_name)

    # --- De 'insight/performance' magie ---
    # Haal de varianties op (Nakagawa methode)
    vars <- insight::get_variance(model_obj)

    # Bereken totale latente SD: sqrt(Var_fixed + Var_random + Var_distribution)
    # De 'var.distribution' is cruciaal voor non-gaussian modellen (Varm)
    total_latent_sd <- sqrt(sum(unlist(vars[c("var.fixed", "var.random", "var.distribution")]), na.rm = TRUE))

    # Haal de data op om SD van de predictors te berekenen
    model_data <- insight::get_data(model_obj)

    # Update de specifieke rijen in de tabel
    curr_rows <- which(coef_table$Response == resp_name & (is.na(coef_table$Std.Estimate) | coef_table$Std.Estimate == "-"))

    for (idx in curr_rows) {
      pred_name <- coef_table$Predictor[idx]
      beta_unstd <- coef_table$Estimate[idx]

      if (pred_name %in% names(model_data)) {
        sd_x <- sd(model_data[[pred_name]], na.rm = TRUE)
        # De formule: beta * (SD_x / SD_y_totaal)
        coef_table$Std.Estimate[idx] <- as.numeric(beta_unstd * (sd_x / total_latent_sd))
      }
    }
  }

  # Dwing de kolom naar numeriek voor verdere analyse
  coef_table$Std.Estimate <- as.numeric(coef_table$Std.Estimate)

  return(coef_table)
}

# ==============================================================================
# FUNCTIE 7: r2
# ==============================================================================

r2_psem_mumin <- function(psem_obj) {
  modellen <- Filter(function(m) inherits(m, c("lm", "glm", "merMod", "glmmTMB")),
                     unclass(psem_obj))
  purrr::map_dfr(modellen, function(m) {
    r <- MuMIn::r.squaredGLMM(m)
    # niet-gaussiaans: twee rijen (theoretical, delta); gaussiaans: één rij
    r <- if ("theoretical" %in% rownames(r)) r["theoretical", , drop = FALSE] else r[1, , drop = FALSE]
    tibble::tibble(
      Response    = all.vars(formula(m)[[2]])[1],
      Class       = class(m)[1],
      Family      = family(m)$family,
      Marginal    = unname(r[, "R2m"]),
      Conditional = unname(r[, "R2c"])
    )
  })
}

# ==============================================================================
# FUNCTIE 7: bewaar r2
# ==============================================================================

bewaar_r2 <- function(psem_obj, groep, typologie, maatlat) {
  map <- here::here("source", "analyse", "sem", "synthese", "sem_r2")
  dir.create(map, showWarnings = FALSE, recursive = TRUE)
  saveRDS(r2_psem_mumin(psem_obj),
          file.path(map, paste0(paste(groep, typologie, maatlat, sep = "_"), ".rds")))
}

# ==============================================================================
# FUNCTIE 8: SAC test psem model
# ==============================================================================

sac_psem_set <- function(psems, data,
                         responses = c("mmif", "ta_xw", "sw_dw", "nst_prop", "mt_sw_prop", "ept_prop", "ekc_mafy", "v_zonder_gep", "vo_zonder_gep", "gv_zonder_gep", "ts_zonder_gep"),
                         site = "meetplaats", x = "x", y = "y",
                         n = 500, seed = 123, min_groups = 10) {

  stopifnot(is.list(psems), !is.null(names(psems)))

  model_classes <- c("glmmTMB", "merMod", "lm", "glm", "lme", "gls")
  get_mods <- function(sem) Filter(function(z) inherits(z, model_classes), unclass(sem))
  resp_of  <- function(m) all.vars(formula(m)[[2]])
  resp_lab <- function(m) paste(deparse(formula(m)[[2]]), collapse = "")

  # ---------- kern: alle tests voor één deelmodel ----------
  moran <- function(v, xx, yy) {
    na <- c(I = NA_real_, expected = NA_real_, sd = NA_real_, p = NA_real_)
    ok <- is.finite(v) & is.finite(xx) & is.finite(yy)
    if (sum(ok) < min_groups || sd(v[ok]) == 0) return(c(na, n = sum(ok)))
    d <- as.matrix(dist(cbind(xx[ok], yy[ok])))
    w <- 1 / d; diag(w) <- 0; w[!is.finite(w)] <- 0
    m <- ape::Moran.I(v[ok], w)
    c(I = m$observed, expected = m$expected, sd = m$sd, p = m$p.value, n = sum(ok))
  }

  get_re <- function(m) {
    if (inherits(m, "glmmTMB")) return(ranef(m)$cond)
    if (inherits(m, "merMod"))  return(ranef(m))
    NULL
  }

  site_resid <- function(m, grp, conditional) {
    if (inherits(m, c("lme", "gls"))) {
      if (conditional) return(NULL)
      return(tapply(resid(m, type = "pearson"), grp, mean))
    }
    res <- if (inherits(m, "glmmTMB")) {
      if (conditional) {
        glmmTMB::set_simcodes(m$obj, val = "fix")
        on.exit(glmmTMB::set_simcodes(m$obj, val = "random"), add = TRUE)
      }
      simulateResiduals(m, n = n, seed = seed, plot = FALSE)
    } else if (inherits(m, "merMod")) {
      simulateResiduals(m, n = n, seed = seed, plot = FALSE,
                        re.form = if (conditional) NULL else NA)
    } else {
      if (conditional) return(NULL)
      simulateResiduals(m, n = n, seed = seed, plot = FALSE)
    }
    recalculateResiduals(res, group = grp)$scaledResiduals
  }

  one_model <- function(m) {
    rn  <- rownames(model.frame(m))
    idx <- match(rn, rownames(data))
    if (anyNA(idx) || length(idx) != nobs(m))
      stop("data komt niet overeen met de modeldata (NA-rijen of subset?)")
    d   <- data[idx, , drop = FALSE]

    grp <- droplevels(factor(d[[site]]))
    cx  <- as.numeric(tapply(d[[x]], grp, mean))
    cy  <- as.numeric(tapply(d[[y]], grp, mean))

    out  <- list()
    safe <- function(expr) tryCatch(expr, error = function(e) NULL)

    r_unc <- safe(site_resid(m, grp, FALSE))
    if (!is.null(r_unc)) out[["residuen_onvoorwaardelijk"]] <- moran(as.numeric(r_unc), cx, cy)

    re <- safe(get_re(m))
    if (length(re) > 0) {
      r_con <- safe(site_resid(m, grp, TRUE))
      if (!is.null(r_con)) out[["residuen_conditioneel"]] <- moran(as.numeric(r_con), cx, cy)

      for (term in names(re)) {
        vars <- strsplit(term, ":", fixed = TRUE)[[1]]
        if (!all(vars %in% names(d))) next
        g   <- do.call(paste, c(d[vars], sep = ":"))
        gx  <- tapply(d[[x]], g, mean)
        gy  <- tapply(d[[y]], g, mean)
        mat <- as.data.frame(re[[term]])
        v   <- mat[[if ("(Intercept)" %in% names(mat)) "(Intercept)" else 1]]
        nms <- rownames(mat)
        out[[paste0("RE_", term)]] <- moran(v, as.numeric(gx[nms]), as.numeric(gy[nms]))
      }
    }
    if (length(out) == 0) stop("geen enkele test kon worden uitgevoerd")
    tab <- as.data.frame(do.call(rbind, out))
    data.frame(type = class(m)[1], test = names(out), tab, row.names = NULL)
  }

  run <- function(m, groep, psem_nm, model_nm) {
    tryCatch(
      cbind(groep = groep, psem = psem_nm, model = model_nm, one_model(m)),
      error = function(e)
        data.frame(groep = groep, psem = psem_nm, model = model_nm,
                   type = class(m)[1], test = paste("FOUT:", conditionMessage(e)),
                   I = NA, expected = NA, sd = NA, p = NA, n = NA))
  }

  # ---------- 1. biologische responsmodellen: per psem één ----------
  bio_rows <- list()
  for (pn in names(psems)) {
    mods <- get_mods(psems[[pn]])
    is_bio <- vapply(mods, function(m) any(resp_of(m) %in% responses), logical(1))
    if (sum(is_bio) != 1)
      stop("psem '", pn, "': ", sum(is_bio),
           " deelmodellen met een biologische respons gevonden (verwacht: 1).")
    m <- mods[[which(is_bio)]]
    bio_rows[[pn]] <- run(m, "biologisch", pn, resp_lab(m))
  }

  # ---------- 2. gedeelde deelmodellen: één keer, uit het eerste psem ----------
  ref_nm   <- names(psems)[1]
  ref_mods <- get_mods(psems[[ref_nm]])
  ref_mods <- ref_mods[!vapply(ref_mods, function(m) any(resp_of(m) %in% responses),
                               logical(1))]
  names(ref_mods) <- make.unique(vapply(ref_mods, resp_lab, character(1)))

  # controle: zijn de gedeelde deelmodellen echt identiek in de andere psems?
  for (pn in names(psems)[-1]) {
    other <- get_mods(psems[[pn]])
    other <- other[!vapply(other, function(m) any(resp_of(m) %in% responses), logical(1))]
    names(other) <- make.unique(vapply(other, resp_lab, character(1)))
    for (nm in names(ref_mods)) {
      same <- nm %in% names(other) &&
        identical(deparse(formula(ref_mods[[nm]])), deparse(formula(other[[nm]]))) &&
        isTRUE(all.equal(as.numeric(logLik(ref_mods[[nm]])),
                         as.numeric(logLik(other[[nm]])), tolerance = 1e-6))
      if (!same)
        warning("Deelmodel '", nm, "' verschilt tussen psem '", ref_nm,
                "' en '", pn, "'; alleen die uit '", ref_nm, "' wordt getest.")
    }
  }

  shared_rows <- Map(function(m, nm) run(m, "gedeeld", ref_nm, nm),
                     ref_mods, names(ref_mods))

  # ---------- 3. samenvoegen + correctie ----------
  res <- bind_rows(c(bio_rows, shared_rows))
  res$p_bh <- p.adjust(res$p, method = "BH")
  res$flag <- ifelse(!is.na(res$p_bh) & res$p_bh < 0.05, "*", "")
  res$I <- round(res$I, 4); res$expected <- round(res$expected, 4)
  res$sd <- round(res$sd, 4)
  res$p <- signif(res$p, 3); res$p_bh <- signif(res$p_bh, 3)
  res
}

# =============================================================================
# sac_followup.R
# Vervolg op sac_psem_set(): op basis van de SAC-tabel
#   1. selecteren welke deelmodellen een sensitiviteitsanalyse nodig hebben
#   2. die modellen herfitten met een ruimtelijk gecorreleerd random effect
#   3. schattingen/SE's vergelijken en een oordeel geven (robuust / let op / gevoelig)
#   4. figuren: overzicht SAC-tabel, correlogrammen, vergelijking coëfficiënten
#
# Vereist: dplyr, tidyr, ggplot2, DHARMa, glmmTMB, lme4 (findbars/nobars), ape, ncf
# Aanname: projectiecoördinaten in meter (bv. Lambert-72), glmmTMB-deelmodellen.
# NIET GETEST: draai eerst op één typologie / één model en controleer de uitvoer.
#
# Gebruik:
#   sac_tab <- sac_psem_set(psems_mi_beek, data = dredge_data)
#   plot_sac_overview(sac_tab)
#   fu <- sac_followup(sac_tab, psems_mi_beek, dredge_data, out_dir = "figuren/sac_mi_beek")
#   fu$plan; fu$resultaten; fu$coef; fu$zin
#
#   # plan eerst bekijken/aanpassen, dan pas rekenen:
#   pl <- sac_plan(sac_tab); pl$run_owl[pl$model == "tax"] <- FALSE
#   fu <- sac_followup(sac_tab, psems_mi_beek, dredge_data, plan = pl)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(DHARMa)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------- helpers ----

.resp_lab <- function(m) paste(deparse(formula(m)[[2]]), collapse = "")

.get_mods <- function(sem) {
  Filter(function(z) inherits(z, c("glmmTMB", "merMod", "lm", "glm", "lme", "gls")),
         unclass(sem))
}

.find_model <- function(psems, psem, model) {
  mods <- .get_mods(psems[[psem]])
  labs <- vapply(mods, .resp_lab, character(1))
  i <- which(labs == model)
  if (length(i) != 1)
    stop("deelmodel '", model, "' niet uniek gevonden in psem '", psem, "'")
  mods[[i]]
}

# data uitlijnen op de rijen die het model echt gebruikte
.align <- function(m, data) {
  rn  <- rownames(model.frame(m))
  idx <- match(rn, rownames(data))
  if (anyNA(idx) || length(idx) != nobs(m))
    stop("data komt niet overeen met de modeldata (NA-rijen of subset?)")
  data[idx, , drop = FALSE]
}

# originele data van het model (uit de call); anders de meegegeven data
.orig_data <- function(m, data, x, y) {
  cl  <- getCall(m)
  env <- environment(formula(m)) %||% globalenv()
  d0  <- tryCatch(eval(cl$data, env), error = function(e) NULL)
  if (is.null(d0) || !is.data.frame(d0) || !all(c(x, y) %in% names(d0))) d0 <- data
  d0
}

.moran <- function(v, xx, yy, min_groups = 10) {
  na <- c(I = NA_real_, expected = NA_real_, sd = NA_real_, p = NA_real_)
  ok <- is.finite(v) & is.finite(xx) & is.finite(yy)
  if (sum(ok) < min_groups || sd(v[ok]) == 0) return(c(na, n = sum(ok)))
  d <- as.matrix(dist(cbind(xx[ok], yy[ok])))
  w <- 1 / d; diag(w) <- 0; w[!is.finite(w)] <- 0
  mm <- ape::Moran.I(v[ok], w)
  c(I = mm$observed, expected = mm$expected, sd = mm$sd, p = mm$p.value, n = sum(ok))
}

# geschaalde DHARMa-residuen per meetplaats (optioneel conditioneel op de RE's)
.site_resid <- function(m, d, site, x, y, conditional, n, seed) {
  grp <- droplevels(factor(d[[site]]))
  if (conditional) {
    glmmTMB::set_simcodes(m$obj, val = "fix")
    on.exit(glmmTMB::set_simcodes(m$obj, val = "random"), add = TRUE)
  }
  res <- simulateResiduals(m, n = n, seed = seed, plot = FALSE)
  r   <- recalculateResiduals(res, group = grp)$scaledResiduals
  data.frame(site = levels(grp),
             r    = as.numeric(r),
             x    = as.numeric(tapply(d[[x]], grp, mean)),
             y    = as.numeric(tapply(d[[y]], grp, mean)))
}

# formule herbouwen: random-effect-term verwijderen en/of termen toevoegen
.new_formula <- function(f, drop = NULL, add = NULL) {
  bars <- lme4::findbars(f)
  lab  <- vapply(bars, function(b) paste(deparse(b), collapse = ""), character(1))
  keep <- rep(TRUE, length(lab))
  if (!is.null(drop)) {
    keep <- gsub("\\s", "", lab) != paste0("1|", drop)
    if (all(keep)) stop("random-effect-term '(1 | ", drop, ")' niet gevonden in het model")
  }
  fx <- lme4::nobars(f)
  tt <- terms(fx)
  term_labels <- attr(tt, "term.labels")
  offs <- character(0)
  if (!is.null(attr(tt, "offset"))) {
    vars <- as.list(attr(tt, "variables"))[-1]
    offs <- vapply(vars[attr(tt, "offset")],
                   function(e) paste(deparse(e), collapse = ""), character(1))
  }
  rhs <- c(term_labels, offs,
           if (any(keep)) paste0("(", lab[keep], ")"), add)
  nf <- reformulate(rhs, response = f[[2]], intercept = attr(tt, "intercept") == 1)
  environment(nf) <- environment(f)
  nf
}

# coördinaten per groep (gemiddelde, in km) als numFactor voor exp()-covariantie
.add_pos <- function(d, key, x, y, scale_km) {
  g  <- factor(d[[key]])
  cx <- ave(d[[x]], g, FUN = function(v) mean(v, na.rm = TRUE)) / scale_km
  cy <- ave(d[[y]], g, FUN = function(v) mean(v, na.rm = TRUE)) / scale_km
  if (anyNA(cx) || anyNA(cy)) stop("NA in de coördinaten voor groep '", key, "'")
  d$sp_pos   <- glmmTMB::numFactor(cx, cy)
  d$sp_dummy <- factor(1)
  d
}

.refit <- function(m, d, f_new) {
  cl <- tryCatch(match.call(glmmTMB::glmmTMB, getCall(m)), error = function(e) getCall(m))
  cl$formula <- f_new
  cl$data    <- d
  eval(cl, environment(formula(m)) %||% globalenv())
}

.fit_ok <- function(fit) {
  if (!(isTRUE(fit$sdr$pdHess) && isTRUE(fit$fit$convergence == 0))) return(FALSE)
  # ook alle fixed-effect SE's moeten bestaan (pdHess alleen is niet genoeg)
  se <- tryCatch(suppressWarnings(summary(fit)$coefficients$cond[, 2]),
                 error = function(e) NA_real_)
  all(is.finite(se))
}

.sp_sd <- function(m) {
  tryCatch({
    vc <- VarCorr(m)$cond
    j  <- grep("sp_dummy", names(vc))[1]
    as.numeric(attr(vc[[j]], "stddev")[1])
  }, error = function(e) NA_real_)
}

.coef_tab <- function(m0, m1, alpha, marg_band = c(0.02, 0.10)) {
  s0 <- summary(m0)$coefficients$cond
  s1 <- summary(m1)$coefficients$cond
  nm <- intersect(rownames(s0), rownames(s1))
  data.frame(term   = nm,
             est_iid = s0[nm, 1], se_iid = s0[nm, 2], p_iid = s0[nm, 4],
             est_sp  = s1[nm, 1], se_sp  = s1[nm, 2], p_sp  = s1[nm, 4],
             row.names = NULL) %>%
    mutate(se_ratio    = se_sp / se_iid,
           shift_se    = (est_sp - est_iid) / se_iid,
           sig_iid     = p_iid < alpha,
           sig_sp      = p_sp  < alpha,
           sig_change  = sig_iid != sig_sp,
           sign_change = sign(est_iid) != sign(est_sp) & (sig_iid | sig_sp),
           # marginaal: significantie wisselt, maar beide p-waarden liggen vlak bij alpha
           marginaal   = sig_change & !sign_change &
             pmin(p_iid, p_sp) >= marg_band[1] &
             pmax(p_iid, p_sp) <= marg_band[2])
}

.verdict <- function(ct, se_tol, est_tol) {
  ct <- ct[ct$term != "(Intercept)", , drop = FALSE]
  if (nrow(ct) == 0) return(NA_character_)
  if (anyNA(ct$se_sp) || anyNA(ct$se_ratio) || anyNA(ct$p_sp))
    return("niet beoordeelbaar (NaN in SE's ruimtelijk model)")
  chg <- ct$sig_change | ct$sign_change
  if (any(chg, na.rm = TRUE))
    return(if (isTRUE(all(ct$marginaal[which(chg)]))) "marginaal" else "gevoelig")
  if (any(abs(ct$se_ratio - 1) > se_tol, na.rm = TRUE) ||
      any(abs(ct$shift_se) > est_tol, na.rm = TRUE)) return("let op")
  "robuust"
}

# ------------------------------------------------------------------ plan -----

#' Bepaal per deelmodel welke sensitiviteitsanalyse nodig is.
#'  - site: residuen_conditioneel of RE_meetplaats significant (BH) en I - verwacht >= min_dI
#'  - owl : RE_owl significant (BH) en I - verwacht >= min_dI
#'          owl_modus "representatief": alleen het model met de sterkste owl-structuur
#'  - bekken: alleen gemeld (te weinig groepen om te modelleren)
sac_plan <- function(sac_tab, alpha = 0.05, min_dI = 0.02,
                     owl_modus = c("representatief", "alle", "geen")) {
  owl_modus <- match.arg(owl_modus)

  tab <- sac_tab %>%
    filter(!grepl("^FOUT", test)) %>%
    mutate(dI  = I - expected,
           sig = !is.na(p_bh) & p_bh < alpha & !is.na(dI) & dI >= min_dI)

  plan <- tab %>%
    group_by(groep, psem, model, type) %>%
    summarise(
      flag_site   = any(sig & test %in% c("residuen_conditioneel", "RE_meetplaats")),
      flag_owl    = any(sig & test == "RE_owl"),
      flag_bekken = any(sig & test == "RE_bekken"),
      dI_owl      = dI[test == "RE_owl"][1],
      reden       = if (any(sig)) paste0(test[sig], " (I=", round(I[sig], 3),
                                         ", p_bh=", signif(p_bh[sig], 2), ")",
                                         collapse = "; ") else "",
      .groups = "drop")

  plan$run_site <- plan$flag_site
  plan$run_owl <- switch(
    owl_modus,
    "geen"   = FALSE,
    "alle"   = plan$flag_owl,
    "representatief" = {
      mx <- suppressWarnings(max(plan$dI_owl[plan$flag_owl], na.rm = TRUE))
      plan$flag_owl & !is.na(plan$dI_owl) & plan$dI_owl == mx
    })
  plan
}

# ---------------------------------------------- sensitiviteit: één model ------

sac_sens_one <- function(m, data, scale = c("site", "owl"),
                         site = "meetplaats", owl = "owl", x = "x", y = "y",
                         scale_km = 1000, n = 500, seed = 123, alpha = 0.05,
                         se_tol = 0.10, est_tol = 1, post_test = TRUE,
                         max_levels = 1500, coord_mult = c(1, 10),
                         marg_band = c(0.02, 0.10)) {
  scale <- match.arg(scale)
  if (!inherits(m, "glmmTMB")) stop("sensitiviteitsanalyse is alleen voor glmmTMB-modellen")

  d0  <- .orig_data(m, data, x, y)
  # alleen de rijen die het model echt gebruikte: anders krijgt de ruimtelijke term
  # levels (coordinaten) zonder waarnemingen, wat NaN-starts en enorme matrices geeft
  d_m <- tryCatch(.align(m, d0), error = function(e) .align(m, data))
  key <- if (scale == "site") site else owl
  n_lev <- length(unique(d_m[[key]]))
  if (n_lev > max_levels)
    stop(n_lev, " groepen op schaal '", scale, "' (> max_levels = ", max_levels,
         "): dense ", n_lev, "x", n_lev, " covariantiematrix is te zwaar")
  add <- "exp(sp_pos + 0 | sp_dummy)"
  f   <- formula(m)

  # strategieën in volgorde: site -> toevoegen (nugget blijft), anders vervangen
  strategies <- if (scale == "site") {
    list(toevoegen = function() .new_formula(f, add = add),
         vervangen = function() .new_formula(f, drop = site, add = add))
  } else {
    list(vervangen = function() .new_formula(f, drop = owl, add = add))
  }

  fit <- NULL; used <- NA_character_; warn <- character(0); msgs <- character(0)
  fallback <- NULL; fallback_nm <- NA_character_; d <- NULL; d_fb <- NULL
  for (mult in coord_mult) {            # coordinateneenheid: scale_km * mult (m)
    d_try <- .add_pos(d_m, key, x, y, scale_km * mult)
    for (nm in names(strategies)) {
      tag <- sprintf("%s, eenheid %g km", nm, mult)
      cand <- tryCatch(
        withCallingHandlers(
          .refit(m, d_try, strategies[[nm]]()),
          warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }),
        error = function(e) { msgs <<- c(msgs, paste0(tag, ": ", conditionMessage(e))); NULL })
      if (is.null(cand)) next
      if (.fit_ok(cand)) { fit <- cand; used <- tag; d <- d_try; break }
      if (is.null(fallback)) { fallback <- cand; fallback_nm <- tag; d_fb <- d_try }
    }
    if (!is.null(fit)) break
  }
  if (is.null(fit) && !is.null(fallback)) { fit <- fallback; used <- fallback_nm; d <- d_fb }
  if (is.null(fit))
    stop("ruimtelijk model kon niet worden gefit (", nrow(d_m), " rijen, ", n_lev,
         " groepen): ", paste(msgs, collapse = " || "))

  ct <- .coef_tab(m, fit, alpha, marg_band)
  ct_nb <- ct[ct$term != "(Intercept)", , drop = FALSE]

  # controle (alleen site-schaal): is er na de ruimtelijke herfit nog autocorrelatie in de
  # ONVOORWAARDELIJKE residuen per meetplaats? (conditioneel simuleren kan niet: glmmTMB
  # ondersteunt set_simcodes niet voor ruimtelijke covariantiestructuren.)
  # Vergelijk post_I met pre_I (zelfde toets op het iid-model).
  post_I <- post_p <- NA_real_
  post_fout <- if (!post_test) "uitgezet" else if (scale != "site") "alleen voor schaal 'site'" else ""
  if (post_test && scale == "site") {
    pt <- tryCatch({
      sr <- .site_resid(fit, .align(fit, d), site, x, y, conditional = FALSE, n, seed)
      .moran(sr$r, sr$x, sr$y)
    }, error = function(e) { post_fout <<- conditionMessage(e); c(I = NA, p = NA) })
    post_I <- unname(pt["I"]); post_p <- unname(pt["p"])
    if (is.na(post_I) && post_fout == "")
      post_fout <- "Moran's I niet berekenbaar (te weinig groepen of geen variantie)"
  }

  chg <- ct_nb[ct_nb$sig_change | ct_nb$sign_change, , drop = FALSE]
  wissel <- if (nrow(chg)) paste0(chg$term, " (p ", signif(chg$p_iid, 2), " -> ",
                                  signif(chg$p_sp, 2), ")", collapse = "; ") else ""

  summ <- data.frame(
    schaal       = scale,
    strategie    = used,
    pdHess       = isTRUE(fit$sdr$pdHess),
    AIC_iid      = AIC(m),
    AIC_ruimtelijk = AIC(fit),
    dAIC         = AIC(fit) - AIC(m),
    sd_ruimtelijk = .sp_sd(fit),
    n_sig_change = sum(ct_nb$sig_change, na.rm = TRUE),
    n_sign_change = sum(ct_nb$sign_change, na.rm = TRUE),
    n_marginaal  = sum(ct_nb$marginaal, na.rm = TRUE),
    wisselende_termen = wissel,
    n_se_na      = sum(is.na(ct_nb$se_sp)),
    max_se_ratio = if (any(is.finite(ct_nb$se_ratio))) max(ct_nb$se_ratio[is.finite(ct_nb$se_ratio)]) else NA_real_,
    min_se_ratio = if (any(is.finite(ct_nb$se_ratio))) min(ct_nb$se_ratio[is.finite(ct_nb$se_ratio)]) else NA_real_,
    max_shift_se = if (any(is.finite(ct_nb$shift_se))) max(abs(ct_nb$shift_se[is.finite(ct_nb$shift_se)])) else NA_real_,
    post_I       = post_I,
    post_p       = post_p,
    post_fout    = post_fout,
    oordeel      = .verdict(ct, se_tol, est_tol),
    waarschuwing = if (length(warn)) paste(unique(warn), collapse = " | ") else "",
    stringsAsFactors = FALSE)

  if (!summ$pdHess) summ$oordeel <- paste0(summ$oordeel, " (model niet pd: onbetrouwbaar)")
  list(summary = summ, coef = ct, fit = fit)
}

# ---------------------------------------------------------------- plots ------

plot_sac_overview <- function(sac_tab, alpha = 0.05) {
  tests <- c("residuen_onvoorwaardelijk", "residuen_conditioneel",
             "RE_meetplaats", "RE_owl", "RE_bekken")
  d <- sac_tab %>%
    filter(test %in% tests) %>%
    mutate(test  = factor(test, levels = tests),
           dI    = I - expected,
           label = ifelse(is.na(I), "", sprintf("%.3f%s", I,
                                                ifelse(!is.na(p_bh) & p_bh < alpha, "*", ""))))
  ggplot(d, aes(test, model, fill = dI)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0, na.value = "grey92",
                         name = "I - verwacht") +
    facet_grid(groep ~ ., scales = "free_y", space = "free_y") +
    labs(x = NULL, y = NULL,
         title = "Ruimtelijke autocorrelatie per deelmodel",
         subtitle = "Moran's I; * = significant na BH-correctie") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid = element_blank())
}

plot_sac_correlogram <- function(df, title, increment_km = 5, max_km = 150,
                                 resamp = 199, min_pairs = 100, scale_km = 1000) {
  cg <- ncf::correlog(df$x / scale_km, df$y / scale_km, df$z,
                      increment = increment_km, resamp = resamp, quiet = TRUE)
  cd <- data.frame(afstand = cg$mean.of.class, r = cg$correlation,
                   p = cg$p, npaar = cg$n)
  cd <- cd[is.finite(cd$afstand) & cd$afstand <= max_km, ]
  cd$sig  <- !is.na(cd$p) & cd$p < 0.05
  cd$weinig <- cd$npaar < min_pairs
  ggplot(cd, aes(afstand, r)) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_line(colour = "grey60") +
    geom_point(aes(fill = sig, alpha = weinig), shape = 21, size = 2.5) +
    scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "black"),
                      name = "p < 0.05") +
    scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.35),
                       name = paste0("< ", min_pairs, " paren"),
                       labels = c("nee", "ja")) +
    labs(x = "Afstand (km, midden van klasse)", y = "Correlatie",
         title = title) +
    theme_minimal(base_size = 11)
}

plot_sac_coef <- function(coef_long) {
  d <- coef_long %>%
    filter(term != "(Intercept)", is.finite(se_iid), is.finite(se_sp)) %>%
    tidyr::pivot_longer(c(est_iid, est_sp, se_iid, se_sp),
                        names_to = c(".value", "spec"), names_sep = "_") %>%
    mutate(spec = dplyr::recode(spec, iid = "iid random effect", sp = "ruimtelijk"))
  ggplot(d, aes(est, term, colour = spec)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
    geom_pointrange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se),
                    position = position_dodge(width = 0.6), size = 0.3) +
    facet_wrap(~ label, scales = "free", ncol = 2) +
    scale_colour_manual(values = c("iid random effect" = "grey40",
                                   "ruimtelijk" = "#d95f02"), name = NULL) +
    labs(x = "Schatting (95% BI)", y = NULL,
         title = "Fixed effects: iid versus ruimtelijk random effect") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

# --------------------------------------------------------- orchestrator ------

sac_followup <- function(sac_tab, psems, data, plan = NULL,
                         site = "meetplaats", owl = "owl", x = "x", y = "y",
                         alpha = 0.05, min_dI = 0.02,
                         owl_modus = c("representatief", "alle", "geen"),
                         n = 500, seed = 123, scale_km = 1000,
                         se_tol = 0.10, est_tol = 1, post_test = TRUE,
                         correlogram = TRUE, max_km = 150, increment_km = 5,
                         resamp = 199, marg_band = c(0.02, 0.10), out_dir = NULL) {

  owl_modus <- match.arg(owl_modus)
  plan <- plan %||% sac_plan(sac_tab, alpha, min_dI, owl_modus)
  if (!is.null(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  summ_l <- list(); coef_l <- list(); fits <- list(); plots <- list(correlogram = list())

  for (i in seq_len(nrow(plan))) {
    for (sc in c("site", "owl")) {
      if (!isTRUE(plan[[paste0("run_", sc)]][i])) next
      p   <- plan[i, ]
      lab <- sprintf("%s | %s | %s", p$groep, p$model, sc)
      message("Sensitiviteit: ", lab)

      if (p$type != "glmmTMB") {
        summ_l[[lab]] <- data.frame(groep = p$groep, psem = p$psem, model = p$model,
                                    schaal = sc, oordeel = "overgeslagen (geen glmmTMB)")
        next
      }

      out <- tryCatch({
        m  <- .find_model(psems, p$psem, p$model)
        sensitivity <- sac_sens_one(m, data, sc, site, owl, x, y, scale_km, n, seed,
                                    alpha, se_tol, est_tol, post_test,
                                    marg_band = marg_band)

        # correlogram van het patroon dat de analyse triggerde
        if (correlogram) {
          dg <- tryCatch({
            d <- .align(m, data)
            if (sc == "site") {
              has_re <- length(ranef(m)$cond) > 0
              sr <- .site_resid(m, d, site, x, y, conditional = has_re, n, seed)
              data.frame(x = sr$x, y = sr$y, z = sr$r)
            } else {
              re <- as.data.frame(ranef(m)$cond[[owl]])
              v  <- re[[if ("(Intercept)" %in% names(re)) "(Intercept)" else 1]]
              gx <- tapply(d[[x]], d[[owl]], mean)
              gy <- tapply(d[[y]], d[[owl]], mean)
              data.frame(x = as.numeric(gx[rownames(re)]),
                         y = as.numeric(gy[rownames(re)]), z = v)
            }
          }, error = function(e) NULL)
          if (!is.null(dg)) {
            ttl <- sprintf("%s (%s): %s", p$model, sc,
                           if (sc == "site") "residuen per meetplaats" else "owl-intercepts")
            plots$correlogram[[lab]] <- plot_sac_correlogram(
              dg[stats::complete.cases(dg), ], ttl, increment_km, max_km, resamp,
              scale_km = scale_km)
          }
        }
        sensitivity
      }, error = function(e) {
        message("  FOUT: ", conditionMessage(e)); NULL })

      if (is.null(out)) {
        summ_l[[lab]] <- data.frame(groep = p$groep, psem = p$psem, model = p$model,
                                    schaal = sc, oordeel = "FOUT bij fitten")
        next
      }
      summ_l[[lab]] <- cbind(groep = p$groep, psem = p$psem, model = p$model, out$summary[-1] ,
                             schaal = sc)
      coef_l[[lab]] <- cbind(groep = p$groep, psem = p$psem, model = p$model,
                             schaal = sc,
                             label = sprintf("%s (%s)", p$model, sc), out$coef)
      fits[[lab]]   <- out$fit
    }
  }

  resultaten <- dplyr::bind_rows(summ_l)
  if (nrow(resultaten) > 0 && all(c("groep", "psem", "model") %in% names(resultaten))) {
    pre <- sac_tab %>%
      filter(test == "residuen_onvoorwaardelijk") %>%
      select(groep, psem, model, pre_I = I, pre_p = p)
    resultaten <- left_join(resultaten, pre, by = c("groep", "psem", "model"))
  }
  coef_long  <- dplyr::bind_rows(coef_l)

  # figuren
  plots$overview <- plot_sac_overview(sac_tab, alpha)
  if (nrow(coef_long) > 0) plots$coef <- plot_sac_coef(coef_long)

  if (!is.null(out_dir)) {
    ggsave(file.path(out_dir, "sac_overzicht.png"), plots$overview,
           width = 9, height = 2 + 0.25 * length(unique(sac_tab$model)), dpi = 200)
    if (!is.null(plots$coef))
      ggsave(file.path(out_dir, "sac_coef_vergelijking.png"), plots$coef,
             width = 10, height = 3 + 2.2 * ceiling(length(unique(coef_long$label)) / 2),
             dpi = 200)
    for (nm in names(plots$correlogram))
      ggsave(file.path(out_dir, paste0("correlogram_", gsub("[^A-Za-z0-9]+", "_", nm), ".png")),
             plots$correlogram[[nm]], width = 6, height = 4, dpi = 200)
    write.csv2(resultaten, file.path(out_dir, "sac_sensitiviteit.csv"), row.names = FALSE)
    write.csv2(coef_long,  file.path(out_dir, "sac_sensitiviteit_coef.csv"), row.names = FALSE)
    write.csv2(plan,       file.path(out_dir, "sac_plan.csv"), row.names = FALSE)
  }

  # samenvattende zin (concept voor methodesectie)
  zin <- NA_character_
  if (nrow(resultaten) > 0 && "oordeel" %in% names(resultaten)) {
    o <- resultaten$oordeel
    zin <- sprintf(paste0(
      "Voor %d model/schaal-combinaties met aanwijzingen voor ruimtelijke autocorrelatie ",
      "werd een sensitiviteitsanalyse uitgevoerd met een ruimtelijk gecorreleerd (exponentieel) ",
      "random effect. De resultaten waren robuust in %d, vroegen aandacht in %d, ",
      "wisselden alleen voor marginale paden (p dicht bij %s) in %d en waren gevoelig in %d gevallen ",
      "(zie supplement)."),
      length(o), sum(o == "robuust", na.rm = TRUE), sum(o == "let op", na.rm = TRUE),
      alpha, sum(o == "marginaal", na.rm = TRUE), sum(grepl("^gevoelig", o)))
  }

  list(plan = plan, resultaten = resultaten, coef = coef_long,
       fits = fits, plots = plots, zin = zin)
}
