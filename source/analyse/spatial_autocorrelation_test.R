library(insight)
library(performance)
library(dplyr)

psem_obj <- mmif_sem_nat_sv_beek

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

coefs_filled <- standardize_psem(mmif_sem_nat_sv_beek)[,-9]

summary(mmif_sem_nat_sv_beek)

vars <- insight::get_variance(mmif_sem_nat_sv_beek[[1]])

coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))
source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten


#beek mmif

model <- mmif_sem_nat_sv_beek[[1]]
summary(model)

data_model <-
  dredge_data %>%
  left_join(mi_nat_sv_beek %>%
              select(meetplaats, monsternamedatum, geom),
            by = c("meetplaats", "monsternamedatum")) %>%
  st_as_sf() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry %>%
  mutate(meetplaats = as.factor(meetplaats))

res <- simulateResiduals(fittedModel = model, plot = TRUE )

testSpatialAutocorrelation(res,
                           x = data_model$x,
                           y = data_model$y)

# 1. Groepeer de residuen per locatie (X en Y gecombineerd)
# We maken een tijdelijke groepsvariabele aan
res_grouped <- recalculateResiduals(res, group = interaction(data_model$x, data_model$y))
res_grouped <- recalculateResiduals(res, group = data_model$meetplaats)

res_grouped <- recalculateResiduals(res, group = dredge_data$bekken)

# 2. Haal de unieke locaties op voor de geaggregeerde residuen
# Belangrijk: de volgorde van de locaties moet matchen met de groepen
group_coords <- data_model %>%
  group_by(x, y) %>%
  summarize(.groups = "drop")

group_coords <- aggregate(cbind(x, y) ~ bekken, data = data_model, mean)


# 3. Voer de test uit op de geaggregeerde residuen
testSpatialAutocorrelation(res_grouped,
                           x = group_coords$x,
                           y = group_coords$y)


model_gam_beek <- gam(
  mmif ~ ec_20_log + o2_s + spei6_s + jaar_s + intensiteit_combo_afstr_s + t_s + n_t_log +      p_t_log + verharding_afstr_s +
    s(meetplaats, bs = "re") +
    # s(bekken, bs = "re") +          # Random effect voor bekken
    s(x, y, bs = "gp", k = 100),     # Ruimtelijke smoother (Gaussian Process)
  data = data_model,
  family = gaussian()
)
summary(model_gam_beek)
res <- simulateResiduals(fittedModel = model_gam_beek, plot = TRUE )
res_grouped <- recalculateResiduals(res, group = data_model$meetplaats)
group_coords <- data_model %>%
  group_by(x, y) %>%
  summarize(.groups = "drop")
testSpatialAutocorrelation(res_grouped,
                           x = group_coords$x,
                           y = group_coords$y)

#### kempen mmif####


load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "mmif_sem_nat_sv_kempen.rdata"))

model <- mmif_sem_nat_sv_kempen[[1]]
summary(model)
plot_model(model, type = "pred")$o2_s

r.squaredGLMM(model)
model <- update(model, . ~ . + (1 | owl))
summary(model)



data_model <-
  dredge_data %>%
  left_join(mi_nat_sv_kempen %>%
              select(meetplaats, monsternamedatum, geom),
            by = c("meetplaats", "monsternamedatum")) %>%
  st_as_sf() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry %>%
  mutate(meetplaats = as.factor(meetplaats))

res <- simulateResiduals(fittedModel = model, plot = TRUE)


# 2. Maak een overzicht van UNIEKE locaties in de juiste volgorde
# We groeperen op meetplaats zodat de volgorde matcht met recalculateResiduals
locaties_match <- data_model %>%
  group_by(meetplaats) %>%
  summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
  arrange(meetplaats) # DHARMa sorteert groepen standaard op naam/factor level

# 3. Aggregeer residuen
res_grouped <- recalculateResiduals(res, group = data_model$meetplaats)

# 4. De test
testSpatialAutocorrelation(res_grouped,
                           x = locaties_match$x,
                           y = locaties_match$y)



# SAC oplossen??
# 1. Maak een factor van de coördinaten (glmmTMB syntax)
data_model$pos <- numFactor(data_model$x, data_model$y)
# 2. Maak een dummy variabele voor de groepering (vaak 1 voor de hele dataset)
data_model$ID <- factor(1:nrow(data_model))

data_model$ID <- factor(data_model$meetplaats)


# 3. Update het model met exp() of mat() structuur
model_sac_corr <- glmmTMB(
  mmif ~ ec_20_log + overstorten_blootstelling_index_log + p_h_s +
    jaar_s + p_t_log + o2_s +      spei6_s +               # Behoud bekken als random intercept
    exp(pos + 0 | ID),           # Voeg spatiële correlatie toe op basis van X,Y
  data = data_model,
  family = gaussian,
  control = glmmTMBControl(optimizer = nlminb)
)
summary(model_sac_corr)

res2 <- simulateResiduals(fittedModel = model_sac_corr, plot = TRUE)


library(mgcv)

model_gam <- gam(
  mmif ~ ec_20_log + overstorten_blootstelling_index_log + p_h_s +
    jaar_s + p_t_log + o2_s + spei6_s +
    s(meetplaats, bs = "re") +
    s(owl, bs = "re"),
    # s(bekken, bs = "re"), # Random effect voor bekken
    # s(x, y, bs = "gp", k = 100),     # Ruimtelijke smoother (Gaussian Process)
  data = data_model %>%
    mutate(meetplaats = as.factor(meetplaats),
           owl = as.factor(owl),
           bekken = as.factor(bekken)),
  family = gaussian()
)
summary(model_gam)

gam.check(model_gam)

# 1. Simuleer residuen
res2 <- simulateResiduals(fittedModel = model_gam)

# 2. Maak een overzicht van UNIEKE locaties in de juiste volgorde
# We groeperen op meetplaats zodat de volgorde matcht met recalculateResiduals
locaties_match <- data_model %>%
  group_by(meetplaats) %>%
  summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
  arrange(meetplaats) # DHARMa sorteert groepen standaard op naam/factor level

# 3. Aggregeer residuen
res_grouped <- recalculateResiduals(res2, group = data_model$meetplaats)

# 4. De test
testSpatialAutocorrelation(res_grouped,
                           x = locaties_match$x,
                           y = locaties_match$y)



library(mgcv)
library(ggplot2)

# 1. Maak een voorspel-dataset (grid)
# We variëren ec_20_log en houden de rest constant op het gemiddelde/modus
new_data <- data.frame(
  ec_20_log = seq(min(data_model$ec_20_log, na.rm = TRUE),
                  max(data_model$ec_20_log, na.rm = TRUE),
                  length.out = 100)
)

# Voeg andere variabelen uit je model toe (gemiddelde waarden)
# Let op: vul hier alle andere fixed effects uit je model in!
new_data$p_t_log <- mean(data_model$p_t_log, na.rm = TRUE)
new_data$o2_s <- 0  # omdat het 'scaled' is, is 0 het gemiddelde
new_data$jaar_s <- 0
new_data$overstorten_blootstelling_index_log <- mean(data_model$overstorten_blootstelling_index_log, na.rm = TRUE)
new_data$p_h_s <- 0
new_data$spei6_s <- 0
# ... voeg hier eventueel andere variabelen toe ...

new_data <- data.frame(
  ec_20_log = seq(min(data_model$ec_20_log, na.rm = TRUE),
                  max(data_model$ec_20_log, na.rm = TRUE),
                  length.out = 100),

  # VOEG DEZE TOE: Dummy variabelen om mgcv tevreden te stellen
  meetplaats = data_model$meetplaats[1],
  bekken     = data_model$bekken[1],
  x          = data_model$x[1],
  y          = data_model$y[1],
  owl        = data_model$owl[1],

  # Zorg ook dat alle andere fixed effects uit je model hier staan:
  o2_s       = 0,
  jaar_s     = 0,
  p_t_log    = mean(data_model$p_t_log, na.rm = TRUE),
  spei6_s    = 0,
  p_h_s      = mean(data_model$p_h_s, na.rm = TRUE),
  overstorten_blootstelling_index_log = mean(data_model$overstorten_blootstelling_index_log, na.rm = TRUE)
)

# 1. Voorspelling voor het GAM model (had je al, zorg dat type="response" aan staat)
preds_gam <- predict(model_gam,
                     newdata = new_data,
                     exclude = c("s(meetplaats)", "s(bekken)", "s(x,y)", "s(owl)"),
                     type = "response",
                     se.fit = TRUE)

new_data$fit_gam <- preds_gam$fit

# 2. Voorspelling voor het glmmTMB (ordbeta) model
# re.form = NA zorgt ervoor dat we naar de gemiddelde trend kijken zonder random effects
preds_ordbeta <- predict(model,
                         newdata = new_data,
                         re.form = NA,
                         type = "response",
                         se.fit = TRUE)

new_data$fit_ordbeta <- preds_ordbeta$fit

# 3. Omzetten naar 'long' formaat voor ggplot
library(tidyr)
plot_data <- new_data %>%
  select(o2_s, fit_gam, fit_ordbeta) %>%
  pivot_longer(cols = starts_with("fit"),
               names_to = "Model",
               values_to = "MMIF")

# 4. De Vergelijkingsplot
ggplot(plot_data, aes(x = o2_s, y = MMIF, color = Model)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("fit_gam" = "steelblue", "fit_ordbeta" = "firebrick"),
                     labels = c("GAM (Spatieel/Smooth)", "GLMM (Ordbeta/Lineair)")) +
  labs(title = "Vergelijking: GAM vs. Ordbeta (GLMM)",
       subtitle = "Marginale effecten van EC op de MMIF-schaal",
       x = "Geleidbaarheid (log EC)",
       y = "Voorspelde MMIF (0-1)") +
  theme_minimal()

#####alle variableen

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Definieer de variabelen die je wilt vergelijken
fixed_vars <- c("ec_20_log", "o2_s", "spei6_s", "jaar_s",
                "p_t_log", "p_h_s", "overstorten_blootstelling_index_log")

# 2. Maak een lege lijst om de resultaten in op te slaan
all_comparisons <- list()

for (v in fixed_vars) {
  # Maak een grid voor de huidige variabele 'v'
  # We maken 100 stappen tussen min en max
  grid <- data.frame(
    var_name = v,
    val = seq(min(data_model[[v]], na.rm = TRUE),
              max(data_model[[v]], na.rm = TRUE),
              length.out = 100)
  )

  # Hernoem 'val' naar de echte naam van de variabele voor de predictie
  names(grid)[2] <- v

  # Voeg alle andere benodigde variabelen toe (op gemiddelde/dummy)
  other_vars <- setdiff(fixed_vars, v)
  for (ov in other_vars) {
    grid[[ov]] <- ifelse(grepl("_s$", ov), 0, mean(data_model[[ov]], na.rm = TRUE))
  }

  # Dummy kolommen voor mgcv/glmmTMB
  grid$meetplaats <- data_model$meetplaats[1]
  grid$bekken     <- data_model$bekken[1]
  grid$x          <- data_model$x[1]
  grid$y          <- data_model$y[1]
  grid$ID         <- factor(1)

  # Voorspel GAM
  grid$fit_gam <- predict(model_gam, newdata = grid,
                          exclude = c("s(meetplaats)", "s(bekken)", "s(x,y)"),
                          type = "response")

  # Voorspel Ordbeta
  grid$fit_ordbeta <- predict(model, newdata = grid,
                              re.form = NA, type = "response")

  # Bewaar alleen de gestandaardiseerde kolom voor de plot
  all_comparisons[[v]] <- grid %>%
    select(val = !!sym(v), fit_gam, fit_ordbeta) %>%
    mutate(variable = v)
}

# 3. Voeg alles samen en plot
plot_df <- bind_rows(all_comparisons) %>%
  pivot_longer(cols = c(fit_gam, fit_ordbeta), names_to = "Model", values_to = "MMIF")

ggplot(plot_df, aes(x = val, y = MMIF, color = Model)) +
  geom_line(size = 1) +
  facet_wrap(~variable, scales = "free_x") +
  scale_color_manual(values = c("fit_gam" = "steelblue", "fit_ordbeta" = "firebrick"),
                     labels = c("GAM (Smooth)", "Ordbeta (Lineair)")) +
  theme_minimal() +
  labs(title = "Vergelijking GAM vs. Ordbeta voor alle predictoren",
       x = "Waarde predictor", y = "Voorspelde MMIF")
