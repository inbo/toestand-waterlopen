
options(na.action = "na.fail") # Verplicht voor dredge

x_vars_schoon <- setdiff(mmif_variabelen, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)


m_mmif <- glmmTMB(data = data_subset2, formula = formula_obj,
                 REML = FALSE,
                 family = ordbeta)

model_family <- ordbeta
# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("data_subset2", "m_mmif"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = m_mmif,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 6),
  fixed = "cond(jaar_s)",
  trace = 2
)

# STAP 5: Netjes opruimen
stopCluster(mijn_cluster)
options(na.action = "na.omit")

# --- STOP TIMER ---
eind_tijd <- Sys.time()
cat("\n--- KLAAR! ---\n")
cat("Totale rekentijd: ")
print(eind_tijd - start_tijd)
cat("----------------\n\n")

print(head(dredge_model, 10))

# 1. Haal het absolute topmodel (nummer 1) uit je dredge object
# subset = 1 pakt de bovenste rij (laagste AICc)
best_model_ML <- get.models(dredge_model, subset = 1)[[1]]

# 2. Update het model naar REML = TRUE
# De update() functie neemt je ML-model, behoudt de winnende formule,
# maar herberekent de wiskunde onder de motorkap via REML.
best_model_REML <- update(best_model_ML, REML = TRUE)

# 3. Bekijk je definitieve, publiceerbare sub-model
summary(best_model_REML)

# 4. Check (optioneel) of je residuals nu mooi verdeeld zijn
# DHARMa::simulateResiduals(best_model_REML, plot = TRUE)
