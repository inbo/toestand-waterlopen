

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("dredge_data", "model"))

clusterEvalQ(mijn_cluster, {
  library(glmmTMB)
  options(na.action = "na.fail")
})

cat("Dredge is bezig over meerdere cores...\n")


dredge_model <- dredge(
  global.model = model,
  cluster = mijn_cluster,     # <--- Hier geven we het door
  rank = "AICc",
  m.lim = c(1, 4),
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
