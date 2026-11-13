# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

load(file = here("source", "analyse", "sem", "mi_nat_sv_rivier", "dredge_data_rivier.rdata"))

plot_groep_correlogram(dredge_data_rivier, c("mmif", "ept_prop", "sw_dw", "nst_prop", "ta_xw", "stress_prop", "spear_pesticides_s"), "correlaties spear met mmif en deelmaatlatten")

dredge_data_rivier %>%
  pivot_longer(., cols = c("mmif", "ept_prop", "sw_dw", "nst_prop", "ta_xw", "stress_prop"), values_to = "respons_waarden",
               names_to = "responsen") %>%
  select(spear_pesticides, responsen, respons_waarden) %>%
  ggplot(aes(spear_pesticides, respons_waarden)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~responsen, scales = "free_y")

model <- glmmTMB(spear_pesticides ~ spei6_s + n_extreme_3m_s + p_sum_7d_s + t_s + o2_s + ec_20_log + n_t_log + p_t_log +
                   zs_log + verharding_oever_s + intensiteit_combo_oeverzone_s + ekc2_waterlichaam_s + czv_log +
                   overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats) +
                   (1 | bekken) + (1 | owl),
                 data = dredge_data_rivier,
                 family = ordbeta
)
summary(model)


options(na.action = "na.fail") # Verplicht voor dredge
model <- glmmTMB(spear_pesticides ~ spei6_s + n_extreme_3m_s + p_sum_7d_s + t_s + o2_s + ec_20_log + n_t_log + p_t_log +
                   zs_log + verharding_oever_s + intensiteit_combo_oeverzone_s + ekc2_waterlichaam_s + czv_log +
                   overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats) +
                   (1 | bekken) + (1 | owl),
                 data = dredge_data_rivier,
                 family = ordbeta,
                 REML = FALSE
)

# --- START TIMER ---
start_tijd <- Sys.time()
cat("Timer gestart om:", format(start_tijd, "%H:%M:%S"), "\n")

aantal_cores <- detectCores() - 1
cat("We gaan parallel rekenen op", aantal_cores, "cores...\n")

# STAP 2 & 3: Cluster opzetten met een NIEUWE NAAM
mijn_cluster <- makeCluster(aantal_cores)

# Exporteer naar de nieuwe clusternaam
clusterExport(mijn_cluster, varlist = c("dredge_data_rivier", "model"))

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

dredge_model %>% model.avg()

###backward selection

# Installeer indien nodig: install.packages("buildmer")
library(buildmer)

# Laat buildmer automatisch het model afbreken tot de beste variabelen overblijven
model_backward <- buildglmmTMB(
  formula = spear_pesticides ~ spei6_s + t_s + o2_s + n_t_log + p_t_log + intensiteit_combo_afstr_s +
    zs_log + verharding_oever_s + intensiteit_combo_oeverzone_s + ekc2_waterlichaam_s + czv_log +
    overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats) +
    (1 | bekken) + (1 | owl),
  data = dredge_data_rivier,
  family = ordbeta,
  # Vertel buildmer dat hij backward selection moet gebruiken
  buildmerControl = buildmerControl(direction = 'backward')
)

# Bekijk welk model buildmer uiteindelijk heeft gekozen
summary(model_backward)

model_spear <- glmmTMB(data = dredge_data_rivier, spear_pesticides ~ 1 + o2_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats) + (1 | bekken) + (1 |      owl),
                       family = ordbeta)
plot_model(model_spear, type = "pred")

model_o2 <- glmmTMB(data = dredge_data_rivier, o2_s ~ 1 + ekc2_waterlichaam_s + jaar_s + (1 | owl),
                    family = gaussian)
summary(model_o2)
plot_model(model_o2, type = "pred")

model_ntot <- glmmTMB(data = dredge_data_rivier, n_t_log ~ spei6_s + intensiteit_combo_oeverzone_s +
                        zs_log + jaar_s + (1 | meetplaats) + (1 | owl) + (1|bekken),
                      family = gaussian)
summary(model_ntot)

model_czv <- glmmTMB(data = dredge_data_rivier, czv_log ~ spei6_s + intensiteit_combo_oeverzone_s +
                       zs_log + jaar_s  + (1 | owl) + (1|bekken),
                     family = gaussian)
summary(model_czv)

model_zs <- glmmTMB(data = dredge_data_rivier, zs_log ~ spei6_s + intensiteit_combo_oeverzone_s +
                      + jaar_s + (1 | owl) + (1|bekken),
                    family = gaussian)
summary(model_zs)

spear_sem <- psem(model_spear, model_o2)
summary(spear_sem)
dSep(spear_sem)
coefs_filled <- standardize_psem(spear_sem)[,-9]

source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde fouten
saveWidget(network, file = here("output", "figuren", "psem_inter_netw_mi_nat_sv_speartest_rivier.html"), selfcontained = TRUE)

model <- glmmTMB(spear_tu_estimated ~ o2_s + ec_20_log +
                   zs_log + verharding_oever_s + intensiteit_combo_oeverzone_s + ekc2_waterlichaam_s + intensiteit_combo_afstr_s +
                   overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats) +
                   (1 | bekken) + (1 | owl),
                 data = dredge_data_rivier,
                 family = gaussian
)
summary(model)

model <- glmmTMB(n_t_log ~ zs_log + verharding_oever_s + intensiteit_combo_oeverzone_s + ekc2_waterlichaam_s + spear_tu_estimated +
                   overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats) +
                   (1 | bekken) + (1 | owl),
                 data = dredge_data_rivier,
                 family = gaussian
)
summary(model)

model <- glmmTMB(p_t_log ~ zs_log + verharding_oever_s + intensiteit_combo_oeverzone_s + ekc2_waterlichaam_s + spear_tu_estimated +
                   overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats) +
                   (1 | bekken) + (1 | owl),
                 data = dredge_data_rivier,
                 family = gaussian
)
summary(model)
