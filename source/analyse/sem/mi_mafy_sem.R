load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_mafy_2km_2jaar.rdata"))
load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_vis_2km_2jaar.rdata"))

load(here("data", "verwerkt", "mi_data.rdata")) # macroinvertebraten

load(here("data", "ruw", "vis", "traits_all_obs_no_marien_EQR.Rdata"))
vis_data <- data_traits_EQR %>%
  filter(MethodeGroepOmschrijving == "Elektrisch") %>%
  select(Datum, WaarnemingVispuntID, VispuntX, VispuntY, VHAS, VHAG,
         perc_inv_fresh, perc_exoot, EQR) %>%
  st_as_sf(coords = c("VispuntX", "VispuntY"), crs = st_crs(mi_data)) %>%
  rename(meetplaats = WaarnemingVispuntID,
         monsternamedatum = Datum) %>%
  mutate(monsternamedatum = as.Date(monsternamedatum),
         meetplaats = as.factor(meetplaats)) %>%
  filter(!is.na(VHAS))

mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

koppeling_sleutel_mafy <-
  mi_met_mafy_2km_2jaar %>%
  select(meetplaats, monsternamedatum, mp_meetplaats, mp_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    mp_monsternamedatum = as.Date(mp_monsternamedatum)
  )

koppeling_sleutel_vis <-
  mi_met_vis_2km_2jaar %>%
  select(meetplaats, monsternamedatum, mp_meetplaats, mp_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    vis_monsternamedatum = as.Date(mp_monsternamedatum),
    vis_meetplaats = as.factor(mp_meetplaats)
  ) %>%
  select(-mp_meetplaats, -mp_monsternamedatum)

load(here("data", "verwerkt", "mafy_data.rdata"))

gekoppelde_data_mi_mafy <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel_mafy, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(mafy_data,
            by = c("mp_meetplaats" = "meetplaats",
                   "mp_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_mafy"))

gekoppelde_data_mi_mafy %>%
  filter(is.na(index_zonder_gep)) %>%
  group_by(categorie) %>%
  summarise(n())

 test <- gekoppelde_data_mi_mafy %>%
  filter(groep == "beek") %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(!is.na(index_zonder_gep))

 ggplot(test, aes(index_zonder_gep, mmif)) +
   geom_point() +
   geom_smooth()


################################################################################
#  ultieme test -> mafy toevoegen aan sem data mi_nat_sv_beek
################################################################################
mi_mafy_data <-  data_subset %>%
   st_drop_geometry() %>%
   filter(jaar > 2009) %>%
   # 1. Koppel de match-informatie aan je MI data
   # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
   left_join(koppeling_sleutel_mafy, by = c("meetplaats", "monsternamedatum")) %>%

   # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
   left_join(mafy_data,
             by = c("mp_meetplaats" = "meetplaats",
                    "mp_monsternamedatum" = "monsternamedatum"),
             suffix = c("", "_mafy")) %>%
  select(meetplaats, bekken, jaar_s, mmif, ept_prop, ta_xw, index_zonder_gep, o2_s, n_t_log, p_t_log, czv_log, intensiteit_combo_afstr_s, overstorten_blootstelling_index_log, ekc2_waterlichaam_s) %>%
  na.omit() %>%
  mutate(jaar_s_sq = jaar_s^2,
          o2_s_sq = o2_s^2)


mmif <- glmmTMB(
  mmif ~ index_zonder_gep + o2_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_data)
summary(mmif)

ept <- glmmTMB(
  ept_prop ~ index_zonder_gep + o2_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s  + (1 | bekken),
  family = binomial(link = "logit"),
  weights = mi_mafy_data$ta_xw,
  data = mi_mafy_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ intensiteit_combo_afstr_s + n_t_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(ptot)

czv <- glmmTMB(
  czv_log ~ n_t_log + p_t_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(czv)

o2 <- glmmTMB(
  o2_s ~ czv_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(o2)

mafy <- glmmTMB(
  index_zonder_gep ~ o2_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_data)
summary(mafy)


sem_resultaat <- psem(mmif, mafy, o2, ptot, ntot, czv)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source(here("source", "analyse", "sem", "sem_standardised_coef_flexible.R"))

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R"))

################################################################################
# VIS + MI
################################################################################

mi_vis_data <-  data_subset %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  left_join(koppeling_sleutel_vis, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(vis_data,
            by = c("vis_meetplaats" = "meetplaats",
                   "vis_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_vis")) %>%
  select(meetplaats, bekken, jaar_s, mmif, ept_prop, ta_xw, EQR, perc_inv_fresh, perc_exoot,
         o2_s, n_t_log, p_t_log, czv_log, intensiteit_combo_afstr_s, overstorten_blootstelling_index_log, ekc2_waterlichaam_s) %>%
  na.omit() %>%
  mutate(perc_exoot = perc_exoot/100,
         perc_inv_fresh = perc_inv_fresh/100,
    jaar_s_sq = jaar_s^2,
         o2_s_sq = o2_s^2,
         perc_exoot_log = log(perc_exoot + 1),
         perc_inv_fresh_log = log(perc_inv_fresh+1))


mmif <- glmmTMB(
  mmif ~ perc_exoot + o2_s + n_t_log + p_t_log  + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_vis_data)
summary(mmif)

ept <- glmmTMB(
  ept_prop ~ perc_exoot + o2_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s  + (1 | bekken),
  family = binomial(link = "logit"),
  weights = mi_vis_data$ta_xw,
  data = mi_vis_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_vis_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_vis_data)
summary(ptot)

czv <- glmmTMB(
  czv_log ~ n_t_log + p_t_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_vis_data)
summary(czv)

o2 <- glmmTMB(
  o2_s ~  overstorten_blootstelling_index_log + p_t_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_vis_data)
summary(o2)

vis <- glmmTMB(
  EQR ~ o2_s + n_t_log + p_t_log  + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_vis_data)
summary(vis)

perc_exoot <- glmmTMB(
  perc_exoot ~ o2_s + n_t_log + p_t_log  + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_vis_data)
summary(perc_exoot)

sem_resultaat <- psem(mmif, perc_exoot, o2, ptot, ntot, n_t_log %~~% p_t_log)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source(here("source", "analyse", "sem", "sem_standardised_coef_flexible.R"))

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R"))

################################################################################
# VIS + MI + MAFY
################################################################################

mi_mafy_vis_data <-  data_subset %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  left_join(koppeling_sleutel_vis, by = c("meetplaats", "monsternamedatum")) %>%
  left_join(vis_data,
            by = c("vis_meetplaats" = "meetplaats",
                   "vis_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_vis")) %>%
  left_join(koppeling_sleutel_mafy, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(mafy_data %>%
              select(meetplaats, monsternamedatum, index_zonder_gep, v_zonder_gep, ts_zonder_gep, gv_zonder_gep, vo_zonder_gep),
            by = c("mp_meetplaats" = "meetplaats",
                   "mp_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_mafy")) %>%
  select(meetplaats, bekken, jaar_s, mmif, ept_prop, ta_xw, jaar,
         EQR, perc_inv_fresh, perc_exoot,
         index_zonder_gep, v_zonder_gep, ts_zonder_gep, gv_zonder_gep, vo_zonder_gep,
         o2_s, n_t_log, p_t_log, czv_log, intensiteit_combo_afstr_s, overstorten_blootstelling_index_log, ekc2_waterlichaam_s) %>%
  na.omit() %>%
  mutate(jaar_s_sq = jaar_s^2,
         o2_s_sq = o2_s^2,
         perc_exoot = perc_exoot/100,
         perc_inv_fresh = perc_inv_fresh/100)
plot_groep_correlogram(mi_mafy_vis_data, c("index_zonder_gep", "v_zonder_gep", "ts_zonder_gep", "gv_zonder_gep", "vo_zonder_gep"))

mmif <- glmmTMB(
  mmif ~  perc_exoot + perc_inv_fresh + index_zonder_gep +ekc2_waterlichaam_s+ o2_s + n_t_log + p_t_log + overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(mmif)

mmif_vis <- glmmTMB(
  mmif ~  EQR + index_zonder_gep +ekc2_waterlichaam_s+ o2_s + n_t_log + p_t_log + overstorten_blootstelling_index_log + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(mmif)

ept <- glmmTMB(
  ept_prop ~ EQR + o2_s + n_t_log + p_t_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s  + (1 | bekken),
  family = binomial(link = "logit"),
  weights = mi_mafy_vis_data$ta_xw,
  data = mi_mafy_vis_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ intensiteit_combo_afstr_s + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(ptot)

czv <- glmmTMB(
  czv_log ~ n_t_log + p_t_log + overstorten_blootstelling_index_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(czv)

o2 <- glmmTMB(
  o2_s ~ index_zonder_gep + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_vis_data)
summary(o2)

vis_exoot <- glmmTMB(
  perc_exoot ~ o2_s + p_t_log + index_zonder_gep + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(vis_exoot)

vis_invoor <- glmmTMB(
  perc_inv_fresh ~ o2_s + p_t_log + index_zonder_gep + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(vis_invoor)

vis <- glmmTMB(
  EQR ~ o2_s + index_zonder_gep + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(vis_exoot)

mafy <- glmmTMB(
  index_zonder_gep ~ p_t_log + intensiteit_combo_afstr_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_vis_data)
summary(mafy)

sem_resultaat <- psem(mmif, vis_exoot, vis_invoor, mafy, o2, ptot, ntot,  p_t_log %~~% n_t_log)
                      # EQR %~~% perc_exoot,
                      # EQR %~~% perc_inv_fresh)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source(here("source", "analyse", "sem", "sem_standardised_coef_flexible.R"))

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R"))

# --- Stap 4: Plot met geforceerde bogen ---
set.seed(123)

plot <- ggraph(g, layout = "sugiyama") +

  # GEBRUIK ARC VOOR ALTIJD GEBOGEN LIJNEN
  geom_edge_arc(aes(color = effect_dir,
                    width = weight,
                    label = round(Std.Estimate, 2)),
                strength = 0.15,           # De mate van buiging (0.1 tot 0.2 is ideaal)
                angle_calc = 'along',
                label_dodge = unit(3, 'mm'),
                label_size = 3,
                arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                end_cap = circle(8, 'mm'),
                start_cap = circle(8, 'mm')) +

  geom_node_point(size = 10,
                  color = "white",
                  fill = "lightblue",
                  shape = 21,
                  stroke = 1.5) +

  # Gebruik geom_node_label om overlap met de nieuwe bochten te voorkomen
  geom_node_label(aes(label = label),
                  repel = TRUE,
                  size = 3,
                  fontface = "bold",
                  label.size = NA,
                  fill = alpha("white", 0.7)) +

  scale_edge_width(range = c(0.5, 3), guide = "none") +

  scale_edge_color_manual(
    values = c("Positief" = "darkgreen",
               "Negatief" = "firebrick"),
    name = "Effectrichting"
  ) +

  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "Piecewise SEM Resultaten",
       subtitle = "Sugiyama layout met geforceerde bochten (geom_edge_arc)")

print(plot)
ggsave(
  filename =  here("output", "figuren", "SEM_mi_nat_sv_beek_combo_mimafyvis.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)

sem_resultaat <- psem(mmif_vis, vis, mafy, o2, ptot, ntot,  p_t_log %~~% n_t_log)
# EQR %~~% perc_exoot,
# EQR %~~% perc_inv_fresh)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source(here("source", "analyse", "sem", "sem_standardised_coef_flexible.R"))

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R"))

# --- Stap 4: Plot met geforceerde bogen ---
set.seed(123)

plot <- ggraph(g, layout = "sugiyama") +

  # GEBRUIK ARC VOOR ALTIJD GEBOGEN LIJNEN
  geom_edge_arc(aes(color = effect_dir,
                    width = weight,
                    label = round(Std.Estimate, 2)),
                strength = 0.15,           # De mate van buiging (0.1 tot 0.2 is ideaal)
                angle_calc = 'along',
                label_dodge = unit(3, 'mm'),
                label_size = 3,
                arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                end_cap = circle(8, 'mm'),
                start_cap = circle(8, 'mm')) +

  geom_node_point(size = 10,
                  color = "white",
                  fill = "lightblue",
                  shape = 21,
                  stroke = 1.5) +

  # Gebruik geom_node_label om overlap met de nieuwe bochten te voorkomen
  geom_node_label(aes(label = label),
                  repel = TRUE,
                  size = 3,
                  fontface = "bold",
                  label.size = NA,
                  fill = alpha("white", 0.7)) +

  scale_edge_width(range = c(0.5, 3), guide = "none") +

  scale_edge_color_manual(
    values = c("Positief" = "darkgreen",
               "Negatief" = "firebrick"),
    name = "Effectrichting"
  ) +

  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "Piecewise SEM Resultaten",
       subtitle = "Sugiyama layout met geforceerde bochten (geom_edge_arc)")

print(plot)
ggsave(
  filename =  here("output", "figuren", "SEM_mi_nat_sv_beek_combo_mimafyvis2.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)
