library(glmmTMB)
library(piecewiseSEM)

# ==============================================================================
# STAP 1: Extraheer de pure, definitieve formules uit je handmatige updates
# ==============================================================================
# De functie formula() zorgt ervoor dat we alleen de schone structuur overhouden.
# De weggelaten random effects (- (1 | meetplaats)) zijn hier al netjes in verwerkt.

f_mmif  <- formula(mmif_best_model_updated)
f_ntot  <- formula(ntot_best_model_updated)
f_ptot  <- formula(ptot_best_model_updated)
f_czv   <- formula(czv_best_model_updated)
f_o2    <- formula(o2_best_model_updated)
f_ec20  <- formula(ec20_best_model_updated)
f_t     <- formula(t_best_model_updated)


# ==============================================================================
# STAP 2: Run alle deelmodellen volledig fris vanaf nul (Clean Run)
# ==============================================================================
# LET OP: Controleer of de 'family' per model klopt met je originele keuzes.
# Ik neem hier aan dat de overige modellen de standaard 'gaussian' (normale) verdeling gebruiken.

# 2a. Het hoofdmodel voor MMIF (inclusief de DHARMa-fix voor de dispersie!)
mmif_clean <- glmmTMB(
  formula     = f_mmif,
  data        = dredge_data,
  family      = ordbeta()
)

# 2b. De overige fysiochemische deelmodellen volledig fris fitten
ntot_clean  <- glmmTMB(f_ntot,  data = dredge_data, family = gaussian())
ptot_clean  <- glmmTMB(f_ptot,  data = dredge_data, family = gaussian())
czv_clean   <- glmmTMB(f_czv,   data = dredge_data, family = gaussian())
o2_clean    <- glmmTMB(f_o2,    data = dredge_data, family = gaussian()) # Nu zonder singularity-ruis
ec20_clean  <- glmmTMB(f_ec20,  data = dredge_data, family = gaussian())
t_clean     <- glmmTMB(f_t,     data = dredge_data, family = gaussian()) # Nu zonder singularity-ruis


# ==============================================================================
# STAP 3: Bouw je definitieve pSEM met de schone deelmodellen
# ==============================================================================
mmif_sem_nat_sv_beek_clean <- psem(
  mmif_clean,
  ntot_clean,
  ptot_clean,
  czv_clean,
  o2_clean,
  ec20_clean,
  t_clean,

  # Je gecorreleerde fouten (correlated errors)
  n_t_log %~~% p_t_log,
  p_t_log %~~% ec_20_log
  # p_t_log %~~% czv_log, # Eventueel activeren op basis van je nieuwe d-sep resultaten
  # n_t_log %~~% czv_log  # Eventueel activeren op basis van je nieuwe d-sep resultaten
)

# Bekijk het uiteindelijke, stabiele resultaat
summary(mmif_sem_nat_sv_beek_clean)
