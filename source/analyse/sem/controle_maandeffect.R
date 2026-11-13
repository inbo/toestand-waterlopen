library(piecewiseSEM)
library(glmmTMB)

cat("\n▶ STAP 1: De 'Oude' modellen updaten met de (1 | maand) term...\n")

# We gebruiken de handige update() functie om de random effect term toe te voegen
# aan jouw reeds gefitte, schone modellen.
ept_maand <- update(ept_clean, . ~ . + (1 | maand))
ntot_maand <- update(ntot_clean, . ~ . + (1 | maand))
ptot_maand <- update(ptot_clean, . ~ . + (1 | maand))
czv_maand  <- update(czv_clean,  . ~ . + (1 | maand))
o2_maand   <- update(o2_clean,   . ~ . + (1 | maand))
ec20_maand <- update(ec20_clean, . ~ . + (1 | maand))
zs_maand   <- update(zs_clean,   . ~ . + (1 | maand))

cat("\n▶ STAP 2: De twee PSEMs opbouwen...\n")

# 1. De OUDE PSEM (zoals je hem al had)
psem_oud <- psem(
  ept_clean, ntot_clean, ptot_clean, czv_clean, o2_clean, ec20_clean, zs_clean,
  n_t_log %~~% t_s,
  p_t_log %~~% t_s,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  n_t_log %~~% p_t_log
)

# 2. De NIEUWE PSEM (met seizoenscorrectie)
psem_nieuw <- psem(
  ept_maand, ntot_maand, ptot_maand, czv_maand, o2_maand, ec20_maand, zs_maand,
  n_t_log %~~% t_s,
  p_t_log %~~% t_s,
  n_t_log %~~% czv_log,
  p_t_log %~~% czv_log,
  n_t_log %~~% p_t_log
)

cat("\n▶ STAP 3: Vergelijk de Fisher's C en model fits...\n")
summary(psem_oud)
summary(psem_nieuw)

# Optioneel: Haal de coëfficiënten direct naast elkaar om te vergelijken
coefs_oud <- coefs(psem_oud)
coefs_nieuw <- coefs(psem_nieuw)

coefs_filled <- standardize_psem(psem_nieuw)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_interactive.R")) #zonder cluster gecorreleerde
library(dplyr)

cat("\n▶ STAP 4: Verschillen in Gestandaardiseerde Estimates berekenen (incl. ordbeta)...\n")

# 1. Haal de coëfficiënten op met JOUW fix-functie!
c_oud_volledig <- standardize_psem(psem_oud)
c_nieuw_volledig <- standardize_psem(psem_nieuw)

# 2. Selecteer enkel de kolommen die we nodig hebben voor de vergelijking
c_oud <- c_oud_volledig %>%
  select(Response, Predictor, Std.Estimate_oud = Std.Estimate, p_oud = P.Value)

c_nieuw <- c_nieuw_volledig %>%
  select(Response, Predictor, Std.Estimate_nieuw = Std.Estimate, p_nieuw = P.Value)

# 3. Plak ze aan elkaar en bereken de absolute en relatieve verschillen
vergelijking <- full_join(c_oud, c_nieuw, by = c("Response", "Predictor")) %>%
  mutate(
    # Zorg dat alles numeriek is (voor de zekerheid)
    Std.Estimate_oud = as.numeric(Std.Estimate_oud),
    Std.Estimate_nieuw = as.numeric(Std.Estimate_nieuw),

    # Afronden voor de leesbaarheid
    Std.Estimate_oud = round(Std.Estimate_oud, 3),
    Std.Estimate_nieuw = round(Std.Estimate_nieuw, 3),
    p_oud = round(p_oud, 3),
    p_nieuw = round(p_nieuw, 3),

    # Wat is het absolute verschil?
    Verschil = round(Std.Estimate_nieuw - Std.Estimate_oud, 3),
    Abs_Verschil = abs(Verschil)
  ) %>%
  # Sorteer zodat de grootste schommelingen bovenaan staan
  arrange(desc(Abs_Verschil)) %>%
  # Houd het overzichtelijk
  select(Response, Predictor, Std.Estimate_oud, Std.Estimate_nieuw, Verschil, p_oud, p_nieuw)

# 4. Toon de overzichtstabel
cat("\n--- Top verschuivingen na seizoenscorrectie (1|maand) ---\n")
print(vergelijking, n = 30)
