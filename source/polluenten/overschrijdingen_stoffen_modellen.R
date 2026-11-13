load(file = here("data", "verwerkt", "overschrijdingen.rdata"))


test <- data5 %>%
  left_join(df_finale_dataset,
            by = c("meetplaats" = "Sample Point Naam", "jaar" = "Jaar"))
test2 <- test %>%
  select(Pesticiden_Aantal_Overschreden,VOS_Aantal_Overschreden, PAK_Aantal_Overschreden, ZwareMetalen_Aantal_Overschreden, PFAS_Aantal_Overschreden, Pesticiden_Proportie_Overschreden,
         Pesticiden_Druk_Max, Pesticiden_Aantal_Getoetst, ZwareMetalen_Proportie_Overschreden, ZwareMetalen_Aantal_Getoetst,
         bekken, jaar, spear_pesticides, PAK_Proportie_Overschreden, PAK_Aantal_Getoetst,
         intensiteit_combo_afstr, intensiteit_combo_oeverzone, mmif, sw_dw, ta_xw, ep_tw, n_t, p_t, o2, meetplaats, owl, monsternamedatum, groep,
         ekc2_waterlichaam) %>%
  mutate(pest_over = log(Pesticiden_Aantal_Overschreden + 0.01),
         pest_druk = log(Pesticiden_Druk_Max + 0.01),
         metaal_over = log(ZwareMetalen_Aantal_Overschreden + 0.01),
         pak_over = log(PAK_Aantal_Overschreden + 0.01),
         ept_prop = as.integer(ep_tw)/as.integer(ta_xw)) %>%
  na.omit()

model <- glmmTMB(data = test2,
                 mmif ~ log(n_t) + log(p_t) + scale(o2) + pest_over + scale(intensiteit_combo_afstr) + scale(jaar) + (1 | owl) + (1|meetplaats) + (1|bekken),
                 family = ordbeta)
summary(model)
plot_model(model, type = "pred")

library(dplyr)
library(glmmTMB) # of het pakket dat je gebruikt voor ordbeta

# 1. Voer de transformaties vooraf uit en sla ze op als nieuwe kolommen
test3 <- test2 %>%
  mutate(
    log_n_t = log(n_t),
    log_p_t = log(p_t),
    # [,1] trekt de output van scale() uit matrixvorm om problemen te voorkomen
    o2_scaled = as.numeric(scale(o2)),
    intens_scaled = as.numeric(scale(intensiteit_combo_afstr)),
    intens_oever_scaled = as.numeric(scale(intensiteit_combo_oeverzone)),
    jaar_scaled = as.numeric(scale(jaar))
  )

# 2. Fit het model opnieuw, nu met uitsluitend de schone variabelenamen
model_nieuw <- glmmTMB(
  mmif ~ log_n_t + log_p_t + o2_scaled + Pesticiden_Proportie_Overschreden + intens_scaled + jaar_scaled + intens_oever_scaled + PAK_Proportie_Overschreden + ZwareMetalen_Proportie_Overschreden + (1|groep) +
    (1 | owl) + (1 | meetplaats) + (1|bekken),
  data = test3,
  family = ordbeta
)
summary(model_nieuw)

model_nieuw <- glmmTMB(
  ept_prop ~ log_n_t + log_p_t + o2_scaled + Pesticiden_Proportie_Overschreden + intens_scaled + jaar_scaled + intens_oever_scaled + PAK_Proportie_Overschreden + ZwareMetalen_Proportie_Overschreden +
    (1 | owl) + (1 | meetplaats) + (1|bekken),
  data = test3,
  family = binomial,
  weights = ta_xw
)
summary(model_nieuw)

pest <- glmmTMB(
  Pesticiden_Proportie_Overschreden ~ log_n_t + log_p_t + o2_scaled  + intens_scaled + intens_oever_scaled + jaar_scaled +
    (1 | owl) + (1 | meetplaats),
  data = test3,
  family = binomial(link = "logit"),
  weights = Pesticiden_Aantal_Getoetst,
  ziformula = ~ intens_oever_scaled
)
summary(pest)
# 3. Maak de plot. Dit zal nu zonder foutmeldingen draaien.
plot_model(model_nieuw, type = "pred")
