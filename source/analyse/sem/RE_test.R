# 1. Het volledige model (al aanwezig, zorg voor REML = TRUE)
model_full <- glmmTMB(mmif ~ 1 + (1 | bekken) + (1 | meetplaats),
                      data = dredge_data,
                      family = ordbeta,
                      REML = TRUE)

# 2. Het gereduceerde model (zonder 'bekken')
model_no_bekken <- glmmTMB(mmif ~ 1 + (1 | meetplaats),
                           data = dredge_data,
                           family = ordbeta,
                           REML = TRUE)

# 3. Likelihood Ratio Test
anova(model_full, model_no_bekken)
