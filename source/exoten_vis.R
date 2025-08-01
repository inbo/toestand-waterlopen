library(here)
library(tidyverse)
library(sjPlot)
library(lme4)
library(lmerTest)

load(here("data", "verwerkt", "exoten_aantalsoorten.rdata"))
exoten_data_soortenaantal %>% arrange(Jaar)

ggplot(exoten_data_soortenaantal, aes(Jaar, aantal_soorten)) +
  geom_point() +
  geom_smooth(method = "lm")
glm <- glmer(formula = aantal_soorten ~ Jaar, data = exoten_data_soortenaantal,
           family = "poisson")
summary(glm)
plot_model(glm, type = "pred", terms = "Jaar", show.data = T)
