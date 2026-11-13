mi_nat_sv_beek %>% select(monsternamedatum,opo4, opo4_f, p_t) %>%
  ggplot(aes(p_t, opo4)) +
  geom_point() +
  geom_abline(a=1, b = 0)

mi_nat_sv_beek %>% select(monsternamedatum,opo4, opo4_f, p_t) %>%
  ggplot(aes(p_t, opo4_f)) +
  geom_point() +
  geom_abline(a=1, b = 0)

cor.test(data5$p_t, data5$opo4_f)
cor.test(data5$ec_20, data5$cl)

data5 %>% ggplot(aes(log(cl), log(ec_20))) + geom_point() + geom_smooth()
ec20 <- glmmTMB(data = data5, log(ec_20) ~ log(cl) + log(n_t))
simulateResiduals(ec20, plot = TRUE)
r.squaredGLMM(ec20)
