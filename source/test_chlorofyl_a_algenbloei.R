fc_selectie %>%
  ggplot(aes(log(zs), clfyl_a, col = log(czv))) +
  geom_point() + geom_smooth()
testie <- glmmTMB(data=fc_selectie,
                  o2 ~ scale(clfyl_a),
                  na.action = na.omit)
summary(testie)
fc_selectie %>%
  ggplot(aes(log(zs), clfyl_a, col = log(czv))) +
  geom_point() + geom_smooth()

fc_selectie %>% filter(!is.na(clfyl_a)) %>% drop_na(o2) %>% filter(o2 < 100000) %>%
  ggplot(aes(log(clfyl_a), o2)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = log(75), color = "red")

fc_selectie %>% filter(!is.na(clfyl_a)) %>% drop_na(zs) %>% filter(o2 < 100000) %>%
  ggplot(aes(log(clfyl_a), log(zs))) +
  geom_point() +
  geom_smooth()


fc_selectie %>% filter(!is.na(clfyl_a)) %>% drop_na(t) %>% filter(o2 < 100000) %>%
  ggplot(aes(t, log(clfyl_a))) +
  geom_point() +
  geom_smooth()

fc_selectie %>% filter(!is.na(clfyl_a)) %>% drop_na(o2) %>% filter(o2 < 100000) %>%
  ggplot(aes(t, o2)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = log(75), color = "red")
