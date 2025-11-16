load(here("data", "verwerkt", "overstorten", "mi_meetpunten_vuilvracht_500m.rdata"))
load(file = here("data", "verwerkt", "overstorten", "mi_meetpunten_aantal_overstorten_afstroomgebied.rdata"))
load(here("data", "verwerkt", "mi_data.rdata"))
vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))
watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))
points_in_watersheds <- st_join(vuilvracht_overstorten, watersheds_buffered, left = FALSE)


#### correlatie van vuilvrachten over alle overstorten van de aangeleverde laag

numerieke_var <- vuilvracht_overstorten %>%
  select(NT, NKJ, PT, BZV, CZV, IE, NO2, NO3, NH4) %>%
  st_drop_geometry() %>%
  na.omit()
cor_matrix <- cor(numerieke_var)
corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, addCoef.col = "black")

#correlatie van vuilvrachtparameters voor alle overstorten binnen de afstroomgebieden van de meetpunten

numerieke_var <- points_in_watersheds %>%
  st_drop_geometry() %>%
  select(NT, NKJ, PT, BZV, CZV, IE, NO2, NO3, NH4) %>%
  na.omit()
cor_matrix <- cor(numerieke_var)
corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, addCoef.col = "black")

###correlatie van cumulatieve vuilvrachtparameters per meetpunten

numerieke_var <- cum_vuilvracht_watershed %>%
  select(-meetplaats, -fid)
cor_matrix <- cor(numerieke_var)
corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, addCoef.col = "black")


vuilvracht_data <- mi_nat_sv %>%
  filter(jaar %in% c(2022)) %>%
  left_join(cum_vuilvracht_watershed_500m, by = "meetplaats") %>%
  mutate(ta_xw = as.integer(ta_xw),
         ep_tw = as.integer(ep_tw),
         ns_tw = as.integer(ns_tw)) %>%
  select(starts_with("cum_"), ta_xw, mmif, mt_sw, ns_tw, ep_tw, bekken, meetplaats, o2) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  filter(cum_gewogen_BZV < 1000)


model_vuilvracht <- glmmTMB(mmif ~
                         scale(cum_gewogen_BZV)  + scale(o2) +
                         (1 | bekken),
                       data = vuilvracht_data,
                      control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_vuilvracht)

model_vuilvracht <- glmmTMB(ta_xw ~
                              (scale(cum_gewogen_BZV)) +
                              (1 | bekken) ,
                            data = vuilvracht_data,
                            family = poisson(link = "log"),
                            control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_vuilvracht)

model_vuilvracht <- glmmTMB(ep_tw /ta_xw ~
                              (scale(cum_gewogen_BZV)) +
                              (1 | bekken),
                            data = vuilvracht_data,
                            weights = ta_xw,
                            family = binomial,
                            control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_vuilvracht)
plot_model(model_vuilvracht, "pred")

vuilvracht_data %>% ggplot(aes(x = cum_gewogen_BZV, y = mt_sw)) + geom_point() + geom_smooth(method = "lm")

model_vuilvracht <- glmmTMB(ns_tw /ta_xw ~
                              (scale(cum_gewogen_BZV)) +
                              (1 | bekken),
                            data = vuilvracht_data,
                            weights = ta_xw,
                            family = binomial,
                            control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_vuilvracht)
plot_model(model_vuilvracht, "pred")

model_vuilvracht <- glmmTMB(mt_sw ~
                              (scale(cum_gewogen_BZV)) +
                              (1 | bekken),
                            data = vuilvracht_data,
                            control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_vuilvracht)

model_vuilvracht <- glmmTMB(o2 ~
                              (scale(cum_gewogen_BZV)) + (1|meetplaats) +
                              (1 | bekken),
                            data = vuilvracht_data,
                            control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_vuilvracht)


##############################################################
# Analyse gekoppelde meetpunten 100 m + afstroomgebied
#######################################################

load(file = here("data", "verwerkt", "overstorten", "overstorten_meetpunten_match_100m.rdata"))

mi_data_2022 <- mi_nat_sv %>%
  filter(jaar %in% c(2022))

overstorten_100m_data <- mi_data_2022 %>%
  inner_join(matched_df, by = "meetplaats") %>%
  left_join(., vuilvracht_overstorten, by = c("id_overstort" = "ID")) %>%
  select(meetplaats, jaar, groep, mmif_20, ep_tw, ta_xw, o2, ec_20, kjn, NT, NKJ, PT, BZV, CZV, IE, NO2, NO3, NH4) %>%
  na.omit()

model_overstorten_100m <- glmmTMB(cbind(mmif_20, 20 - mmif_20) ~
                                    BZV + groep ,
                          data = overstorten_100m_data %>%
                            filter(BZV > 0) %>%
                            mutate(BZV = log(BZV)),
                          family = binomial(link = "logit"),
                          control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_overstorten_100m)


model_overstorten_100m <- glmmTMB(ep_tw ~
                                    BZV + groep ,
                                  data = overstorten_100m_data %>%
                                    filter(BZV > 0) %>%
                                    mutate(BZV = log(BZV)),
                                  family = poisson(link = "log"),
                                  control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_overstorten_100m)

model_overstorten_100m <- glmmTMB(ta_xw ~
                                    BZV + groep ,
                                  data = overstorten_100m_data %>%
                                    filter(BZV > 0) %>%
                                    mutate(BZV = log(BZV)),
                                  family = poisson(link = "log"),
                                  control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_overstorten_100m)
plot_model(model_overstorten_100m, type = "pred", show.data = T)

model_overstorten_100m <- glmmTMB(o2 ~
                                    BZV + groep ,
                                  data = overstorten_100m_data %>%
                                    filter(BZV > 0) %>%
                                    mutate(BZV = log(BZV)),
                                  control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_overstorten_100m)
plot_model(model_overstorten_100m, type = "pred", show.data = T)

model_overstorten_100m <- glmmTMB(kjn ~
                                    BZV + groep ,
                                  data = overstorten_100m_data %>%
                                    filter(BZV > 0) %>%
                                    mutate(BZV = log(BZV)),
                                  control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)))
summary(model_overstorten_100m)
plot_model(model_overstorten_100m, type = "pred", )

###NADENKEN OVER WELKE AFSTAND EN WELKE JAREN EN HOE DAT IN MODELLETEJE steken
