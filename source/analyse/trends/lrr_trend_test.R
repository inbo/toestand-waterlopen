# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# Laad de benodigde packages
library(dplyr)

# 1. Bepaal de initiële meting (eerste opname, van VOOR 2013)
df_init <- data5 %>%
  filter(jaar < 2013) %>%
  group_by(meetplaats) %>%
  # Selecteer de oudste monsternamedatum per meetplaats
  slice_min(monsternamedatum, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Selecteer enkel de variabelen die we willen behouden of analyseren
  select(meetplaats, owl, bekken, groep,
         jaar_init = jaar, datum_init = monsternamedatum,
         n_t, p_t, o2, ec_20, czv, intensiteit_combo_afstr, mmif, ep_tw, ta_xw, sw_dw)

# 2. Bepaal de laatste meting (meest recente opname, van NA 2020)
df_last <- data5 %>%
  filter(jaar > 2020) %>%
  group_by(meetplaats) %>%
  # Selecteer de meest recente monsternamedatum per meetplaats
  slice_max(monsternamedatum, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(meetplaats,
         jaar_last = jaar, datum_last = monsternamedatum,
         n_t, p_t, o2, ec_20, czv, intensiteit_combo_afstr, mmif, ep_tw, ta_xw, sw_dw)

# 3. Voeg de initiële en laatste data samen en bereken de LRR
# Een offset is nodig omdat ep_tw en mogelijke andere variabelen 0 kunnen zijn.
offset <- 0.01

df_model <- df_init %>%
  # inner_join zorgt ervoor dat enkel meetplaatsen met ZOWEL een meting voor 2013 als na 2020 overblijven
  inner_join(df_last, by = "meetplaats", suffix = c("_init", "_last")) %>%
  mutate(
    # Predictoren LRR
    lrr_n_t   = log((n_t_last + offset) / (n_t_init + offset)),
    lrr_p_t   = log((p_t_last + offset) / (p_t_init + offset)),
    lrr_o2    = log((o2_last + offset) / (o2_init + offset)),
    lrr_lb    = log((intensiteit_combo_afstr_last + offset) / (intensiteit_combo_afstr_init + offset)),
    lrr_ec_20    = log((ec_20_last + offset) / (ec_20_init + offset)),
    lrr_czv    = log((czv_last + offset) / (czv_init + offset)),
    # Responsen LRR
    lrr_mmif  = log((mmif_last + offset) / (mmif_init + offset)),
    lrr_ep_tw = log((ep_tw_last + offset) / (ep_tw_init + offset)),
    lrr_ta_xw = log((ta_xw_last + offset) / (ta_xw_init + offset)),
    lrr_sw_dw = log((sw_dw_last + offset) / (sw_dw_init + offset))
  )

test <- glmmTMB(data = df_model,
                lrr_ta_xw ~ lrr_n_t + lrr_p_t + lrr_ec_20 + lrr_o2 + lrr_czv + ta_xw_init + groep + (1 | owl))
summary(test)
plot_model(test, type = "pred", show.data = T)
