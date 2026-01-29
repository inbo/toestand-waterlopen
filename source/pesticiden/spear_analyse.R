load(file = here("data", "verwerkt", "spear_data.rdata"))
load(file = "data/temp/fc_lu_data.rdata")
load(file = "data/verwerkt/mi_nat_sv.rdata")
load(file = "data/verwerkt/fc_data.rdata")
load(here("data", "verwerkt", "tu_resultaten.rdata"))

data0 <- mi_nat_sv %>%
  left_join(spear_data,
            by = c("meetplaats", "monsternamedatum")) %>%
  select(mmif, ep_tw, ns_tw, mt_sw, sw_dw, ta_xw, aantal_pesticiden_met_overschrijding, meetplaats, bekken, groep, jaar, landbouw_intens_afstr, akker, intensiteit_combo, hooggroen_afstr, spear_pesticides, tu_estimated, ekc2_waterlichaam, kjn, p_t, monsternamedatum) %>%
  tidyr::drop_na() %>%
  filter(groep == "beek") %>%
  left_join(mi_met_pesticide %>%
              drop_na(qual_meetplaats) %>%
              st_drop_geometry() %>%
              select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
              mutate(qual_monsternamedatum = as.Date(qual_monsternamedatum, origin = "1970-01-01")),
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(tu_per_sample,
            by = c("qual_meetplaats" = "meetplaats", "qual_monsternamedatum" = "monsternamedatum"))

overschrijdingen_soorten_stoffen <- openxlsx2::read_xlsx("data/ruw/fys_chem/tabel_overschrijdingen.xlsx")

pesticiden <- overschrijdingen_soorten_stoffen %>%
  filter(categorie == "Pesticiden (Bestrijdingsmiddelen)") %>%
  pull(stofnaam)

pesticiden_metingen <- fc_data %>%
  filter(parameter_omschrijving %in% pesticiden) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(pesticiden_totaal = sum(resultaat_detectielimiet))




pesticiden_koppeling <- data0 %>%
  select(meetplaats, monsternamedatum) %>%
  left_join(.,
            pesticiden_metingen,
            by = c("meetplaats"), suffix = c("", "_pest")) %>%
  filter(

    {
      days_before <- as.numeric(difftime(monsternamedatum, monsternamedatum_pest, units = "days"))
      days_before < 365 &
        days_before > -14

    }
  ) %>%
  group_by(meetplaats, monsternamedatum) %>% #dubbele samples uitmiddelen
  summarise(
    across(
      where(is.numeric), \(x) sum(x, na.rm = TRUE) # enkel numerische kolommen om de mean te pakken
    ), # voor niet numerische waarden gewoon de eerste string nemen om te behouden
    across(
      where(is.factor) | where(is.character),
      ~ first(.)
    ),
    .groups = "drop" # Drop the grouping at the end
  )

data <- data0 %>%
  left_join(pesticiden_koppeling,
            by = c("meetplaats"),
            suffix = c("", "_pest")) %>%
  drop_na()

model_mmif <- glmmTMB(data = data0,
                      mmif ~ scale(spear_pesticides) + scale(intensiteit_combo) + scale(TU_sum) + scale(jaar) + (1 | bekken/meetplaats),
                      family = ordbeta)
summary(model_mmif)
sjPlot::plot_model(model_mmif, "pred")
simulationOutput <- simulateResiduals(model_mmif, plot = TRUE)

model_mmif <- glmmTMB(data = data,
                      mmif ~ scale(spear_pesticides) + scale(akker) + scale(hooggroen_afstr) + scale(log(resultaat_detectielimiet)) +
                        scale(jaar) + (1 | bekken/meetplaats),
                      family = ordbeta)
summary(model_mmif)
sjPlot::plot_model(model_mmif, "pred")
simulationOutput <- simulateResiduals(model_mmif, plot = TRUE)

model_spear <- glmmTMB(data = data0,
                      spear_pesticides ~ scale(TU_sum) + scale(jaar) + (1 | meetplaats),
                      family = ordbeta)
summary(model_spear)

model_mts <- glmmTMB(data = data,
                      mt_sw/10 ~ scale(log(pesticiden_totaal)) + scale(spear_pesticides) + scale(aantal_pesticiden_met_overschrijding) + scale(hooggroen_afstr) + scale(jaar) + (1 | bekken/meetplaats),
                      family = ordbeta)
summary(model_mts)
sjPlot::plot_model(model_mts, "pred")
simulationOutput <- simulateResiduals(model_mts, plot = TRUE)

psem <- psem(model_spear, model_mmif)
summary(psem)
plot(psem)
