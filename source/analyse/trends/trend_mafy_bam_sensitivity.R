library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(parallel)

source("source/analyse/mafy_datasets_prep.R")

# --- 1. Data Voorbereiding ---
mafy_subsets_plot <- mafy_data3 %>%
  drop_na(index_nieuw) %>%
  mutate(
    subset = case_when(
      status == "NAT" & groep == "beek"   ~ "nat_sv_beek",
      status == "NAT" & groep == "kempen" ~ "nat_sv_kempen",
      status == "NAT" & groep == "polder" ~ "nat_sv_polder",
      status == "NAT" & groep == "rivier" ~ "nat_sv_rivier",
      status == "KUNST"                   ~ "kunstmatig", # aanpssen zodat enkel waterlopen zijn en geen zout!
      waterlichaamcategorietype == "RtNt"                                                      ~ "rtnt",
      TRUE                                                                ~ "overig"
    )
  ) %>%
  filter(jaar > 2006)
%>%
  # --- NIEUWE STAP: Filter meetplaatsen met >= 5 metingen ---
  group_by(meetplaats) %>%
  filter(n_distinct(monsternamedatum) >= 5) %>%
  ungroup()

# Nesten voor mgcv
mafy_nested <- mafy_subsets_plot %>%
  filter(subset != "overig") %>%
  pivot_longer(cols = c(index_nieuw, v_zonder_gep, ts_zonder_gep, vo_zonder_gep, gv_zonder_gep),
               names_to = "index",
               values_to = "waarde") %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         index = factor(index, levels = c("index_nieuw", "v_zonder_gep", "ts_zonder_gep", "vo_zonder_gep", "gv_zonder_gep")),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken),
         owl = as.factor(owl)) %>%
  group_nest(subset, index)

# --- 1.5. Parallel cluster opzetten ---
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

# --- 2. Modellen fitten met MGCV (BAM) ---
mafy_models <- mafy_nested %>%
  # Filter eventueel uit als je test, maar dit werkt nu ook voor alles!
  # filter(index %in% c("index_nieuw", "gv_zonder_gep")) %>%
  mutate(
    model = map(data, ~ {
      # 1. Haal NA's eruit voor deze specifieke subset
      df <- drop_na(.x, waarde)

      # 2. Als er niet genoeg metingen zijn (bijv. minder dan 10), breek af en return NULL
      if (nrow(df) < 10) return(NULL)

      # 3. Probeer het model te fitten. Vang errors op met tryCatch.
      tryCatch({
        bam(waarde ~ s(jaar_num, k = 4) +
              s(meetplaats, bs = "re") +
              s(bekken, bs = "re") +
              s(owl, bs = "re"),
            family = quasibinomial,
            data = df,
            method = "fREML",
            discrete = TRUE,
            threads = n_cores)
      }, error = function(e) NULL) # Als het faalt, geef NULL terug i.p.v. een crash
    }),

    # Extraheer de p-waarde. Controleer eerst of het model NULL is.
    p_val_jaar = map_dbl(model, ~ if (is.null(.x)) NA_real_ else summary(.x)$s.table["s(jaar_num)", "p-value"]),

    # Bepaal of het significant is. NA p-waarden worden als "Nee" geclassificeerd.
    is_significant = factor(ifelse(is.na(p_val_jaar) | p_val_jaar >= 0.05, "Nee", "Ja"), levels = c("Ja", "Nee")),

    # Voorspellingen doen
    predictions = map(model, ~ {
      if (is.null(.x)) {
        # Maak een dummy data.frame met NA's zodat de rij behouden blijft na unnest!
        data.frame(x = NA_real_, predicted = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
      } else {
        as.data.frame(ggpredict(.x,
                                terms = "jaar_num [all]",
                                type = "fixed",
                                exclude = c("s(meetplaats)", "s(bekken)", "s(owl)")))
      }
    })
  )

stopCluster(cl)

# --- 3. Uitpakken en Visualiseren ---
mafy_trends <- mafy_models %>%
  select(subset, index, is_significant, predictions) %>%
  unnest(predictions)

ggplot(mafy_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  # Voeg drop = FALSE toe, zodat lege factor combinaties als lege kaders worden geplot
  facet_grid(index ~ subset, scales = "free_y", drop = FALSE) +
  theme_minimal() +
  labs(title = "MGCV BAM-trends per Subset en Index (Macrofyten)",
       subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Voorspelde waarde") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  guides(color = "none", fill = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  filename =  here("output", "figuren", "trend_mafy_bam_1989-2023.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)

