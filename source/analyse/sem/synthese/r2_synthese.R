# ==============================================================================
# R2 (marginal + conditional) uit piecewise SEM-modellen
# LET OP: rsquared() heeft de psem-OBJECTEN nodig (.rdata/.rds met class "psem"),
# niet de coefficienten-tabellen uit de map sem_output.
# ==============================================================================
library(tidyverse)
library(here)
library(piecewiseSEM)

# ---- 0. Instellingen ---------------------------------------------------------
map_modellen <- here("source", "analyse", "sem", "synthese", "sem_output")
patroon      <- "_clean\\.rdata$"

# De groep (MI / MAFY) zit niet in de bestandsnaam, enkel de maatlat. Indeling hieronder
# is een aanname op basis van je bestandslijst: controleer ze even.
maatlatten_mi   <- c("mmif", "ept", "mts", "nst", "swd", "tax")
maatlatten_mafy <- c("mafy_index", "groeivorm", "typspec", "verstoring", "vo")

# Voor deze maatlatten wordt R2 berekend met MuMIn::r.squaredGLMM (op elk submodel)
# in plaats van piecewiseSEM::rsquared. Voor alle modellen: maatlatten_mumin <- c(maatlatten_mi, maatlatten_mafy)
maatlatten_mumin <- c("ept", "nst")

typologie_labels <- c(
  beek = "Beken", kempen = "Kempense beken", rivier = "Rivieren",
  polder = "Polderlopen", rtnt = "Bovenlopen", kunstmatig = "Kunstmatig"
)
biologische_vars <- c("mmif", "tax", "ept", "nst", "swd", "mts",
                      "index_nieuw", "v", "gv", "ts", "vo")

kort_naam <- function(x) {
  x <- str_remove(x, "_zonder_gep$")
  case_match(x,
             "ta_xw" ~ "tax", "ept_prop" ~ "ept", "nst_prop" ~ "nst",
             c("mt_sw", "mt_sw_prop") ~ "mts", "sw_dw" ~ "swd",
             .default = x)
}

# ---- 1. model_overzicht opnieuw opbouwen -------------------------------------
# Bestandsnaam: <maatlat>_sem_nat_sv_<typologie>_clean.rdata
#   bv. mmif_sem_nat_sv_beek_clean.rdata, mafy_index_sem_nat_sv_kempen_clean.rdata
bestanden <- list.files(map_modellen, pattern = patroon, full.names = TRUE,
                        ignore.case = TRUE)
stopifnot("Geen modelbestanden gevonden: controleer map_modellen" = length(bestanden) > 0)

model_overzicht <- tibble(bestandspad = bestanden) %>%
  mutate(
    naam      = tools::file_path_sans_ext(basename(bestandspad)),
    maatlat   = str_match(naam, "^(.+)_sem_nat_sv_(.+)_clean$")[, 2],
    typologie = str_match(naam, "^(.+)_sem_nat_sv_(.+)_clean$")[, 3],
    groep     = case_when(maatlat %in% maatlatten_mi   ~ "mi",
                          maatlat %in% maatlatten_mafy ~ "mafy",
                          TRUE ~ NA_character_)
  ) %>%
  select(groep, typologie, maatlat, bestandspad)

# Controle: geen NA's in groep/typologie/maatlat?
print(count(model_overzicht, groep, maatlat))
stopifnot("Sommige bestandsnamen of maatlatten werden niet herkend" =
            !anyNA(model_overzicht[c("groep", "typologie", "maatlat")]))

# ---- 2. R2 oogsten -----------------------------------------------------------
lees_psem <- function(pad) {
  if (tolower(tools::file_ext(pad)) == "rds") {
    readRDS(pad)
  } else {
    env <- new.env()
    load(pad, envir = env)
    # eerste object in het bestand dat een psem is (ipv blind ls()[1])
    Find(function(o) inherits(o, "psem"), mget(ls(env), envir = env))
  }
}

# R2 per submodel met MuMIn::r.squaredGLMM (Nakagawa), rechtstreeks op de modelobjecten in het psem
r2_mumin <- function(obj, maatlat = NA_character_) {
  ondersteund <- c("lm", "glm", "merMod", "glmmTMB", "lme", "gls")
  modellen <- Filter(function(m) inherits(m, ondersteund), unclass(obj))

  map_dfr(modellen, function(m) {
    r <- MuMIn::r.squaredGLMM(m)          # matrix met R2m en R2c; bij niet-gaussiaanse
    r <- r[1, , drop = FALSE]             # families meerdere rijen: rij 1 = "theoretical"
    f <- tryCatch(formula(m), error = function(e) NULL)   # (kies r["delta", ] voor de delta-methode)
    lhs <- if (is.null(f)) NULL else f[[2]]
    # binomiaal met cbind(succes, falen): de responsnaam is dan de maatlat van dit bestand
    resp <- if (is.null(lhs)) NA_character_ else
      if (is.call(lhs) && identical(lhs[[1]], as.name("cbind"))) maatlat else all.vars(lhs)[1]
    tibble(
      Response    = resp,
      Class       = class(m)[1],
      Family      = tryCatch(family(m)$family, error = function(e) NA_character_),
      Marginal    = unname(r[, "R2m"]),
      Conditional = unname(r[, "R2c"])
    )
  })
}

haal_r2 <- function(pad, maatlat = NA_character_) {
  obj <- lees_psem(pad)
  r2 <- if (inherits(obj, "psem")) {
    if (maatlat %in% maatlatten_mumin) r2_mumin(obj, maatlat) else piecewiseSEM::rsquared(obj)
  } else if (is.data.frame(obj) && any(c("Marginal", "R.squared") %in% names(obj))) {
    obj   # bestand bevat al een R2-tabel
  } else {
    stop("geen psem-object (of R2-tabel) in dit bestand")
  }
  as_tibble(r2) %>%
    mutate(across(any_of(c("Response", "Class", "Family", "Link")), as.character),
           across(any_of(c("R.squared", "Marginal", "Conditional")), as.numeric))
}

resultaat <- model_overzicht %>%
  mutate(
    r2   = map2(bestandspad, maatlat, safely(haal_r2)),
    fout = map_chr(r2, ~ if (is.null(.x$error)) NA_character_ else conditionMessage(.x$error)),
    r2   = map(r2, "result")
  )

mislukt <- filter(resultaat, !is.na(fout))
if (nrow(mislukt) > 0) {
  message(nrow(mislukt), " van ", nrow(resultaat), " bestanden mislukt:")
  print(select(mislukt, bestandspad, fout))
}

master_r2_df <- resultaat %>%
  filter(map_lgl(r2, negate(is.null))) %>%
  select(groep, typologie, maatlat, r2) %>%
  unnest(r2)

# kolommen die enkel bij lm (R.squared) of enkel bij mixed models (Marginal/Conditional) bestaan
for (k in c("R.squared", "Marginal", "Conditional")) {
  if (!k %in% names(master_r2_df)) master_r2_df[[k]] <- NA_real_
}

saveRDS(master_r2_df, here("source", "analyse", "sem", "synthese", "sem_r2_master_df.rds"))
message("R2 oogst klaar: ", nrow(master_r2_df), " submodellen.")

# ---- 3. Data opschonen -------------------------------------------------------
# - lm (geen random effecten): R.squared telt als Marginal, en Conditional = Marginal
# - zelfde submodel in meerdere maatlat-modellen: gemiddelde per groep x typologie x respons
r2_data <- master_r2_df %>%
  mutate(
    groep       = tolower(groep),
    typologie   = factor(coalesce(unname(typologie_labels[typologie]), typologie),
                         levels = typologie_labels),
    Response    = kort_naam(Response),
    Marginal    = coalesce(Marginal, R.squared),
    Conditional = coalesce(Conditional, Marginal)
  ) %>%
  filter(groep %in% c("mi", "mafy")) %>%
  group_by(groep, typologie, Response) %>%
  summarise(Marginal = mean(Marginal, na.rm = TRUE),
            Conditional = mean(Conditional, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(Marginal, Conditional), ~ if_else(is.nan(.x), NA_real_, .x)),
         type = if_else(Response %in% biologische_vars, "Biologie", "Abiotisch"))

r2_lang <- r2_data %>%
  pivot_longer(c(Marginal, Conditional), names_to = "R2_type", values_to = "R2") %>%
  mutate(R2_type = factor(R2_type, levels = c("Marginal", "Conditional"),
                          labels = c("Marginaal (enkel vaste effecten)",
                                     "Conditioneel (+ random effecten)")))

groep_labels <- c(mafy = "Macrofyten (MAFY)", mi = "Macro-invertebraten (MI)")

# ---- 4A. Heatmap: respons x typologie, naast elkaar marginal en conditional ---
plot_r2_heatmap <- function(g) {
  d <- r2_lang %>%
    filter(groep == g) %>%
    mutate(Response = fct_reorder(Response, R2, .fun = \(x) median(x, na.rm = TRUE)))

  ggplot(d, aes(x = typologie, y = Response, fill = R2)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = if_else(is.na(R2), "", sprintf("%.2f", R2))), size = 3) +
    scale_fill_gradient(low = "white", high = "#2c7bb6",
                        limits = c(0, max(r2_lang$R2, na.rm = TRUE)),
                        na.value = "grey95", name = expression(R^2)) +
    scale_x_discrete(drop = FALSE) +
    facet_grid(type ~ R2_type, scales = "free_y", space = "free_y") +
    labs(title = paste0("Verklaarde variantie (R²) per submodel: ", groep_labels[[g]]),
         x = NULL, y = "Afhankelijke variabele (submodel)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = "right")
}

heatmaps <- set_names(unique(r2_data$groep)) %>% map(plot_r2_heatmap)
heatmaps$mi
heatmaps$mafy

# ---- 4B. Boxplot-samenvatting (zoals je originele figuur) --------------------
plot_r2_box <- ggplot(
  drop_na(r2_lang, R2),
  aes(x = R2, y = fct_reorder(Response, R2, .fun = median), fill = R2_type, color = R2_type)
) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1.8, alpha = 0.7) +
  facet_wrap(~ groep, ncol = 2, scales = "free_y",
             labeller = labeller(groep = groep_labels)) +
  scale_fill_manual(name = "Verklaarde variantie:",
                    values = c("#fc8d62", "#8da0cb")) +
  scale_color_manual(name = "Verklaarde variantie:",
                     values = c("#fc8d62", "#8da0cb")) +
  labs(title = "Verklaarde variantie (R²) per submodel in het pSEM",
       x = expression(R^2), y = "Afhankelijke variabele (submodel)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = "white"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

# print(plot_r2_box)

# ---- 5. Opslaan (optioneel) ---------------------------------------------------
# dir.create(here("output", "figuren"), recursive = TRUE, showWarnings = FALSE)
# iwalk(heatmaps, ~ ggsave(here("output", "figuren", paste0("r2_heatmap_", .y, ".png")),
#                          .x, width = 11, height = 9, dpi = 300))
# ggsave(here("output", "figuren", "r2_boxplot.png"), plot_r2_box, width = 12, height = 8, dpi = 300)
