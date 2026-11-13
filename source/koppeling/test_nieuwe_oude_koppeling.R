# ==============================================================================
# TEST: geeft de NIEUWE code gelijkaardige matches als de OUDE code?
# ==============================================================================
#
# VOORAF
#   1. Het OUDE resultaat moet nog bestaan:
#        data/verwerkt/koppeling/koppeling_mi_nutrient.rdata  (gemaakt met de oude code)
#      Heb je de nieuwe code al laten opslaan op die naam, dan is het oude
#      resultaat overschreven: draai dan eerst de oude code opnieuw.
#      Zekerheidshalve een back-up:
#        file.copy(here("data","verwerkt","koppeling","koppeling_mi_nutrient.rdata"),
#                  here("data","verwerkt","koppeling","koppeling_mi_nutrient_OUD.rdata"))
#   2. Het NIEUWE hoofdscript (koppeling_biota_kwaliteit.R) slaat zijn resultaat nu op als
#        data/verwerkt/koppeling/koppeling_mi_nutrient_nieuw.rdata
#      Draai dat script dus eerst.
#   3. (Optioneel, om het effect van de METHODE los te zien van het effect van het
#      herbouwde NETWERK): bewaar het oude netwerk VOOR je het .rds verwijdert:
#        file.copy(here("data","verwerkt","koppeling","river_network_fine.rds"),
#                  here("data","verwerkt","koppeling","river_network_fine_oud.rds"))
#
# WAT WORDT VERGELEKEN? Per MI-staal (sleutel = meetplaats + monsternamedatum):
#   - had de oude / nieuwe code een match?
#   - zelfde gekoppelde meetpunt (meetplaats + datum)?
#   - hoe verschilt de gebruikte afstand?
#
# WAT MAG JE VERWACHTEN?
#   - "enkel oud": matches die enkel via de vogelvlucht-regel (<= 200 m, zijtak of
#     ander segment) gevonden werden; die zijn bewust weggevallen. Hun oude
#     rivierafstand is meestal <= 200 m.
#   - "enkel nieuw": zou (bijna) 0 moeten zijn. Een klein aantal kan voorkomen door
#     het snappen op segmentbegin (graaf vs. echte positie).
#   - "andere meetplaats": kan als de oude match enkel via vogelvlucht kwam en er
#     nu een verdere stroomopwaartse match gekozen wordt.
#   - afstanden: voor "zelfde match" moeten die (bijna) gelijk zijn.
# ==============================================================================

library(dplyr)
library(sf)

pad_oud         <- here("data", "verwerkt", "koppeling", "koppeling_mi_nutrient.rdata")
pad_nieuw       <- here("data", "verwerkt", "koppeling", "koppeling_mi_nutrient_nieuw.rdata")
pad_netwerk_oud <- here("data", "verwerkt", "koppeling", "river_network_fine_oud.rds")
pad_export      <- here("data", "verwerkt", "koppeling", "vergelijking_oud_nieuw.csv")

# Laad een object uit een .rdata zonder bestaande objecten te overschrijven
laad_object <- function(pad, naam = "koppeling_mi_nutrient") {
  e <- new.env()
  load(pad, envir = e)
  get(naam, envir = e)
}

# Unieke sleutel per MI-staal: meetplaats | datum # volgnummer binnen dubbele sleutels
maak_sleutel <- function(x) {
  k <- paste(x$meetplaats, as.character(as.Date(x$monsternamedatum)), sep = "|")
  paste(k, stats::ave(seq_along(k), k, FUN = seq_along), sep = "#")
}


# ------------------------------------------------------------------------------
# Vergelijkingsfunctie
# ------------------------------------------------------------------------------
# oud, nieuw   : sf-resultaten van match_upstream_strahler() (oude / nieuwe code)
# label        : naam voor in de uitvoer
# nutrient_sf  : optioneel, de kwaliteitspunten (nutrient_data); laat toe om bij
#                "enkel oud" de vogelvluchtafstand tot de oude match te berekenen
# Geeft een data.frame terug met 1 rij per MI-staal en een kolom `status`.

vergelijk <- function(oud, nieuw, label = "nieuw", nutrient_sf = NULL) {

  oud_df   <- st_drop_geometry(oud)
  nieuw_df <- st_drop_geometry(nieuw)
  oud_df$sleutel   <- maak_sleutel(oud_df)
  nieuw_df$sleutel <- maak_sleutel(nieuw_df)
  oud_df$rij_oud   <- seq_len(nrow(oud_df))

  cat("\n==============================================================\n")
  cat("OUD versus", toupper(label), "\n")
  cat("==============================================================\n")
  cat("Rijen oud:", nrow(oud_df), "| rijen nieuw:", nrow(nieuw_df), "\n")
  cat("Sleutels enkel in oud:", length(setdiff(oud_df$sleutel, nieuw_df$sleutel)),
      "| enkel in nieuw:", length(setdiff(nieuw_df$sleutel, oud_df$sleutel)), "\n")

  d <- inner_join(
    oud_df %>% transmute(sleutel, rij_oud, meetplaats, monsternamedatum,
                         o_plaats = qual_meetplaats,
                         o_datum  = as.character(qual_monsternamedatum),
                         o_dist   = qual_river_dist_m,
                         o_n      = qual_n_matches),
    nieuw_df %>% transmute(sleutel,
                           n_plaats = qual_meetplaats,
                           n_datum  = as.character(qual_monsternamedatum),
                           n_dist   = qual_river_dist_m,
                           n_n      = qual_n_matches),
    by = "sleutel"
  ) %>%
    mutate(
      o_match = !is.na(o_plaats),
      n_match = !is.na(n_plaats),
      status = case_when(
        !o_match & !n_match                      ~ "geen match (beide)",
        o_match  & !n_match                      ~ "enkel oud",
        !o_match & n_match                       ~ "enkel nieuw",
        o_plaats == n_plaats & o_datum == n_datum ~ "zelfde match",
        o_plaats == n_plaats                     ~ "zelfde meetplaats, andere datum",
        TRUE                                     ~ "andere meetplaats"
      )
    )

  # --- 1. Overzicht ---
  cat("\nAantal matches: oud =", sum(d$o_match), "| nieuw =", sum(d$n_match), "\n")
  cat("\nStatus per MI-staal:\n")
  tab <- table(d$status)
  print(data.frame(aantal = as.integer(tab),
                   procent = round(100 * as.numeric(prop.table(tab)), 1),
                   row.names = names(tab)))

  # --- 2. Beide gematcht: afstanden ---
  beide <- d %>% filter(o_match, n_match)
  if (nrow(beide) > 0) {
    cat("\nBeide gematcht:", nrow(beide), "staaltjes\n")
    cat("  zelfde match (meetplaats + datum):",
        round(100 * mean(beide$status == "zelfde match"), 1), "%\n")
    verschil <- beide$n_dist - beide$o_dist
    cat("  afstandsverschil nieuw - oud (m):\n")
    print(summary(verschil))
    cat("  |verschil| <= 1 m:", round(100 * mean(abs(verschil) <= 1, na.rm = TRUE), 1), "%\n")

    # Alleen bij zelfde match zouden de afstanden gelijk moeten zijn
    zelfde <- beide %>% filter(status == "zelfde match")
    if (nrow(zelfde) > 0) {
      cat("  bij 'zelfde match': |verschil| <= 1 m:",
          round(100 * mean(abs(zelfde$n_dist - zelfde$o_dist) <= 1, na.rm = TRUE), 1), "%\n")
    }

    plot(beide$o_dist, beide$n_dist,
         xlab = "afstand oud (m)", ylab = paste("afstand", label, "(m)"),
         main = paste("Gebruikte afstand: oud vs", label),
         pch = 16, cex = 0.6, col = rgb(0, 0, 0, 0.4))
    abline(0, 1, col = "red")
  }

  # --- 3. Enkel oud: waarom gingen ze verloren? ---
  enkel_oud <- d %>% filter(status == "enkel oud")
  if (nrow(enkel_oud) > 0) {
    cat("\nEnkel-oud-matches:", nrow(enkel_oud), "\n")
    cat("  oude rivierafstand (m):\n")
    print(summary(enkel_oud$o_dist))
    cat("  aandeel met oude afstand <= 200 m (= typisch lokale/vogelvlucht-match):",
        round(100 * mean(enkel_oud$o_dist <= 200, na.rm = TRUE), 1), "%\n")

    if (!is.null(nutrient_sf)) {
      q <- nutrient_sf %>%
        mutate(sl = paste(meetplaats, as.character(as.Date(monsternamedatum)), sep = "|")) %>%
        distinct(sl, .keep_all = TRUE)
      idx <- match(paste(enkel_oud$o_plaats, enkel_oud$o_datum, sep = "|"), q$sl)
      ok  <- !is.na(idx)
      if (any(ok)) {
        afst <- st_distance(st_geometry(oud)[enkel_oud$rij_oud[ok]],
                            st_geometry(q)[idx[ok]], by_element = TRUE)
        cat("  vogelvluchtafstand biota -> oude match (m):\n")
        print(summary(as.numeric(afst)))
      }
    }
  }

  # --- 4. Enkel nieuw (zou ~0 moeten zijn) ---
  if (any(d$status == "enkel nieuw")) {
    cat("\nLET OP: ", sum(d$status == "enkel nieuw"),
        " staaltjes hebben enkel in de NIEUWE code een match (zie export).\n", sep = "")
  }

  invisible(d)
}


# ------------------------------------------------------------------------------
# Uitvoeren
# ------------------------------------------------------------------------------
oud   <- laad_object(pad_oud)
nieuw <- laad_object(pad_nieuw)

# kwaliteitspunten (uit het hoofdscript) voor de extra diagnose bij "enkel oud"
nutrient_ref <- if (exists("nutrient_data")) nutrient_data else NULL

# A. Oud (oud netwerk) vs nieuw (nieuw netwerk)
d_nieuw <- vergelijk(oud, nieuw, label = "nieuw (nieuw netwerk)", nutrient_sf = nutrient_ref)

# Alles wat NIET identiek is, exporteren om in detail te bekijken
write.csv(d_nieuw %>% filter(!status %in% c("zelfde match", "geen match (beide)")),
          pad_export, row.names = FALSE)
cat("\nVerschillen geëxporteerd naar:", pad_export, "\n")

# B. (Optioneel) Nieuwe methode op het OUDE netwerk: isoleert het effect van de
#    methode (vogelvlucht -> stroomafwaarts langs graaf, afstandsregel) van het
#    effect van het herbouwde netwerk. Vereist dat het hoofdscript is ingeladen
#    (functie + data) en dat river_network_fine_oud.rds bestaat.
if (file.exists(pad_netwerk_oud) && exists("match_upstream_strahler")) {
  net_oud <- readRDS(pad_netwerk_oud)

  nieuw_oudnet <- match_upstream_strahler(
    biota_sf         = mi_data,
    quality_sf       = nutrient_data,
    network_list     = net_oud,
    strahler_sf      = strahler,
    use_strahler     = FALSE,
    strahler_col     = "orde",
    max_downstream_m = 200,
    max_dist_m       = 5000,
    days_before      = 180,
    days_after       = 14,
    col_date_biota   = "monsternamedatum",
    col_date_quality = "monsternamedatum",
    selection_mode   = "closest_distance",
    grouping_col     = "VHAG",
    vhas_col_network = "VHAS",
    vhas_col_biota   = "vhas",
    vhas_col_quality = "vhas"
  )

  d_oudnet <- vergelijk(oud, nieuw_oudnet, label = "nieuwe methode op oud netwerk",
                        nutrient_sf = nutrient_ref)
} else {
  message("B overgeslagen: river_network_fine_oud.rds of match_upstream_strahler() niet beschikbaar.")
}
