# ==============================================================================
# APART SCRIPT: SPEI-24 BEREKENEN EN TOEVOEGEN AAN BESTAANDE DATASET
# ==============================================================================
library(SPEI)
library(tidyverse)
library(data.table)
library(lubridate)
library(sf)
library(here)

message("Start berekening SPEI-24...")

# 1. Laad de opgeslagen dagelijkse reeksen in
file_daily <- here("data", "verwerkt", "hydrologisch", "dagelijkse_reeksen_per_catchment.rds")
if (!file.exists(file_daily)) stop("Kan dagelijkse reeksen niet vinden. Heb je stap 3-5 gerund?")
dt_daily <- readRDS(file_daily)

# 2. Zet om naar maandelijkse data en maak het continue 'skelet' (1991 - einde)
dt_daily[, `:=`(year = lubridate::year(day), month = lubridate::month(day))]
dt_monthly <- dt_daily[, .(
  P_month = sum(P_mean, na.rm = TRUE),
  E_month = sum(E_mean, na.rm = TRUE)
), by = .(meetplaats, year, month)]

min_yr_global <- 1991
max_yr_global <- max(dt_monthly$year)

all_combinations <- CJ(meetplaats = unique(dt_monthly$meetplaats), year = min_yr_global:max_yr_global, month = 1:12)
dt_monthly_full <- merge(all_combinations, dt_monthly, by = c("meetplaats", "year", "month"), all.x = TRUE)
dt_monthly_full[is.na(P_month), P_month := 0]
dt_monthly_full[is.na(E_month), E_month := 0]
setorder(dt_monthly_full, meetplaats, year, month)

# 3. Functie om uitsluitend SPEI-24 te berekenen

calc_spei24 <- function(df_sub) {
  balance <- df_sub$P_month - df_sub$E_month
  ts_balance <- ts(balance, start = c(min_yr_global, 1), frequency = 12)

  tryCatch({
    spei_obj24 <- spei(ts_balance, scale = 24,
                       ref.start = c(1991, 1), ref.end = c(2020, 12),
                       verbose = FALSE)
    df_sub$spei24 <- as.numeric(spei_obj24$fitted)
  }, error = function(e) {
    df_sub$spei24 <- NA
  })

  # AANGEPAST: 'meetplaats' verwijderd uit de return.
  # data.table plakt deze er zelf weer aan vast dankzij 'by = meetplaats'
  return(df_sub[, .(year, month, spei24)])
}

# 4. Voer de berekening uit per meetplaats
message("   -> SPEI-24 model fitten per afstroomgebied...")
dt_spei24 <- dt_monthly_full[, calc_spei24(.SD), by = meetplaats]

# 5. Laad je eerdere voltooide dataset in
file_dataset <- here("data", "verwerkt", "metingen_met_klimaat_compleet.gpkg")
if (!file.exists(file_dataset)) stop("Kan de final dataset niet vinden.")
final_dataset <- st_read(file_dataset, quiet = TRUE) %>%
  mutate(
    meetplaats = as.character(meetplaats),
    year = year(monsternamedatum),
    month = month(monsternamedatum)
  )

# 6. Join de nieuwe SPEI-24 waarden
message("   -> Data koppelen en opslaan...")
dt_spei24_df <- as.data.frame(dt_spei24)

final_dataset_updated <- final_dataset %>%
  left_join(dt_spei24_df, by = c("meetplaats", "year", "month")) %>%
  select(-year, -month) # Hulpkolommen weggooien

# 7. Sla op
outfile_nieuw <- here("data", "verwerkt", "metingen_met_klimaat_compleet_incl_spei24.gpkg")
st_write(final_dataset_updated, outfile_nieuw, delete_dsn = TRUE)

hydro_data2 <- final_dataset_updated %>% st_drop_geometry()
save(hydro_data2, file = here("data", "verwerkt", "hydro_data_incl_spei24.rdata"))

message("✅ Klaar! Bestand opgeslagen als: ", outfile_nieuw)
