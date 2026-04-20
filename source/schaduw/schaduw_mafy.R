library(data.table)
library(lubridate)

# 1. Omzetten naar data.table format
load(here("data", "verwerkt", "mafy_data.rdata"))
schaduw_data <- read_excel("data/ruw/macrofyten/mafy_schaduw.xlsx")

setDT(mafy_data)
setDT(schaduw_data)

# 2. Zorg dat het jaar in beide datasets een integer is voor de match
mafy_data[, jaar_meting := year(monsternamedatum)]
# schaduw_data heeft al 'jaar_schaduw'

# 3. Sorteren is cruciaal voor een rolling join
setkey(mafy_data, meetplaats, jaar_meting)
setkey(schaduw_data, meetplaats, jaar_schaduw)

# 4. De Join: koppel op basis van meetplaats en het dichtstbijzijnde jaar
# We maken een kopie van jaar_schaduw omdat de join kolom vaak overschreven wordt
schaduw_data[, jaar_match := jaar_schaduw]

resultaat <- schaduw_data[mafy_data, roll = "nearest", on = .(meetplaats, jaar_schaduw = jaar_meting)]

# Resultaat opschonen: hernoem kolommen indien gewenst
setnames(resultaat, "jaar_schaduw", "jaar_meting")

mafy_schaduw_data <- resultaat %>%
  select(meetplaats, monsternamedatum, perc_schaduw)

save(mafy_schaduw_data, file = here("data", "verwerkt", "mafy_schaduw_data.rdata"))
