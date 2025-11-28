library(here)
library(openxlsx2)
library(stringdist)
library(tidyverse)
# R-code om uit te voeren

load(file = "data/verwerkt/fc_data.rdata")

if(!exists(here("data", "ruw", "fys_chem", "alle_stoffen.xlsx"))){
stoffen <- fc_data %>%
  select(parameter_omschrijving, parameter_symbool) %>%
  unique()
write_xlsx(stoffen, here("data", "ruw", "fys_chem", "alle_stoffen.xlsx"))
}

stoffen <- read_xlsx(here("data", "ruw", "fys_chem", "alle_stoffen.xlsx"))
chemicals_cas <- read_xlsx("data/ruw/fys_chem/chemicals_cas.xlsx") %>%
  janitor::clean_names()

amatch(x = stoffen, table = chemicals_cas$chemical_name)





cids <- get_cid(stoffen_clean)
info <- pc_prop(cids$cid, properties = c("IUPACName", "CanonicalSMILES"))

cas  <- pc_synonyms(cids$cid)
cas_batch <- pc_synonyms_batch(cids$cid)

extract_cas <- function(x) {
  cas <- regmatches(x, regexpr("\\b\\d{2,7}-\\d{2}-\\d\\b", x))
  if (length(cas) == 0) return(NA)
  return(cas)
}

cas_numbers <- sapply(cas$synonyms, function(s) {
  extract_cas(paste(s, collapse = " "))
})


result <- tibble(
  input = stoffen,
  cid = cids$cid,
  iupac = info$IUPACName,
  cas = cas_numbers
)

result


library(jsonlite)

get_cas_pubchem <- function(cid) {
  url <- paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/",
    cid,
    "/JSON/?response_type=save&response_basename=pubchem"
  )

  tryCatch({
    json <- fromJSON(url)

    # Extract all possible synonym strings from the JSON tree
    syns <- unlist(json, use.names = FALSE)

    cas <- regmatches(syns, gregexpr("\\b\\d{2,7}-\\d{2}-\\d\\b", syns))[[1]]
    cas <- cas[nchar(cas) > 0]

    if (length(cas) == 0) return(NA)
    cas[1]
  }, error = function(e) NA)
}
cas_numbers <- sapply(cids$cid, get_cas_pubchem)
cas_numbers
