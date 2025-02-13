# packages inlezen ----
library(tidyverse)
library(openxlsx2)
library(readxl)
library(inbodb)
library(here)
library(sf)

# Analyseresultaten ----

# List all sheet names
sheet_names <- excel_sheets(here(
  "data",
  "241024_Analyseresultaten per meetplaats_2010_2024.xlsx"
))

# Limit to the first 8 sheets (if there are fewer than 8 sheets, adjust accordingly)
sheets_to_read <- sheet_names[1:8]

# Read all sheets into a list of dataframes
list_of_dataframes <- lapply(sheets_to_read, function(sheet) {
  read_excel(here(
    "data",
    "241024_Analyseresultaten per meetplaats_2010_2024.xlsx"
  ),
  sheet = sheet)
})

# Combine all dataframes into one
combined_dataframe <- do.call(rbind, list_of_dataframes)

anaresult1 <- combined_dataframe

anaresult <- janitor::clean_names(anaresult1)

anaresult_locations <-
  read_xlsx("Data/241024_Analyseresultaten per meetplaats_2010_2024.xlsx",
            sheet = 10)
rm(combined_dataframe)
rm(list_of_dataframes)

# unieke stoffen
anaresult %>%
  select(parameter_omschrijving) %>%
  unique() %>%
  View()


anaresult %>%
  filter(parameter_omschrijving == "diuron")
