# Alle nodige packages laden
library(tidyverse)
library(lme4)
library(here)
library(openxlsx2)
library(readxl)
library(inbodb)
library(sf)
library(mapview)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("read_xlsx", "readxl")
conflicted::conflicts_prefer(dplyr::filter)
