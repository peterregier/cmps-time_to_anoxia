# TIME TO ANOXIA

## 0b-initial_processing.R
## Use this script to download data and metadata files from Google Drive.

## KFP, August 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#
# download sample metadata files ----------------------------------------------
## sample key --------------------------------------------------------------
## load and clean sample key
sample_key = read_sheet("1ZngRDe_jdiXKymQBaR5MI-F5sGS4rakoAkf6yYJ0YgQ") %>% mutate_all(as.character) %>% dplyr::select(-initials)

## export
sample_key %>% write.csv("1-data/sample_key.csv", row.names = FALSE, na = "")

#
## sample weights -----------------------------------------------------
## load and clean sample weight
sample_weights = read_sheet("1UrW8HWnARQ540Ze82HQpS28jX2E-OpGqHlX9IYxsZPE") %>% mutate_all(as.character)

## export
sample_weights %>% write.csv("1-data/sample_weights.csv", row.names = F, na = "")

#
## analysis key ------------------------------------------------------------
## load and clean analysis key
analysis_key = read_sheet("17b85H5JGmHQZRh8CwMKPInaz0E5JDAqJStu0SfVv_wE", sheet = "analysis_key") %>% mutate_all(as.character)
analysis_key[analysis_key == "NULL"] <- NA

## export
analysis_key %>% write.csv("1-data/analysis_key.csv", row.names = F, na = "")


#
######################## ####
