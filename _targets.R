# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# Load the R scripts with your custom functions:
source("2-code/0-packages.R")
source("2-code/optode_processing.R")


# list of targets
list(
  # optode
  tar_target(optode_data, import_optode_data("1-data/optodes")),
  tar_target(optode_map, read_sheet("1Pumt5ZA2Ojc8Ow-Ex-gH63ChtTtZs1gPKc50UM098rY") %>% 
               mutate(optode_disc_number = as.character(optode_disc_number))),
  tar_target(sample_key, read_sheet("1ZngRDe_jdiXKymQBaR5MI-F5sGS4rakoAkf6yYJ0YgQ")),
  tar_target(optode_data_processed, process_optode_data(optode_data, optode_map, sample_key)),
  tar_target(gg_optode, plot_optode_data(optode_data_processed)),
  
  # report
  tar_render(report, path = "3-reports/anoxia_report.Rmd")
  

  
  )

