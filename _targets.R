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
source("2-code/0b-initial_processing.R")
source("2-code/1-functions_processing.R")
source("2-code/2-functions_analysis.R")

# list of targets
list(
  tar_target(sample_key_file, "1-data/sample_key.csv", format = "file"),
  tar_target(sample_key, read.csv(sample_key_file)),
  tar_target(sample_weights, read.csv("1-data/sample_weights.csv")),
  tar_target(analysis_key, read.csv("1-data/analysis_key.csv")),
  
  # optode
  tar_target(optode_data, import_optode_data("1-data/optodes")),
  tar_target(optode_map, read_sheet("1Pumt5ZA2Ojc8Ow-Ex-gH63ChtTtZs1gPKc50UM098rY") %>% 
               mutate(optode_disc_number = as.character(optode_disc_number))),
  tar_target(optode_data_processed, process_optode_data(optode_data, optode_map, sample_key)),
  tar_target(gg_optode_all, plot_optode_data_all_samples(optode_data_processed)),
  tar_target(gg_optode, plot_optode_data(optode_data_processed)),
  
  # weoc
  tar_target(weoc_data, import_weoc_data(FILEPATH = "1-data/npoc", PATTERN = "Summary")),
  tar_target(weoc_processed, process_weoc(weoc_data, analysis_key)),
  tar_target(gg_weoc, plot_weoc(weoc_processed)),
  
  # report
  tar_render(report, path = "3-reports/anoxia_report.Rmd")
  

  
  )

