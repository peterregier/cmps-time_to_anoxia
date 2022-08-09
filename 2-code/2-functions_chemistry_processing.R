



# WEOC
import_weoc_data = function(FILEPATH, PATTERN){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))

}

process_weoc = function(weoc_data, analysis_key, moisture_processed, subsampling){
  
  npoc_processed = 
    weoc_data %>% 
    # remove skipped samples
    filter(!`Sample ID` %in% "skip") %>% 
    # keep only relevant columns and rename them
    dplyr::select(`Sample Name`, `Result(NPOC)`) %>% 
    rename(analysis_ID = `Sample Name`,
           npoc_mgL = `Result(NPOC)`) %>% 
    # keep only sampple rows 
    filter(grepl("DOC_", analysis_ID)) %>% 
    # join the analysis key to get the sample_label
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_name, NPOC_dilution)) %>%
    # do blank/dilution correction
    mutate(blank_mgL = case_when(sample_name == "blank-filter" ~ npoc_mgL)) %>% 
    fill(blank_mgL, .direction = c("up")) %>% 
    mutate(NPOC_dilution = as.numeric(NPOC_dilution),
           npoc_corr_mgL = (npoc_mgL-blank_mgL) * NPOC_dilution) %>% 
    # join gwc and subsampling weights to normalize data to soil weight
#    left_join(moisture_processed) %>% 
#    left_join(subsampling %>% dplyr::select(notes, sample_label, WSOC_g)) %>% 
#    rename(fm_g = WSOC_g) %>% 
#    mutate(od_g = fm_g/((gwc_perc/100)+1),
#           soilwater_g = fm_g - od_g,
#           npoc_ug_g = npoc_corr_mgL * ((40 + soilwater_g)/od_g),
#           npoc_ug_g = round(npoc_ug_g, 2)) %>% 
#    dplyr::select(sample_label, npoc_corr_mgL, npoc_ug_g, notes) %>% 
    force()
  
  npoc_samples = 
    npoc_processed %>% 
    filter(grepl("anoxia", sample_name))
  
  npoc_samples
}
