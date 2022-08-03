


# load files
import_optode_data = function(FILEPATH){
  filePaths_spectra <- list.files(path = FILEPATH,pattern = "*.csv", full.names = TRUE)
  spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
    df <- read.csv(path, header=TRUE, skip = 4, check.names = F)
    df %<>%
      rownames_to_column("timestep") %>% 
      pivot_longer(-timestep, names_to = "optode_disc_number", values_to = "do_mg_L") %>% 
      mutate(time_minutes = as.numeric(timestep) * 5)
    df[["source"]] <- rep(path, nrow(df))
    df}))
}


## optode_data = import_optode_data("1-data/optodes")

## optode_map = read_sheet("1Pumt5ZA2Ojc8Ow-Ex-gH63ChtTtZs1gPKc50UM098rY") %>% mutate(optode_disc_number = as.character(optode_disc_number))
## sample_key = read_sheet("1ZngRDe_jdiXKymQBaR5MI-F5sGS4rakoAkf6yYJ0YgQ")
## 
## 

process_optode_data = function(optode_data, optode_map, sample_key){
  optode_data_processed = 
    optode_data %>% 
    mutate(start_date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           start_date = lubridate::ymd(start_date)) %>% 
    dplyr::select(-source) %>% 
    left_join(optode_map %>% dplyr::select(start_date, optode_disc_number, sample_name)) %>% 
    left_join(sample_key) %>% 
    mutate(location = factor(location, levels = c("upland-A", "upland-B", "transition-A", "wetland-A", "water")))
}

plot_optode_data = function(optode_data_processed){
  
  grouped = 
    optode_data_processed %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = sample_name))+
    geom_line()+
    labs(#title = "Time to Anoxia",
         x = "Elapsed time, minutes",
         y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(timepoint ~ location)+
    theme(legend.position = "none")
  
  individual = 
    optode_data_processed %>% 
    filter(!is.na(location)) %>%
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = location))+
    geom_line()+
    labs(#title = "Time to Anoxia",
         x = "Elapsed time, minutes",
         y = "Dissolved oxygen, mg/L")+
    facet_wrap(~timepoint+sample_name, ncol = 5)+
  #  theme(legend.position = "none")+
    NULL
  
  list(grouped = grouped,
       individual = individual)
}

