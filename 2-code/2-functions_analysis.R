
# OPTODES ----
plot_optode_data_all_samples = function(optode_data_processed){
  
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
  
#  grouped %>% plotly::ggplotly(.)
  
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

plot_optode_data = function(optode_data_processed){
  
  grouped = 
    optode_data_processed %>% 
    filter(!grepl("skip", notes)) %>% 
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
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>%
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = location))+
    geom_line()+
  #  geom_point(data =   x_min %>% filter(is_min), color = "black")+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      y = "Dissolved oxygen, mg/L")+
    facet_wrap(~timepoint+sample_name, ncol = 5)+
    #  theme(legend.position = "none")+
    NULL
  
  list(grouped = grouped,
       individual = individual)
}

compute_optode_slope_OLD = function(optode_data_processed){
  x = 
    optode_data_processed %>% 
    arrange(location, sample_name, time_minutes) %>% 
    group_by(sample_name) %>% 
    mutate(do_rolling_7 = zoo::rollmean(do_mg_L, k = 7, fill = NA),
           do_slope = do_rolling_7 - lag(do_rolling_7),
           do_slope2 = do_slope - lag(do_slope))
  
  x %>% 
    filter(!is.na(location)) %>%
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_slope2, color = sample_name))+
    geom_line(show.legend = F)+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      #y = "Dissolved oxygen, mg/L"
      )+
    facet_wrap(~timepoint+sample_name, ncol = 5)+
    #  theme(legend.position = "none")+
    NULL
  
}

compute_optode_slope_OLD2 = function(optode_data_processed){
  x = 
    optode_data_processed %>% 
    arrange(location, sample_name, time_minutes) %>% 
    group_by(sample_name) %>% 
    mutate(do_minimum = min(do_mg_L),
           is_min = do_mg_L == do_minimum)
  
  x_min = 
    x %>% 
    filter(is_min) %>%
    group_by(sample_name) %>% 
    mutate(keep = time_minutes == min(time_minutes))
  
  x %>% 
    filter(!is.na(location)) %>%
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_slope2, color = sample_name))+
    geom_line(show.legend = F)+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      #y = "Dissolved oxygen, mg/L"
    )+
    facet_wrap(~timepoint+sample_name, ncol = 5)+
    #  theme(legend.position = "none")+
    NULL
  
}

compute_optode_slope = function(optode_data_processed){
  
  # first, figure out when the system turned anoxic
  max_time = 
    optode_data_processed %>% 
    group_by(sample_name) %>% 
    mutate(max = time_minutes == max(time_minutes)) %>% 
    filter(max) %>% 
    dplyr::select(-max)

  anoxia_time_manual = tribble(
    ~sample_name, ~anoxia_minutes,
    "anoxia_032", 690,
    "anoxia_034", 550,
    "anoxia_035", 695,
    "anoxia_045", 805,
    "anoxia_046", 1070,
    "anoxia_047", 1345,
    "anoxia_053", 765,
    "anoxia_056", 665,
    "anoxia_057", 645,
    "anoxia_060", 800,
    "anoxia_061", 450,
    "anoxia_062", 360
  )  

  starting_do = 
    optode_data_processed %>% 
    group_by(sample_name) %>% 
    mutate(min = time_minutes == min(time_minutes)) %>% 
    filter(min) %>% 
    rename(do_mg_L_START = do_mg_L,
           time_minutes_START = time_minutes) %>% 
    dplyr::select(sample_name, do_mg_L_START, time_minutes_START)
  
  
  
  anoxia_time = 
    max_time %>% 
    left_join(anoxia_time_manual) %>% 
    mutate(time_minutes_STOP = case_when(!is.na(anoxia_minutes) ~ anoxia_minutes,
                                            TRUE ~ time_minutes)) %>% 
    dplyr::select(sample_name, time_minutes_STOP, do_mg_L) %>% 
    rename(do_mg_L_STOP = do_mg_L) %>% 
    left_join(sample_key) %>% 
    left_join(starting_do) %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(!grepl("blank", location)) %>% 
    dplyr::select(-ends_with("START"), -ends_with("STOP"), ends_with("START"), ends_with("STOP")) %>% 
    mutate(do_slope = (do_mg_L_START - do_mg_L_STOP) / ((time_minutes_STOP - time_minutes_START)/60))
    
  
    
}

#
# WEOC ----
 
plot_weoc = function(weoc_processed, sample_key){
  weoc = 
    weoc_processed %>% 
    left_join(sample_key) %>% 
    recode_levels()
  
  mg_l = 
    weoc %>% 
    ggplot(aes(x = timepoint, y = npoc_corr_mgL))+
    geom_point()+
    facet_wrap(~location)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, mg/L")
  
  ug_g = 
    weoc %>% 
    ggplot(aes(x = timepoint, y = npoc_ug_g))+
    geom_point()+
    facet_wrap(~location)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, μg/g soil")
  
  list(mg_l = mg_l,
       ug_g = ug_g)
}


#
# IONS ----

plot_ions = function(ions_processed, sample_key){
  ions = 
    ions_processed %>% 
    left_join(sample_key)

  ions_mg_l = 
    ions %>% 
    recode_levels() %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = amount_ppm))+
    geom_point()+
    facet_wrap(ion ~ location, scales = "free_y")+
    labs(title = "Dissolved ions",
         x = "", 
         y = "Ion concentrations, mg/L")  
  
  ions_ug_g =
    ions %>% 
    recode_levels() %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = amount_ug_g))+
    geom_point()+
    facet_wrap(ion ~ location, scales = "free_y")+
    labs(title = "Dissolved ions",
         x = "", 
         y = "Ion concentrations, μg/g soil")
  
  list(ions_mg_l = ions_mg_l,
       ions_ug_g = ions_ug_g)
}

# pH ----

plot_pH = function(pH_processed, sample_key){
  
  pH = 
    pH_processed %>% 
    left_join(sample_key)
  
    pH %>% 
    recode_levels() %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = pH))+
    geom_point()+
    facet_wrap(. ~ location)+
    labs(title = "pH",
         x = "", 
         y = "pH")  
  
}
