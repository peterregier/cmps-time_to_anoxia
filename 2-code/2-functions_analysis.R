

pal_horizons = soilpalettes::soil_palette("eutrostox", 2)


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

plot_optode_data_OLD = function(optode_data_processed){
  
  grouped = 
    optode_data_processed %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("24-hour", "2-week-rep")) %>% 
    ggplot(aes(x = time_minutes/60, y = corrected_do_mg_L, color = sample_name))+
    geom_line()+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, hours",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(timepoint ~ location)+
    theme(legend.position = "none")
  
  grouped2 = 
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


plot_optode_data = function(optode_data_processed){
  
  optode_combined2 = 
    optode_data_processed %>% 
    mutate(sample_type = location) %>% 
    separate(sample_type, sep = "-", into = c("transect", "horizon")) %>% 
    mutate(transect = factor(transect, levels = c("upland", "transition", "wetland")))
  
  gg_24hr = 
    optode_combined2 %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("24-hour")) %>% 
    filter(timepoint == c("24-hour", "2-week-rep")) %>% 
    ggplot(aes(x = time_minutes/60, y = corrected_do_mg_L, color = horizon, group = sample_name))+
    geom_line()+
    scale_color_manual(values = pal_horizons)+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, hours",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(. ~ transect)+
    theme(legend.position = "none")    
  
  
  gg_2wk = 
    optode_combined2 %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("2-week-rep")) %>% 
    ggplot(aes(x = (time_minutes/60)/24, y = corrected_do_mg_L, color = location, group = location))+
    geom_line(size = 1)+
    #   scale_color_manual(values = soilpalettes::soil_palette("podzol", 5))+
    #      scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 4))+
    
    scale_color_manual(values = c("#cc5c76", "#f9ad2a", "#625a94", "#1d457f"))+
    
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, days",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    # facet_grid(timepoint ~ .)+
    theme(legend.position = c(0.5,0.5))  +
    NULL
  
  cowplot::plot_grid(gg_24hr + ggtitle("A: 24-hour"), gg_2wk + ggtitle("B: 2-week\n\n"), 
                     rel_widths = c(2.7,1),
                     label_y = 1,
                     label_x = 0)
  
  
  
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
    
  anoxia_time_summary = 
    anoxia_time %>% 
    group_by(location, timepoint) %>%
    dplyr::summarise(mean_slope = mean(do_slope))
    
  
  anoxia_time_24hr = 
    anoxia_time %>% 
    filter(timepoint == "24-hour")
  
  a = aov(do_slope ~ location, data = anoxia_time_24hr)
  summary(a)
  
  h = agricolae::HSD.test(a, "location")
  
  
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
    ggplot(aes(x = timepoint, y = npoc_corr_mgL, shape = horizon, color = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    facet_wrap(~transect)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, mg/L")
  
  ug_g = 
    weoc %>% 
    ggplot(aes(x = timepoint, y = npoc_ug_g, shape = horizon, color = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    facet_wrap(~transect)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, μg/g soil")+
    theme(legend.position = "top")
  
  list(mg_l = mg_l,
       ug_g = ug_g)
}


#
# IONS ----

plot_ions = function(ions_processed, sample_key){
  ions = 
    ions_processed %>% 
    left_join(sample_key) %>% 
    recode_levels() 

  ions_mg_l = 
    ions %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = amount_ppm, color = horizon, shape = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    facet_grid(ion ~ transect, scales = "free_y")+
    labs(title = "Dissolved ions",
         x = "", 
         y = "Ion concentrations, mg/L")  
  
  ions_ug_g =
    ions %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = amount_ug_g, color = horizon, shape = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    expand_limits(y = 0)+
    facet_grid(ion ~ transect, scales = "free_y")+
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
    ggplot(aes(x = timepoint, y = pH, color = horizon, shape = horizon))+
      geom_point(size = 2, stroke = 1)+
      scale_shape_manual(values = c(19, 1))+
      scale_color_manual(values = pal_horizons)+
      facet_wrap(. ~ transect)+
    labs(title = "pH",
         x = "", 
         y = "pH")  
  
}
