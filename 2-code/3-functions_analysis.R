


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



plot_weoc = function(weoc_processed){
  weoc = 
    weoc_processed %>% 
    left_join(sample_key) %>% 
    recode_levels()
  
  weoc %>% 
    ggplot(aes(x = timepoint, y = npoc_corr_mgL))+
    geom_point()+
    facet_wrap(~location)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, mg/L")
}



