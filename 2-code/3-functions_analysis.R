

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



recode_levels = function(dat){
  dat %>% 
    mutate(location = factor(location, 
                             levels = c("upland-A", "upland-B", "transition-A", "wetland-A")),
           timepoint = factor(timepoint, 
                             levels = c("time-zero", "12-hour", "24-hour", "2-week")))
}
