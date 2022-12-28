

source("2-code/0-packages.R")
targets::tar_load_everything()

pal_horizons_agu = soilpalettes::soil_palette("redox2", 2)



# optodes -----------------------------------------------------------------

optode_combined2 = 
  optode_data_processed %>% 
  mutate(sample_type = location) %>% 
  separate(sample_type, sep = "-", into = c("transect", "horizon")) %>% 
  mutate(transect = factor(transect, levels = c("upland", "transition", "wetland"))) %>% 
  arrange(sample_name, time_minutes) %>% 
  group_by(sample_name) %>% 
  mutate(do_rolling = zoo::rollmean(corrected_do_mg_L, k = 7, fill = NA)) %>% 
  arrange(desc(do_rolling)) %>% 
  # change in slope
  # calculate the differential as well as the double differential
  mutate(diffy = lead(do_rolling)-do_rolling,
         diffx = lead(time_minutes) - time_minutes,
         slope = round(diffy/diffx, 3),
         maxslope = slope == max(slope, na.rm = TRUE),
         time_hr = time_minutes/60,
         slope2 = (lead(slope)-slope) / (lead(time_minutes) - time_minutes),
         slope2 = round(slope2, 3))

# plot of slopes
optode_combined2 %>% 
  filter(!grepl("skip", notes)) %>% 
  filter(!is.na(location)) %>% 
  filter(location != "water") %>% 
  filter(timepoint == c("24-hour")) %>% 
  filter(timepoint == c("24-hour", "2-week-rep")) %>% 
  filter(horizon == "A") %>% 
  ggplot(aes(x = time_hr, y = slope, color = sample_name, group = sample_name))+
  geom_line()+
#  scale_color_manual(values = pal_horizons)+
  labs(#title = "Time to Anoxia",
    x = "Elapsed time, hours",
    y = "slope")+
  #geom_smooth(se = F)+
  facet_grid(. ~ transect)+
  theme(legend.position = "none")  +
  facet_wrap(~sample_name)


optode_do_summary = 
  optode_combined2 %>% 
  filter(!is.na(location)) %>% 
  filter(location != "upland-B") %>% 
  filter(timepoint == "24-hour") %>% 
  group_by(sample_name) %>% 
  filter(slope == 0 & slope2 == 0) %>% 
  group_by(sample_name) %>% 
  slice(which.min(time_hr)) %>% 
  dplyr::select(sample_name, location, timepoint, transect, horizon, time_hr) %>% 
  mutate(time_hr = case_when(sample_name == "anoxia_047" ~ 24, TRUE ~ time_hr)) %>% 
  group_by(transect, horizon) %>% 
  dplyr::summarise(hours_mean = mean(time_hr),
                   hours_min = min(time_hr),
                   hours_max = max(time_hr))


# plot - A horizons only, no mean arrows
gg_optode_a = 
  optode_combined2 %>% 
  filter(!grepl("skip", notes)) %>% 
  filter(!is.na(location)) %>% 
  filter(location != "water") %>% 
  filter(timepoint == c("24-hour")) %>% 
 # filter(timepoint == c("24-hour", "2-week-rep")) %>% 
  filter(horizon == "A") %>% 
  ggplot(aes(x = time_minutes/60, y = do_rolling, color = horizon, group = sample_name))+
  geom_line()+
  scale_color_manual(values = pal_horizons_agu)+
  labs(#title = "Time to Anoxia",
    x = "Elapsed time, hours",
    y = "Dissolved oxygen, mg/L")+
  ylim(0,10)+
  facet_grid(. ~ transect)+
  theme(legend.position = "none")  

gg_optode_a_arrows = 
  gg_optode_a +
  geom_segment(data = optode_do_summary,
               aes(y = 0.5, yend = 2.5, group = transect, x = hours_mean, xend = hours_mean),
               color = "black", size = 1, arrow = arrow(ends = "first", length = unit(3, "mm")))

gg_optode_ab = 
  optode_combined2 %>% 
  filter(!grepl("skip", notes)) %>% 
  filter(!is.na(location)) %>% 
  filter(location != "water") %>% 
  filter(timepoint == c("24-hour")) %>% 
  # filter(timepoint == c("24-hour", "2-week-rep")) %>% 
  # filter(horizon == "A") %>% 
  ggplot(aes(x = time_minutes/60, y = do_rolling, color = horizon, group = sample_name))+
  geom_line()+
  scale_color_manual(values = pal_horizons_agu)+
  labs(#title = "Time to Anoxia",
    x = "Elapsed time, hours",
    y = "Dissolved oxygen, mg/L")+
  ylim(0,10)+
  facet_grid(. ~ transect)+
  theme(legend.position = "none")  

gg_optode_upland_2wk = 
  optode_combined2 %>% 
  filter(!grepl("skip", notes)) %>% 
  filter(!is.na(location)) %>% 
  filter(transect == "upland") %>% 
  # filter(timepoint == c("24-hour")) %>% 
   filter(timepoint == c("2-week-rep")) %>% 
  # filter(horizon == "A") %>% 
  ggplot(aes(x = time_minutes/60/24, y = do_rolling, color = horizon, group = sample_name))+
  geom_line()+
  scale_color_manual(values = pal_horizons_agu)+
  labs(#title = "Time to Anoxia",
    x = "Elapsed time, days",
    y = "")+
  ylim(0,10)+
  facet_grid(. ~ transect)+
  theme(legend.position = "none")  


ggsave("agu/optode-1-ahz_24hr.png", plot = gg_optode_a, height = 4, width = 12)
ggsave("agu/optode-2-ahz_arrow_24hr.png", plot = gg_optode_a_arrows, height = 4, width = 12)
ggsave("agu/optode-3-abhz_24hr.png", plot = gg_optode_ab, height = 4, width = 12)
ggsave("agu/optode-4-upland_2wk.png", plot = gg_optode_upland_2wk, height = 4, width = 4)




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


