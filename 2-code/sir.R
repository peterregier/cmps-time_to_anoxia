

library(tidyverse)
library(googlesheets4)


sir_data = read_sheet("1aKSpp3FVN90XfWjUt-P2Zwtcokhos1g3KzMpsnjfPls", sheet = "egm") %>% mutate_all(as.character)
sir_data_processed = 
  sir_data %>% 
  filter(!notes %in% "skip") %>% 
  filter(!is.na(type)) %>% 
  mutate_at(vars(contains("egm")), as.numeric) %>% 
  mutate_at(vars(contains("weight")), as.numeric) %>% 
  mutate(glucose_ug = as.numeric(glucose_ug),
         respiration = egm_CO2_ppm - egm_ambient_ppm,
         weight_dry_g = weight_vial_and_dry_soil_g - weight_empty_vial_g,
         respiration_g = respiration/weight_dry_g,
         glucose_ug_g = glucose_ug/weight_dry_g)


sir_data_processed %>% 
  ggplot(aes(x = glucose_ug_g/1000, y = respiration_g, color = type))+
  geom_point(size = 3)+ 
  geom_line(size = 1)+
  labs(title = "Substrate-induced respiration",
       x = "Glucose added, mg/g soil",
       y = "CO2 produced, ppm/g-hr")+
  theme_bw()





sir_data %>% 
  ggplot(aes(y = CO2, x = glucose_conc_uM))+
  geom_point()+
  facet_wrap(~type)

sir_data_processed %>% 
  ggplot(aes(y = CO2_diff, x = glucose_conc_uM))+
  geom_point()+
  facet_wrap(~type, scales = "free")
