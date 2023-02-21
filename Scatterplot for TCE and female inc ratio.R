inc_02_spouse_prov <- inc_02_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female") %>% 
  group_by(tinh) %>% 
  summarise(inc_ratio_02 = weighted.mean(inc_ratio, hhwt))

inc_02_spouse_prov <- merge(inc_02_spouse_prov, preBTA_provtariff, by = "tinh")

inc_06_spouse_prov <- inc_06_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female") %>% 
  group_by(tinh) %>% 
  summarise(inc_ratio_06 = weighted.mean(inc_ratio, hhwt))

inc_06_spouse_prov <- merge(inc_06_spouse_prov, postBTA_provtariff, by = "tinh")

inc_0206_spouse_prov <- merge(inc_02_spouse_prov, inc_06_spouse_prov, by = "tinh") %>% 
  mutate(TCE = postprov_tariff - preprov_tariff,
         inc_ratio_change = inc_ratio_06 - inc_ratio_02)

ggplot(inc_0206_spouse_prov, aes(x = TCE*100, y = inc_ratio_change*100, colour = tinh)) +
  geom_point()+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(x = "Change in province-level tariff",
       y = "Change in province-level female income as a share of her total household income") +
  theme(legend.position = "none")
