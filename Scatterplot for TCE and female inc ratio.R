# 2002 
inc_02_spouse_prov <- inc_02_spouse %>% 
  filter(!is.na(inc_ratio)) %>% 
  group_by(tinh) %>% 
  summarise(inc_ratio_02 = weighted.mean(inc_ratio, hhwt))

inc_02_spouse_prov_agri <- inc_02_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female",
         agri_work == 1 ) %>%   
  group_by(tinh) %>% 
  summarise(inc_ratio_02_agri = weighted.mean(inc_ratio, hhwt))

inc_02_spouse_prov_tal <- inc_02_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female",
         tal == 1 ) %>%   
  group_by(tinh) %>% 
  summarise(inc_ratio_02_tal = weighted.mean(inc_ratio, hhwt))

inc_02_spouse_prov <- list(inc_02_spouse_prov, inc_02_spouse_prov_agri, inc_02_spouse_prov_tal, preBTA_provtariff) %>% 
  reduce(full_join, by = "tinh")

#2006
inc_06_spouse_prov <- inc_06_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female") %>% 
  group_by(tinh) %>% 
  summarise(inc_ratio_06 = weighted.mean(inc_ratio, hhwt))

inc_06_spouse_prov_agri <- inc_06_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female",
         agri_work == 1 ) %>%   
  group_by(tinh) %>% 
  summarise(inc_ratio_06_agri = weighted.mean(inc_ratio, hhwt))

inc_06_spouse_prov_tal <- inc_06_spouse %>% 
  filter(!is.na(inc_ratio),
         sex == "Female",
         tal == 1 ) %>%   
  group_by(tinh) %>% 
  summarise(inc_ratio_06_tal = weighted.mean(inc_ratio, hhwt))

inc_06_spouse_prov <- list(inc_06_spouse_prov, inc_06_spouse_prov_agri, inc_06_spouse_prov_tal, postBTA_provtariff) %>% 
  reduce(full_join, by = "tinh")

inc_0206_spouse_prov <- merge(inc_02_spouse_prov, inc_06_spouse_prov, by = "tinh") %>% 
  mutate(TCE = postprov_tariff - preprov_tariff,
         inc_ratio_change = inc_ratio_06 - inc_ratio_02,
         inc_ratio_agri_change = inc_ratio_06_agri - inc_ratio_02_agri,
         inc_ratio_tal_change = inc_ratio_06_tal - inc_ratio_02_tal)

ggplot(inc_0206_spouse_prov, aes(x = TCE*100, y = inc_ratio_change*100, colour = tinh)) +
  geom_point()+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(x = "Change in province-level tariff",
       y = "Change in province-level female income as a share of her total household income") +
  theme(legend.position = "none")
ggsave(file = "Scatter-F_Contribution_0206.png", device = png, width = 7, height = 7)

ggplot(inc_0206_spouse_prov, aes(x = TCE*100, y = inc_ratio_agri_change*100, colour = tinh)) +
  geom_point()+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(x = "Change in province-level tariff",
       y = "Change in province-level female income as a share of her total household income") +
  theme(legend.position = "none")
ggsave(file = "Scatter-F_Contribution_Agri_0206.png", device = png, width = 7, height = 7)

ggplot(inc_0206_spouse_prov, aes(x = TCE*100, y = inc_ratio_tal_change*100, colour = tinh)) +
  geom_point()+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(x = "Change in province-level tariff",
       y = "Change in province-level female income as a share of her total household income") +
  theme(legend.position = "none")
ggsave(file = "Scatter-F_Contribution_TAL_0206.png", device = png, width = 7, height = 7)