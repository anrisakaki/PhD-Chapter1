#############################################################################
# SCATTER PLOT FOR FEMALE INCOME AS SHARE OF TOTAL HOUSEHOLD INCOME AND TCE #
#############################################################################

inc_spouse_02_prov <- inc_spouse_0206_p %>% 
  group_by(tinh) %>% 
  summarise(finc_ratio_prov_02 = mean(finc_ratio, na.rm = TRUE))

inc_spouse_06_prov <- inc_spouse_0602_p %>% 
  group_by(tinh) %>% 
  summarise(finc_ratio_prov_06 = mean(finc_ratio, na.rm = TRUE))

inc_spouse_0602_prov <- list(preBTA_provtariff, postBTA_provtariff, inc_spouse_02_prov, inc_spouse_06_prov) %>% 
  reduce(full_join, by = "tinh") %>% 
  mutate(tce = postprov_tariff - preprov_tariff) %>% 
  mutate(finc_ratio_prov_0602 = finc_ratio_prov_06 - finc_ratio_prov_02)

ggplot(inc_spouse_0602_prov, aes(x = tce*100, y = finc_ratio_prov_0602*100, colour = tinh)) +
  geom_point()+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(x = "Change in province-level tariff",
       y = "Change in province-level female income as a share of her total household income") +
  theme(legend.position = "none")

####################################################################################################
# KDE OF FEMALE INCOME AS SHARE OF TOTAL HOUSEHOLD INCOME FOR WOMEN WHO DID AND DID NOT REALLOCATE #
####################################################################################################

inc_spouse_02_reallocated <- inc_spouse_0206_p %>% 
  select(ivid02, finc_ratio)

inc_spouse_06_reallocated <- inc_spouse_0602_p %>% 
  rename_at(vars(-c("ivid02")), function(x) paste0(x, "_06")) %>% 
  select(ivid02, finc_ratio_06) 

inc_spouse_reallocated_0206 <- list(reallocated_f_0206, inc_spouse_02_reallocated, inc_spouse_06_reallocated) %>% 
  reduce(full_join, by = "ivid02") %>% 
  filter(!is.na(reallocated_tal)) 

ggplot(inc_spouse_reallocated_0206, aes(log(finc_ratio), fill = factor(reallocated_tal))) +
  geom_density(alpha = 0.2) +
  labs(x = "(log) Income as a share of total household income in 2001",
       y = "Density") +
  scale_fill_discrete(type = c("blue", "red"),
                      name="",
                      breaks=c("1", "0"),
                      labels=c("Will not reallocate", "Will reallocate"))
ggsave(file = "KDE-Reallocation-F_Contribution_02.png", device = png, width = 7, height = 7)


ggplot(inc_spouse_reallocated_0206, aes(log(finc_ratio_06), fill = factor(reallocated_tal))) +
  geom_density(alpha = 0.2) +
  labs(x = "(log) Income as a share of total household income in 2005",
       y = "Density") +
  scale_fill_discrete(type = c("blue", "red"),
                      name="",
                      breaks=c("1", "0"),
                      labels=c("Did not reallocate", "Reallocated"))
ggsave(file = "KDE-Reallocation-F_Contribution_06.png", device = png, width = 7, height = 7)
