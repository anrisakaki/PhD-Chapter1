######################
# REALLOCATION PANEL #
######################

reallocation_f_02_p <-  employment0206_p %>% 
  filter(year == 2002,
         sex == "Female",
         agri_work == 1 | industry2 == 0) %>% 
  select(ivid02)

reallocation_f_06_p <-  employment0206_p %>% 
  filter(year == 2006,
         sex == "Female") %>% 
  mutate(reallocated_tal = as.numeric(tal == 1)) %>% 
  select(ivid02, reallocated_tal)

reallocated_f_0206 <- merge(reallocation_f_02_p, reallocation_f_06_p, by = "ivid02")
