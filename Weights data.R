###############################
# SETTING UP DATA FOR WEIGHTS #
###############################

# 2002 
weights_02 <- inc_02 %>%
  select(tinh02, xa02, diaban02, huyen02, wt75, urban) %>% 
  rename(hhwt = wt75) %>% 
  distinct()

weights_exp_02 <- exp_02 %>% 
  select(tinh, huyen, xa, hoso, wt30) %>% 
  distinct() %>% 
  rename(hhwt_p = wt30) %>% 
  mutate(year = 2002)

# 2004 
weights_04 <- inc_04 %>%
  select(tinh, huyen, xa, wt45, urban) %>% 
  rename(hhwt = wt45) %>% 
  distinct() 

weights_exp_04 <- exp_04 %>% 
  select(tinh, huyen, xa, hoso, hhwt) %>% 
  rename(hhwt_p = hhwt) %>% 
  mutate(year = 2004)

# 2006 
weights_06 <- exp_06 %>%
  select(tinh, huyen, xa, wt45, urban06) %>% 
  rename("urban" = urban06,
         "hhwt" = wt45) %>% 
  distinct()  

weights_exp_06 <- exp_06 %>% 
  select(tinh, huyen, xa, hoso, wt9) %>% 
  rename(hhwt_p = wt9) %>% 
  mutate(year = 2006)

# Weights for panel
weights0204_p <- bind_rows(weights_exp_02, weights_exp_04)
weights0206_p <- bind_rows(weights_exp_02, weights_exp_06)
