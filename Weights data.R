###############################
# SETTING UP DATA FOR WEIGHTS #
###############################

# These are weights for cross-sectional observations 

# 2002 
weights_02 <- inc_02 %>%
  select(tinh02, xa02, diaban02, huyen02, wt75, urban) %>% #Weights are determined by tinh, xa, diaban, huyen
  rename(hhwt = wt75) %>% 
  distinct() %>% 
  mutate(xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02)

# 2004 
weights_04 <- inc_04 %>%
  select(tinh, huyen, xa, wt45, urban) %>% 
  rename(hhwt = wt45) %>% 
  distinct() 

# 2006 
weights_06 <- exp_06 %>%
  select(tinh, huyen, xa, wt45, urban06) %>% 
  rename("urban" = urban06,
         "hhwt" = wt45) %>% 
  distinct()  
