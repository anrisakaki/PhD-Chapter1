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
  rename(hhwt_p = wt30)

# 2004 
weights_04 <- inc_04 %>%
  select(tinh, huyen, xa, wt45, urban) %>% 
  rename(hhwt = wt45) %>% 
  distinct() 

weights_exp_04 <- exp_04 %>% 
  select(tinh, huyen, xa, hoso, hhwt) %>% 
  rename(hhwt_p = hhwt)

# 2006 
weights_06 <- exp_06 %>%
  select(tinh, huyen, xa, wt45, urban06) %>% 
  rename("urban" = urban06,
         "hhwt" = wt45) %>% 
  distinct()  

weights_exp_06 <- exp_06 %>% 
  select(tinh, huyen, xa, hoso, wt9) %>% 
  rename(hhwt_p = wt9) 
