# SETTING UP FOR REGRESSION ON HOUSEWORK # 

housework_02 <- employment_mf_02 %>%
  mutate(housework = ifelse(m3c12 == "1", 1, 0),
         female = ifelse(sex == "Female", 1, 0),
         married = ifelse(m1c6 == "2", 1, 0)) %>% 
  select(tinh, ivid02, female, married, housework, hhwt)

housework_04 <- m123a_04 %>%
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0)) %>% 
  select(tinh, ivid, female, married)

housework_04a <- m4a_04 %>% 
  mutate(housework = ifelse(m4ac26 == "1", 1, 0))