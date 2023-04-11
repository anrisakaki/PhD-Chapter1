###########################
# EDUCATION-LEVEL OF WIFE # 
###########################

# 2002 
married02 <- m1_02 %>% 
  filter(m1c2 == 2 & m1c6 == 2) %>% 
  select(hhid, ivid)

educ02 <- m2_02 %>% 
  select(hhid, ivid, m2c1)

educ02 <- merge(married02, educ02, by = c("hhid", "ivid")) %>% 
  rename(hhid02 = hhid)

# 2004 
educ04 <- m123a_04 %>% 
  filter(m1ac6 == 2 & m1ac2 == 2) %>% 
  select(hhid, ivid, m2c1) %>% 
  rename(educ = m2c1)

# 2006 
married06 <- m1a_06 %>% 
  filter(m1ac6 == 2 & m1ac2 == 2) %>% 
  select(hhid, ivid)

educ06 <- m2a_06 %>% 
  select(hhid, ivid, m2ac1) %>% 
  rename(educ = m2ac1)

educ06 <- merge(married06, educ06, by = c("hhid", "ivid"))

#######################################################################
# MERGING EDUCATION AND MARRIAGE DATA WITH HOUSEHOLD EXPENDITURE DATA #
#######################################################################

exp_spouse_02 <- merge(educ02, exp_02, by = "hhid02") %>% 
  distinct()

exp_spouse_04 <- merge(educ04, exp_04, by = "hhid")

exp_spouse_06 <- merge(educ06, exp_06, by = "hhid")
