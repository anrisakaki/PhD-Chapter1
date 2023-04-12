###########################
# EDUCATION-LEVEL OF WIFE # 
###########################

# 2002 
educ02 <- m2_02 %>% 
  select(hhid, ivid, m2c1) %>% 
  rename(educ = m2c1,
         hhid02 = hhid)

# 2004 
educ04 <- m123a_04 %>% 
  select(hhid, ivid, m2c1) %>% 
  rename(educ = m2c1)

# 2006
educ06 <- m2a_06 %>% 
  select(hhid, ivid, m2ac1) %>% 
  rename(educ = m2ac1)

#######################################################################
# MERGING EDUCATION AND MARRIAGE DATA WITH HOUSEHOLD EXPENDITURE DATA #
#######################################################################

exp_spouse_02 <- merge(educ02, exp_02, by = "hhid02") %>% 
  distinct()

exp_spouse_04 <- merge(educ04, exp_04, by = "hhid")

exp_spouse_06 <- merge(educ06, exp_06, by = "hhid")
