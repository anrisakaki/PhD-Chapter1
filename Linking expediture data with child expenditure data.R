######################################################################
# CONSTRUCTING DATABASE OF CHILDREN AND THEIR EDUCATIONAL ATTAINMENTS#
######################################################################

# 2002 
juniors_02 <- m1_02 %>% 
  select(tinh02, xa02, diaban02, huyen02, hhid, ivid, m1c2, m1c5) %>% 
  rename(Female = m1c2,
         age = m1c5) %>% 
  mutate(Female = as.numeric(Female == 2)) %>% 
  filter(age > 5, 
         age <= 18)

schooling_02 <- m2_02 %>% 
  select(hhid, ivid, m2c4, m2c5h) %>% 
  rename(enrolled = m2c4) %>% 
  rename(educ_exp = m2c5h) %>% 
  mutate(enrolled = as.numeric(enrolled == 1)) %>% 
  replace(is.na(.), 0)

schooling_02 <- merge(juniors_02, schooling_02, by = c("hhid", "ivid"))

# 2004 
schooling_04 <- m123a_04 %>% 
  select(tinh, huyen, xa, hhid, ivid, m1ac2, m1ac5, m2c4, m2c11h) %>% 
  rename(Female = m1ac2,
         age = m1ac5, 
         enrolled = m2c4,
         educ_exp = m2c11h) %>% 
  filter(age > 5,
         age <= 18) %>% 
  mutate(Female = as.numeric(Female == 2),
         enrolled = as.numeric(enrolled < 3),
         educ_exp = ifelse(is.na(educ_exp), 0 , educ_exp))

# 2006 
juniors_06 <- m1a_06 %>% 
  select(tinh, huyen, xa, hhid, ivid, m1ac2, m1ac5) %>% 
  rename(Female = m1ac2,
         age = m1ac5) %>% 
  mutate(Female = as.numeric(Female == 2)) %>% 
  filter(age > 5,
         age <= 18)

schooling_06 <- m2a_06 %>% 
  select(hhid, ivid, m2ac5, m2ac13k) %>% 
  rename(enrolled = m2ac5,
         educ_exp = m2ac13k) %>% 
  mutate(enrolled = as.numeric(enrolled < 3),
         educ_exp = ifelse(is.na(educ_exp), 0 , educ_exp))

schooling_06 <- merge(juniors_06, schooling_06, by = c("hhid", "ivid"))

