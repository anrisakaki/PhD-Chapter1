#############################################
# SETTING UP REGIONAL AND MONTHLY DEFLATORS #
#############################################

rcpi_02 <- inc_02 %>% 
  mutate(hhinc = rlincomepc * hhsize * 12) %>% 
  select(hhid, rcpi, mcpi, hhinc) %>% 
  rename(hhid02 = hhid) %>% 
  distinct()

rcpi_04 <- inc_04 %>%
  mutate(hhinc = rlincomepc * hhsize * 12) %>%   
  select(hhid, rcpi, mcpi, hhinc) %>% 
  distinct()

rcpi_06 <- inc_06 %>% 
  mutate(hhinc = rlincomepc * hhsize * 12) %>%     
  select(hhid, rcpi, mcpi, hhinc) %>% 
  distinct() 

################################################
# CREATING DATAFRAME FOR INCOME AND EMPLOYMENT #
################################################

# 2002 
inc02 <- m5a_02 %>% 
  mutate(inc = m5ac6 + m5ac7e) %>% 
  select(hhid, ivid, inc) %>% 
  rename(hhid02 = hhid, 
         ivid02 = ivid)

inc02 <- left_join(employment_mf_02, inc02, by = c("hhid02", "ivid02")) %>% 
  distinct()

inc02 <- left_join(inc02, rcpi_02, by = "hhid02") %>% 
  distinct() %>% 
  mutate(female = ifelse(sex == "Female", 1, 0),
         inc_ratio = inc/hhinc,
         inc_ratio = ifelse(is.na(inc_ratio), 0, inc_ratio)) %>% 
  select(tinh, hhid02, ivid02, female, age, wage_work, married, educ, inc, hhinc, inc_ratio, agri_work, tal, manu, traded, traded_nonagri, provtariff, provtariff_k, provtariff_f, provtariff_fk, hhwt, urban)# N = 163,501

# 2004 
inc04 <- m4a_04 %>% 
  mutate(m4ac11 = ifelse(is.na(m4ac11), 0 , m4ac11),
         m4ac12e = ifelse(is.na(m4ac12e), 0 , m4ac12e),
         inc = m4ac11 + m4ac12e,
         across(tinh, as.factor)) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, inc)

# hhinc_04 <- ho1_04 %>% select(hhid, thunhap)

inc04 <- left_join(inc04, rcpi_04, by = "hhid") %>% distinct()

inc04 <- left_join(employment_mf_04, inc04, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid")) %>% 
  distinct() %>% 
  mutate(inc_ratio = inc/hhinc,
         female = ifelse(sex == "Female", 1, 0)) %>% 
  distinct() %>% 
  select(tinh, hhid, ivid, female, age, wage_work, married, educ, inc, hhinc, inc_ratio, agri_work, tal, manu, traded, traded_nonagri, provtariff, provtariff_k, provtariff_f, provtariff_fk, hhwt, urban)# N = 163,501

# 2006 
inc06 <- m4a_06 %>% 
  mutate(inc = (m4ac11) + m4ac12f) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, inc) %>% 
  mutate(across(c(huyen, xa, hoso, matv), as.numeric),
         across(tinh, as.factor))

# hhinc_06 <- ttchung_06 %>% select(hhid, thunhap)

inc06 <- merge(inc06, rcpi_06, by = "hhid")

inc06 <- left_join(employment_mf_06, inc06, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid")) %>% 
  distinct() %>% 
  mutate(across(tinh, as.double)) %>% 
  mutate(inc_ratio = inc/hhinc,
         inc_ratio = ifelse(is.na(inc_ratio), 0, inc_ratio),
         female = ifelse(sex == "Female", 1, 0)) %>% 
  rename(married = m1ac6) %>% 
  select(tinh, hhid, ivid, female, age, wage_work, married, educ, inc, hhinc, inc_ratio, agri_work, tal, manu, traded, traded_nonagri, provtariff, provtariff_k, provtariff_f, provtariff_fk, hhwt, urban)# N = 163,501

##############################################################
# DESCRIPTIVE STATISTICS FOR INCOME AND SECTOR OF EMPLOYMENT #
##############################################################

inc02 %>%
  filter(agri_work == 1 | tal == 1) %>% 
  group_by(tal, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))

inc02 %>%
  filter(agri_work == 1 | construction == 1) %>% 
  group_by(construction, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))

inc06 %>%
  filter(agri_work == 1 | tal == 1) %>% 
  group_by(tal, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))

inc06 %>%
  filter(agri_work == 1 | construction == 1) %>% 
  group_by(construction, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))