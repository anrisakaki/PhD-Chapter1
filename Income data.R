#############################################
# SETTING UP REGIONAL AND MONTHLY DEFLATORS #
#############################################

rcpi_02 <- inc_02 %>% 
  mutate(total_income = rlincomepc * hhsize) %>% 
  select(hhid, rcpi, mcpi, total_income) %>% 
  rename(hhid02 = hhid) %>% 
  distinct()

rcpi_04 <- inc_04 %>%
  mutate(total_income = rlincomepc * hhsize) %>%   
  select(hhid, rcpi, mcpi, total_income) %>% 
  distinct()

rcpi_06 <- inc_06 %>% 
  mutate(total_income = rlincomepc * hhsize) %>%     
  select(tinh, huyen, xa, hoso, rcpi, mcpi, total_income) %>% 
  distinct() 

################################################
# CREATING DATAFRAME FOR INCOME AND EMPLOYMENT #
################################################

# 2002 
inc02 <- m5a_02 %>% 
  mutate(totalinc = m5ac6 + m5ac7e) %>% 
  select(xa02, huyen02, matv, hhid, ivid, totalinc) %>% 
  rename(hhid02 = hhid, 
         ivid02 = ivid)

inc02 <- merge(employment_mf_02, inc02, by = c("xa02", "matv", "hhid02", "ivid02")) %>% 
  distinct()

inc02 <- list(inc02, rcpi_02, inc_hh) %>% 
  reduce(left_join, by = "hhid02") %>% 
  distinct() %>% 
  mutate(income = (totalinc / rcpi / mcpi/12),
         hhinc = (hhinc / rcpi/mcpi),
         female = ifelse(sex == "Female", 1, 0)) %>% 
  select(tinh, hhid02, ivid02, female, wage_work, married, educ, income, total_income, hhinc, agri_work, tal, manu, traded, traded_nonagri, provtariff, provtariff_k, provtariff_f, provtariff_fk, hhwt, urban)# N = 163,501

# 2004 
inc04 <- m4a_04 %>% 
  mutate(totalinc = (m4ac11) + m4ac12e) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, totalinc)

inc04 <- merge(employment_mf_04, inc04, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid"), all.x = TRUE) %>% 
  distinct() # N = 101,799

inc_04_hh <- inc04 %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(totalinc = ifelse(is.na(totalinc), 0, totalinc)) %>% 
  summarise(hhinc = sum(totalinc)) %>% 
  mutate(hhinc = ifelse(hhinc == 0, NA, hhinc/12))

inc04 <- list(inc04, rcpi_04, inc_04_hh) %>%
  reduce(left_join, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  mutate(income = (totalinc / rcpi / mcpi / 12),
         hhinc = (hhinc / rcpi/mcpi),
         female = ifelse(sex == "Female", 1, 0)) %>% 
  distinct() %>% 
  select(tinh, hhid, ivid, female, wage_work, married, educ, income, total_income, hhinc, agri_work, tal, manu, traded, traded_nonagri, provtariff, provtariff_k, provtariff_f, provtariff_fk, hhwt, urban)# N = 163,501


# 2006 
inc06 <- m4a_06 %>% 
  mutate(totalinc = (m4ac11) + m4ac12f) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, totalinc) %>% 
  mutate(across(c(huyen, xa, hoso, matv), as.numeric))

inc06 <- merge(employment_mf_06, inc06, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid"), all.x = TRUE) %>% 
  distinct()

inc_06_hh <- inc06 %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(totalinc = ifelse(is.na(totalinc), 0, totalinc)) %>% 
  summarise(hhinc = sum(totalinc)) %>% 
  mutate(hhinc = ifelse(hhinc == 0, NA, hhinc/12))

inc06 <- list(inc06, rcpi_06, inc_06_hh) %>% 
  reduce(left_join, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  distinct() %>% 
  mutate(income = totalinc / rcpi / mcpi / 12,
         female = ifelse(sex == "Female", 1, 0)) %>% 
  rename(married = m1ac6) %>% 
  select(tinh, hhid, ivid, female, wage_work, married, educ, income, total_income, hhinc, agri_work, tal, manu, traded, traded_nonagri, provtariff, provtariff_k, provtariff_f, provtariff_fk, hhwt, urban)# N = 163,501

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