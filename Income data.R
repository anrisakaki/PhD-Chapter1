#############################################
# SETTING UP REGIONAL AND MONTHLY DEFLATORS #
#############################################

rcpi_02 <- inc_02 %>% 
  mutate(xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02) %>% 
  select(xa02, huyen02, rcpi) %>% 
  distinct()

mcpi_02 <- inc_02 %>% 
  select(qui, mcpi) %>% 
  distinct()

rcpi_04 <- inc_04 %>% 
  select(tinh, huyen, xa, rcpi) %>% 
  distinct() %>% 
  mutate(across(tinh, as.factor))

mcpi_04 <- inc_04 %>% 
  select(quimo, mcpi) %>% 
  distinct()

rcpi_06 <- inc_06 %>% 
  select(tinh, huyen, xa, rcpi) %>% 
  distinct()

mcpi_06 <- inc_06 %>% 
  select(monthint, mcpi) %>% 
  distinct()

################################################
# CREATING DATAFRAME FOR INCOME AND EMPLOYMENT #
################################################

# 2002 
inc02 <- m5a_02 %>% 
  mutate(totalinc = (m5ac6) + m5ac7e) %>% 
  select(xa02, matv, hhid, ivid, totalinc) %>% 
  rename(hhid02 = hhid, 
         ivid02 = ivid)

inc02 <- merge(employment_mf_02, inc02, by = c("xa02", "matv", "hhid02", "ivid02"), all.x = TRUE) %>% 
  distinct() %>% 
  rename(huyen02 = huyen)

inc02 <- left_join(inc02, rcpi_02, by = c("xa02", "huyen02")) %>% 
  distinct() %>% 
  mutate(totalinc = totalinc * rcpi) # N = 163,501

# 2004 
inc04 <- m4a_04 %>% 
  mutate(totalinc = (m4ac11) + m4ac12e) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, totalinc)

inc04 <- merge(employment_mf_04, inc04, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid"), all.x = TRUE) %>% 
  distinct() # N = 101,799

inc04 <- left_join(inc04, rcpi_04, by = c("tinh", "huyen", "xa")) %>% 
  mutate(totalinc = totalinc * rcpi) %>% 
  distinct()

# 2006 
inc06 <- m4a_06 %>% 
  mutate(totalinc = (m4ac11) + m4ac12f) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, totalinc) %>% 
  mutate(across(c(huyen, xa, hoso, matv), as.numeric))

inc06 <- merge(employment_mf_06, inc06, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid"), all.x = TRUE) %>% 
  distinct()

inc06 <- merge(inc06, rcpi_06, by = c("tinh", "huyen", "xa")) %>% 
  distinct() %>% 
  mutate(totalinc = totalinc * rcpi)

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
