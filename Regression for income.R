######################################
# SETING UP FOR REGRESSION ON INCOME #
######################################

# Cross-sectional data 
inc0204 <- bind_rows(inc02, inc04)

inc0206 <- bind_rows(inc02, inc06)

# Panel data 
inc02_p <- merge(ivid02, inc02, by = c("ivid02", "hhid02")) %>% 
  mutate(year = 2002)

ivid0206p <- merge(ivid020406, inc02, by = "ivid02")

ivid0206p <- merge(ivid0206p, hhid020406, by = "hhid02")

inc04_p <- merge(ivid0204, inc04, by = c("tinh", "huyen", "xa", "hoso", "matv", "ivid", "hhid")) %>% 
  mutate(year = 2004) %>% 
  mutate(across(tinh, as.factor))

inc06_p <- inc06 %>% 
  rename(ivid06 = ivid)

inc06_p <- merge(ivid020406, inc06_p, by = "ivid06")

inc06_p <- inc06_p %>% 
  mutate(year = 2006) %>% 
  mutate(across(tinh, as.factor)) %>% 
  rename(hhid06 = hhid)

inc0402_p <- bind_rows(inc02_p, inc04_p) %>% 
  mutate(Female = as.numeric(sex == "Female"))

inc0602_p <- bind_rows(ivid0206p, inc06_p) %>% 
  mutate(Female = as.numeric(sex == "Female"))

########################################################################################
# SETING UP FOR REGRESSION ON INCOME OF WOMEN AS A SHARE OF HER TOTAL HOUSEHOLD INCOME #
########################################################################################

inc_02_spouse <- inc02 %>%
  select(hhid02, ivid02,tinh, hhwt, sex, m1c3, provtariff, provtariff_k, year, totalinc, tal, agri_work, manu, traded_manu, wage_work, urban, educ, age)

m5aho_02 <- m5aho_02 %>% 
  rename(hhid02 = hhid) %>%
  mutate(across(tinh, as.factor))

m5d_02 <- m5d_02 %>% 
  rename(hhid02 = hhid) %>%
  mutate(across(tinh, as.factor))

inc_02_spouse <- list(inc_02_spouse, m5aho_02, m5d_02) %>% 
  reduce(full_join, by = c("tinh", "hhid02")) %>% 
  distinct() %>% 
  mutate(totalinc_hh = m5ac7e + m5ac9 + m5ac10 + m5d1cong + m5d2cong,
         inc_ratio = totalinc/totalinc_hh) %>%
  select(-ends_with(".x")) %>% 
  select(-ends_with(".y"))

#2004
id_04 <- m123a_04 %>%
  select(ivid, m1ac3)

inc04 <- merge(id_04, inc04, by = "ivid") %>%
  distinct()

hhinc_04 <- ho1_04 %>% 
  select(hhid, thunhap) %>% 
  rename(totalinc_hh = thunhap)

inc_04_spouse <- merge(inc04, hhinc_04, by  = "hhid") %>% 
  mutate(inc_ratio = totalinc/totalinc_hh)

# 2006
hhinc_06 <- ttchung_06 %>% 
  select(hhid, thunhap) %>% 
  rename(hhid06 = hhid,
         totalinc_hh = thunhap)

inc06 <- inc06 %>% 
  rename(hhid06 = hhid)

inc_06_spouse <- merge(inc06, hhinc_06, by = "hhid06") %>% 
  mutate(inc_ratio = totalinc/totalinc_hh) %>% 
  rename(ivid06 = ivid)

# Constructing panel data 
## 2002 - 2004 
inc_02_spouse_p <- merge(hhid0204, inc_02_spouse, by = "hhid02") %>% 
  rename(tinh = tinh.x) %>% 
  select(-c("tinh.y")) %>% 
  distinct() %>% 
  mutate(across(tinh, as.factor)) %>% 
  mutate(year = 2002)

inc_04_spouse_p <- merge(hhid0204, inc_04_spouse, by = c("tinh", "huyen", "xa", "hoso", "hhid")) %>% 
  distinct() %>% 
  mutate(year = 2004) %>% 
  mutate(across(tinh, as.factor))

inc_0204_spouse_p <- bind_rows(inc_02_spouse_p, inc_04_spouse_p) %>% 
  mutate(Female = as.numeric(sex == "Female"))

## 2002 - 2006 
inc_0602_spouse_p <- merge(hhid020406, inc_02_spouse, by = "hhid02") %>% 
  mutate(year = 2002)

inc_06_spouse_p <- merge(hhid020406, inc_06_spouse, by = "hhid06") %>% 
  mutate(year = 2006)

inc_0206_spouse_p <- bind_rows(inc_0602_spouse_p, inc_06_spouse_p) %>% 
  mutate(Female = as.numeric(sex == "Female"))

#####################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING PANEL DATA #
#####################################################################################

etable(list(
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, Female == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

#################################################################
# REGRESSION ON SPOUSAL WAGE GAP - BY SECTOR - USING PANEL DATA #
#################################################################

# 2002 - 2004 
etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & agri_work == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & agri_work == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),  
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & manu == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & manu == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),  
  feols(inc_ratio ~ provtariff| hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & tal == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k| hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & tal == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

###############################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING CROSS-SECTIONAL DATA #
###############################################################################################

###########################################################################
# REGRESSION ON TARIFF CUT EXPOSURE AND INCOME USING CROSS-SECTIONAL DATA #
###########################################################################

etable(list(
  feols(log(totalinc) ~ factor(sex)*provtariff + factor(urban) + educ + age + age^2| year + tinh,
              data = inc0204,
              vcov = ~tinh,
              weights = ~hhwt),
  feols(log(totalinc) ~ factor(sex)*provtariff_k + factor(urban) + educ + age + age^2| year + tinh,
                          data = inc0204,
                          vcov = ~tinh,
                          weights = ~hhwt),
  feols(log(totalinc) ~ factor(sex)*provtariff + educ + age + age^2 + factor(urban) | year + tinh,
        inc0206,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(totalinc) ~ factor(sex)*provtariff_k + educ + age + age^2 + factor(urban) | year + tinh,
        inc0206,
        vcov = ~tinh,
        weights = ~hhwt)),
  tex = TRUE)
