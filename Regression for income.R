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

inc0402_p <- bind_rows(inc02_p, inc04_p)

inc0602_p <- bind_rows(ivid0206p, inc06_p)

########################################################################################
# SETING UP FOR REGRESSION ON INCOME OF WOMEN AS A SHARE OF HER TOTAL HOUSEHOLD INCOME #
########################################################################################

inc_02_fspouse <- inc02 %>%
  filter(m1c3 == 1 | m1c3 == 2) %>%
  filter(sex == "Female") %>%
  filter(married == 2) %>%
  select(hhid02, ivid02, tinh, hhwt, provtariff, provtariff_k, year, totalinc, tal) %>%
  rename(totalinc_wife = totalinc)

m5aho_02 <- m5aho_02 %>% 
  rename(hhid02 = hhid)

inc_02_fspouse <- merge(inc_02_fspouse, m5aho_02, by = c("tinh", "hhid02")) %>% 
  mutate(totalinc_hh = m5ac7e + m5ac9,
         finc_ratio = totalinc_wife/totalinc_hh)

#2004
id_04 <- m123a_04 %>%
  select(ivid, m1ac3)

inc04 <- merge(id_04, inc04, by = "ivid") %>%
  distinct()

inc_04_fspouse <- inc04 %>%
  filter(m1ac3 == 1 | m1ac3 == 2,
         married == 2,
         sex == "Female") %>%
  select(hhid, ivid, tinh, hhwt, provtariff, provtariff_k, year, totalinc, tal) %>%
  rename(totalinc_wife = totalinc)

hhinc_04 <- ho1_04 %>% 
  select(hhid, m4atn) %>% 
  rename(totalinc_hh = m4atn)

inc_04_fspouse <- merge(inc_04_fspouse, hhinc_04, by = "hhid") %>% 
  mutate(finc_ratio = totalinc_wife/totalinc_hh)

# 2006
inc_06_fspouse <- inc06 %>%
  filter(m1ac6 == 2,
         m1ac3 == 1 | m1ac3 == 2,
         sex == "Female") %>%
  select(hhid, ivid, tinh, hhwt, provtariff, provtariff_k, year, totalinc, tal) %>%
  rename(totalinc_wife = totalinc,
         hhid06 = hhid,
         ivid06 = ivid)

hhinc_06 <- ttchung_06 %>% 
  select(hhid, m4atn) %>% 
  rename(hhid06 = hhid,
         totalinc_hh = m4atn)

inc_06_fspouse <- merge(inc_06_fspouse, hhinc_06, by = "hhid06") %>% 
  mutate(finc_ratio = totalinc_wife/totalinc_hh)  

# 2002 - 2004 
inc_hhid0204 <- hhid0204 %>% 
  select(hhid02, hhid)

inc_spouse_0204_p <- merge(inc_hhid0204, inc_02_fspouse, by = "hhid02") %>% 
  distinct()
inc_spouse_0402_p <- merge(inc_hhid0204, inc_04_fspouse, by = "hhid") %>% 
  distinct()

inc_spouse_0402 <- bind_rows(inc_spouse_0204_p, inc_spouse_0402_p)

# 2002 - 2006 

inc_spouse_0206_p <- merge(ivid020406, inc_02_fspouse, by = "ivid02")
inc_spouse_0602_p <- merge(ivid020406, inc_06_fspouse, by = "ivid06")

inc_spouse_0602 <- bind_rows(inc_spouse_0206_p, inc_spouse_0602_p)

#################################################################
# REGRESSION ON TARIFF CUT EXPOSURE AND INCOME USING PANEL DATA #
#################################################################

# Topalova tariffs 
## 2002 - 2004 
etable(list(
  feols(log(totalinc) ~ factor(sex)*provtariff | ivid02 + year,
                           data = inc0402_p,
                           weights = ~hhwt,
                           vcov = ~tinh),
  feols(log(totalinc) ~ factor(sex)*provtariff | hhid02 + year,
        data = inc0402_p,
        weights = ~hhwt, 
        vcov = ~tinh)),
  tex = TRUE) 

## 2002 - 2006 
etable(list(
  feols(log(totalinc) ~ factor(sex)*provtariff | hhid06 + year,
        data = inc0602_p,
        weights = ~hhwt, 
        vcov = ~tinh),
  
  feols(log(totalinc) ~ factor(sex)*provtariff | ivid02 + year,
        data = inc0602_p,
        weights = ~hhwt,
        vcov = ~tinh)
),
tex = TRUE)

# Kovak tariffs
## 2002 - 2004 
etable(list(
  feols(log(totalinc) ~ factor(sex)*provtariff_k | hhid02 + year,
        data = inc0402_p,
        weights = ~hhwt, 
        vcov = ~tinh),
  
  feols(log(totalinc) ~ factor(sex)*provtariff_k | ivid02 + year,
                             data = inc0402_p,
                             weights = ~hhwt,
                             vcov = ~tinh)
),
tex = TRUE)

# 2002 - 2006 
etable(list(
  feols(log(totalinc) ~ factor(sex)/provtariff_k | hhid06 + year,
        data = inc0602_p,
        weights = ~hhwt, 
        vcov = ~tinh),
  
  feols(log(totalinc) ~ factor(sex)*provtariff_k | ivid02 + year,
        data = inc0602_p,
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)

#####################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING PANEL DATA #
#####################################################################################

# 2002 - 2004 
etable(list(
  feols(
    log(finc_ratio) ~ provtariff | hhid02 + year,
                  inc_spouse_0402,
                  vcov = ~tinh,
                  weights = ~hhwt),
  feols(log(finc_ratio) ~ provtariff_k | hhid02 + year,
            inc_spouse_0402,
            vcov = ~tinh,
            weights = ~hhwt),
  feols(log(finc_ratio) ~ provtariff | ivid02 + year,
        inc_spouse_0602,
        vcov = ~tinh,
        weights = ~hhwt),
  
  feols(log(finc_ratio) ~ provtariff_k | ivid02 + year,
        inc_spouse_0602,
        vcov = ~tinh,
        weights = ~hhwt)),
  tex = TRUE)

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
