##########################################################################
# SETTING UP FOR REGRESSION ON TARIFF-CUT EXPOSURE AND SCHOOL ENROLLMENT #
##########################################################################

# Merging school enrollment data with weights data 

schooling_02 <- merge(schooling_02, weights_02, by = c("tinh02", "xa02", "diaban02", "huyen02"))

schooling_04 <- merge(schooling_04, weights_04, by = c("tinh", "huyen", "xa")) %>% 
  mutate(across(tinh, as.factor))

schooling_06 <- merge(schooling_06, weights_06, by = c("tinh", "huyen", "xa")) %>% 
  mutate(across(tinh, as.factor))  

# Merging school enrollment data with province-level tariff data 
schooling_02 <- schooling_02 %>% 
  rename(tinh = tinh02) %>% 
  mutate(across(tinh, as.factor))

schooling_02 <- list(schooling_02, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

schooling_04 <- list(schooling_04, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)  

schooling_06 <- list(schooling_06, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)  

# Creating panel 
## 2002 - 2004 
schooling_02 <- schooling_02 %>% 
  rename(ivid02 = ivid, 
         hhid02 = hhid)

schooling_0204 <- merge(ivid0204, schooling_02, by = c("hhid02", "ivid02", "tinh", "xa02")) %>% 
  mutate(year = 2002 )
schooling_0402 <- merge(ivid0204, schooling_04, by = c("tinh", "huyen", "xa", "hhid", "ivid")) %>% 
  mutate(year = 2004)

# 2002 - 2006 
schooling_06 <- schooling_06 %>% 
  rename(ivid06 = ivid)

schooling_0206 <- merge(ivid020406, schooling_02, by = "ivid02") %>% 
  mutate(year = 2002)  
schooling_0206 <- merge(hhid020406, schooling_0206,  by = "hhid02") %>% 
  distinct()
schooling_0602 <- merge(ivid020406, schooling_06, by = "ivid06") %>% 
  mutate(year = 2006) %>% 
  rename(hhid06 = hhid)

# Panel data 

schooling_0204_p <- bind_rows(schooling_0204, schooling_0402)
schooling_0206_p <- bind_rows(schooling_0206, schooling_0602)

############################################################################
# REGRESSION ON TARIFF-CUT EXPOSURE AND SCHOOL ENROLLMENT USING PANEL DATA #
############################################################################

etable(list(
  feols(enrolled ~ i(Female, provtariff) | ivid02 + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff_k) | ivid02 + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff) | ivid02 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff_k) | ivid02 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)

etable(list(
  feols(enrolled ~ i(Female, provtariff)| ivid02 + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff_k) | ivid02 + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff) | ivid02 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff_k) | ivid02 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)

etable(list(
  feols(enrolled ~ i(Female, provtariff)| hhid + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff_k) | hhid + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff) | hhid06 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(Female, provtariff_k) | hhid06 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)


#################################################################
# REGRESSION ON TARIFF-CUT EXPOSURE AND EDUCATIONAL EXPENDITURE #
#################################################################

etable(list(
  feols(log(educ_exp) ~ i(Female, provtariff) | ivid02 + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff_k) | ivid02 + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff) | ivid02 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff_k) | ivid02 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)

etable(list(
  feols(log(educ_exp) ~ i(Female, provtariff) | ivid02 + year,
        subset(schooling_0204_p, enrolled == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff_k) | ivid02 + year,
        subset(schooling_0204_p, enrolled == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff) | ivid02 + year,
        subset(schooling_0206_p, enrolled == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff_k) | ivid02 + year,
        subset(schooling_0206_p, enrolled == 1),
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)

etable(list(
  feols(log(educ_exp) ~ i(Female, provtariff) | hhid + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff_k)  | hhid + year,
        schooling_0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff)  | hhid06 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(Female, provtariff_k)  | hhid06 + year,
        schooling_0206_p,
        weights = ~hhwt,
        vcov = ~tinh)),
  tex = TRUE)


######################################################################################
# REGRESSION ON TARIFF-CUT EXPOSURE AND SCHOOL ENROLLMENT USING CROSS-SECTIONAL DATA #
######################################################################################

