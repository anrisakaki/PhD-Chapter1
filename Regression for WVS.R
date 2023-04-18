####################################
# SETTING UP FOR REGRESSION OF WVS #
####################################

WVS_01 <- WVS_01 %>% 
  select(-"tinh")

WVS_01 <- merge(WVS_01, WVS_preBTA_tariffs, by = "XVIE_V225D")

WVS_05 <- WVS_05 %>% 
  select(-"tinh")

WVS_05 <- merge(WVS_05, WVS_postBTA_tariffs, by = "V257")

# Renaming variables 

WVS_01 <- WVS_01 %>% 
  rename(job_scarce = V78,
         working_mother = V115,
         housewife = V116,
         both_contribute = V117,
         provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k, 
         sex = V223,
         age = V225, 
         educ = V226,
         emp = V230,
         divorce = V211) %>% 
  mutate(female = as.numeric(sex > 1),
         year = 2001) %>% 
  select(tinh, provtariff, provtariff_k, female, emp, age, educ, housewife, job_scarce, divorce, year)

WVS_05 <- WVS_05 %>% 
  rename(job_scarce = V44,
         housewife = V60,
         provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k,
         sex = V235,
         educ = V238,
         age = V237,
         emp = V242,
         divorce = V205) %>% 
  mutate(female = as.numeric(sex > 1),
         year = 2005) %>% 
  select(tinh, provtariff, provtariff_k, female, emp, age, educ, housewife, job_scarce, divorce, year)  

WVS_0105 <- bind_rows(WVS_01, WVS_05) %>% 
  mutate(housewife = as.numeric(housewife < 3),
         job_scarce = as.numeric(job_scarce < 2)) %>% 
  filter(divorce = as.numeric(divorce == 1, 1, 0))

#####################
# REGRESSION OF WVS #
#####################

# Housewife 

etable(list(
  feols(housewife ~ provtariff + as.factor(female) + age + as.factor(educ) + as.factor(emp) | year + tinh,
        WVS_0105, 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + as.factor(female) + age + as.factor(educ) + as.factor(emp) | year + tinh,
        WVS_0105, 
        vcov = ~tinh)
), tex = TRUE)


etable(list(
  feols(housewife ~ provtariff + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh)
), tex = TRUE)

etable(list(
  feols(housewife ~ provtariff + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 0), 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 0), 
        vcov = ~tinh)
), tex = TRUE)

# Job scarcity 

etable(list(
  feols(job_scarce ~ provtariff + as.factor(female) + age + as.factor(educ) + as.factor(emp) | year + tinh,
        WVS_0105, 
        vcov = ~tinh),
  feols(job_scarce ~ provtariff_k + as.factor(female) + age + as.factor(educ) + as.factor(emp) | year + tinh,
        WVS_0105, 
        vcov = ~tinh)
), tex = TRUE)

etable(list(
  feols(job_scarce ~ provtariff + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh),
  feols(job_scarce ~ provtariff_k + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh)
), tex = TRUE)

etable(list(
  feols(job_scarce ~ provtariff + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 0), 
        vcov = ~tinh),
  feols(job_scarce ~ provtariff_k + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 0), 
        vcov = ~tinh)
), tex = TRUE)

# Divorce 

etable(list(
  feols(divorce ~ provtariff + as.factor(female) + age + as.factor(educ) + as.factor(emp) | year + tinh,
        WVS_0105, 
        vcov = ~tinh),
  feols(divorce ~ provtariff_k + as.factor(female) + age + as.factor(educ) + as.factor(emp) | year + tinh,
        WVS_0105, 
        vcov = ~tinh)
), tex = TRUE)

etable(list(
  feols(divorce ~ provtariff + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh),
  feols(divorce ~ provtariff_k + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh)
), tex = TRUE)

etable(list(
  feols(divorce ~ provtariff + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 0), 
        vcov = ~tinh),
  feols(divorce ~ provtariff_k + age + as.factor(educ) + as.factor(emp) | year + tinh,
        subset(WVS_0105, female == 0), 
        vcov = ~tinh)
), tex = TRUE)
