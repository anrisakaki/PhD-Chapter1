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
         educ = v226) %>% 
  mutate(female = as.numeric(sex > 1),
         year = 2001) %>% 
  select(tinh, provtariff, provtariff_k, female, age, educ, housewife)

WVS_05 <- WVS_05 %>% 
  rename(job_scarce = V44,
         housewife = V60,
         provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k,
         sex = V235,
         educ = V238,
         age = V237) %>% 
  mutate(female = as.numeric(sex > 1),
         year = 2005) %>% 
  select(tinh, provtariff, provtariff_k, female, age, educ, housewife)  

WVS_0105 <- bind_rows(WVS_01, WVS_05) %>% 
  mutate(housewife = as.numeric(housewife < 3))

#####################
# REGRESSION OF WVS #
#####################

etable(list(
  feols(housewife ~ provtariff + as.factor(female) + age + as.factor(educ) | year + tinh,
        WVS_0105, 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + as.factor(female) + age + as.factor(educ) | year + tinh,
        WVS_0105, 
        vcov = ~tinh)
), tex = TRUE)


etable(list(
  feols(housewife ~ provtariff + age + as.factor(educ) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + age + as.factor(educ) | year + tinh,
        subset(WVS_0105, female == 1), 
        vcov = ~tinh),
), tex = TRUE)

etable(list(
  feols(housewife ~ provtariff + as.factor(female) + age + as.factor(educ) | year + tinh,
        subset(WVS_0105, educ <= 5), 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + as.factor(female) + age + as.factor(educ) | year + tinh,
        subset(WVS_0105, educ <= 5), 
        vcov = ~tinh)  
  ), tex = TRUE)

etable(list(
  feols(housewife ~ provtariff + as.factor(female) + age + as.factor(educ) | year + tinh,
        subset(WVS_0105, educ > 5), 
        vcov = ~tinh),
  feols(housewife ~ provtariff_k + as.factor(female) + age + as.factor(educ) | year + tinh,
        subset(WVS_0105, educ > 5), 
        vcov = ~tinh)  
), tex = TRUE)
