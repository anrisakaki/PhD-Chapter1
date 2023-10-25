#####################################################
# SETTING UP FOR REGRESSION ON HOURS WORKED BY WIFE # 
#####################################################

hours_02 <- employment_mf_02 %>%
  filter(work == 1) %>% 
  mutate(female = ifelse(sex == "Female", 1, 0),
         married = ifelse(married == "2", 1, 0),
         year = 2002) %>% 
  mutate(hours = m3c3/7) %>% 
  select(tinh, ivid02, female, married, hours, hhwt, year)

hours_02 <- list(hours_02, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

hours_04 <- m123a_04 %>%
  filter(m1ac5 >= 18) %>%
  filter(m1ac5 < 65) %>%
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         tinh = as.factor(tinh),
         year = 2004) %>% 
  select(tinh, huyen, xa, ivid, female, married, year)

hours_04 <- merge(hours_04, weights_04, by = c("tinh", "huyen", "xa")) %>% 
  select(-c("huyen", "xa", "urban"))

hours_04a <- m4a_04 %>% 
  filter(m4ac2 == 1) %>% 
  mutate(hours = m4ac8) %>% 
  select(ivid, hours)

hours_04 <- merge(hours_04, hours_04a, by = "ivid") %>% distinct()

hours_04 <- list(hours_04, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)

hours_06 <- employment_mf_06 %>% 
  filter(work == 1) %>% 
  mutate(married = ifelse(m1ac6 == 2, 1, 0),
         female = ifelse(sex == "Female", 1, 0)) %>% 
  rename(hours = m4ac8) %>% 
  select(tinh, ivid, female, married, hours, hhwt, year)

hours_06 <- list(hours_06, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)

# constructing panel data 
## 2002 - 2004
ivid0204 <- ivid0204 %>% select(ivid, ivid02)

hours_02_p <- merge(ivid0204, hours_02, by = "ivid02")
hours_04_p <- merge(ivid0204, hours_04, by = "ivid") %>% distinct()

hours0204_p <- bind_rows(hours_02_p, hours_04_p)

## 2002 - 2006 
ivid0206 <- ivid020406 %>% select(ivid02, ivid06) %>% 
  rename(ivid = ivid06)

hours_0602_p <- merge(ivid0206, hours_02, by = "ivid02")
hours06_p <- merge(ivid0206, hours_06, by = "ivid")

hours0206_p <- bind_rows(hours_0602_p, hours06_p)

hours0204_p$hours <- round(hours0204_p$hours, digits = 0)
hours0206_p$hours <- round(hours0206_p$hours, digits = 0)

save(hours0204_p, file = "hours0204_p.rda")
save(hours0206_p, file = "hours0206_p.rda")

etable(list(
  feols(hours ~ provtariff | year + ivid,
        subset(hours0204_p, female == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(hours ~ provtariff_k | year + ivid,
        subset(hours0204_p, female == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(hours ~ provtariff | year + ivid,
        subset(hours0206_p, female == 1),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(hours ~ provtariff_k | year + ivid,
        subset(hours0206_p, female == 1),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = T)

etable(list(
  feols(hours ~ provtariff | year + ivid,
        subset(hours0204_p, female == 0),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(hours ~ provtariff_k | year + ivid,
        subset(hours0204_p, female == 0),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(hours ~ provtariff | year + ivid,
        subset(hours0206_p, female == 0),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(hours ~ provtariff_k | year + ivid,
        subset(hours0206_p, female == 0),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = T)

