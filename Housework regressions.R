# SETTING UP FOR REGRESSION ON HOUSEWORK # 

## 2002 
housework_02 <- employment_mf_02 %>%
  mutate(housework = ifelse(m3c12 == "1", 1, 0),
         female = ifelse(sex == "Female", 1, 0),
         married = ifelse(married == "2", 1, 0),
         year = 2002) %>% 
  select(tinh, ivid02, female, married, housework, hhwt, year)

housework_02 <- list(housework_02, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

## 2004 
housework_04 <- m123a_04 %>%
  filter(m1ac5 >= 18) %>%
  filter(m1ac5 < 65) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         tinh = as.factor(tinh),
         year = 2004) %>% 
  select(tinh, huyen, xa, ivid, female, married, year)

housework_04 <- merge(housework_04, weights_04, by = c("tinh", "huyen", "xa")) %>% 
  select(-c("huyen", "xa", "urban"))

housework_04a <- m4a_04 %>% 
  mutate(housework = ifelse(m4ac26 == 1, 1, 0)) %>% 
  select(ivid, housework)

housework_04 <- merge(housework_04, housework_04a, by = "ivid") %>% distinct()

housework_04 <- list(housework_04, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)

## 2006 
housework_06 <- employment_mf_06 %>% 
  mutate(housework = ifelse(m4ac26 == 1, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         female = ifelse(sex == "Female", 1, 0)) %>% 
  select(tinh, ivid, female, married, housework, hhwt, year)

housework_06 <- list(housework_06, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)

# constructing panel data 
## 2002 - 2004
ivid0204 <- ivid0204 %>% select(ivid, ivid02)

housework_02_p <- merge(ivid0204, housework_02, by = "ivid02")
housework_04_p <- merge(ivid0204, housework_04, by = "ivid")

housework0204_p <- bind_rows(housework_02_p, housework_04_p)

## 2002 - 2006 
ivid0206 <- ivid020406 %>% select(ivid02, ivid06) %>% 
  rename(ivid = ivid06)

housework_0602_p <- merge(ivid0206, housework_02, by = "ivid02")
housework_06_p <- merge(ivid0206, housework_06, by = "ivid")

housework0206_p <- bind_rows(housework_0602_p, housework_06_p)#

save(housework0204_p, file = "housework0204_p.rda")
save(housework0206_p, file = "housework0206_p.rda")

# TWFE FOR TARIFF CUT EXPOSURE AND PROBABILITY OF HUSBAND DOING HOUSEWORK # 

load("housework0204_p.rda")
load("housework0206_p.rda")

etable(list(
  feols(housework ~ provtariff | year + ivid,
        subset(housework0204_p, married == 1 & female == 0),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(housework ~ provtariff_k | year + ivid,
        subset(housework0204_p, married == 1 & female == 0),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(housework ~ provtariff | year + ivid,
        subset(housework0206_p, married == 1 & female == 0),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(housework ~ provtariff_k | year + ivid,
        subset(housework0206_p, married == 1 & female == 0),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = T)

