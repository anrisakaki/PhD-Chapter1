#########################################
# SETTING UP FOR REGRESSION FOR DIVORCE # 
#########################################

div_02 <- m1_02 %>% 
  select(ivid, m1c6, tinh02, xa02, diaban02, huyen02) %>% 
  rename(ivid02 = ivid) %>% 
  mutate(divorced = as.numeric(m1c6 == 4, 1, 0)) %>% 
  select(-c("m1c6"))

div_02 <- merge(div_02, weights_02, by = c("tinh02", "xa02", "diaban02", "huyen02")) %>% 
  select(tinh02, ivid02, divorced, hhwt) %>% 
  rename(tinh = tinh02) %>% 
  mutate(tinh = factor(tinh))

div_02 <- list(div_02, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

div_04 <- m123a_04 %>% 
  select(ivid, m1ac6, tinh, huyen, xa) %>% 
  mutate(divorced = as.numeric(m1ac6 == 4, 1, 0)) %>% 
  select(-c("m1ac6"))

div_04 <- merge(weights_04, div_04, by = c("tinh", "huyen", "xa")) %>% 
  select(tinh, ivid, divorced, hhwt)

div_06 <- m1a_06 %>% 
  select(ivid, m1ac6, tinh, huyen, xa) %>% 
  mutate(divorced = as.numeric(m1ac6 == 4, 1, 0))

div_06 <- merge(weights_06, div_06, by = c("tinh", "huyen", "xa")) %>% 
  select(tinh, ivid, divorced, hhwt)

div0406 <- c("div_04", "div_06")

for(i in div0406){
  
  assign(i, get(i) %>% 
           mutate(tinh = factor(tinh)))
  
  assign(i, list(get(i), postBTA_provtariff, postBTA_provtariff_k) %>% 
           reduce(full_join, by = "tinh"))
  
  assign(i, get(i) %>% 
           rename(provtariff = postprov_tariff,
                  provtariff_k = postprov_tariff_k))
}

# Setting up panel 

# 2002 - 2004 

ivid0204_div <- ivid0204 %>% 
  select(ivid02, ivid)

div_02_p <- merge(ivid0204_div, div_02, by = "ivid02") %>%
  distinct() %>% 
  mutate(year = 2002)

div_04_p <- merge(ivid0204_div, div_04, by = "ivid") %>%
  distinct() %>% 
  mutate(year = 2004)

div0204_p <- bind_rows(div_02_p, div_04_p)

# 2002 - 2006 

ivid0206_div <- ivid020406 %>% 
  select(ivid02, ivid06)
div_06 <- div_06 %>% rename(ivid06 = ivid)

div_0602_p <- merge(ivid0206_div, div_02, by = "ivid02") %>% mutate(year = 2002)
div06_p <- merge(ivid0206_div, div_06, by = "ivid06") %>% mutate(year = 2006)

div_0206_p <- bind_rows(div_0602_p, div06_p)

#####################################
# TWFE FOR EFFECT OF BTW ON DIVORCE #
#####################################

etable(list(
  feols(divorced ~ provtariff | ivid + year,
        div0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(divorced ~ provtariff_k | ivid + year,
        div0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(divorced ~ provtariff | ivid06 + year,
        div_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(divorced ~ provtariff_k | ivid06 + year,
        div_0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
))
