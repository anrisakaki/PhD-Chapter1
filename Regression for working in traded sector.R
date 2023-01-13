##########################################################################
# REGRESSION FOR WORKING IN TRADED MANUFACTURING SECTOR USING PANEL DATA #
##########################################################################

employment0204_p <- employment0204_p %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

employment0206_p <- employment0206_p %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

etable(list(
  feols(
    traded_manu ~ factor(sex)/provtariff | year + ivid,
    employment0204_p,
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ factor(sex)/provtariff_k | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ factor(sex)/provtariff | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ factor(sex)/provtariff_k | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)
  ),
  tex = TRUE)

######################################################################
# REGRESSION FOR WORKING IN TRADED SECTOR USING CROSS-SECTIONAL DATA #
######################################################################

employment0204 <- employment0204 %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

employment0206 <- employment0206 %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

etable(list(
  feols(traded_manu ~ factor(sex)/provtariff + age + age^2  + educ + factor(urban) | year + tinh,
        data = employment0204,
        vcov = ~tinh,
        weights = ~ hhwt),
  feols(traded_manu ~ factor(sex)/provtariff_k + age + age^2  + educ + factor(urban) | year + tinh,
        data = employment0204,
        vcov = ~tinh,
        weights = ~ hhwt),
  feols(traded_manu ~ factor(sex)/provtariff + age + age^2  + educ + factor(urban) | year + tinh,
        data = employment0206,
        vcov = ~tinh,
        weights = ~ hhwt),  
  feols(traded_manu ~ factor(sex)/provtariff_k + age + age^2  + educ + factor(urban) | year + tinh,
        data = employment0206,
        vcov = ~tinh,
        weights = ~ hhwt)  
))
