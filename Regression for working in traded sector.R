############################################################
# REGRESSION FOR WORKING IN TRADED SECTOR USING PANEL DATA #
############################################################

etable(list(
  feols(
    traded ~ i(as.factor(Female), provtariff) | year + ivid,
    employment0204_p,
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded ~ i(as.factor(Female), provtariff_k) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded ~ i(as.factor(Female), provtariff) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)
),
tex = TRUE)

##########################################################################
# REGRESSION FOR WORKING IN TRADED MANUFACTURING SECTOR USING PANEL DATA #
##########################################################################

employment0204_p <- employment0204_p %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

employment0206_p <- employment0206_p %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

etable(list(
  feols(
    traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
    employment0204_p,
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_k) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)),
tex = TRUE)

etable(list(
  feols(
    traded_manu ~ i(as.factor(Female), provtariff_f) | year + ivid,
    employment0204_p,
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_fk) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_f) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_fk) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)),
  tex = TRUE)

# Rural 
etable(list(
  feols(
    traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
    subset(employment0204_p, urban == 1),
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)
),
tex = TRUE)

etable(list(
  feols(
    traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
    subset(employment0204_p, urban == 2),
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, urban == 2),
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
