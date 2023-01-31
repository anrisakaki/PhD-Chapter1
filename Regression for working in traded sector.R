##########################################################################
# REGRESSION FOR WORKING IN TRADED MANUFACTURING SECTOR USING PANEL DATA #
##########################################################################

employment0204_p <- employment0204_p %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

employment0206_p <- employment0206_p %>% 
  mutate(traded_manu = as.numeric(traded == 1 & manu == 1))

# Switching from agriculture pre-BTA to traded manufacturing post-BTA 

etable(list(
  feols(
    traded_manu ~ as.factor(Female)/provtariff | year + ivid,
    subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)
),
tex = TRUE)

# Rural 
etable(list(
  feols(
    traded_manu ~ as.factor(Female)/provtariff | year + ivid,
    subset(employment0204_p, urban == 2 & work == 1),
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, urban == 2 & work == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, urban == 2 & work == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, urban == 2 & work == 1),
        vcov = ~tinh,
        weights = ~hhwt)
),
tex = TRUE)

# Education 
etable(list(
  feols(
    traded_manu ~ as.factor(Female)/provtariff | year + ivid,
    subset(employment0204_p, educ < 10 & year == 2002 | year == 2004),
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, educ < 10 & year == 2002 | year == 2006),
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
