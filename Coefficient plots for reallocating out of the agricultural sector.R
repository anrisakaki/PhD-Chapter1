###############################################################
# PLOTTING REALLOCATION INTO THE TRADED- MANUFACTURING SECTOR # 
###############################################################

iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))

# By education level 

iplot(list(
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))

iplot(list(
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
