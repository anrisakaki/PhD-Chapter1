
coefplot(list(
  feols(manu ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  # feols(manu ~ as.factor(Female)/provtariff_k | year + ivid,
  #       subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
  #       vcov = ~tinh,
  #       weights = ~hhwt),   
  feols(manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)
  # feols(manu ~ as.factor(Female)/provtariff_k | year + ivid02,
  #       subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
  #       vcov = ~tinh,
  #       weights = ~hhwt)),
  # drop = "as.factor(Female)"))
))

coefplot(list(
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  # feols(manu ~ as.factor(Female)/provtariff_k | year + ivid,
  #       subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
  #       vcov = ~tinh,
  #       weights = ~hhwt),   
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)
  # feols(manu ~ as.factor(Female)/provtariff_k | year + ivid02,
  #       subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
  #       vcov = ~tinh,
  #       weights = ~hhwt)),
  # drop = "as.factor(Female)"))
))
