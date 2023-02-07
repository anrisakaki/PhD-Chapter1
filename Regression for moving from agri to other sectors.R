###################################################################################
# REGRESSION ON STRUCTURAL TRANSFORMATION FROM THE AGRICULTURE INTO OTHER SECTORS #
###################################################################################

# Manufacturing 
etable(list(
  feols(manu ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(manu ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE
)

# Traded-manufacturing 
etable(list(
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid,
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
        weights = ~hhwt)), tex = TRUE
)


# Wearing apparel and leather 
etable(list(
  feols(tal ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tal ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

