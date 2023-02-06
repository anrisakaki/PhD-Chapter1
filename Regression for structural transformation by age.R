###############################################################################
# REGRESSION ON STRUCTURAL TRANSFORMATION USING PANEL DATA - EXTENSIVE MARGIN #
###############################################################################

# Wearing apparel and leather 

etable(list(
  feols(tal ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
  ), tex = TRUE)

etable(list(
  feols(tal ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = TRUE)

etable(list(
  feols(tal ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 & urban == 1| year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 & urban == 1 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 & urban == 1 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 & urban == 1 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
))

etable(list(
  feols(tal ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 & urban == 2| year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 & urban == 2 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 & urban == 2 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 & urban == 2 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = TRUE)

etable(list(
  feols(tal ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tal ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
))

# Out of agriculture 

etable(list(
  feols(agri_work ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(agri_work ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(agri_work ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(agri_work ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
))

# Manufacturing 

etable(list(
  feols(manu ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(manu ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
))

etable(list(
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid,
        subset(employment0204_p, age >18 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(traded_manu ~ as.factor(Female)/provtariff | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(traded_manu ~ as.factor(Female)/provtariff_k | year + ivid02,
        subset(employment0206_p, age >18 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
))
