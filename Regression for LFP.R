#############################################################
# REGRESSION ON LABOUR FORCE PARTICIPATION USING PANEL DATA #
#############################################################

etable(list(
  feols(work ~ as.factor(Female)/provtariff | ivid + year,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff_k | ivid + year,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff | ivid02 + year,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff_k | ivid02 + year,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = TRUE)

# Rural 
etable(list(
  feols(work ~ as.factor(Female)/provtariff | ivid + year,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff_k | ivid + year,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = TRUE)

# Education 
etable(list(
  feols(work ~ as.factor(Female)/provtariff | ivid + year,
        subset(employment0204_p, educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff_k | ivid + year,
        subset(employment0204_p, educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(employment0206_p, educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(work ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(employment0206_p, educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = TRUE)