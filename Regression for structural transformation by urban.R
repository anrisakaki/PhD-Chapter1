#######################################################################
# HETEROGENOUS EFFECT OF BTA ON STRUCTURAL TRANSFORMATION - EDUCATION # 
########################################################### ###########

etable(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ provtariff * as.factor(Female) | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k * as.factor(Female) | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff * as.factor(Female) | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ provtariff_k * as.factor(Female) | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, educ > 6 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, educ > 6 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, educ > 6 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, educ > 6 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ provtariff * as.factor(Female) | year + ivid,
        subset(employment0204_p, educ > 6 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k * as.factor(Female) | year + ivid,
        subset(employment0204_p, educ > 6 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff * as.factor(Female) | year + ivid02,
        subset(employment0206_p, educ > 6 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ provtariff_k * as.factor(Female) | year + ivid02,
        subset(employment0206_p, educ > 6 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ provtariff * as.factor(Female) | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k * as.factor(Female) | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff * as.factor(Female) | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ provtariff_k * as.factor(Female) | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

