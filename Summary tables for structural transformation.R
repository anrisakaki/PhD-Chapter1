#########
# Urban #
#########

etable(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)    
), tex = T)

etable(list(
  feols(tal ~ provtariff*as.factor(Female) | year + ivid,
        subset(employment0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k*as.factor(Female) | year + ivid,
        subset(employment0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff*as.factor(Female) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k*as.factor(Female) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)    
), tex = T)

etable(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)    
), tex = T)

etable(list(
  feols(tal ~ provtariff*as.factor(Female) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k*as.factor(Female) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff*as.factor(Female) | year + ivid02,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ provtariff_k*as.factor(Female) | year + ivid02,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)    
), tex = T)
