ggcoef(list(
  feols(tal ~ i(Female, provtariff) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(Female, provtariff) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)))

coefplot(list(
  feols(tal ~ i(Female, provtariff) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(Female, provtariff) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)))