###########################################
# REGRESSION FOR WORKING IN TRADED SECTOR #
###########################################

etable(list(
  feols(
    traded ~ factor(sex)/provtariff | year + ivid,
    employment0204_p,
    vcov = ~tinh,
    weights = ~hhwt),
  feols(traded ~ factor(sex)/provtariff_k | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded ~ factor(sex)/provtariff | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded ~ factor(sex)/provtariff_k | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)
  ),
  tex = TRUE)
