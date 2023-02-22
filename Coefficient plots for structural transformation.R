coefplot_reallocation_lpm_0204 <- data.frame(
  "names" = as.factor(c("Female")),
  "coefs" = coef(models_0204_p_summary[[3]])[2:3],
  "lower" = confint(models_0204_p_summary[[3]])[2:3, 1],
  "upper" = confint(models_0204_p_summary[[3]])[2:3, 2]
)

coefplot_reallocation_lpm_0204 <- data.frame(
  "names" = as.factor(c("Female")),
  "coefs" = coef(models_0204_p_summary[[3]])[2:3],
  "lower" = confint(models_0204_p_summary[[3]])[2:3, 1],
  "upper" = confint(models_0204_p_summary[[3]])[2:3, 2]
)

ggplot(coefplot_reallocation_lpm_0204, aes(x = coefs, y = names)) +
  geom_point(col="red", size=3) + 
  geom_errorbar(aes(xmin=lower, xmax=upper),
                col="red", width =0.1, size=1)

ggcoef_model(list(
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