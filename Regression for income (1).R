#################################################################
# REGRESSION ON TARIFF CUT EXPOSURE AND INCOME USING PANEL DATA #
#################################################################

etable(list(
  feols(log(inc) ~ i(as.factor(female), -provtariff) | hhid + year,
        subset(inc_0204_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), -provtariff) | hhid02 + year,
        subset(inc_0206_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), -provtariff_f) | hhid + year,
        subset(inc_0204_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), -provtariff_f) | hhid02+ year,
        subset(inc_0206_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

