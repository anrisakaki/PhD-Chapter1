coefplot(list(
  feols(food_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "")

coefplot(list(
  feols(food_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "")

