########################################################
# HETEROGENOUS EFFECTS OF BTA ON HOUSEHOLD EXPENDITURE #
########################################################

y_exp <- c("food_share", "tobac_share", "educ_share", "health_share")

# 2002 - 2004 
## Urban-rural 
etable(list(
  feols(food_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(food_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)  
))

etable(list(
  feols(food_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(food_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)  
))

etable(list(
  feols(tobac_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)
))

etable(list(
  feols(tobac_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)
))

# 2002 - 2006 

## Urban- Rural

etable(list(
  feols(food_share ~ provtariff | hhid02 + yearint,
        subset(exp_0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(food_share ~ provtariff_k | hhid02 + yearint,
        subset(exp_0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid02 + yearint,
        subset(exp_0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + yearint,
        subset(exp_0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff | hhid02 + yearint,
        subset(exp_0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + yearint,
        subset(exp_0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)  
))

etable(list(
  feols(food_share ~ provtariff | hhid02 + yearint,
        subset(exp_0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(food_share ~ provtariff_k | hhid02 + yearint,
        subset(exp_0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid02 + yearint,
        subset(exp_0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + yearint,
        subset(exp_0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff | hhid02 + yearint,
        subset(exp_0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + yearint,
        subset(exp_0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)  
))

etable(list(
  feols(tobac_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)
))

etable(list(
  feols(tobac_share ~ provtariff | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + year,
        subset(exp_0402_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)
))
