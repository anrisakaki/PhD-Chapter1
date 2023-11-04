#############################################
# REGRESSION ON TOTAL HOUSEHOLD EXPENDITURE #
#############################################

etable(list(
  feols(log(hhexp2rl) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(hhexp2rl) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
))

#######################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS #
#######################################################

etable(list(
  feols(food_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(food_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), tex = T)

etable(list(
  feols(log(foodreal) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(educex_2) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(log(hlthex_2) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(log(tobac12m) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(log(foodreal) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(educex_2) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(log(hlthex_2) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(log(tobac12m) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt) 
), tex = T)

# Education per child 

etable(list(
  feols(enrolled ~ i(as.factor(female), provtariff) | hhid + year,
        school0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), provtariff) | hhid + year,
        school0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), provtariff) | hhid + year,
        subset(school0204_p, age > 16),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), provtariff) | hhid + year,
        subset(school0206_p, age > 16),
        weights = ~hhwt,
        vcov = ~tinh)  
))

etable(list(
  feols(log(educ_exp) ~ i(as.factor(female), provtariff) | hhid + year,
        school0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(as.factor(female), provtariff) | hhid + year,
        school0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(as.factor(female), provtariff) | hhid + year,
        subset(school0204_p, age > 11),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(as.factor(female), provtariff) | hhid + year,
        subset(school0206_p, age > 11),
        weights = ~hhwt,
        vcov = ~tinh)  
), tex = T)
