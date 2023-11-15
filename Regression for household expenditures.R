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
), tex = T)

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

etable(list(
  feols(log(totarea) ~ provtariff | hhid + year,
        exp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(totarea) ~ provtariff | hhid + year,
        exp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(renovation) ~ provtariff | hhid + year,
        exp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(renovation) ~ provtariff | hhid + year,
        exp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)  
))


##################################
# WEDDINGS, FUNERALS AND SAVINGS #
##################################

etable(list(
  feols(log(wedding) ~ provtariff | hhid + year, 
  exp0204_p,
  weights = ~hhwt,
  vcov = ~tinh),
  feols(log(funeral) ~ provtariff | hhid + year, 
        exp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(wedding) ~ provtariff | hhid + year, 
        exp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(funeral) ~ provtariff | hhid + year, 
        exp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)), tex = T)

etable(list(
  feols(log(savings) ~ provtariff | hhid + year, 
        exp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(gold) ~ provtariff | hhid + year, 
        exp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(savings) ~ provtariff | hhid + year, 
        exp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(gold) ~ provtariff | hhid + year, 
        exp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)), tex = T)

###########################################
# CHILDRENS EDUCATION EXP. AND ENROLLMENT #
###########################################

etable(list(
  feols(enrolled ~ i(as.factor(female), -provtariff) | hhid + year,
        school0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), -provtariff) | hhid + year,
        school0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), -provtariff) | hhid + year,
        subset(school0204_p, age > 16),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), -provtariff) | hhid + year,
        subset(school0206_p, age > 16),
        weights = ~hhwt,
        vcov = ~tinh)  
), tex = T)

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
