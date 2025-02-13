#############################################
# REGRESSION ON TOTAL HOUSEHOLD EXPENDITURE #
#############################################

etable(list(
  feols(log(hhexp2rl) ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(hhexp2rl) ~ tariff_f | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt)
), tex = T)

#######################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS #
#######################################################

etable(list(
  feols(food_share ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt)
), tex = T)

etable(list(
  feols(log(foodreal) ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(educex_2) ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(log(hlthex_2) ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(log(tobac12m) ~ tariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt)
), tex = T)

###########################################
# CHILDRENS EDUCATION EXP. AND ENROLLMENT #
###########################################

etable(list(
  feols(enrolled ~ i(as.factor(female), -tariff) | hhid + year,
        school0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), -tariff) | hhid + year,
        school0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), -tariff) | hhid + year,
        subset(school0204_p, age > 16),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(enrolled ~ i(as.factor(female), -tariff) | hhid + year,
        subset(school0206_p, age > 16),
        weights = ~hhwt,
        vcov = ~tinh)  
), tex = T)

etable(list(
  feols(log(educ_exp) ~ i(as.factor(female), tariff) | hhid + year,
        school0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(as.factor(female), tariff) | hhid + year,
        school0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(as.factor(female), tariff) | hhid + year,
        subset(school0204_p, age > 11),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(educ_exp) ~ i(as.factor(female), tariff) | hhid + year,
        subset(school0206_p, age > 11),
        weights = ~hhwt,
        vcov = ~tinh)  
), tex = T)
