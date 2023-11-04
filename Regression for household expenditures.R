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

########################################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS USING PANEL DATA #
########################################################################

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
))

