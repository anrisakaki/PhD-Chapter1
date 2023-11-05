#####################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING PANEL DATA #
#####################################################################################

etable(list(
  feols(inc_share ~ provtariff | ivid + year,
        subset(emp0204_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff| ivid + year,
        subset(emp0206_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~provtariff_f | ivid + year,
        subset(emp0204_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff_f | ivid + year,
        subset(emp0206_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

##########################
# TOTAL HOUSEHOLD INCOME #
##########################

etable(list(
  feols(log(hhinc) ~ provtariff | hhid + year,
        inc_0204_spouse_p,
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(hhinc) ~ provtariff | hhid02 + year,
        inc_0206_spouse_p,
        weights = ~hhwt, 
        vcov = ~tinh)), tex = T)
