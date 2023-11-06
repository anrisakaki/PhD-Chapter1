#####################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING PANEL DATA #
#####################################################################################

etable(list(
  feols(log(inc) ~ i(as.factor(female), provtariff) | ivid + year,
        subset(emp0204_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), provtariff)| ivid + year,
        subset(emp0206_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), provtariff_f) | ivid + year,
        subset(emp0204_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), provtariff_f) | ivid + year,
        subset(emp0206_p, work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, work == 1 & female == 1 & married == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff| hhid + year,
        subset(emp0206_p, work == 1 & female == 1 & married == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~provtariff_f | hhid + year,
        subset(emp0204_p, work == 1 & female == 1 & married == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff_f | hhid + year,
        subset(emp0206_p, work == 1 & female == 1 & married == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

##########################
# TOTAL HOUSEHOLD INCOME #
##########################

etable(list(
  feols(log(hhinc) ~ provtariff | hhid + year,
        hhinc0204_p,
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(hhinc) ~ provtariff | hhid + year,
        hhinc0206_p,
        weights = ~hhwt, 
        vcov = ~tinh)), tex = T)
