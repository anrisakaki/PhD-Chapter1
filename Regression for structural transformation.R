################################################
# REGRESSION ON FORMALISATION USING PANEL DATA #
################################################

etable(list(
  # All observations 
  feols(manu ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  
  # Agriculture in 2002 
  feols(manu ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  
  # Manufacturing in 2002 
  feols(manu ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  # All observations 
  feols(manu ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  
  # Agriculture in 2002 
  feols(manu ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  
  # Manufacturing in 2002 
  feols(manu ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

#########################################
# REGRESSION ON INCOME USING PANEL DATA #
#########################################

etable(list(
  feols(log(rlinc) ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        inc0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        inc0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(imputed_income) ~ i(as.factor(female), mccaig_bta) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)
