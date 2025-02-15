################################################
# REGRESSION ON FORMALISATION USING PANEL DATA #
################################################

etable(list(
  feols(hhbus ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_manu ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_service ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_manu ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_service ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

# Restricting to those who were unemployed or in agriculture in 2002 

etable(list(
  feols(hhbus ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

# Restricting to those who were in manufacturing in 2002 

etable(list(
  feols(hhbus ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1 & hhbus_2002 == 1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

#########################################
# REGRESSION ON INCOME USING PANEL DATA #
#########################################

etable(list(
  feols(log(rlinc) ~ i(as.factor(female), mccaig_bta) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), mccaig_bta) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(imputed_income) ~ i(as.factor(female), mccaig_bta) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), mccaig_bta) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)
