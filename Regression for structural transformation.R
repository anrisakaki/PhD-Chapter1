################################################
# REGRESSION ON FORMALISATION USING PANEL DATA #
################################################

etable(list(
  feols(hhbus ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(private ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(private ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus == 0 & agri == 1 ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus == 0 & agri == 1 ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(hours) ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, hours > -1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(hours) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, days > -1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(days) ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, hours > -1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(days) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, days > -1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

#########################################
# REGRESSION ON INCOME USING PANEL DATA #
#########################################

etable(list(
  feols(log(inc) ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), tariff) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), tariff) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)
