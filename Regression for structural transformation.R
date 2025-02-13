################################################
# REGRESSION ON FORMALISATION USING PANEL DATA #
################################################

etable(list(
  feols(hhbus ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(private ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(private ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus == 0 & agri == 1 ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(hhbus == 0 & agri == 1 ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & manu == 1 ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(hhbus == 0 & service == 1 ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(hours) ~ i(as.factor(female), tariff) | year + ivid,
        subset(subset(emp0204_p, age > 19 & age < 65), hours > -1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(hours) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(subset(emp0204_p, age > 19 & age < 65), days > -1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(days) ~ i(as.factor(female), tariff) | year + ivid,
        subset(subset(emp0204_p, age > 19 & age < 65), hours > -1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(days) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(subset(emp0204_p, age > 19 & age < 65), days > -1),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

#########################################
# REGRESSION ON INCOME USING PANEL DATA #
#########################################

etable(list(
  feols(rlinc/rlhhinc ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(rlinc) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(log(imputed_income) ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) | year + ivid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(log(imputed_income) ~ i(as.factor(female), tariff_f) + as.factor(female) | year + hhid,
        subset(emp0204_p, age > 19 & age < 65),
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)
