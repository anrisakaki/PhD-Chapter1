##########################################################
# REGRESSION FOR THE EFFECT OF THE BTA ON INCOME, BY AGE #
##########################################################

etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & age > 17 & age < 30 & year == 2002| year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & age > 17 & age < 30 & year == 2002| year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1& age > 17 & age < 30 & year == 2002| year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & age > 17 & age < 30 & year == 2002| year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & age > 17 & age < 30 & year == 2002 & urban == 2| year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & age > 17 & age < 30 & year == 2002 & urban == 2 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1& age > 17 & age < 30 & year == 2002 & urban == 2| year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & age > 17 & age < 30 & year == 2002 & urban == 2| year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & age > 17 & age < 30 & year == 2002 & educ < 10 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & age > 17 & age < 30 & year == 2002 & educ < 10 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1& age > 17 & age < 30 & year == 2002 & educ < 10 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & age > 17 & age < 30 & year == 2002 & educ < 10 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

########################################################################
# REGRESSION FOR THE EFFECT OF THE BTA ON THE SPOUSAL WAGE GAP, BY AGE #
########################################################################

etable(list(
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, Female == 1 & wage_work == 1 & age > 17 & age < 31 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & wage_work == 1 & age > 17 & age < 31 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & wage_work == 1 & age > 17 & age < 31 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & wage_work == 1 & age > 17 & age < 31 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)
