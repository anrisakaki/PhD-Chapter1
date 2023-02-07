#################################################################
# REGRESSION ON TARIFF CUT EXPOSURE AND INCOME USING PANEL DATA #
#################################################################

# Household fixed effects 
etable(list(
  feols(log(totalinc) ~ as.factor(Female)*provtariff | hhid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | hhid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff | hhid06 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | hhid06 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | hhid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | hhid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | hhid06 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | hhid06 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

## Rural
etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | hhid02 + year,
        subset(inc0402_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | hhid02 + year,
        subset(inc0402_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | hhid06 + year,
        subset(inc0602_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | hhid06 + year,
        subset(inc0602_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)*provtariff | hhid02 + year,
        subset(inc0402_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | hhid02 + year,
        subset(inc0402_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff | hhid06 + year,
        subset(inc0602_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | hhid06 + year,
        subset(inc0602_p, urban == 2 & wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

# Individual fixed effects 
etable(list(
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

# Rural 
etable(list(
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)/provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

# Education 
etable(list(
  feols(log(totalinc) ~ as.factor(sex)/provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(sex)/provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(sex)/provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(sex)/provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(sex)*provtariff | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(sex)*provtariff_k | ivid02 + year,
        subset(inc0402_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(sex)*provtariff | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(sex)*provtariff_k | ivid02 + year,
        subset(inc0602_p, wage_work == 1 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)
