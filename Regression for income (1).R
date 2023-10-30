#################################################################
# REGRESSION ON TARIFF CUT EXPOSURE AND INCOME USING PANEL DATA #
#################################################################

inc_0204_spouse_p <- inc_0204_spouse_p %>% 
  mutate(income = ifelse(inc == 0, NA, inc))

inc_0206_spouse_p <- inc_0206_spouse_p %>% 
  mutate(income = ifelse(inc == 0, NA, inc))

etable(list(
  feols(log(inc) ~ i(as.factor(female), -provtariff) | hhid + year,
        subset(inc_0204_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), -provtariff) | hhid02 + year,
        subset(inc_0206_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), -provtariff_f) | hhid + year,
        subset(inc_0204_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(inc) ~ i(as.factor(female), -provtariff_f) | hhid02+ year,
        subset(inc_0206_spouse_p, married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0402_p),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0402_p),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0602_p),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0602_p),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_f) | ivid02 + year,
        subset(inc0402_p),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_fk) | ivid02 + year,
        subset(inc0402_p),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_f) | ivid02 + year,
        subset(inc0602_p),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_fk) | ivid02 + year,
        subset(inc0602_p),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

## Rural
etable(list(
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0402_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0402_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0602_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0602_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0402_p, urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0402_p, urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0602_p, urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0602_p, urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0402_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0402_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff | ivid02 + year,
        subset(inc0602_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ as.factor(Female)*provtariff_k | ivid02 + year,
        subset(inc0602_p, urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

# Education 
etable(list(
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0402_p,educ > 9 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0402_p, educ > 9 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0602_p, educ > 9 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0602_p, educ > 9 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0402_p,educ > 5 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0402_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0602_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0602_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0402_p, educ >5 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0402_p, educ >5 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff) | ivid02 + year,
        subset(inc0602_p, educ >5 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(log(totalinc) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
        subset(inc0602_p, educ >5 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)
