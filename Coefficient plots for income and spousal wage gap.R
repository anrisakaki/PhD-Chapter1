dict = c("as.factor(Female)" = "Female", "provtariff" = "Province-level tariff")

setFixest_coefplot(dict = dict, grid = F)

coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "Effect of BTA on women's relative income")

coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "Effect of BTA on women's relative income")

coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ > 9 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ < 6 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "Effect of BTA on women's relative income \n(by eduction level)")

coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ > 9 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ < 6 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "Effect of BTA on women's relative income \n(by eduction level)")
