dict = c("as.factor(Female)" = "Female", "provtariff" = "Province-level tariff", "provtariff_k" = "Province-level tariff")

setFixest_coefplot(dict = dict, grid = F)

coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "Effect of BTA on women's relative income")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Urban", "Rural"))

coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "Effect of BTA on women's relative income")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Urban", "Rural"))

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
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Tertiary", "Secondary", "Primary"))

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

coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & tal == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & manu == 1),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "Effect of BTA on women's relative income \n(by sector of employment)")
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))

coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & agri_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & tal == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & manu == 1),
        weights = ~hhwt, 
        vcov = ~tinh)), zero.par = list(col = "black", lwd = 1),
  main = "Effect of BTA on women's relative income \n(by sector of employment)")
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))

coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & tal == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & manu == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "Effect of BTA on women's relative income \n(by sector of employment)")
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))

coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & agri_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & tal == 1 & year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & manu == 1 & year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "Effect of BTA on women's relative income \n(by sector of employment)")
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))
