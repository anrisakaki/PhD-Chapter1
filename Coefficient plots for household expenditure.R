coefplot(list(
  feols(food_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health"))

coefplot(list(
  feols(food_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health"))

coefplot(list(
  feols(tobac_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("2001-2003", "2001-2005"))
