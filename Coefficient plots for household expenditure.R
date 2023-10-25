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
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + year,
        exp_0402_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))

exp_0206_p <- exp_0206_p %>% 
  mutate(provtariff_k = provtariff_k*-1)

png("tce_hhexp_0206.png")
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
        weights = ~hhwt),
  feols(tobac_share ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_real_0206.png")
coefplot(list(
  feols(log(foodreal) ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(educex_2) ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(hlthex_2) ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(tobac12m) ~ provtariff_k | hhid02 + yearint,
        exp_0206_p,
        vcov = ~tinh,
        weights = ~hhwt)  
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

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

png("tce_educexp_0206.png")
iplot(feols(log(educ_exp) ~ i(as.factor(Female), provtariff_k) | ivid02 + year,
                 schooling_0206_p,
                 weights = ~hhwt,
                 vcov = ~tinh), main = "", zero.par = list( type="dotted", lty=2))
dev.off()
