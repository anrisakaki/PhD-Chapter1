exp0204_p <- exp0204_p %>% 
  mutate(provtariff = -provtariff)
exp0206_p <- exp0206_p %>% 
  mutate(provtariff = -provtariff)

png("tce_hhexp_0204.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_0206.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt) 
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_real_0204.png")
coefplot(list(
  feols(log(foodreal) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(educex_2) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(log(hlthex_2) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(log(tobac12m) ~ provtariff | hhid + year,
        data = exp0204_p,
        vcov = ~tinh,
        weights = ~hhwt)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_real_0206.png")
coefplot(list(
  feols(log(foodreal) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(educex_2) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(log(hlthex_2) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(log(tobac12m) ~ provtariff | hhid + year,
        data = exp0206_p,
        vcov = ~tinh,
        weights = ~hhwt)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()
