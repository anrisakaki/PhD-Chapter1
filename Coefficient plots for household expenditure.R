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
        weights = ~hhwt)), main = "", xlab = "", zero.par = list( type="dotted", lty=2))
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

# Real 

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

# Urban 

png("tce_hhexp_0204_urban.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        subset(exp0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_0204_rural.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        subset(exp0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        subset(exp0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        subset(exp0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        subset(exp0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_0206_urban.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

png("tce_hhexp_0206_rural.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt),   
  feols(tobac_share ~ provtariff | hhid + year,
        subset(exp0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:4, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Food", "Education", "Health", "Tobacco"))
dev.off()

# Education 

educ_exp_0204_p <- educ_exp_0204_p %>% mutate(provtariff = -provtariff)
educ_exp_0206_p <- educ_exp_0206_p %>% mutate(provtariff = -provtariff)

png("food_0204_educ.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(food_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(food_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("educ_0204_educ.png")
coefplot(list(
  feols(educ_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(educ_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("health_0204_educ.png")
coefplot(list(
  feols(health_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("tobac_0204_educ.png")
coefplot(list(
  feols(tobac_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tobac_share ~ provtariff | hhid + year,
        subset(educ_exp_0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

# 2002 - 2006

png("food_0206_educ.png")
coefplot(list(
  feols(food_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(food_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(food_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("educ_0206_educ.png")
coefplot(list(
  feols(educ_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(educ_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(educ_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("health_0206_educ.png")
coefplot(list(
  feols(health_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(health_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(health_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("tobac_0206_educ.png")
coefplot(list(
  feols(tobac_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tobac_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tobac_share ~ provtariff | hhid + year,
        subset(educ_exp_0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()