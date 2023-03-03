dict = c("as.factor(Female)" = "Female", "provtariff" = "Marginal effect of province-level tariff", "provtariff_k" = "Marginal effect of province-level tariff")

setFixest_coefplot(dict = dict, grid = F)

png("tce_finc_020406.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1),
        weights = ~hhwt, 
        vcov = ~tinh)   
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("2001-2003", "2001-2005"))
dev.off()

# Urban - rural

png("tce_finc_urban_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Urban", "Rural"))
dev.off()

png("tce_finc_urban_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Urban", "Rural"))
dev.off()

# Education 
png("tce_finc_educ_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ > 9 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ < 6 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("tce_finc_educ_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ > 9 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ < 6 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "")
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

# Age 
png("tce_finc_age_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, age > 17 & age < 31 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, age > 30 & year == 2002 | year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("18-30", "> 30"))
dev.off()

png("tce_finc_age_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, age > 17 & age < 31 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, age > 30 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("18-30", "> 30"))
dev.off()

# Sector 
png("tce_finc_secremained_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & tal == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & manu == 1),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))
dev.off()

png("tce_finc_secremained_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & agri_work == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & tal == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0206_spouse_p, Female == 1 & manu == 1),
        weights = ~hhwt, 
        vcov = ~tinh)),
  main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))
dev.off()

png("tce_finc_sec_0204.png")
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
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))
dev.off()

png("tce_finc_sec_0206.png")
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
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Agriculture", "Wearing apparel and leather", "Manufacturing"))
dev.off()
