dict = c( "as.factor(Female)" = "Female", "provtariff" = "Province-level tariff", "provtariff_k" = "Province-level tariff")

setFixest_coefplot(dict = dict, grid = F)

inc_0204_spouse_p <- inc_0204_spouse_p %>%
  mutate(provtariff = -provtariff,
         provtariff_f = -provtariff_f)
inc_0206_spouse_p <- inc_0206_spouse_p %>%
  mutate(provtariff = -provtariff,
         provtariff_f = -provtariff_f)

png("tce_finc_020406.png")
coefplot(list(
  feols(inc_ratio ~ provtariff | ivid + year,
        subset(inc_0204_spouse_p, female == 1  & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid06 + year,
        subset(inc_0206_spouse_p, female == 1  & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)   
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("2 years", "4 years"))
dev.off()

# Urban - rural

png("tce_finc_urban_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_f | ivid + year,
        subset(inc_0204_spouse_p, female == 1 & urban == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_f | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & urban == 2 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Urban", "Rural"))
dev.off()

png("tce_finc_urban_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff_f | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & urban == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_f | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & urban == 2 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Urban", "Rural"))
dev.off()

# Education 
png("tce_finc_educ_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & educ > 9 & year == 2002 & married == 2 | year == 2004 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & educ > 5 & educ < 10 & year == 2002 & married == 2 | year == 2004 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & educ < 6 & year == 2002 & married == 2 | year == 2004 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("tce_finc_educ_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & educ > 9 & year == 2002 & married == 2 | year == 2006 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & educ > 5 & educ < 10 & year == 2002 & married == 2 | year == 2006 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & educ < 6 & year == 2002 & married == 2 | year == 2006 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "")
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

# Age 
png("tce_finc_age_0204.png")
coefplot(list(
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & age > 15 & age < 31 & year == 2002 & married == 2 |female == 1 & year == 2004 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & age > 30 & year == 2002 & married == 2 | female == 1 & year == 2004 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("18-30", "> 30"))
dev.off()

png("tce_finc_age_0206.png")
coefplot(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & age > 15 & age < 31 & year == 2002 & married == 2 | year == 2006 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & age > 30 & year == 2002 & married == 2 | year == 2006 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("18-30", "> 30"))
dev.off()
