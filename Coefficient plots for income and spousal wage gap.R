dict = c( "as.factor(Female)" = "Female", "provtariff" = " ")

setFixest_coefplot(dict = dict, grid = F)

emp0204_p <- emp0204_p %>%
  mutate(provtariff = -provtariff)
emp0206_p <- emp0206_p %>%
  mutate(provtariff = -provtariff)

png("tce_finc_020406.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, work == 1 & female == 1 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff| hhid + year,
        subset(emp0206_p, work == 1 & female == 1 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("2 years", "4 years"))
dev.off()

# Urban - rural

png("tce_finc_urban_0204.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, work == 1 & female == 1 & relationship == 2 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff| hhid + year,
        subset(emp0204_p, work == 1 & female == 1 & relationship == 2 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Urban", "Rural"))
dev.off()

png("tce_finc_urban_0206.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0206_p, work == 1 & female == 1 & relationship == 2 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff| hhid + year,
        subset(emp0206_p, work == 1 & female == 1 & relationship == 2 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)
), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 1, bty = "n", 
       legend = c("Urban", "Rural"))
dev.off()

# Education 
png("tce_finc_educ_0204.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, female == 1 & educ > 9 & year == 2002 & relationship == 2 | year == 2004 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, female == 1 & educ > 5 & educ < 10 & year == 2002 & relationship == 2 | year == 2004 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, female == 1 & educ < 6 & year == 2002 & relationship == 2 | year == 2004 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("tce_finc_educ_0206.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0206_p, female == 1 & educ > 9 & year == 2002 & relationship == 2 | year == 2006 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0206_p, female == 1 & educ > 5 & educ < 10 & year == 2002 & relationship == 2 | year == 2006 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0206_p, female == 1 & educ < 6 & year == 2002 & relationship == 2 | year == 2006 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()


# Age
png("tce_finc_educ_0204.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, female == 1 & educ > 9 & year == 2002 & relationship == 2 | year == 2004 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, female == 1 & educ > 5 & educ < 10 & year == 2002 & relationship == 2 | year == 2004 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0204_p, female == 1 & educ < 6 & year == 2002 & relationship == 2 | year == 2004 & relationship == 2),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:3, pch = 16, lwd = 2, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("tce_finc_educ_0206.png")
coefplot(list(
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0206_p, age > 25 & age < 31 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh),  
  feols(inc_share ~ provtariff | hhid + year,
        subset(emp0206_p, age > 30 & year == 2002 | year == 2006),
        weights = ~hhwt, 
        vcov = ~tinh)), main = "", zero.par = list( type="dotted", lty=2))
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("18-30", "> 30"))
dev.off()