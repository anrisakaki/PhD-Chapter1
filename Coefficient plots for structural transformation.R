##############################################################
# PLOTTING WORKING IN THE WEARING APPAREL AND LEATHER SECTOR # 
##############################################################

dict = c("as.factor(female)" = "Female", "provtariff" = " ")

setFixest_coefplot(dict = dict, grid = F, zero.par = list( type="dotted", lty=2), main = "")

png("reallocation_lpm_020406.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        emp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        emp0206_p,
        vcov = ~tinh,
        weights = ~hhwt)), zero = F, main = "")
 legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("2 years", "4 years"))
dev.off()

# Age 
png("reallocation_lpm_age_0204.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, age > 15 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), zero = F, main = "")
legend("bottomleft", col = 1:2, pch = 16, cex = 0.9, bty = "n",
       legend = c("16-30", ">30"))
dev.off()

png("reallocation_lpm_age_0206.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, age > 15 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("16-30", "> 30"))
dev.off()

# Education 
png("reallocation_lpm_educ_0204.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("reallocation_lpm_educ_0206.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

# Urban - rural 
png("reallocation_lpm_urban_0204.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0204_p, urban == 2))),
  main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("Urban", "Rural"))
dev.off()

png("reallocation_lpm_urban_0206.png")
iplot(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        subset(emp0206_p, urban == 2))),
  main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("Urban", "Rural"))
dev.off()

png("reallocation_lpm_hours_020406.png")
iplot(list(
  feols(hours ~ i(as.factor(female), -provtariff) | year + ivid,
        emp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(hours) ~ i(as.factor(female), -provtariff) | year + ivid,
        emp0206_p,
        vcov = ~tinh,
        weights = ~hhwt)),  main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("2 years", "4 years"))
dev.off()

png("reallocation_lpm_hours_020406.png")
iplot(list(
  feols(log(days) ~ i(as.factor(female), -provtariff) | year + ivid,
        emp0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(log(days) ~ i(as.factor(female), -provtariff) | year + ivid,
        emp0206_p,
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("2 years", "4 years"))
dev.off()

