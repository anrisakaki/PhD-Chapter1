#####################################################################
# PLOTTING REALLOCATION INTO THE WEARING APPAREL AND LEATHER SECTOR # 
#####################################################################

dict = c("as.factor(Female)" = "Female")

setFixest_coefplot(dict = dict, grid = F, zero.par = list( type="dotted", lty=2), main = "")

png("reallocation_lpm_020406.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)), zero = F, main = "")
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("2 years", "4 years"))
dev.off()

# Age 
png("reallocation_lpm_age_0204.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, age > 17 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), zero = F, main = "")
legend("bottomleft", col = 1:2, pch = 16, cex = 0.9, bty = "n",
       legend = c("18-30", "31-65"))
dev.off()

png("reallocation_lpm_age_0206.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, age > 17 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("18-30", "> 30"))
dev.off()


png("reallocation_lpm_age_0204.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, age > 17 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), zero = F, main = "")
legend("bottomleft", col = 1:2, pch = 16, cex = 0.9, bty = "n",
       legend = c("18-30", "31-65"))
dev.off()

png("reallocation_lpm_age_0206.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, age > 17 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("18-30", "> 30"))
dev.off()

# Education 
png("reallocation_lpm_educ_0204_k.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("reallocation_lpm_educ_0206_k.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

# Urban - rural 
png("reallocation_lpm_urban_0204_k.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, urban == 2))),
  main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("Urban", "Rural"))
dev.off()

png("reallocation_lpm_urban_0206_k.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, urban == 1),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        subset(employment0206_p, urban == 2))),
  main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("Urban", "Rural"))
dev.off()

#######################################################
# PLOTTING REALLOCATION INTO THE MANUFACTURING SECTOR # 
#######################################################

# All observations
png("reallocation_manu_020406.png")
iplot(list(feols(manu ~ i(as.factor(Female), provtariff_k) | year + ivid,
  employment0204_p,
  vcov = ~tinh,
  weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff_k) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nthe manufacturing sector")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.5,
       legend = c("2001-2003", "2001-2005"))
dev.off()

# Urban - rural
iplot(list(feols(manu ~ i(as.factor(Female), provtariff_k) | year + ivid,
                 subset(employment0204_p, urban == 1),
                 vcov = ~tinh,
                 weights = ~hhwt),
           feols(manu ~ i(as.factor(Female), provtariff_k) | year + ivid,
                 subset(employment0204_p, urban == 2),
                 vcov = ~tinh,
                 weights = ~hhwt)), main = "Effect of BTA on reallocation into \nthe manufacturing sector")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.5,
       legend = c("Urban", "Rural"))

iplot(list(feols(manu ~ i(as.factor(Female), provtariff_k) | year + ivid02,
                 subset(employment0206_p, urban == 1),
                 vcov = ~tinh,
                 weights = ~hhwt),
           feols(manu ~ i(as.factor(Female), provtariff_k) | year + ivid02,
                 subset(employment0206_p, urban == 2),
                 vcov = ~tinh,
                 weights = ~hhwt)), main = "Effect of BTA on reallocation into \nthe manufacturing sector")
legend("bottomleft", col = 1:2, pch = 16, cex = 0.5,
       legend = c("Urban", "Rural"))

# By age 
iplot(list(
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, age > 17 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nthe manufacturing sector")
legend("bottomleft", col = 1:2, pch = 16, cex = 0.5,
       legend = c("18-30", "31-65"))

iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, age > 17 & age < 31 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nthe manufacturing sector")
legend("bottomleft", col = 1:2, pch = 16, cex = 0.5,
       legend = c("18-30", "31-65"))

###############################################################
# PLOTTING REALLOCATION INTO THE TRADED- MANUFACTURING SECTOR # 
###############################################################

# All observations 
iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
  employment0204_p,
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)))

# Urban - rural 
iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, urban == 1),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)))

iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
  subset(employment0206_p, urban == 1),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)))

# Age 
iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, age > 17 & age < 31 & year == 2002 | year == 2004),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))

iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
  subset(employment0206_p, age > 17 & age < 31 & year == 2002 | year == 2006),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))

################################################
# PLOTTING REALLOCATION INTO THE TRADED SECTOR # 
################################################

iplot(list(feols(
  traded ~ i(as.factor(Female), provtariff_k) | year + ivid,
  employment0204_p,
  vcov = ~tinh,
  weights = ~hhwt),
  feols(
    traded ~ i(as.factor(Female), provtariff_k) | year + ivid02,
    employment0206_p,
    vcov = ~tinh,
    weights = ~hhwt)))

# Urban - rural
iplot(list(feols(
  traded ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, urban == 1),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, urban == 2),
        vcov = ~tinh,
        weights = ~hhwt)))

# Age 
iplot(list(feols(
  traded ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, age > 17 & age < 31 & year == 2002 | year == 2004),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))

iplot(list(feols(
  traded~ i(as.factor(Female), provtariff) | year + ivid02,
  subset(employment0206_p, age > 17 & age < 31 & year == 2002 | year == 2006),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, age > 30 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
