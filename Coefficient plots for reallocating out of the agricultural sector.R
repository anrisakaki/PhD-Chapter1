#####################################################################
# PLOTTING REALLOCATION INTO THE WEARING APPAREL AND LEATHER SECTOR # 
#####################################################################
png("reallocation_agrital_020406.png")
iplot(list
      (feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
                 subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt),
        feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
                 subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)),
      main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.75,
       legend = c("2 years", "4 years"))
dev.off()

# By education level 
png("reallocation_agrital_educ_0204.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)),
  main = "")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.75,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

png("reallocation_agrital_educ_0206.png")
iplot(list(
  feols(tal ~ i(as.factor(Female), -provtariff_k) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), -provtariff_k) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), -provtariff_k) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)),
  main = "")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.75,
       legend = c("High school and above", "Secondary", "Primary"))
dev.off()

# Urban - rural 
png("reallocation_agrital_urban_0204.png")
iplot(list
      (feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
             subset(employment0204_p, agri_work == 1 & urban == 1 & year == 2002 | year == 2004),
             vcov = ~tinh,
             weights = ~hhwt),
        feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
              subset(employment0204_p, agri_work == 1 & urban == 2 & year == 2002 | year == 2004),
              vcov = ~tinh,
              weights = ~hhwt)),
      main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("Urban", "Rural"))
dev.off()

png("reallocation_agrital_urban_0206.png")
iplot(list
      (feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
             subset(employment0206_p, agri_work == 1 & urban == 1 & year == 2002 | year == 2006),
             vcov = ~tinh,
             weights = ~hhwt),
        feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
              subset(employment0206_p, agri_work == 1 & urban == 2 & year == 2002 | year == 2006),
              vcov = ~tinh,
              weights = ~hhwt)),
      main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("Urban", "Rural"))
dev.off()

# Age 
png("reallocation_agrital_age_0204.png")
iplot(list
      (feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
             subset(employment0204_p, agri_work == 1 & age > 17 & age < 31 & year == 2002 | year == 2004),
             vcov = ~tinh,
             weights = ~hhwt),
        feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid,
              subset(employment0204_p, agri_work == 1 & age > 30 & year == 2002 | year == 2004),
              vcov = ~tinh,
              weights = ~hhwt)),
      main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("18-30", "> 30"))
dev.off()

png("reallocation_agrital_age_0206.png")
iplot(list
      (feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
             subset(employment0206_p, agri_work == 1 & age > 17 & age < 31 & year == 2002 | year == 2006),
             vcov = ~tinh,
             weights = ~hhwt),
        feols(tal ~ i(as.factor(Female), provtariff_k) | year + ivid02,
              subset(employment0206_p, agri_work == 1 & age > 30 & year == 2002 | year == 2006),
              vcov = ~tinh,
              weights = ~hhwt)),
      main = "")
legend("bottomleft", col = 1:2, pch = 16, bty = "n", cex = 0.9,
       legend = c("18-30", "> 30"))
dev.off()

#######################################################
# PLOTTING REALLOCATION INTO THE MANUFACTURING SECTOR # 
#######################################################

iplot(list(feols(manu ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))

# By education level 

iplot(list(
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))

iplot(list(
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))

###############################################################
# PLOTTING REALLOCATION INTO THE TRADED- MANUFACTURING SECTOR # 
###############################################################

iplot(list(feols(
  traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
  subset(employment0204_p, agri_work == 1 & year == 2002 | year == 2004),
  vcov = ~tinh,
  weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))

# By education level 

iplot(list(
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)))

iplot(list(
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ >  9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(traded_manu ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, agri_work == 1 & educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)))
