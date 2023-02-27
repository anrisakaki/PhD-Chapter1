# All observations 
dict = c("as.factor(Female)" = "Female")

setFixest_coefplot(dict = dict, grid = F)

iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        employment0204_p,
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        employment0206_p,
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nwearing apparel and leather sector")

# Age 
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, age > 17 & age < 31 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, age > 30 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nwearing apparel and leather sector")

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
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nwearing apparel and leather sector")

legend("bottomleft", col = 1:2, pch = 16, cex = 0.5,
       legend = c("18-30", "31-65"))



# Education 
iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, educ > 5 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nwearing apparel and leather sector")
legend("bottomleft", col = 1:3, pch = 16, cex = 0.5,
       legend = c("Tertiary", "Secondary", "Primary"))

iplot(list(
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, educ > 5 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ i(as.factor(Female), provtariff) | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), main = "Effect of BTA on reallocation into \nwearing apparel and leather sector")
legend("bottomleft", col = 1:3, pch = 16, cex = 0.5,
       legend = c("Tertiary", "Secondary", "Primary"))