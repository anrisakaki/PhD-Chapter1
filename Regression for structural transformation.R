##########################################################
# SETTING UP FOR REGRESSION ON STRUCTURAL TRANSFORMATION #
##########################################################

vhlss0204 <- bind_rows(vhlss02, vhlss04)
vhlss0206 <- bind_rows(vhlss02, vhlss06)

vhlss0204 <- merge(vhlss0204, bta0204, by = c("tinh", "year"))
vhlss0206 <- merge(vhlss0206, bta0206, by = c("tinh", "year"))

emp0204_p <- merge(vhlss0204, ivid0204, by = ivid)

emp0206_p <- merge(vhlss0206, ivid0206, by = ivid)

# write .rda files 
save(employment_0204, file='employment0204.rda')
save(emp0204_p, file='emp0204_p.rda')
save(employment_0206, file = 'employment0206.rda')
save(emp0206_p, file = 'emp0206_p.rda')

############################################################
# REGRESSION ON STRUCTURAL TRANSFORMATION USING PANEL DATA #
############################################################
setFixest_dict(c("as.factor(female)" = "Female",
                 "-provtariff" = "Tariff_{pt}",
                 "-provtariff_f" = "Tariff_{pt}^f",
                 "as.factor(female)0" = "",
                 "as.factor(female)1" = "Female",
                 "year" = "Year",
                 "ivid" = "Individual",
                 "hhid" = "Household",
                 "ivid02" = "Individual",
                 "hhid02" = "Household",
                 "tal" = "Wearing apparel and leather"))

df <- list(emp0204_p, emp0206_p)

etable(list(
  feols(agri ~ -tariff + i(as.factor(female), -tariff) | year + ivid,
        subset(emp0204_p),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(agri ~ -tariff + i(as.factor(female), -tariff) | year + ivid,
        data = emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(agri ~ -tariff + i(as.factor(female), -tariff_f) | year + ivid,
        data = emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(agri ~ -tariff + i(as.factor(female), -tariff_f) | year + ivid,
        data = emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)  
), tex = T)
