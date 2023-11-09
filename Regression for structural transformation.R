##########################################################
# SETTING UP FOR REGRESSION ON STRUCTURAL TRANSFORMATION #
##########################################################

employment_0204 <- bind_rows(employment_mf_02, employment_mf_04)
employment_0206 <- bind_rows(employment_mf_02, employment_mf_06)

employment_0204 <- merge(employment_0204, provtariffs0204, by = c("tinh", "year"))
employment_0206 <- merge(employment_0206, provtariffs0206, by = c("tinh", "year"))

emp0204_p <- merge(employment_0204, ivid0204, by = ivid) %>% 
  mutate(inc = ifelse(inc == 0, NA, inc))

emp0206_p <- merge(employment_0206, ivid0206, by = ivid) %>% 
  mutate(inc = ifelse(inc == 0, NA, inc))

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

hours_twfe <- lapply(df, function(df) {
  feols(log(hours) ~ i(as.factor(female), -provtariff) | year + ivid, data = df, weights = ~hhwt, vcov = ~tinh)
})

days_twfe <- lapply(df, function(df) {
  feols(log(days) ~ i(as.factor(female), -provtariff) | year + ivid, data = df, weights = ~hhwt, vcov = ~tinh)
})

tal_twfe <- lapply(df, function(df) {
  feols(tal ~ i(as.factor(female), provtariff) | year + ivid, data = df, weights = ~hhwt, vcov = ~tinh)
})

etable(list(feols(log(inc) ~ i(as.factor(female), -provtariff) | year + hhid,
                  data = emp0204_p,
                  weights = ~hhwt,
                  vcov = ~tinh),
            feols(log(inc) ~ i(as.factor(female), -provtariff) | year + hhid,
                  data = emp0206_p,
                  weights = ~hhwt,
                  vcov = ~tinh)), tex = T)

etable(hours_twfe, tex = T)

etable(days_twfe, tex = T)

etable(list(
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        data = emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(tal ~ i(as.factor(female), -provtariff) | year + ivid,
        data = emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(tal ~ i(as.factor(female), -provtariff_f) | year + ivid,
        data = emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(tal ~ i(as.factor(female), -provtariff_f) | year + ivid,
        data = emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)  
), tex = T)

etable(list(
  feols(tal ~ i(as.factor(female), -provtariff_k) | year + ivid,
        data = emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(tal ~ i(as.factor(female), -provtariff_k) | year + ivid,
        data = emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh) 
), tex = T)

 etable(list(
  feols(housework ~ provtariff | year + hhid,
        subset(emp0204_p, female == 0 & married == 1),
        weights = ~hhwt,
        vcov = ~tinh),
  feols(housework ~ provtariff | year + hhid,
        subset(emp0206_p, female == 0 & married == 1),
        weights = ~hhwt,
        vcov = ~tinh)), tex = T)

