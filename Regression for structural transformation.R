emp0204_p <- emp0204_p %>% 
  mutate(private_manu = ifelse(private == 1 & manu == 1, 1, 0),
         fdi_manu = ifelse(fdi == 1 & manu == 1, 1, 0),
         private_manu = ifelse(is.na(private_manu), 0, private_manu),
         fdi_manu = ifelse(is.na(fdi_manu), 0, fdi_manu),
         hhbus_recode = ifelse(is.na(hhbus), 0, hhbus))

################################################
# REGRESSION ON FORMALISATION USING PANEL DATA #
################################################

etable(list(
  # All observations 
  feols(hhbus ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(private ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(private == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  # Agriculture in 2002 
  feols(private == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1),
        weights = ~hhwt,
        vcov = ~diaban),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, agri_2002 == 1),
        weights = ~hhwt,
        vcov = ~diaban),
  
  # Manufacturing in 2002 
  feols(private == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1),
        weights = ~hhwt,
        vcov = ~diaban),
  feols(fdi == 1 & manu == 1 ~ i(as.factor(female), mccaig_bta) + as.factor(female) | year + hhid,
        subset(emp0204_p, manu_2002 == 1),
        weights = ~hhwt,
        vcov = ~diaban)
), tex = T)

#########################################
# REGRESSION ON INCOME USING PANEL DATA #
#########################################

etable(list(
  feols(log(rlinc) ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        inc0204_p,
        weights = ~hhwt,
        vcov = ~diaban),
  feols(log(imputed_income) ~ i(as.factor(female), -mccaig_bta) + as.factor(female) | year + hhid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~diaban)
), tex = T)

