#############################################################
# SETTING UP FOR REGRESSION ON BTA AND THE SPOUSAL WAGE GAP #
#############################################################

hhid02 <- hhid0204 %>% 
  select(hhid02, hhid)
hhid06 <- hhid020406 %>% 
  select(hhid02, hhid06)

exp_spouse_02_p <- merge(hhid02, exp_spouse_02, by = "hhid02")
