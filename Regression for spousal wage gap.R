#############################################################
# SETTING UP FOR REGRESSION ON BTA AND THE SPOUSAL WAGE GAP #
#############################################################

hhid02 <- hhid0204 %>% 
  select(hhid02, hhid)
hhid06 <- hhid020406 %>% 
  select(hhid02, hhid06) %>% 
  rename(hhid = hhid06)

exp_spouse_02_p <- merge(hhid02, exp_spouse_02, by = "hhid02") %>% 
  mutate(year = 2002)
exp_spouse_04_p <- merge(hhid02, exp_spouse_04, by = "hhid") %>% 
  mutate(year = 2004)


exp_spouse_0602_p <- merge(hhid06, exp_spouse_02, by = "hhid02") %>% 
  mutate(year = 2002)
exp_spouse_06_p <- merge(hhid06, exp_spouse_06, by = "hhid") %>% 
  mutate(year = 2006)

exp_spouse_0204_p <- bind_rows(exp_spouse_02_p, exp_spouse_04_p)
exp_spouse_0206_p <- bind_rows(exp_spouse_02_p, exp_spouse_06_p)

###############################################
# REGRESSION FOR BTA AND THE SPOUSAL WAGE GAP #
###############################################

y_exp <- c("food_share", "educ_share", "health_share", "tobac_share")
y_exp_nom <- c("log(foodreal)", "log(tobac12m)", "log(educex_2)", "log(hlthex_2)")

# 2002 - 2004 
exp_tce_0204_hhfe_p_summary <- list()
exp_tce_k_0204_hhfe_p_summary <- list()

exp_tce_0204_nom_hhfe_p_summary <- list()
exp_tce_k_0204_nom_hhfe_p_summary <- list()

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff | hhid02 + year"))
  model <- feols(formula,
                 data = exp_spouse_0204_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_0204_hhfe_p_summary[[i]] <- model
}

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff_k | hhid02 + year"))
  model <- feols(formula,
                 data = exp_spouse_0204_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_0204_hhfe_p_summary[[i]] <- model
}

## 2002 - 2006 
exp_tce_0206_hhfe_p_summary <- list()
exp_tce_k_0206_hhfe_p_summary <- list()
exp_tce_0206_nom_hhfe_p_summary <- list()
exp_tce_k_0206_nom_hhfe_p_summary <- list()

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff | hhid02 + year"))
  model <- feols(formula,
                 data = exp_spouse_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_0206_hhfe_p_summary[[i]] <- model
}

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff_k | hhid02 + year"))
  model <- feols(formula,
                 data = exp_spouse_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_0206_hhfe_p_summary[[i]] <- model
}