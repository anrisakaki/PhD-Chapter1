###############################################################
# SETTING UP FOR REGRESSION ON EXPENDITURE ON HOUSEHOLD GOODS #
###############################################################
# Panel data
exp_02_p <- exp_02 %>% 
  select(tinh, xa02, hhid02, foodreal, educex_2, hlthex_2, tobac12m, riceexp_share, food_share, tobac_share, educ_share, health_share, hhexp2rl, provtariff, provtariff_k, hhwt, urban)

exp_02_p <- merge(hhid02, exp_02_p, by = c("xa02", "hhid02")) %>% 
  mutate(year = 2002)

exp_04_p <- merge(hhid0204, exp_04, by = c("tinh", "xa", "hoso", "hhid")) %>% 
  mutate(across(tinh, as.factor)) %>% 
  select(tinh, hhid02, foodreal, educex_2, hlthex_2, tobac12m, riceexp_share, food_share, tobac_share, educ_share, health_share, hhexp2rl, provtariff, provtariff_k, hhwt, urban) %>% 
  mutate(year = 2004)


exp_0206 <- exp_02
exp_06_p <- exp_06 %>% 
  rename(hhid06 = hhid)
exp_0206 <- merge(hhid020406, exp_0206, by = "hhid02")
exp_06_p <- merge(hhid020406, exp_06_p, by = "hhid06")

# Panel data 

exp_0402_p <- bind_rows(exp_02_p, exp_04_p)
exp_0206_p <- bind_rows(exp_0206, exp_06_p)

# Cross-sectional data 

exp_0402 <- bind_rows(exp_02, exp_04)
exp_0602 <- bind_rows(exp_02, exp_06)

save(exp_0402_p, file = "exp_0402_p.rda")
save(exp_0206_p, file = "exp_0206_p.rda")

########################################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS USING PANEL DATA #
########################################################################

y_exp <- c("food_share", "tobac_share", "educ_share", "health_share")

y_exp_nom <- c("log(foodreal)", "log(tobac12m)", "log(educex_2)", "log(hlthex_2)")

load("exp_0402_p.rda")
load("exp_0206_p.rda")

# 2002 - 2004 
exp_tce_0204_hhfe_p_summary <- list()
exp_tce_k_0204_hhfe_p_summary <- list()

exp_tce_0204_nom_hhfe_p_summary <- list()
exp_tce_k_0204_nom_hhfe_p_summary <- list()

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff | hhid02 + year"))
  model <- feols(formula,
                 data = exp_0402_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_0204_hhfe_p_summary[[i]] <- model
}

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff_k | hhid02 + year"))
  model <- feols(formula,
                 data = exp_0402_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_0204_hhfe_p_summary[[i]] <- model
}

for (i in y_exp_nom){
  formula <- as.formula(paste(i, " ~ provtariff | hhid02 + year"))
  model <- feols(formula,
                 data = exp_0402_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_0204_nom_hhfe_p_summary[[i]] <- model
}

for (i in y_exp_nom){
  formula <- as.formula(paste(i, " ~ provtariff_k | hhid02 + year"))
  model <- feols(formula,
                 data = exp_0402_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_0204_nom_hhfe_p_summary[[i]] <- model
}

## 2002 - 2006 
exp_tce_0206_hhfe_p_summary <- list()
exp_tce_k_0206_hhfe_p_summary <- list()
exp_tce_0206_nom_hhfe_p_summary <- list()
exp_tce_k_0206_nom_hhfe_p_summary <- list()

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff | hhid02 + yearint"))
  model <- feols(formula,
                 data = exp_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_0206_hhfe_p_summary[[i]] <- model
}

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff_k | hhid02 + yearint"))
  model <- feols(formula,
                 data = exp_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_0206_hhfe_p_summary[[i]] <- model
}

for (i in y_exp_nom){
  formula <- as.formula(paste(i, " ~ provtariff | hhid02 + yearint"))
  model <- feols(formula,
                 data = exp_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_0206_nom_hhfe_p_summary[[i]] <- model
}

for (i in y_exp_nom){
  formula <- as.formula(paste(i, " ~ provtariff_k | hhid02 + yearint"))
  model <- feols(formula,
                 data = exp_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_0206_nom_hhfe_p_summary[[i]] <- model
}

##################################################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS USING CROSS SECTIONAL DATA #
##################################################################################

# With province-level tariff as RHS 
## 2002 - 2004 
exp_tce_models_0204_summary <- list()
exp_tce_k_models_0204_summary <- list()

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff + hhexp2rl | yearint + tinh"))
  model <- feols(formula,
                 data = exp_0402,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_models_0204_summary[[i]] <- model
}

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff_k + hhexp2rl | yearint + tinh"))
  model <- feols(formula,
                 data = exp_0402,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_models_0204_summary[[i]] <- model
}

## 2002 - 2006
exp_tce_models_0206_summary <- list()
exp_tce_k_models_0206_summary <- list()

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff + hhexp2rl | yearint + tinh"))
  model <- feols(formula,
                 data = exp_0602,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_models_0206_summary[[i]] <- model
}

for (i in y_exp){
  formula <- as.formula(paste(i, " ~ provtariff_k + hhexp2rl | yearint + tinh"))
  model <- feols(formula,
                 data = exp_0602,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  exp_tce_k_models_0206_summary[[i]] <- model
}

##########################################################
# SUMMARY TABLES FOR REGRESSIONS ON HOUEHOLD EXPENDITURE #
##########################################################

# TCE 
## 2002 - 2004 
etable(list(
  # Food 
  exp_tce_0204_hhfe_p_summary[[1]],
  exp_tce_k_0204_hhfe_p_summary[[1]],
  # Education 
  exp_tce_0204_hhfe_p_summary[[3]],
  exp_tce_k_0204_hhfe_p_summary[[3]],
  #Health
  exp_tce_0204_hhfe_p_summary[[4]],
  exp_tce_k_0204_hhfe_p_summary[[4]]
), tex = TRUE)


etable(list(
  # Food 
  exp_tce_0204_nom_hhfe_p_summary[[1]],
  exp_tce_k_0204_nom_hhfe_p_summary[[1]],
  # Education 
  exp_tce_0204_nom_hhfe_p_summary[[3]],
  exp_tce_k_0204_nom_hhfe_p_summary[[3]],
  #Health
  exp_tce_0204_nom_hhfe_p_summary[[4]],
  exp_tce_k_0204_nom_hhfe_p_summary[[4]]
), tex = TRUE)

etable(list(
  exp_tce_0204_hhfe_p_summary[[2]],
  exp_tce_k_0204_hhfe_p_summary[[2]]
), tex = TRUE)

etable(list(
  exp_tce_0204_nom_hhfe_p_summary[[2]],
  exp_tce_k_0204_nom_hhfe_p_summary[[2]]),
  tex = TRUE)

# 2002 - 2006 
etable(list(
  # Food 
  exp_tce_0206_hhfe_p_summary[[1]],
  exp_tce_k_0206_hhfe_p_summary[[1]],
  # Education 
  exp_tce_0206_hhfe_p_summary[[3]],
  exp_tce_k_0206_hhfe_p_summary[[3]],
  #Health
  exp_tce_0206_hhfe_p_summary[[4]],
  exp_tce_k_0206_hhfe_p_summary[[4]]  
), tex = TRUE)

etable(list(
  # Food 
  exp_tce_0206_nom_hhfe_p_summary[[1]],
  exp_tce_k_0206_nom_hhfe_p_summary[[1]],
  # Education 
  exp_tce_0206_nom_hhfe_p_summary[[3]],
  exp_tce_k_0206_nom_hhfe_p_summary[[3]],
  #Health
  exp_tce_0206_nom_hhfe_p_summary[[4]],
  exp_tce_k_0206_nom_hhfe_p_summary[[4]]  
), tex = TRUE)

etable(list(
  exp_tce_0204_nom_hhfe_p_summary[[2]],
  exp_tce_k_0204_nom_hhfe_p_summary[[2]],
  exp_tce_0206_nom_hhfe_p_summary[[2]],
  exp_tce_k_0206_nom_hhfe_p_summary[[2]]  
), tex = T)
