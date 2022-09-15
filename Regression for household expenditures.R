######################################################################
# SETTING UP FOR REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS #
######################################################################

# Panel data
exp_02_p <- exp_02 %>% 
  rename(hhid02 = hhid) %>% 
  select(tinh, xa02, hhid02, foodreal, educex_2, hlthex_2, riceexp_share, food_share, tobac_share, educ_share, health_share, provtariff, provtariff_k, hhwt)

exp_02_p <- merge(hhid02, exp_02_p, by = c("xa02", "hhid02")) %>% 
  mutate(year = 2002)

exp_04_p <- merge(hhid0204, exp_04, by = c("tinh", "xa", "hoso", "hhid")) %>% 
  mutate(across(tinh, as.factor)) %>% 
  select(tinh, hhid02, foodreal, educex_2, hlthex_2, riceexp_share, food_share, tobac_share, educ_share, health_share, provtariff, provtariff_k, hhwt) %>% 
  mutate(year = 2004)

exp_0402_p <- bind_rows(exp_02_p, exp_04_p)

exp_0206 <- exp_02 %>% 
  rename(hhid02 = hhid)

exp_06_p <- exp_06 %>% 
  rename(hhid06 = hhid)

exp_0206 <- merge(hhid020406, exp_0206, by = "hhid02")

exp_06_p <- merge(hhid020406, exp_06_p, by = "hhid06")

exp_0206_p <- bind_rows(exp_0206, exp_06_p)

# Cross-sectional data 

exp_0402 <- bind_rows(exp_02, exp_04)
exp_0602 <- bind_rows(exp_02, exp_06)

#######################################################################
# SETTING UP FOR REGRESSION ON EXPENDITURE ON HOUSEHOLD PRIVATE GOODS #
#######################################################################

# Panel data
## 2002 
totalexp_02_p <- totalexp_02 %>% 
  rename(hhid02 = hhid)

totalexp_02_p <- merge(hhid0204, totalexp_02_p, by = c("xa02", "hhid02")) %>% 
  select(-c("hoso02.x", "hoso02.y", "tinh")) %>% 
  rename(tempcon_ratio = tempcon_ratio_02,
         fcon_ratio = fcon_ratio_02) %>% 
  mutate(year = 2002) %>% 
  rename(tinh = tinh02) %>% 
  mutate(across(tinh, as.factor))

totalexp_0206_p <- totalexp_02 %>% 
  rename(hhid02 = hhid)

totalexp_0206_p <- merge(hhid020406, totalexp_0206_p, by = "hhid02") %>% 
  rename(tempcon_ratio = tempcon_ratio_02,
         fcon_ratio = fcon_ratio_02) %>% 
  mutate(year = 2002) %>% 
  rename(tinh = tinh02) %>% 
  mutate(across(tinh, as.factor))

totalexp_02_p <- list(totalexp_02_p, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

totalexp_0206_p <- list(totalexp_0206_p, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

## 2004 
totalexp_04_p <- merge(hhid0204, totalexp_04, by = c("tinh", "huyen", "xa", "hoso", "hhid")) %>% 
  mutate(year = 2004) %>% 
  mutate(across(tinh, as.factor))

totalexp_04_p <- list(totalexp_04_p, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)

## 2006 
totalexp_06 <- totalexp_06 %>% 
  rename(hhid06 = hhid)
totalexp_06_p <- merge(hhid020406, totalexp_06, by = "hhid06") %>% 
  mutate(year = 2006) %>% 
  mutate(across(tinh, as.factor))

totalexp_06_p <- list(totalexp_06_p, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = postprov_tariff,
         provtariff_k = postprov_tariff_k)

totalexp_0204_p <- bind_rows(totalexp_02_p, totalexp_04_p)
totalexp_0206_p <- bind_rows(totalexp_0206_p, totalexp_06_p)

#########################################################
# SETTING UP FOR REGRESSION ON FIXED AND DURABLE ASSETS #
#########################################################

# Cross-section 
## Merging data with data on tariffs 
### 2002
dur_exp_02 <- list(dur_exp_02, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(full_join, by = "tinh") %>% 
  rename(provtariff = preprov_tariff,
         provtariff_k = preprov_tariff_k)

### 2004 - 2006 
for(i in dur_exp_020406){
  if(i %in% c("dur_exp_04", "dur_exp_06")){
    
    assign(i, list(get(i), postBTA_provtariff, postBTA_provtariff_k) %>% 
             reduce(full_join, by = "tinh") %>% 
             rename(provtariff = postprov_tariff,
                    provtariff_k = postprov_tariff_k))
  }
}

dur_exp_0204 <- bind_rows(dur_exp_02, dur_exp_04)
dur_exp_0206 <- bind_rows(dur_exp_02, dur_exp_06)

# Panel data 
## 2002 - 2004 
dur_exp_02_p <- dur_exp_02 %>% 
  rename(hhid02 = hhid)

dur_exp_02_p <- merge(hhid02, dur_exp_02_p, by = c("hhid02", "xa02"))

dur_exp_04_p <- merge(hhid0204, dur_exp_04, by = c("tinh", "hhid")) %>% 
  mutate(across(tinh, as.factor))

## 2002 - 2006 
dur_exp_0602_p <- dur_exp_02 %>% 
  rename(hhid02 = hhid)

dur_exp_0602_p <- merge(hhid020406, dur_exp_0602_p, by = "hhid02")

dur_exp_06_p <- dur_exp_06 %>% 
  rename(hhid06 = hhid)

dur_exp_06_p <- merge(hhid020406, dur_exp_06_p, by = "hhid06")

dur_exp_0204_p <- bind_rows(dur_exp_02_p, dur_exp_04_p)
dur_exp_0206_p <- bind_rows(dur_exp_0602_p, dur_exp_06_p)

########################################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS USING PANEL DATA #
########################################################################

y_exp <- c("log(food_share)", "log(tobac_share)", "log(educ_share)", "log(health_share)")

# 2002 - 2004 
exp_tce_0204_hhfe_p_summary <- list()
exp_tce_k_0204_hhfe_p_summary <- list()

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

# 2002 - 2006 
exp_tce_0206_hhfe_p_summary <- list()
exp_tce_k_0206_hhfe_p_summary <- list()

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

#########################################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PRIVATE GOODS USING PANEL DATA #
#########################################################################

y <- c("log(fcon_ratio)", "log(tempcon_ratio)")

fptg_tce_0402_model_summary <- list()
fptg_tce_0602_model_summary <- list()
fptg_tce_k_0402_model_summary <- list()
fptg_tce_k_0602_model_summary <- list()

for(i in y){
  formula <- as.formula(paste(i, " ~ provtariff | year + hhid"))
  model <- feols(formula,
                 data = totalexp_0204_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  fptg_tce_0402_model_summary[[i]] <- model  
}

for(i in y){
  formula <- as.formula(paste(i, " ~ provtariff_k | year + hhid"))
  model <- feols(formula,
                 data = totalexp_0204_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  fptg_tce_k_0402_model_summary[[i]] <- model  
}


for(i in y){
  formula <- as.formula(paste(i, " ~ provtariff | year + hhid02"))
  model <- feols(formula,
                 data = totalexp_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  fptg_tce_0602_model_summary[[i]] <- model  
}

for(i in y){
  formula <- as.formula(paste(i, " ~ provtariff_k | year + hhid02"))
  model <- feols(formula,
                 data = totalexp_0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  fptg_tce_k_0602_model_summary[[i]] <- model  
}

##################################################################################
# REGRESSION ON EXPENDITURE ON HOUSEHOLD PUBLIC GOODS USING CROSS SECTIONAL DATA #
##################################################################################

# 2002 - 2004 
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

# 2002 - 2006
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