#######################################################################
# HETEROGENOUS EFFECT OF BTA ON STRUCTURAL TRANSFORMATION - EDUCATION # 
########################################################### ###########

etable(list(
  feols(tal ~ as.factor(Female)*provtariff | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)*provtariff_k | year + ivid,
        subset(employment0204_p, educ < 6 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)*provtariff | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)*provtariff_k | year + ivid02,
        subset(employment0206_p, educ < 6 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ as.factor(Female)*provtariff | year + ivid,
        subset(employment0204_p, educ > 6 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)*provtariff_k | year + ivid,
        subset(employment0204_p, educ > 6 & educ < 10 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)*provtariff | year + ivid02,
        subset(employment0206_p, educ > 6 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)*provtariff_k | year + ivid02,
        subset(employment0206_p, educ > 6 & educ < 10 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

etable(list(
  feols(tal ~ as.factor(Female)*provtariff | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)*provtariff_k | year + ivid,
        subset(employment0204_p, educ > 9 & year == 2002 | year == 2004),
        vcov = ~tinh,
        weights = ~hhwt),
  feols(tal ~ as.factor(Female)*provtariff | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt),  
  feols(tal ~ as.factor(Female)*provtariff_k | year + ivid02,
        subset(employment0206_p, educ > 9 & year == 2002 | year == 2006),
        vcov = ~tinh,
        weights = ~hhwt)), tex = TRUE)

# 2002 - 2004 
## Topalova tariffs
models_0204_educ_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff | year + ivid"))
  model <- feols(formula,
                 subset(employment0204_p, educ < 10 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_educ_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0204_educ_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff_k | year + ivid"))
  model <- feols(formula,
                 subset(employment0204_p, educ < 10 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_educ_p_k_summary[[i]] <- model
}

# 2002 - 2006 
## Topalova tariffs
models_0206_educ_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff | year + ivid02"))
  model <- feols(formula,
                 subset(employment0206_p, educ < 10 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_educ_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0206_educ_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff_k | year + ivid02"))
  model <- feols(formula,
                 subset(employment0206_p, educ < 10 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_educ_p_k_summary[[i]] <- model
}

###################################################################
# HETEROGENOUS EFFECT OF BTA ON STRUCTURAL TRANSFORMATION - URBAN # 
########################################################### #######

## 2002 - 2004 
models_0204_rural_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)*provtariff | year + ivid"))
  model <- feols(formula,
                 subset(employment0204_p, urban == 2 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_rural_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0204_rural_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)*provtariff_k | year + ivid"))
  model <- feols(formula,
                 subset(employment0204_p, urban == 2 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_rural_p_k_summary[[i]] <- model
}


# 2002 - 2006 
## Topalova tariffs
models_0206_rural_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)*provtariff | year + ivid02"))
  model <- feols(formula,
                 subset(employment0206_p, urban == 2 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_rural_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0206_rural_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)*provtariff_k | year + ivid02"))
  model <- feols(formula,
                 subset(employment0206_p, urban == 2 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_rural_p_k_summary[[i]] <- model
}

# Urban 
## 2002 - 2004 
models_0204_urban_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff | year + ivid"))
  model <- feols(formula,
                 subset(employment0204_p, urban == 1 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_urban_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0204_urban_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff_k | year + ivid"))
  model <- feols(formula,
                 subset(employment0204_p, urban == 1 & year == 2002 | year == 2004),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_urban_p_k_summary[[i]] <- model
}


## 2002 - 2006 
## Topalova tariffs
models_0206_urban_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff | year + ivid02"))
  model <- feols(formula,
                 subset(employment0206_p, urban == 1 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_urban_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0206_urban_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff_k | year + ivid02"))
  model <- feols(formula,
                 subset(employment0206_p, urban == 1 & year == 2002 | year == 2006),
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_urban_p_k_summary[[i]] <- model
}