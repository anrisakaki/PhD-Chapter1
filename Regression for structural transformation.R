##########################################################
# SETTING UP FOR REGRESSION ON STRUCTURAL TRANSFORMATION #
##########################################################

emp02 <- c("employment_mf_02", "employment_mf_02p")

for(i in emp02){
  
  assign(i, get(i) %>% 
           select(-c(diaban02)) %>% 
           mutate(across(tinh, as.factor)) %>% 
           mutate(traded_nonagri = as.numeric(traded == 1 & agri_work == 0),
                  traded_manu = as.numeric(traded == 1 & manu == 1))
  )
  
  assign(i, left_join(get(i), preBTA_provtariff, by = "tinh")) # Province-level tariffs created per McCaig 
  
  assign(i, get(i) %>%
           rename(
             married = m1c6,
             age = m1c5,
             provtariff = preprov_tariff,
             industry2 = industry
           ) %>%
           filter(m3c2 == 1) %>%
           mutate(year = 2002))
  
  if (i %in% c("employment_mf_02p")){
    assign(i, get(i) %>%
             select(-ends_with("02")))
  }
  
  if (i %in% c("employment_mf_02")){
    assign(i, get(i) %>%
             rename(huyen= huyen02) %>%
             select(-ends_with(c(".x", ".y"))))
  }
  
  assign(i, left_join(get(i), preBTA_provtariff_k, by = "tinh")) # Province-level tariffs created per Kovak
  
  assign(i, get(i) %>% rename(provtariff_k = preprov_tariff_k))
  
  assign(i, get(i) %>%
           mutate(across(industry2, as.factor)))
  
  assign(i,get(i) %>%
           mutate(
             age1 = as.numeric(age > 17 & age < 26),
             age2 = as.numeric(age > 25 & age < 36),
             age3 = as.numeric(age > 35 & age < 46),
             age4 = as.numeric(age > 45 & age < 56),
             age5 = as.numeric(age > 55 & age < 66)))
}


postBTA_tariffs <- c("postBTA_provtariff", "postBTA_provtariff_k")

for(i in postBTA_tariffs){
  
  assign(i, get(i) %>% mutate(across(tinh, as.factor)))
}

emp0406 <- c("employment_mf_04", "employment_mf_04p", "employment_mf_06", "employment_mf_06p")

for (i in emp0406){
  
  assign(i, get(i) %>%
           mutate(across(tinh, as.factor))
  )
  
  assign(i, get(i) %>% 
           mutate(traded_nonagri = as.numeric(traded == 1 & agri_work == 0),
                  traded_manu = as.numeric(traded == 1 & manu == 1)))
  
  assign(i, left_join(get(i), postBTA_provtariff, by = "tinh")) # Province-level tariffs created per McCaig   
  
  assign(i, left_join(get(i), postBTA_provtariff_k, by = "tinh"))
  
  if (i %in% c("employment_mf_04", "employment_mf_04p")){
    assign(i, get(i) %>%
             rename("industry2" = industry,
                    "educ" = m2c1,
                    "married" = "m1ac6",
                    "age" = "m1ac5",
                    "provtariff" = "postprov_tariff") %>%
             mutate(year = 2004) %>%
             filter(m4ac2 == 1))
  }
  
  if(i %in% c("employment_mf_06", "employment_mf_06p")){
    
    educ_06 <- m2a_06 %>%
      select(tinh, huyen, xa, diaban, hoso, matv, m2ac1)
    
    assign(i, merge(get(i), educ_06, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv")))
    
    assign(i, get(i) %>% 
             select(c(-ends_with(c(".x", ".y")))) %>% 
             rename("educ" = m2ac1,
                    "industry2" = industry,
                    "age" = "m1ac5",
                    "provtariff" = "postprov_tariff") %>% 
             mutate(year = 2006) %>%
             filter(m4ac2 == 1))
  }
  
  assign(i,get(i) %>%
           rename(provtariff_k = postprov_tariff_k))
  
  assign(i, get(i) %>% mutate(across(industry2, as.factor)))
  
  assign(i,get(i) %>%
           mutate(
             age1 = as.numeric(age > 17 & age < 26),
             age2 = as.numeric(age > 25 & age < 36),
             age3 = as.numeric(age > 35 & age < 46),
             age4 = as.numeric(age > 45 & age < 56),
             age5 = as.numeric(age > 55 & age < 66)))
}

employment0204_p <- bind_rows(employment_mf_02p, employment_mf_04p) %>% 
  mutate(Female = as.numeric(sex == "Female"))

employment_mf_0206_p <- merge(ivid020406, employment_mf_02, by = "ivid02")

employment0206_p <- bind_rows(employment_mf_0206_p, employment_mf_06p) %>% 
  mutate(Female = as.numeric(sex == "Female"))

employment0204 <- bind_rows(employment_mf_02, employment_mf_04) %>% 
  mutate(Female = as.numeric(sex == "Female"))

employment_mf_06 <- employment_mf_06 %>% 
  mutate(across(c(huyen, diaban, xa, hoso, matv), as.numeric)) %>% 
  mutate(across(tinh, as.factor))

employment0206 <- bind_rows(employment_mf_02, employment_mf_06) %>% 
  mutate(Female = as.numeric(sex == "Female"))

y <- c("agri_work", "manu", "tal", "construction", "traded_manu")

############################################################
# REGRESSION ON STRUCTURAL TRANSFORMATION USING PANEL DATA #
############################################################

# 2002 - 2004 
## Topalova tariffs
models_0204_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ as.factor(Female)/provtariff | year + ivid"))
  model <- feols(formula,
                 data = employment0204_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0204_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)/provtariff_k | year + ivid"))
  model <- feols(formula,
                 data = employment0204_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_p_k_summary[[i]] <- model
}

# 2002 - 2006 
## Topalova tariffs
models_0206_p_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)/provtariff | year + ivid02"))
  model <- feols(formula,
                 data = employment0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_p_summary[[i]] <- model
}

## Kovak tariffs 
models_0206_p_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)/provtariff_k | year + ivid02"))
  model <- feols(formula,
                 data = employment0206_p,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_p_k_summary[[i]] <- model
}

#############################################
# REGRESSION FOR LABOUR FORCE PARTICIPATION #
#############################################

etable(list(
  feols()
))

######################################################################
# REGRESSION ON STRUCTURAL TRANSFORMATION USING CROSS SECTIONAL DATA #
######################################################################

# 2002 - 2004 
## Topalova tariffs 
models_0204_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)*provtariff + age + age^2  + educ + factor(urban) | year + tinh"))
  model <- feols(formula,
                 data = employment0204,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_summary[[i]] <- model
}

## Kovak tariffs 
models_0204_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)*provtariff_k + age + age^2 + i(urban) + educ| year + tinh"))
  model <- feols(formula,
                 data = employment0204,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0204_k_summary[[i]] <- model
}

# 2002 - 2006 
## Topalova tariffs 
models_0206_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)*provtariff + age + age^2 + educ + factor(urban) | year + tinh"))
  model <- feols(formula,
                 data = employment0206,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_summary[[i]] <- model
}

## Kovak tariffs 
models_0206_k_summary <- list()

for (i in y){
  formula <- as.formula(paste(i, " ~ factor(sex)/provtariff_k + age + age^2 + i(urban) + educ| year + tinh"))
  model <- feols(formula,
                 data = employment0206,
                 vcov = ~tinh,
                 weights = ~hhwt)
  
  models_0206_k_summary[[i]] <- model
}

###############################################################
# SUMMARY TABLES FOR REGRESSIONS ON STRUCTURAL TRANSFORMATION #
###############################################################

# Panel data 
## Agriculture 
etable(list(
  models_0204_p_summary[[1]],
  models_0204_p_k_summary[[1]],
  models_0206_p_summary[[1]],
  models_0206_p_k_summary[[1]]),
  tex = TRUE
)

## Manufacturing 
etable(list(
  models_0204_p_summary[[2]],
  models_0204_p_k_summary[[2]],
  models_0206_p_summary[[2]],
  models_0206_p_k_summary[[2]]
),
tex = TRUE
)

## Wearing apparel and leather 
etable(list(
  models_0204_p_summary[[3]],
  models_0204_p_k_summary[[3]],
  models_0206_p_summary[[3]],
  models_0206_p_k_summary[[3]]
),
tex = TRUE
)

## Construction 
etable(list(
  models_0204_p_summary[[4]],
  models_0204_p_k_summary[[4]],
  models_0206_p_summary[[4]],
  models_0206_p_k_summary[[4]]
  ),
  tex = TRUE
)
