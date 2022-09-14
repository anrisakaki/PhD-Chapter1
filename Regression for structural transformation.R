##########################################################
# SETTING UP FOR REGRESSION ON STRUCTURAL TRANSFORMATION #
##########################################################

emp02 <- c("employment_mf_02", "employment_mf_02p")

for(i in emp02){
  
  assign(i, get(i) %>% 
           select(-c(diaban02)) %>% 
           mutate(across(tinh, as.factor))
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