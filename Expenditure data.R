##############################################
# EXPENDITURE DATA ON HOUSEHOLD PUBLIC GOODS #
##############################################

exp_020406 <- c("exp_02", "exp_04", "exp_06")

for(i in exp_020406){
  assign(i, get(i) %>% 
           mutate(riceexp_share = riceexp/hhexp2rl, 
                  food_share = foodreal/hhexp2rl,
                  tobac_share = tobac12m/hhexp2rl,
                  educ_share = educex_2/hhexp2rl,
                  health_share = hlthex_2/hhexp2rl) %>%
           mutate(across(tinh, as.factor)))
  
  if(i %in% c("exp_02")){
    assign(i, left_join(get(i), preBTA_provtariff, by = "tinh"))
    
    assign(i, left_join(get(i), preBTA_provtariff_k, by = "tinh"))
    
    assign(i, get(i) %>% 
             rename(provtariff = preprov_tariff,
                    provtariff_k = preprov_tariff_k,
                    hhwt = wt30))
  }
  
  if(i %in% c("exp_04", "exp_06")){
    assign(i, left_join(get(i), postBTA_provtariff, by = "tinh"))
    
    assign(i, left_join(get(i), postBTA_provtariff_k, by = "tinh"))
    
    assign(i, get(i) %>% 
             rename(provtariff = postprov_tariff,
                    provtariff_k = postprov_tariff_k))
  }
}

exp_06 <- exp_06 %>% 
  mutate(across(xa, as.character)) %>% 
  rename(hhwt = wt9,
         urban = urban06) %>%
  mutate(across(c(xa, hoso, hhid, huyen), as.double))  

exp_04 <- exp_04 %>% 
  select(-huyen) %>% 
  rename(urban = urban04)
