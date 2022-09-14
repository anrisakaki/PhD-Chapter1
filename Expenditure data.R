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
  rename(hhwt = wt9) %>%
  mutate(across(c(xa, hoso, hhid, huyen), as.double))  

exp_04 <- exp_04 %>% 
  select(-huyen)

###############################################
# EXPENDITURE DATA ON HOUSEHOLD PRIVATE GOODS #
###############################################

# 2002 
## Daily non food expenditures 
daily_nonfood_02 <- m6b1_02 %>%
  select(hhid, madong, m6b1c4) %>% 
  group_by(hhid) %>% 
  pivot_wider(names_from = madong, values_from = m6b1c4, values_fn = mean) %>% 
  replace(is.na(.), 0) %>% 
  rename("cosmetics" = 20)

## Annual non-food expenditures 
nonfood_exp_02 <- m6b2_02 %>% 
  select(-c("m6b2c4", "m6b2cong", "m6b2c5")) %>% 
  rename (fabric = m6b2c2h301) %>% 
  rename (clothing = m6b2c2h302) %>% 
  rename (scarves = m6b2c2h304) %>% 
  rename (sewing = m6b2c2h306) %>% 
  rename (tailor = m6b2c2h307) %>% 
  rename (shoes = m6b2c2h308) %>% 
  rename (bags = m6b2c2h314) %>% 
  rename (jewelry = m6b2c2h329) %>% 
  rename (domserv = m6b2c2h331)

## Food expenditures 
food_exp_02 <- m6a2_02 %>%
  select(hhid, madong, m6a2c6) %>% 
  group_by(hhid) %>% 
  pivot_wider(names_from = madong, values_from = m6a2c6, values_fn = mean) %>% 
  replace(is.na(.), 0)  

## Merging dataframes together to construct a total expenditures dataframe 
ttexp_02 <- exp_02 %>%
  select(hhid, hhexp2rl)

totalexp_02 <- list(nonfood_exp_02, daily_nonfood_02, food_exp_02, ttexp_02) %>% 
  reduce(full_join, by = "hhid") %>% 
  select(-c(tinh, xa, hoso, qui, phieu)) %>% 
  select(c("tinh02", "xa02", "hoso02", "hhid"), everything()) %>% 
  mutate(across(hoso02, as.numeric))

totalexp_02 <- totalexp_02 %>% 
  rename(alcohol = 154, 
         cigarettes = 156)

# 2004 
## Daily non food expenditures 
daily_nonfood_04 <- m5b1_04 %>%
  select(tinh, huyen, xa, diaban, hoso, ky, hhid, m5b1ma, m5b1c4) %>%  
  replace(is.na(.), 0) %>%
  group_by(hhid) %>% 
  pivot_wider(names_from = m5b1ma, values_from = m5b1c4, values_fill = 0)

## Annual non food expenditures 
annual_nonfood_04 <- m5b2_04 %>%
  select(hhid, m5b2ma, m5b2c2) %>% 
  replace(is.na(.), 0) %>% 
  group_by(hhid) %>% 
  pivot_wider(names_from = m5b2ma, values_from = m5b2c2, values_fill = 0)

## Food expenditures 
food_exp_04 <- m5a2_04 %>%
  select(hhid, m5a2ma, m5a2c6) %>% 
  group_by(hhid) %>% 
  pivot_wider(names_from = m5a2ma, values_from = m5a2c6, values_fill = 0)

## Merging dataframes together to construct a total expenditures dataframe
ttexp_04 <- exp_04 %>%
  select(hhid, hhexp2rl)

totalexp_04 <- list(annual_nonfood_04, daily_nonfood_04, food_exp_04, ttexp_04) %>% 
  reduce(full_join, by = "hhid") %>% 
  select(c("tinh", "huyen", "xa", "diaban", "hoso", "hhid"), everything()) %>% 
  select(-"ky") %>% 
  replace(is.na(.), 0)

## Renaming variables 
totalexp_04 <- totalexp_04 %>% 
  replace(is.na(.), 0) %>%
  mutate(across(tinh, as.factor)) %>%
  rename(
    "liquor" = 92,
    "beer" = 76,
    "cigarettes" = 78,
    "cosmetics" = 55,
    "fabric" = 16,
    "tailor" = 17,
    "jewelry" = 27,
    "domserv" = 37,
    "scarves" = 9)

# 2006 
# Daily non food expenditures
daily_nonfood_06 <- m5b1_06 %>%
  select(tinh, huyen, xa, diaban, hoso, hhid, m5b1c1, m5b1c4) %>% 
  replace(is.na(.), 0) %>% 
  pivot_wider(names_from = m5b1c1, values_from = m5b1c4, values_fill = 0)

# Annual non food expenditures
annual_nonfood_06 <- m5b2_06 %>% 
  select(hhid, m5b2c1, m5b2c2) %>% 
  replace(is.na(.), 0) %>% 
  pivot_wider(names_from = m5b2c1, values_from = m5b2c2, values_fill = 0)

# Food expenditures 
food_exp_06 <- m5a2_06 %>%
  select(hhid, m5a2c1, m5a2c6) %>% 
  group_by(hhid) %>% 
  pivot_wider(names_from = m5a2c1, values_from = m5a2c6, values_fill = 0)

ttexp_06 <- exp_06 %>%
  select(hhid, hhexp2rl)

totalexp_06 <- list(daily_nonfood_06, annual_nonfood_06, food_exp_06, ttexp_06) %>% 
  reduce(full_join, by = "hhid") %>% 
  select(c("tinh", "huyen", "xa", "diaban", "hoso", "hhid"), everything()) %>% 
  replace(is.na(.), 0)  

totalexp_06 <- totalexp_06 %>% 
  replace(is.na(.), 0) %>% 
  rename(
    "liquor" = 84,
    "beer" = 104,
    "cigarettes" = 76,
    "cosmetics" = 21,
    "fabric" = 43,
    "tailor" = 45,
    "jewelry" = 34,
    "domserv" = 59,
    "scarves" = 42  
  )  

#Calculating expenditure on FPG and temptation goods as share of total HH expenditure 
totalexp_02 <- totalexp_02 %>% 
  mutate(tempcon_ratio_02 = (alcohol + cigarettes) / hhexp2rl) %>% 
  mutate(fcon = fabric + sewing + tailor + jewelry + scarves + cosmetics) %>% 
  mutate (fcon_ratio_02 = fcon/hhexp2rl)

totalexp0406 <- c("totalexp_04", "totalexp_06")

for( i in totalexp0406) {
  assign(i, mutate(get(i),
                   tempcon_ratio = (liquor + beer + cigarettes)/hhexp2rl,
                   fcon_ratio = (fabric + tailor + jewelry + scarves + cosmetics)/hhexp2rl))
}