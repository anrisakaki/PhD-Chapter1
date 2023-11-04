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
                  health_share = hlthex_2/hhexp2rl))
         
}

exp_02 <- exp_02 %>% 
  mutate(year = 2002) %>% 
  select(year, tinh02, huyen02, xa02, hoso02, riceexp_share, food_share, tobac_share, educ_share, health_share, riceexp, foodreal, tobac12m, educex_2, hlthex_2, urban02, wt30, hhexp2rl) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  rename(hhwt = wt30)

exp_02 <- inner_join(exp_02, diaban02, by = c("tinh" = "tinh02", "huyen" = "huyen02", "xa" = "xa02")) %>% 
  rename(diaban = diaban02)

exp_04 <- exp_04 %>% 
  mutate(year = 2004) %>% 
  select(year, tinh, huyen, xa, hoso, riceexp_share, food_share, tobac_share, educ_share, health_share, riceexp, foodreal, tobac12m, educex_2, hlthex_2, urban04, hhwt, hhexp2rl) %>% 
  rename(urban = urban04)

exp_04 <- merge(exp_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

exp_06 <- exp_06 %>% 
  mutate(year = 2006) %>% 
  select(year, tinh, huyen, xa, hoso, riceexp_share, food_share, tobac_share, educ_share, health_share, riceexp, foodreal, tobac12m, educex_2, hlthex_2, urban06, wt9, hhexp2rl) %>% 
  rename(urban = urban06,
         hhwt = wt9)

exp_06 <- inner_join(exp_06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

exp0204 <- bind_rows(exp_02, exp_04) %>% select(tinh, huyen, xa, diaban, hoso, everything())
exp0206 <- bind_rows(exp_02, exp_06) %>% select(tinh, huyen, xa, diaban, hoso, everything())

exp0204 <- merge(exp0204, provtariffs0204, by = c("tinh", "year"))
exp0206 <- merge(exp0206, provtariffs0206, by = c("tinh", "year"))

exp0204_p <- merge(panel0204, exp0204, by = hhid)
exp0206_p <- merge(panel0206, exp0206, by = hhid)

save(exp0204_p, file = "exp0204_p.rda")
save(exp0206_p, file = "exp0206_p.rda")