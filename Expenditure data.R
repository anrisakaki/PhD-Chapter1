##############################################
# EXPENDITURE DATA ON HOUSEHOLD PUBLIC GOODS #
##############################################

expenditure_fn <- function(i){
  
  i %>%
    mutate(riceexp_share = riceexp/hhexp2rl,
           food_share = foodreal/hhexp2rl,
           tobac_share = tobac12m/hhexp2rl,
           educ_share = educex_2/hhexp2rl,
           health_share = hlthex_2/hhexp2rl)
         
}

exp_02 <- exp_02 %>% 
  expenditure_fn() %>% 
  select(tinh02, huyen02, xa02, hoso02, riceexp_share, food_share, tobac_share, educ_share, health_share, riceexp, foodreal, tobac12m, educex_2, hlthex_2, urban02, wt30, hhexp2rl) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  rename(hhwt = wt30) %>% 
  provrecode_fn() %>% 
  left_join(provtariffs02, by = "tinh_old") 

exp_04 <- exp_04 %>% 
  expenditure_fn() %>% 
  select(tinh, huyen, xa, hoso, riceexp_share, food_share, tobac_share, educ_share, health_share, riceexp, foodreal, tobac12m, educex_2, hlthex_2, urban04, hhwt, hhexp2rl) %>% 
  rename(urban = urban04) %>% 
  provrecode_fn() %>% 
  left_join(provtariffs04, by = "tinh_old") 

exp_06 <- exp_06 %>% 
  expenditure_fn() %>% 
  select(tinh, huyen, xa, hoso, riceexp_share, food_share, tobac_share, educ_share, health_share, riceexp, foodreal, tobac12m, educex_2, hlthex_2, ttnt, wt45, hhexp2rl) %>% 
  rename(urban = ttnt,
         hhwt = wt45) %>% 
  provrecode_fn() %>% 
  left_join(provtariffs06, by = "tinh_old") 

exp0204 <- bind_rows(exp_02, exp_04) 
exp0206 <- bind_rows(exp_02, exp_06) 

exp0204_p <- merge(panel0204, exp0204, by = hhid)
exp0204_p <- merge(panel0206, exp0206, by = hhid)

###################################
# EDUCATION EXPENDITURE PER CHILD #
###################################

# 2002 
juniors_02 <- m1_02 %>% 
  select(tinh02, xa02, diaban02, huyen02, hoso02, matv02, m1c2, m1c5) %>% 
  mutate(matv02 = ifelse(nchar(matv02) > 2, substr(matv02, nchar(matv02) - 1, nchar(matv02)), matv02),
         across(matv02, as.numeric)) %>% 
  rename(age = m1c5) %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0)) %>% 
  filter(age > 5, 
         age <= 18) %>% 
  select(-m1c2)

schooling_02 <- m2_02 %>% 
  select(tinh02, xa02, diaban02, huyen02, hoso02, matv02, m2c4, m2c5h) %>% 
  mutate(matv02 = ifelse(nchar(matv02) > 2, substr(matv02, nchar(matv02) - 1, nchar(matv02)), matv02),
         across(matv02, as.numeric)) %>%   
  rename(enrolled = m2c4) %>% 
  rename(educ_exp = m2c5h) %>% 
  mutate(enrolled = as.numeric(enrolled == 1)) 

schooling_02 <- merge(schooling_02, weights_02, by = c("tinh02", "huyen02", "xa02", "diaban02"))

schooling_02 <- merge(juniors_02, schooling_02, by = c("tinh02", "xa02", "diaban02", "huyen02", "hoso02", "matv02")) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())

# 2004 
schooling_04 <- m123a_04 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5, m2c4, m2c11h) %>% 
  rename(age = m1ac5, 
         enrolled = m2c4,
         educ_exp = m2c11h) %>% 
  filter(age > 5,
         age <= 18) %>% 
  mutate(female = as.numeric(m1ac2 == 2),
         enrolled = as.numeric(enrolled < 3),
         year = 2004) %>% 
  select(-m1ac2)

schooling_04 <- list(schooling_04, diaban04, weights_04) %>% 
  reduce(full_join, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

# 2006 
juniors_06 <- m1a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5) %>% 
  rename(age = m1ac5) %>% 
  mutate(female = as.numeric(m1ac2 == 2)) %>% 
  filter(age > 5,
         age <= 18) %>% 
  select(-m1ac2)

juniors_06 <- merge(juniors_06, weights_06, by = c("tinh", "huyen","xa"))

juniors_06 <- left_join(juniors_06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

schooling_06 <- m2a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m2ac5, m2ac13k) %>% 
  rename(enrolled = m2ac5,
         educ_exp = m2ac13k) %>% 
  mutate(enrolled = as.numeric(enrolled < 3),
         year = 2006)

schooling_06 <- merge(juniors_06, schooling_06, by = c("tinh", "huyen", "xa", "hoso", "matv"))

school0204 <- bind_rows(schooling_02, schooling_04)
school0206 <- bind_rows(schooling_02, schooling_06)

school0204 <- merge(school0204, bta0204, by = c("tinh", "year"))
school0206 <- merge(school0206, bta0206, by = c("tinh", "year"))

school0204_p <- merge(ivid0204, school0204, by = ivid)
school0206_p <- merge(ivid0206, school0206, by = ivid)
