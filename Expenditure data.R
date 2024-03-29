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

school0204 <- merge(school0204, provtariffs0204, by = c("tinh", "year"))
school0206 <- merge(school0206, provtariffs0206, by = c("tinh", "year"))

school0204_p <- merge(ivid0204, school0204, by = ivid)
school0206_p <- merge(ivid0206, school0206, by = ivid)

# Education level of wife 

educwife_0204 <- emp0204_p %>% 
  filter(work == 1 & female == 1 & relationship == 2) %>% 
  select(year, hhid, ivid, tinh, huyen, xa, diaban, hoso, matv, educ)

educwife_0206 <- emp0206_p %>% 
  filter(work == 1 & female == 1 & relationship == 2) %>% 
  select(year, hhid, ivid, tinh, huyen, xa, diaban, hoso, matv, educ)

educ_exp_0204_p <- merge(educwife_0204, exp0204_p, by = c("year", "tinh", "huyen", "xa", "diaban", "hoso", "hhid"))
educ_exp_0206_p <- merge(educwife_0206, exp0206_p, by = c("year", "tinh", "huyen", "xa", "diaban", "hoso")) %>% 
  select(-hhid.x) %>% 
  rename(hhid = hhid.y)

# Savings 

savings_02 <- m6b34_02 %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, m6b3c2h404, m6b3c2h405, m6b4c2h509b, m6b4c2h509d) %>% 
  rename(savings = m6b4c2h509b, 
         wedding = m6b3c2h404,
         funeral = m6b3c2h405,
         gold = m6b4c2h509d) %>%
  rename_with(~ str_replace(.x, "02", ""), everything()) %>%   
  mutate(year = 2002)

savings_04 <- m5b34_04 %>% 
  select(tinh, huyen, xa, hoso, m5b3c2_4, m5b3c2_5, m5b4c2_3, m5b4c2_4) %>% 
  rename(wedding = m5b3c2_4,
         funeral = m5b3c2_5,
         gold = m5b4c2_3, 
         savings = m5b4c2_4) %>% 
  mutate(year = 2004)
savings_04 <- merge(savings_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

savings_06 <- m5b34_06 %>% 
  select(tinh, huyen, xa, hoso, m5b3c2_04, m5b3c2_05, m5b4c2_3, m5b4c2_4) %>% 
  rename(wedding = m5b3c2_04,
         funeral = m5b3c2_05,
         gold = m5b4c2_3,
         savings = m5b4c2_4) %>% 
  mutate(year = 2006)
savings_06 <- inner_join(savings_06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

savings0204 <- bind_rows(savings_02, savings_04)
savings0206 <- bind_rows(savings_02, savings_06)

exp0204_p <- left_join(exp0204_p, savings0204, by = hhid) %>% 
  select(year, tinh, huyen, xa, diaban, everything())
exp0206_p <- left_join(exp0206_p, savings0206, by = hhid)

# Dwelling 

dwelling_02 <- m8_02 %>% 
  mutate(
    tinh = substr(as.character(xa), 1, 3),
    huyen = substr(as.character(xa), 4, 5),
    xa = substr(as.character(xa), 6, 7),
    hoso = substr(as.character(hoso), 4, 5),
    across(c(tinh, huyen, xa, hoso), as.numeric),
    m8c21 = ifelse(is.na(m8c21), 0 , m8c21),
    m8c22 = ifelse(is.na(m8c22), 0 , m8c22),
    renovation = m8c21 + m8c22,
    year = 2002
  ) %>% 
  rename(totarea = m8c2) %>% 
  select(year, tinh, huyen, xa, hoso, totarea, renovation)
dwelling_02 <- inner_join(dwelling_02, diaban02, by = c("tinh" = "tinh02", "huyen" = "huyen02", "xa" = "xa02")) %>% 
  rename(diaban = diaban02)

dwelling_04 <- m7_04 %>% 
  mutate(m7c24 = ifelse(is.na(m7c24), 0 , m7c24),
         m7c25 = ifelse(is.na(m7c25), 0 , m7c25),
         renovation = m7c24 + m7c25) %>% 
  rename(totarea = m7c2) %>% 
  select(tinh, huyen, xa, hoso, totarea, renovation) %>% 
  mutate(year = 2004)
dwelling_04 <- merge(dwelling_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

dwelling_06 <- m7_06 %>% 
  mutate(m7c24 = ifelse(is.na(m7c24), 0 , m7c24),
         m7c25 = ifelse(is.na(m7c25), 0 , m7c25),
         renovation = m7c25 + m7c24) %>% 
  rename(totarea = m7c2) %>% 
  select(tinh, huyen, xa, hoso, totarea, renovation) %>% 
  mutate(year = 2006)  
dwelling_06 <- inner_join(dwelling_06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

dwelling_0204 <- bind_rows(dwelling_02, dwelling_04)
dwelling_0206 <- bind_rows(dwelling_02, dwelling_06)

exp0204_p <- left_join(exp0204_p, dwelling_0204, by = hhid)
exp0206_p <- left_join(exp0206_p, dwelling_0206, by = hhid)
