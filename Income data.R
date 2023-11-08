inc02 <- inc_02 %>% 
  mutate(hhinc = rlincomepc * 12 * hhsize) %>%   
  select(tinh02, huyen02, xa02, diaban02, hoso02, hhinc) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  mutate(year = 2002)

inc04 <- inc_04 %>% 
  mutate(hhinc = rlincomepc * 12 * hhsize) %>%   
  select(tinh, huyen, xa, hoso, hhinc) %>% 
  mutate(year = 2004)

inc04 <- merge(inc04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

inc06 <- inc_06 %>% 
  mutate(hhinc = rlincomepc * 12 * hhsize) %>%   
  select(tinh, huyen, xa, hoso, hhinc) %>% 
  mutate(year = 2006)

inc06 <- inner_join(inc06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

hhinc0204 <- bind_rows(inc02, inc04)
hhinc0206 <- bind_rows(inc02, inc06)

hhinc0204_p <- merge(hhinc0204, panel0204, by = hhid)
hhinc0204_p <- merge(hhinc0204_p, provtariffs0204, by = c("tinh", "year"))
hhinc0204_p <- inner_join(hhinc0204_p, weights_02, by = c("tinh" = "tinh02", "huyen" = "huyen02", "xa" = "xa02"))

hhinc0206_p <- merge(hhinc0206, panel0206, by = hhid)
hhinc0206_p <- merge(hhinc0206_p, provtariffs0206, by = c("tinh", "year"))
hhinc0206_p <- merge(hhinc0206_p, weights_06, by = c("tinh", "huyen", "xa"))

emp0204_p <- merge(emp0204_p, hhinc0204, by = hhid) %>%
  mutate(inc_share = inc/hhinc, 
         inc_share = ifelse(inc_share >= 1 | inc_share < 0, NA, inc_share),
         inc_share = ifelse(is.na(inc_share), 0, inc_share)) %>%
  group_by(hhid, year) %>% 
  mutate(tot_inc_share = sum(inc_share),
         agri_impute = ifelse(agri_work == 1 & tot_inc_share == 0, 0.5, inc_share),
         inc_impute = ifelse(wage_work == 1 & inc_share == 0 | self_bus == 1 & inc_share == 0 | self_agri == 1 & inc_share == 0, 1 - tot_inc_share, agri_impute),
         inc_share = ifelse(inc_share == 0, NA, inc_share),
         inc_impute = ifelse(inc_impute == 0, NA, inc_impute),
         wider = ifelse(any(relationship > 3), 1, 0),
         spouse = ifelse(relationship == 1 | relationship == 2, 1, 0)) %>% 
  ungroup() %>% 
  select(-c(tot_inc_share, agri_impute))

emp0206_p <- merge(emp0206_p, hhinc0206, by = hhid) %>%
  mutate(inc_share = inc/hhinc, 
         inc_share = ifelse(inc_share >= 1 | inc_share < 0, NA, inc_share),
         inc_share = ifelse(is.na(inc_share), 0, inc_share)) %>%
  group_by(hhid, year) %>% 
  mutate(tot_inc_share = sum(inc_share),
         agri_impute = ifelse(agri_work == 1 & tot_inc_share == 0, 0.5, inc_share),
         inc_impute = ifelse(wage_work == 1 & inc_share == 0 | self_bus == 1 & inc_share == 0 | self_agri == 1 & inc_share == 0, 1 - tot_inc_share, agri_impute),
         inc_share = ifelse(inc_share == 0, NA, inc_share),
         inc_impute = ifelse(inc_impute == 0, NA, inc_impute),
         wider = ifelse(any(relationship > 3), 1, 0),
         spouse = ifelse(relationship == 1 | relationship == 2, 1, 0)) %>% 
  ungroup() %>% 
  select(-c(tot_inc_share, agri_impute))
  
# Wage income 

hhwage_02 <- m5aho_02 %>% 
  filter(m5ac1 == 1) %>% 
  mutate(totwage = m5ac7e+m5ac9) %>% 
  select(tinh, xa, hoso, totwage) %>% 
  mutate(
    huyen = substr(as.character(xa), 4, 5),   
    diaban = substr(as.character(hoso), 1, 2),
    hoso = substr(as.character(hoso), nchar(as.character(hoso)) - 1, nchar(as.character(hoso))),
    xa = substr(as.character(xa), 6, 7),
    across(c(tinh, huyen, xa, hoso, diaban), as.numeric)    
  ) %>% 
  select(tinh, huyen, xa, diaban, hoso, everything()) %>% 
  mutate(year = 2002)

hhwage_04 <- ho1_04 %>% 
  select(tinh, huyen, xa, hoso, m4atn) %>% 
  rename(totwage = m4atn)
hhwage_04 <- merge(hhwage_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04) %>% 
  mutate(year = 2004)

hhwage_06 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, m4atn) %>% 
  rename(totwage = m4atn)  
hhwage_06 <- inner_join(hhwage_06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06) %>% 
  mutate(year = 2006)

hhwage0204 <- bind_rows(hhwage_02, hhwage_04)
hhwage0206 <- bind_rows(hhwage_02, hhwage_06)

hhinc0204_p <- left_join(hhinc0204_p, hhwage0204, by = hhid)
hhinc0206_p <- left_join(hhinc0206_p, hhwage0206, by = hhid)
