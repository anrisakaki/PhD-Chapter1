# GENERATNG INDIVIDUAL HOUSEHOLD NUMBER 
exp_02 <- exp_02 %>%
  select(-(diaban02))

diaban02 <- inc_02 %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02) %>% 
  distinct()

exp_02 <- left_join(exp_02, diaban02, by = c("tinh02", "huyen02", "xa02", "hoso02")) %>% 
  distinct()

exp_02 <- exp_02 %>% 
  mutate(diaban02 = if_else(diaban02 == 14, 14.1, diaban02)) %>% 
  mutate(hoso = diaban02*10^3 + hoso) %>% 
  mutate(xa = tinh * 10^4 + huyen * 10^2 + xa)

hh02 <- c("exp_02", "m1_02", "m2_02", "m3_02", "m5a_02", "m5b4_02", "m6a2_02", "m6b1_02", "m6b2_02", "m6b34_02")

for(i in hh02){
  assign(i, mutate(get(i),
                   xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02))
  
  assign(i, get(i) %>% 
           mutate(hhid = xa02*10^5 + hoso))
}

m5aho_02 <- m5aho_02 %>% 
  mutate(hhid = xa*10^5 + hoso)

diaban04 <- m123a_04 %>% 
  select(tinh, huyen, xa, diaban) %>% 
  filter(!is.na(diaban)) %>% 
  distinct()

exp_04 <- left_join(exp_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  distinct()

hh04 <- c("exp_04", "ho1_04", "m123a_04", "m1b_04", "m4a_04", "m4a_04a", "m5a2_04", "m5b1_04", "m5b2_04")

for(i in hh04){
  
  if (i %in% c("ho1_04", "m123a_04", "m1b_04", "m4a_04", "m4a_04a")){
    
    assign(i, get(i) %>% 
             select(-diaban))
    
    assign(i, left_join(get(i), diaban04, by = c("tinh", "huyen", "xa")) %>% 
             distinct())
    
    assign(i, get(i) %>% 
             mutate(hhid = tinh*10^9+huyen*10^7+xa*10^5+diaban*100+hoso))
  }   
  
  if (i %in% c("exp_04", "m5a2_04", "m5b1_04", "m5b2_04")){
    
    assign(i, get(i) %>% 
             mutate(hhid = tinh*10^9+huyen*10^7+xa*10^5+diaban*100+hoso))
  } 
  
}

# 2006
diaban06 <- m1a_06 %>% 
  mutate(across(diaban, as.numeric)) %>%   
  filter(!is.na(diaban)) %>%   
  select(tinh, huyen, xa, diaban) %>% 
  distinct()

hh06 <- c("exp_06", "m1a_06", "m1b_06", "m2a_06", "m4a_06", "m5a2_06", "m5b1_06", "m5b2_06", "ttchung_06")

for(i in hh06){
  
  if (i %in% c("m1a_06", "m1b_06", "m2a_06", "m4a_06", "m5a2_06", "m5b1_06", "m5b2_06", "ttchung_06")){
    assign(i, get(i) %>%
             select(-diaban))
  }
  
  assign(i, left_join(get(i), diaban06, by = c("tinh", "huyen", "xa")) %>% 
           distinct())
  
  assign(i, get(i) %>% 
           mutate(hhid = tinh*10^9 + huyen*10^7 + xa*10^5 + diaban*10^2 + hoso))
}