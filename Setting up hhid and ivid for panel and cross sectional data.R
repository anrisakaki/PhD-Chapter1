####################################
# GENERATING HOUSEHOLD IDENTIFIERS #
####################################

# 2002 
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

# 2004 
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

# Setting up HOUSEHOLD panel 

## 2002 - 2004 
ho1_04 <- ho1_04 %>%
  mutate(xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02)

hhid0204a <- ho1_04 %>%
  filter(m1c1 == 1) %>% 
  select(tinh02, huyen02, xa02, hoso02, tinh, huyen, xa, hoso, hhid) %>% 
  filter(!is.na(tinh02)) %>% 
  select(xa02, hoso02, tinh, huyen, xa, hoso, hhid) %>% 
  mutate(across(c(xa02, hoso02, tinh, huyen, xa, hoso, hhid), as.numeric))

diaban02a <- diaban02 %>% 
  select(-hoso02) %>% 
  distinct() %>% 
  mutate(xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02) %>% 
  select(xa02, diaban02)

hhid0204a <- left_join(hhid0204a, diaban02a, by = "xa02")

hhid0204a <- hhid0204a %>% 
  mutate(diaban02 = if_else(diaban02 == 14, 14.1, diaban02)) %>%   
  mutate(hoso02 = diaban02*10^3 + hoso02,
         hhid02 = xa02*10^5 + hoso02)

## Households who have only clusterid02 and hoso02 as identifiers 
hhid0204_clusterid02 <- ho1_04 %>%
  filter(m1c1 == 1) %>% 
  filter(is.na(tinh02)) %>%
  select(hoso02, clusterid02, tinh, huyen, xa, diaban, hoso, hhid) %>% 
  mutate(across(c(tinh, huyen, xa), as.numeric)) %>% 
  mutate(xa02 = tinh * 10^4 + huyen * 100 + xa) %>% 
  select(xa02, hoso02, tinh, huyen, xa, hoso, hhid) %>% 
  mutate(across(c(xa02, hoso02, tinh, huyen, xa, hoso, hhid), as.numeric)) %>% 
  mutate(hhid02 = xa02*10^5 + hoso02)

hhid0204 <- bind_rows(hhid0204a, hhid0204_clusterid02) %>% 
  select(xa02, hoso02, hhid02, tinh, huyen, xa, hoso, hhid)

hhid02 <- hhid0204 %>%
  select(xa02, hoso02, hhid02)

hhid04 <- ho1_04 %>% 
  filter(m1c1 == 1) %>% 
  select(tinh, huyen, xa, hoso, hhid)

## 2002 - 2006  

diaban04 <- diaban04 %>% 
  rename(diaban04 = diaban,
         tinh04 = tinh,
         huyen04 = huyen, 
         xa04 = xa)

hhid0406 <- merge(ttchung_06, diaban04, by = c("tinh04", "huyen04", "xa04")) %>% 
  mutate(hhid04 = tinh04*10^9+huyen04*10^7+xa04*10^5+diaban04*100+hoso04) %>% 
  rename(hhid06 = hhid) %>% 
  select(hhid04, hhid06)

## 2002- 2004 - 2006 panel final 

hhid020406 <- hhid0204 %>% 
  select(hhid02, hhid) %>% 
  rename(hhid04 = hhid)

hhid020406 <- merge(hhid020406, hhid0406, by = "hhid04") # N = 10,778

#####################################
# GENERATING INDIVIDUAL IDENTIFIERS #
#####################################

## 2002 
id02 <- c("m1_02", "m2_02", "m3_02", "m5a_02")

for(i in id02){
  assign(i, mutate(get(i), ivid = xa*10^7 + matv))
  
  df <- get(i)
  
  df$ivid <- as.character(as.numeric(df$ivid))
  
  assign(i, df)
}

## 2004 
m1b_04 <- m1b_04 %>%
  rename("matv" = m1bc7)

id04 <- c("m123a_04", "m1b_04", "m4a_04", "m4a_04a")

for(i in id04){
  assign(i, mutate(get(i), ivid = hhid*100 + matv))
  
  df04 <- get(i)
  
  df04$ivid <- as.character(as.numeric(df04$ivid))
  
  assign(i, df04)
}

# 2006 
m1b_06 <- m1b_06 %>% 
  rename(matv = m1bc7)

id06 <- c("m1a_06", "m1b_06", "m2a_06", "m4a_06")

for(i in id06){
  assign(i, mutate(get(i), ivid = hhid*100 + matv))
  
  df06 <- get(i)
  
  df06$ivid <- as.character(as.numeric(df06$ivid))
  
  assign(i, df06)
  
}


# Setting up ivid02 ivid04 for panel data

ivid04 <- m1b_04 %>%
  filter(m1bc6 == 1) %>% 
  filter(!is.na(matv)) %>% 
  select(tinh, huyen, xa, hoso, matv, hhid, ivid, m1bc3, m1bc4, m1bc5) %>% 
  rename("matv02" = m1bc3) #N = 91,723

ivid0204 <- merge(ivid04, hhid0204, by = c("tinh", "huyen", "xa", "hoso", "hhid"), all.x=TRUE) %>% 
  distinct() # List of individuals by their 2002 and 2004 identifiers (N = 91,738)

ivid0204 <- ivid0204 %>% 
  mutate(ivid02 = hhid02*10^2 + matv02)

ivid0204$ivid02 <- as.character(as.numeric(ivid0204$ivid02))

# Setting up 