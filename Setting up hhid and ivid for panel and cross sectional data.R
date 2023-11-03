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

hh02 <- c("exp_02", "m1_02", "m2_02", "m3_02", "m5a_02", "m5b4_02", "m6a2_02", "m6b1_02", "m6b2_02", "m6b34_02", "m7_02", "inc_02")

for(i in hh02){
  assign(i, mutate(get(i),
                   xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02))
  
  assign(i, get(i) %>% 
           mutate(hhid = xa02*10^5 + hoso))
}

m5aho_02 <- m5aho_02 %>% 
  mutate(hhid = xa*10^5 + hoso)

m5d_02 <- m5d_02 %>% 
  mutate(hhid = xa*10^5 + hoso)

# 2004 
diaban04 <- m123a_04 %>% 
  select(tinh, huyen, xa, diaban) %>% 
  filter(!is.na(diaban)) %>% 
  distinct()

exp_04 <- left_join(exp_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  distinct()

hh04 <- c("exp_04", "ho1_04", "m123a_04", "m1b_04", "m4a_04", "m4a_04a", "m5a2_04", "m5b1_04", "m5b2_04", "m5b34_04", "m6a_04", "m6b_04", "inc_04")

for(i in hh04){
  
  if (i %in% c("ho1_04", "m123a_04", "m1b_04", "m4a_04", "m4a_04a", "m6a_04", "m6b_04")){
    
    assign(i, get(i) %>% 
             select(-diaban))
    
    assign(i, left_join(get(i), diaban04, by = c("tinh", "huyen", "xa")) %>% 
             distinct())
    
    assign(i, get(i) %>% 
             mutate(hhid = tinh*10^9+huyen*10^7+xa*10^5+diaban*100+hoso))
  }
  
  if(i %in% c("inc_04")){
    
    assign(i, left_join(get(i), diaban04, by = c("tinh", "huyen", "xa")) %>% 
             distinct())
    
    assign(i, get(i) %>% 
             mutate(hhid = tinh*10^9+huyen*10^7+xa*10^5+diaban*100+hoso))    
  }
  
  if (i %in% c("exp_04", "m5a2_04", "m5b1_04", "m5b2_04", "m5b34_04")){
    
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

inc_06 <- left_join(inc_06, diaban06, by = c("tinh", "huyen", "xa"))

exp_06 <- left_join(exp_06, diaban06, by = c("tinh", "huyen", "xa"))

m5b34_06 <- m5b34_06 %>% mutate(across(diaban, as.numeric))

hh06 <- c("exp_06", "m1a_06", "m1b_06", "m2a_06", "m4a_06", "m5a2_06", "m5b1_06", "m5b2_06", "m6a_06", "m6b_06", "ttchung_06", "inc_06", "m5b34_06")

for(i in hh06){
  
  if (i %in% c("m1a_06", "m1b_06", "m2a_06", "m4a_06", "m5a2_06", "m5b1_06", "m5b2_06", "m6a_06", "m6b_06", "ttchung_06")){m5b34_06
    assign(i, get(i) %>%
             select(-diaban))
    
    assign(i, left_join(get(i), diaban06, by = c("tinh", "huyen", "xa")) %>% 
             distinct())
  }
  
  assign(i, get(i) %>% 
           mutate(hhid = tinh*10^9 + huyen*10^7 + xa*10^5 + diaban*10^2 + hoso))
}

##############################
# SETTING UP HOUSEHOLD PANEL #
##############################

# 2002 - 2004 
hhid0204a <- ho1_04 %>%
  filter(m1c1 == 1) %>% 
  select(tinh02, huyen02, xa02, hoso02, tinh, huyen, xa, hoso, hhid) %>% 
  filter(!is.na(tinh02)) %>% 
  mutate(across(c(xa02, hoso02, tinh, huyen, xa, hoso, hhid), as.numeric),
         xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02,
         hhid02 = xa02*10^5 + hoso02)

diaban02a <- diaban02 %>% 
  select(-hoso02) %>% 
  distinct() %>% 
  mutate(xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02) %>% 
  select(xa02, diaban02)

hhid0204a <- left_join(hhid0204a, diaban02a, by = "xa02")

hhid0204a <- hhid0204a %>% 
  mutate(diaban02 = if_else(diaban02 == 14, 14.1, diaban02)) %>%   
  mutate(hoso02 = diaban02*10^3 + hoso02,
         hhid02 = xa02*10^5 + hoso02) %>% 
  select(hhid02, hhid)

## Households who have only clusterid02 and hoso02 as identifiers 
hhid0204_clusterid02 <- ho1_04 %>%
  filter(m1c1 == 1) %>% 
  filter(is.na(tinh02)) %>%
  select(hoso02, clusterid02, tinh, huyen, xa, diaban, hoso, hhid) %>% 
  mutate(across(c(tinh, huyen, xa), as.numeric)) %>% 
  mutate(xa02 = tinh * 10^4 + huyen * 100 + xa) %>% 
  select(xa02, hoso02, tinh, huyen, xa, hoso, hhid) %>% 
  mutate(across(c(xa02, hoso02, tinh, huyen, xa, hoso, hhid), as.numeric)) %>% 
  mutate(hhid02 = xa02*10^5 + hoso02) %>% 
  select(hhid02, hhid)

hhid0204 <- bind_rows(hhid0204a, hhid0204_clusterid02)

# 2002 - 2006  

diaban04 <- diaban04 %>% 
  rename(diaban04 = diaban,
         tinh04 = tinh,
         huyen04 = huyen, 
         xa04 = xa)

hhid0406 <- merge(ttchung_06, diaban04, by = c("tinh04", "huyen04", "xa04")) %>% 
  mutate(hhid04 = tinh04*10^9+huyen04*10^7+xa04*10^5+diaban04*100+hoso04) %>% 
  rename(hhid06 = hhid) %>% 
  select(hhid04, hhid06)

# 2002- 2004 - 2006 panel final 

hhid020406 <- hhid0204 %>% 
  select(hhid02, hhid) %>% 
  rename(hhid04 = hhid)

hhid020406 <- merge(hhid020406, hhid0406, by = "hhid04") # N = 10,778

#####################################
# GENERATING INDIVIDUAL IDENTIFIERS #
#####################################

# 2002 
id02 <- c("m1_02", "m2_02", "m3_02", "m5a_02")

for(i in id02){
  assign(i, mutate(get(i), ivid = xa*10^7 + matv))
  
  df <- get(i)
  
  df$ivid <- as.character(as.numeric(df$ivid))
  
  assign(i, df)
}

# 2004 
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

###################################
# SETTING UP FOR INDIVIDUAL PANEL #
###################################

# 2002 - 2004
ivid04 <- m1b_04 %>%
  filter(m1bc6 == 1) %>% 
  filter(!is.na(matv)) %>% 
  select(hhid, ivid, m1bc3, m1bc4, m1bc5) %>% 
  rename("matv02" = m1bc3) #N = 91,723

ivid0204 <- merge(ivid04, hhid0204, by = "hhid") %>% 
  distinct() %>% 
  mutate(ivid02 = as.character(hhid02*100 + matv02)) %>% 
  select(-matv02) # List of individuals by their 2002 and 2004 identifiers (N = 91,738)

# 2002 - 2006 
ivid0406 <- m1b_06 %>% 
  filter(m1bc6 == 1) %>% 
  rename(matv04 = m1bc3) %>% 
  select(hhid, ivid, matv04, m1bc4, m1bc5) %>% 
  rename(hhid06 = hhid,
         ivid06 = ivid) # N = 87,330

ivid0406 <- merge(hhid0406, ivid0406, by = "hhid06") # N = 87,330

ivid0406 <- ivid0406 %>% 
  mutate(ivid04 = hhid04*100 + matv04) %>% 
  select(ivid04, ivid06, m1bc4, m1bc5)

######################################
# CHECKING ACCURACY OF PANEL CREATED #
######################################

# 2002 - 2004 
## Checking if merged dataset is accurate by comparing age in 2002 and 2002 age and sex reported in VHLSS 2004 to VHLSS 2002 
age_vhlss02 <- m1_02 %>%
  select(hhid, ivid, m1c2, m1c5) %>% 
  rename(hhid02 = hhid,
  ivid02 = ivid)

age_vhlss0204 <- merge(ivid0204, age_vhlss02, by = c("hhid02", "ivid02")) ##Keeping the Age02 dataframe to those in the panel (N = 87,685)

age_vhlss0204 <- age_vhlss0204 %>%
  mutate(agediff = m1bc5 - m1c5,
         sexdiff = m1bc4 - m1c2)

age_vhlss0204$agecorrect <- 1
age_vhlss0204$agecorrect[age_vhlss0204$agediff > 0] <- 0
age_vhlss0204$agecorrect[age_vhlss0204$agediff < 0] <- 0

age_vhlss0204$sexcorrect <- 1
age_vhlss0204$sexcorrect[age_vhlss0204$sexdiff > 0] <- 0
age_vhlss0204$sexcorrect[age_vhlss0204$sexdiff < 0] <- 0

age_vhlss0204$correct <- ifelse(age_vhlss0204$agecorrect == 1 & age_vhlss0204$sexcorrect == 1, 1, 0)

proportions(table(age_vhlss0204$correct)) ## 17.01% of the matches are incorrect. I drop the mismatched observations.  

ivid0204 <- age_vhlss0204 %>% 
  filter(correct == 1) %>% 
  filter(!is.na(ivid02))

ivid0204 <- ivid0204 %>%
  select(ivid02, hhid02, ivid, hhid) # Finally, I have a panel of 72,769 individuals 

# 2002 - 2006 
age_vhlss04 <- m123a_04 %>% 
  select(ivid, m1ac2, m1ac5) %>% 
  rename(ivid04 = ivid)

age_vhlss04 <- merge(age_vhlss04, ivid0406, by = "ivid04")

age_vhlss04 <- age_vhlss04 %>% 
  mutate(agediff = m1bc5 - m1ac5,
         sexdiff = m1bc4 - m1ac2)

age_vhlss04$agecorrect <- 1
age_vhlss04$agecorrect[age_vhlss04$agediff > 0] <- 0
age_vhlss04$agecorrect[age_vhlss04$agediff < 0] <- 0

age_vhlss04$sexcorrect <- 1
age_vhlss04$sexcorrect[age_vhlss04$sexdiff > 0] <- 0
age_vhlss04$sexcorrect[age_vhlss04$sexdiff < 0] <- 0

age_vhlss04$correct <- ifelse(age_vhlss04$agecorrect == 1 & age_vhlss04$sexcorrect == 1, 1, 0)

proportions(table(age_vhlss04$correct)) # 93.5% of observations were matched correctly. 

ivid0406 <- age_vhlss04 %>% 
  filter(correct == 1) %>% 
  select(ivid04, ivid06) # N = 80,869

# 2002 - 2006 panel final 
ivid020406 <- ivid0204 %>% 
  rename(ivid04 = ivid)

ivid020406 <- merge(ivid020406, ivid0406, by = "ivid04") %>% 
  select(ivid02, ivid04, ivid06)

