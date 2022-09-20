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

hh02 <- c("exp_02", "m1_02", "m2_02", "m3_02", "m5a_02", "m5b4_02", "m6a2_02", "m6b1_02", "m6b2_02", "m6b34_02", "m7_02")

for(i in hh02){
  assign(i, mutate(get(i),
                   xa02 = tinh02 * 10^4 + huyen02 * 10^2 + xa02))
  
  assign(i, get(i) %>% 
           mutate(hhid = xa02*10^5 + hoso) %>%
           mutate(hhid = recode(hhid,
                                "823091901120" = 823091901020,
                                "823110826116" = 823110826016,
                                "823110826119" = 823110826019,
                                "823110826120" = 823110826020,
                                "823131710116" = 823131710016)))
}

m5aho_02 <- m5aho_02 %>% 
  mutate(hhid = xa*10^5 + hoso) %>% 
  mutate(hhid = recode(hhid,
                       "823091901120" = 823091901020,
                       "823110826116" = 823110826016,
                       "823110826119" = 823110826019,
                       "823110826120" = 823110826020,
                       "823131710116" = 823131710016))

# 2004 
diaban04 <- m123a_04 %>% 
  select(tinh, huyen, xa, diaban) %>% 
  filter(!is.na(diaban)) %>% 
  distinct()

exp_04 <- left_join(exp_04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  distinct()

hh04 <- c("exp_04", "ho1_04", "m123a_04", "m1b_04", "m4a_04", "m4a_04a", "m5a2_04", "m5b1_04", "m5b2_04", "m6a_04", "m6b_04")

for(i in hh04){
  
  if (i %in% c("ho1_04", "m123a_04", "m1b_04", "m4a_04", "m4a_04a", "m6a_04", "m6b_04")){
    
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
  
  assign(i, get(i) %>% 
           mutate(hoso_new = hoso,
                  hoso_new = case_when(
                    hhid==106072900714 & quyen==2 ~ 94,
                    hhid==111033100205 & quyen==2 ~ 95,
                    hhid==117012500401 & quyen==2 ~ 91,
                    hhid==211030100502 & quyen==2 ~ 92,
                    hhid==211073300808 & quyen==2 ~ 98,
                    hhid==211114700103 & quyen==2 ~ 93,
                    hhid==301072902115 & quyen==2 ~ 95,
                    hhid04==305010700902 & quyen==2 ~ 92, 
                    hhid04==401111300714 & quyen==2 ~ 94, 
                    hhid04==503212100901 & quyen==2 ~ 91, 
                    hhid04==503212100902 & quyen==2 ~ 92, 
                    hhid04==503212100903 & quyen==2 ~ 93,
                    
                  ))
         )
  
}

# 2006
diaban06 <- m1a_06 %>% 
  mutate(across(diaban, as.numeric)) %>%   
  filter(!is.na(diaban)) %>%   
  select(tinh, huyen, xa, diaban) %>% 
  distinct()

hh06 <- c("exp_06", "m1a_06", "m1b_06", "m2a_06", "m4a_06", "m5a2_06", "m5b1_06", "m5b2_06", "m6a_06", "m6b_06", "ttchung_06")

for(i in hh06){
  
  if (i %in% c("m1a_06", "m1b_06", "m2a_06", "m4a_06", "m5a2_06", "m5b1_06", "m5b2_06", "m6a_06", "m6b_06", "ttchung_06")){
    assign(i, get(i) %>%
             select(-diaban))
  }
  
  assign(i, left_join(get(i), diaban06, by = c("tinh", "huyen", "xa")) %>% 
           distinct())
  
  assign(i, get(i) %>% 
           mutate(hhid = tinh*10^9 + huyen*10^7 + xa*10^5 + diaban*10^2 + hoso))
}

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

####################################################################################
# SETTING UP 2002 - 2006 PANEL USING MCCAIG'S HOUSEHOLD AND INDIVIDUAL IDENTIFIERS #
####################################################################################

hhid0406 <- hhid0406 %>% 
  select(hhid04_revised, hhid)

hhid0206 <- merge(hhid0204, hhid0406, by = "hhid04")
