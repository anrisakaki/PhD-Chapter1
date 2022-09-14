###############################################
# SETTING UP DATA FRAME FOR EMPLOYMENT TRENDS #
###############################################

# 2002 

m3_02$m3c7[is.na(m3_02$m3c7)] <- 0

educ_02 <- m2_02 %>% select(tinh, xa, hoso, matv, tinh02, xa02, hoso02, matv02, hhid, ivid, m2c1)

employment_mf_02 <- list(m1_02, m3_02, educ_02) %>% 
  reduce(full_join, by = c("tinh", "xa", "hoso", "matv", "tinh02", "xa02", "hoso02", "matv02", "hhid", "ivid")) %>% 
  rename("hhid02" = hhid,
         "ivid02" = ivid,
         "educ" = m2c1,
         "wage_work" = m3c1a,
         "industry" = m3c7,
         "huyen02" = huyen02.x,
         "diaban02" = diaban02.x) %>%
  filter(m1c5 >= 18) %>%
  filter(m1c5 < 65) %>% 
  select(-c(huyen02.y, diaban02.y)) # N = 185,749

employment_mf_02 <- merge(employment_mf_02, weights_02, by = c("xa02", "diaban02", "huyen02")) # N = 185,749

## Recoding binary variable for sex
employment_mf_02$sex <- factor(employment_mf_02$m1c2,
                               c(1,2),
                               c("Male", "Female"))

## 2002 - 2004 panel 
employment_mf_02p <- employment_mf_02 %>% 
  select(-c(tinh, xa, hoso, matv)) %>% 
  select(-ends_with(".x")) %>% 
  select(-ends_with(".y"))

employment_mf_02p <- merge(ivid0204, employment_mf_02p, by = c("xa02", "hhid02", "ivid02")) %>% 
  distinct() %>% 
  select(-ends_with(".y")) %>% 
  rename(hoso02 = hoso02.x,
         matv02 = matv02.x) # N = 39,988

emp_ivid04 <- employment_mf_02p %>% select("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid") # ivid of individuals who reappears in the VHLSS 2004, based on their 2004 identifiers 