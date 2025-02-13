##########################################
# SETTING UP HOUSEHOL DPANEL IDENTIFIERS #
##########################################

# 2002 - 2004 

panel0204 <- ho1_04 %>%
  filter(m1c1 == 1) %>% 
  select(tinh02, huyen02, xa02, hoso02, tinh, huyen, xa, diaban, hoso) %>% 
  mutate(tinh02 = ifelse(is.na(tinh02), tinh, tinh02),
         huyen02 = ifelse(is.na(huyen02), huyen, huyen02),
         xa02 = ifelse(is.na(xa02), xa, xa02),
         hoso02 = ifelse(nchar(hoso02) > 2, substr(hoso02, nchar(hoso02) - 1, nchar(hoso02)), hoso02),
         across(hoso02, as.numeric))

diaban04 <- m5a2_04 %>% 
  select(tinh, huyen, xa, diaban) %>% 
  distinct() %>% 
  rename(diaban04 = diaban)

panel0204 <- left_join(panel0204, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  mutate(diaban = ifelse(is.na(diaban), diaban04, diaban)) %>% 
  select(-diaban04)

diaban02 <- inc_02 %>% 
  select(tinh02, huyen02, xa02, diaban02) %>% 
  distinct()

panel0204_ <- left_join(panel0204, diaban02, by = c("tinh02", "huyen02", "xa02")) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, everything()) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup()

# # 2002 - 2004 - 2006  

panel0406 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, tinh04, huyen04, xa04, hoso04) %>% 
  filter(!is.na(hoso04))

diaban06 <- m5a2_06 %>% 
  mutate(across(diaban, as.numeric)) %>%   
  filter(!is.na(diaban)) %>%   
  select(tinh, huyen, xa, diaban) %>% 
  distinct() %>% 
  rename_with(~ paste0(.x, "06"), everything())

panel0406 <- left_join(panel0406, diaban04, by = (c("tinh04" = "tinh", "huyen04" = "huyen", "xa04" = "xa")))

panel0406 <- left_join(panel0406, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06"))

panel0406 <- panel0406 %>% rename(tinh06 = tinh, huyen06 = huyen, xa06 = xa, hoso06 = hoso)

panel0206_ <- inner_join(panel0204_, panel0406, by = c("tinh" = "tinh04", "huyen" = "huyen04", "xa" = "xa04", "diaban" = "diaban04", "hoso" = "hoso04")) %>% 
  distinct() %>% 
  mutate(diaban02 = ifelse(is.na(diaban02), diaban06, diaban02)) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, tinh, huyen, xa, diaban, hoso, tinh06, huyen06, xa06, diaban06) %>%
  select(-c(tinh, huyen, xa, diaban, hoso)) %>%   
  mutate(hhid = cur_group_id()) %>%
  ungroup()

# Clean 

panel06 <- panel0206_ %>% 
  select(c(matches("06"), hhid)) %>% 
  mutate(year = 2006) %>% 
  rename_with(~ str_replace(.x, "06", ""), everything())

panel02 <- panel0206_ %>% 
  select(c(matches("02"), hhid)) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())

panel0206 <- bind_rows(panel02, panel06)

panel02_04 <- panel0204_ %>% 
  select(c(matches("02"), hhid)) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())  

panel04 <- panel0204_ %>% 
  select(c(tinh, huyen, xa, diaban, hoso, hhid)) %>% 
  mutate(year = 2004)

panel0204 <- bind_rows(panel02_04, panel04)

###########################################
# SETTING UP INDIVIDUAL PANEL IDENTIFIERS #
###########################################

# 2002 - 2004 

ivid04 <- m1b_04 %>%
  filter(m1bc6 == 1) %>% 
  select(tinh, huyen, xa, hoso, m1bc3, m1bc4, m1bc5, m1bc7) %>% 
  rename("matv02" = m1bc3) %>% 
  select(tinh, huyen, xa, hoso, m1bc7, everything())

ivid0204 <- merge(ivid04, panel0204_, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  select(hhid, tinh, huyen, xa, diaban, hoso, m1bc7, tinh02, huyen02, xa02, diaban02, hoso02, matv02, m1bc4, m1bc5)

# Checking accuracy of panel based on age and sex 

age_vhlss02 <- m1_02 %>%
  select(c(matches("02"), m1c2, m1c5)) %>% 
  mutate(matv02 = ifelse(nchar(matv02) > 2, substr(matv02, nchar(matv02) - 1, nchar(matv02)), matv02),
         across(matv02, as.numeric))

age_vhlss02 <- merge(ivid0204, age_vhlss02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02")) %>% distinct()

ivid0204_ <- age_vhlss02 %>% 
  mutate(correct = (m1bc5 - m1c5) + (m1bc4 - m1c2),
         correct = ifelse(correct == 0, 1, 0)) %>% 
  filter(correct == 1) %>% 
  group_by(hhid, tinh02, huyen02, xa02, diaban02, hoso02, matv02, m1c5, m1c2, tinh, huyen, xa, diaban, hoso, m1bc7, m1bc5, m1bc4) %>% 
  mutate(ivid = cur_group_id()) %>%   
  select(-correct) %>% 
  ungroup() %>% 
  rename(matv04 = m1bc7)

ivid02 <- ivid0204_ %>% 
  select(c(hhid, ivid, matches("02"), m1c2, m1c5)) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  rename(sex = m1c2, 
         age = m1c5)

age_vhlss04 <- m123a_04 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5) %>% 
  rename(sex = m1ac2,
         age = m1ac5)

ivid04 <- ivid0204_ %>% 
  select(c(hhid, ivid, tinh, huyen, xa, diaban, hoso, matv04, m1bc4)) %>% 
  mutate(year = 2004) %>% 
  rename(matv = matv04,
         sex = m1bc4) 

ivid04 <- merge(ivid04, age_vhlss04, by = c("tinh", "huyen", "xa", "hoso", "matv", "sex"))

ivid0204 <- bind_rows(ivid02, ivid04)

# 2004 - 2006 

ivid0406 <- m1b_06 %>% 
  filter(m1bc6 == 1) %>% 
  rename(matv04 = m1bc3) %>% 
  select(tinh, huyen, xa, hoso, m1bc7, matv04, m1bc4, m1bc5) %>% 
  rename(tinh06 = tinh, huyen06 = huyen, xa06 = xa, hoso06 = hoso, matv06 = m1bc7)

ivid0406 <- merge(ivid0406, panel0406, by = c("tinh06", "xa06", "huyen06", "hoso06")) %>% 
  group_by(tinh04, huyen04, xa04, diaban04, hoso04, tinh06, huyen06, xa06, diaban06, hoso06) %>%   
  mutate(hhid = cur_group_id()) %>%
  ungroup()

# Checking accuracy 

age_vhlss04 <- m123a_04 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5) %>% 
  rename(tinh04 = tinh, huyen04 =huyen, xa04 = xa, hoso04 = hoso, matv04 = matv)

age_vhlss04 <- merge(age_vhlss04, ivid0406, by = c("tinh04", "huyen04", "xa04", "hoso04", "matv04")) %>% 
  mutate(correct = (m1ac5 - m1bc5) + (m1ac2 - m1bc4),
         correct = ifelse(correct == 0, 1, 0)) %>% 
  filter(correct == 1) %>% 
  select(-correct) %>% 
  group_by(tinh04, huyen04, xa04, diaban04, matv04, m1ac5, m1bc5, m1ac2, m1bc4, tinh06, huyen06, xa06, diaban06, hoso06, matv06) %>% 
  mutate(ivid = cur_group_id()) %>% 
  ungroup()

ivid0204_a <- ivid0204_ %>% 
  select(-c(hhid, ivid, m1bc5, m1bc4))

ivid0206_ <- inner_join(ivid0204_a, age_vhlss04, by = c("tinh" = "tinh04", "huyen" = "huyen04", "xa" = "xa04", "hoso" = "hoso04", "matv04")) %>% 
  select(-c(tinh, huyen, xa, diaban, hoso, matv04, m1ac2)) %>% 
  select(ends_with("02"), ends_with("06"), hhid, ivid, m1c2, m1c5) %>% 
  distinct()

ivid02_06 <- ivid0206_ %>% 
  select(c(matches("02"), hhid, ivid, m1c2, m1c5)) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  ungroup() %>% 
  mutate(year = 2002) %>% 
  rename(sex = m1c2,
         age = m1c5)

age06 <- m1a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5)

ivid06 <- ivid0206_ %>% 
  select(c(matches("06"), hhid, ivid)) %>% 
  rename_with(~ str_replace(.x, "06", ""), everything()) %>% 
  ungroup() %>% 
  mutate(year = 2006)

ivid06 <- merge(ivid06, age06, by = c("tinh", "huyen", "xa", "hoso", "matv")) %>% 
  rename(sex = m1ac2,
         age = m1ac5)

ivid0206 <- bind_rows(ivid02_06, ivid06)

ivid0204 <- ivid0204 %>% 
  mutate(across(sex, as.numeric),
         female = ifelse(sex == 2, 1, 0)) %>% 
  select(-sex)

ivid0206 <- ivid0206 %>% 
  mutate(across(sex, as.numeric),
         female = ifelse(sex == 2, 1, 0)) %>% 
  select(-sex)

ivid <- c("tinh", "huyen", "xa", "hoso", "matv", "year", "age", "female")
hhid <- c("tinh", "huyen", "xa", "hoso", "year")