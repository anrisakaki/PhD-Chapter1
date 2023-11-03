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

diaban04 <- panel0204 %>% filter(!is.na(diaban)) %>% select(tinh, huyen, xa, diaban) %>% distinct() %>% rename(diaban04 = diaban)

panel0204 <- left_join(panel0204, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  mutate(diaban = ifelse(is.na(diaban), diaban04, diaban)) %>% 
  select(-diaban04)

diaban02 <- inc_02 %>% 
  select(tinh02, huyen02, xa02, diaban02) %>% 
  distinct()

panel0204_ <- left_join(panel0204, diaban02, by = c("tinh02", "huyen02", "xa02")) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, everything())

# # 2002 - 2004 - 2006  

panel0406 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, tinh04, huyen04, xa04, hoso04) %>% 
  filter(!is.na(hoso04))

diaban06 <- m1a_06 %>% 
  mutate(across(diaban, as.numeric)) %>%   
  filter(!is.na(diaban)) %>%   
  select(tinh, huyen, xa, diaban) %>% 
  distinct() %>% 
  rename_with(~ paste0(.x, "06"), everything())

panel0406 <- left_join(panel0406, diaban04, by = (c("tinh04" = "tinh", "huyen04" = "huyen", "xa04" = "xa")))

panel0406 <- left_join(panel0406, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06"))

panel0406 <- panel0406 %>% rename(tinh06 = tinh, huyen06 = huyen, xa06 = xa, hoso06 = hoso)

panel0206_ <- left_join(panel0204_, panel0406, by = c("tinh" = "tinh04", "huyen" = "huyen04", "xa" = "xa04", "diaban" = "diaban04", "hoso" = "hoso04")) %>% 
  filter(!is.na(tinh06)) %>% 
  mutate(diaban02 = ifelse(is.na(diaban02), diaban06, diaban02)) %>% 
  select(-c(tinh, huyen, xa, diaban, hoso)) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup()

panel06 <- panel0206_ %>% 
  select(c(matches("06"), hhid)) %>% 
  mutate(year = 2006) %>% 
  rename_with(~ str_replace(.x, "06", ""), everything())

panel02 <- panel0206_ %>% 
  select(c(matches("02"), hhid)) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())

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

panel0204_ <- panel0204_ %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup()

panel02_04 <- panel0204_ %>% 
  select(c(matches("02"), hhid)) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())  

panel04 <- panel0204_ %>% 
  select(c(tinh, huyen, xa, hoso, hhid)) %>% 
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
  select(tinh, huyen, xa, diaban, hoso, m1bc7, tinh02, huyen02, xa02, diaban02, hoso02, matv02, m1bc4, m1bc5)

# Checking accuracy of panel based on age and sex 

age_vhlss02 <- m1_02 %>%
  select(c(matches("02"), m1c2, m1c5)) %>% 
  mutate(matv02 = ifelse(nchar(matv02) > 2, substr(matv02, nchar(matv02) - 1, nchar(matv02)), matv02),
         across(matv02, as.numeric))

age_vhlss02 <- left_join(ivid0204, age_vhlss02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02")) %>% distinct()

ivid0204_ <- age_vhlss02 %>% 
  mutate(correct = (m1bc5 - m1c5) + (m1bc4 - m1c2),
         correct = ifelse(correct == 0, 1, 0)) %>% 
  filter(correct == 1) %>% 
  select(-c(m1bc4, m1bc5, correct, m1c2, m1c5)) %>% 
  group_by(tinh, huyen, xa, diaban, hoso, m1bc7) %>% 
  mutate(ivid = cur_group_id()) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup()

ivid02 <- ivid0204_ %>% 
  select(c(matches("02"), hhid, ivid)) %>% 
  mutate(year = 2002) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())  

ivid04 <- ivid0204_ %>% 
  select(c(tinh, huyen, xa, diaban, hoso, hhid, ivid)) %>% 
  mutate(year = 2004)

ivid0204 <- bind_rows(ivid02, ivid04)

# 2004 - 2006 

ivid0406 <- m1b_06 %>% 
  filter(m1bc6 == 1) %>% 
  rename(matv04 = m1bc3) %>% 
  select(tinh, huyen, xa, hoso, m1bc7, matv04, m1bc4, m1bc5) %>% 
  rename(tinh06 = tinh, huyen06 = huyen, xa06 = xa, hoso06 = hoso)

ivid0406 <- merge(ivid0406, panel0406, by = c("tinh06", "xa06", "huyen06", "hoso06"))

# Checking accuracy 

age_vhlss04 <- m123a_04 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5) %>% 
  rename(tinh04 = tinh, huyen04 =huyen, xa04 = xa, hoso04 = hoso, matv04 = matv)

age_vhlss04 <- merge(age_vhlss04, ivid0406, by = c("tinh04", "huyen04", "xa04", "hoso04", "matv04")) %>% 
  mutate(correct = (m1ac5 - m1bc5) + (m1ac2 - m1bc4),
         correct = ifelse(correct == 0, 1, 0)) %>% 
  filter(correct == 1) %>% 
  select(-c(correct, m1ac5, m1bc5, m1ac2, m1bc4, diaban04))

ivid0206_ <- inner_join(ivid0204_, age_vhlss04, by = c("tinh" = "tinh04", "huyen" = "huyen04", "xa" = "xa04", "hoso" = "hoso04", "m1bc7")) %>% 
  select(-c(tinh, huyen, xa, diaban, hoso, matv04)) %>% 
  rename(matv06 = m1bc7) %>% 
  select(ends_with("02"), ends_with("06"), hhid, ivid) %>% 
  distinct()

ivid02_06 <- ivid0206_ %>% 
  select(c(matches("02"), hhid, ivid)) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  ungroup() %>% 
  mutate(year = 2002)

ivid06 <- ivid0206_ %>% 
  select(c(matches("06"), hhid, ivid)) %>% 
  rename_with(~ str_replace(.x, "06", ""), everything()) %>% 
  ungroup() %>% 
  mutate(year = 2006)

ivid0206 = bind_rows(ivid02_06, ivid06)

ivid <- c("tinh", "huyen", "xa", "diaban", "hoso", "matv", "year")
hhid <- c("tinh", "huyen", "xa", "diaban", "hoso", "year")
