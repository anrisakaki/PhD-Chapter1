##########################################
# SETTING UP HOUSEHOL DPANEL IDENTIFIERS #
##########################################

# 2002 - 2004 

panel0204 <- ho1_04 %>%
  filter(m1c1 == 1) %>% 
  select(tinh02, huyen02, xa02, hoso02, quy02, tinh, huyen, xa, hoso) %>% 
  mutate(tinh02 = ifelse(is.na(tinh02), tinh, tinh02),
         huyen02 = ifelse(is.na(huyen02), huyen, huyen02),
         xa02 = ifelse(is.na(xa02), xa, xa02),
         hoso02 = ifelse(nchar(hoso02) > 2, substr(hoso02, nchar(hoso02) - 1, nchar(hoso02)), hoso02),
         across(hoso02, as.numeric)) %>% 
  group_by(tinh02, huyen02, xa02, hoso02, tinh, huyen, xa, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup()

###########################################
# SETTING UP INDIVIDUAL PANEL IDENTIFIERS #
###########################################

# 2002 - 2004 

ivid04 <- m1b_04 %>%
  filter(m1bc6 == 1) %>% 
  select(tinh, huyen, xa, hoso, m1bc3, m1bc4, m1bc7, ky) %>% 
  rename("matv02" = m1bc3) %>% 
  select(tinh, huyen, xa, hoso, m1bc7, everything())

ivid0204_ <- merge(ivid04, panel0204, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  select(hhid, tinh, huyen, xa, hoso, m1bc7, ky, tinh02, huyen02, xa02, hoso02, matv02,quy02, m1bc4) %>% 
  rename(matv04 = m1bc7,
         female = m1bc4,
         qui = quy02) %>% 
  mutate(female = ifelse(female == 2, 1, 0)) %>% 
  group_by(hhid, tinh02, huyen02, xa02, hoso02, matv02, qui, ky, female, tinh, huyen, xa, hoso) %>% 
  mutate(ivid = cur_group_id()) %>% 
  ungroup()

ivid02 <- ivid0204_ %>%
  select(tinh02, huyen02, xa02, hoso02, matv02, qui, female, hhid, ivid) %>% 
  rename(tinh = tinh02,
         huyen = huyen02,
         xa = xa02,
         hoso = hoso02,
         matv = matv02,
         ky = qui) %>% 
  mutate(year = 2002)

ivid04 <- ivid0204_ %>%
  select(tinh, huyen, xa, hoso, matv04, ky, female, hhid, ivid) %>% 
  rename(matv = matv04) %>% 
  mutate(year = 2004)

ivid0204 <- bind_rows(ivid02, ivid04)

ivid <- c("year", "ky", "tinh", "huyen", "xa", "hoso", "matv", "female")
hhid <- c("tinh", "huyen", "xa", "hoso", "year")
