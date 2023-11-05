inc02 <- m5aho_02 %>% 
  mutate(m5ac10 = ifelse(is.na(m5ac10), 0, m5ac10),
         wage_tot = m5ac9 + m5ac7e + m5ac10) %>% 
  select(tinh, xa, hoso, wage_tot) %>% 
  mutate(
    xa = substr(as.character(xa), nchar(as.character(xa)) - 1, nchar(as.character(xa))),
    huyen = substr(as.character(xa), 1, 2),
    diaban = substr(as.character(hoso), 1, 2),
    hoso = substr(as.character(hoso), nchar(as.character(hoso)) - 1, nchar(as.character(hoso))),
    across(c(tinh, huyen, xa, hoso), as.numeric)
  ) %>% 
  select(-diaban)

inc02 <- inner_join(inc02, diaban02, by = c("tinh" = "tinh02", "huyen" = "huyen02", "xa" = "xa02")) %>% 
  rename(diaban = diaban02) %>% 
  select(tinh, huyen, xa, diaban, hoso, wage_tot)

inc04 <- ho1_04 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tthu_4) %>% 
  mutate(year = 2004) %>% 
  rename(hhinc = thunhap, 
         wage_tot = tthu_4)

inc06 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tongthu_04) %>% 
  rename(hhinc = thunhap,
         wage_tot = tongthu_04) %>% 
  mutate(year = 2006)


##############################################################
# DESCRIPTIVE STATISTICS FOR INCOME AND SECTOR OF EMPLOYMENT #
##############################################################

inc02 %>%
  filter(agri_work == 1 | tal == 1) %>% 
  group_by(tal, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))

inc02 %>%
  filter(agri_work == 1 | construction == 1) %>% 
  group_by(construction, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))

inc06 %>%
  filter(agri_work == 1 | tal == 1) %>% 
  group_by(tal, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))

inc06 %>%
  filter(agri_work == 1 | construction == 1) %>% 
  group_by(construction, sex) %>% 
  summarise(avg_wage = weighted.mean(totalinc, wt = hhwt, na.rm = TRUE))