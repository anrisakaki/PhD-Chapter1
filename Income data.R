inc02 <- m5aho_02 %>% 
  mutate(m5ac10 = ifelse(is.na(m5ac10), 0, m5ac10),
         wage_tot = m5ac9 + m5ac7e + m5ac10) %>% 
  select(tinh, xa, hoso, wage_tot) %>% 
  mutate(
    xa = substr(as.character(xa), nchar(as.character(xa)) - 1, nchar(as.character(xa))),
    huyen = substr(as.character(xa), 1, 2),
    diaban = substr(as.character(hoso), 1, 2),
    hoso = substr(as.character(hoso), nchar(as.character(hoso)) - 1, nchar(as.character(hoso))),
    across(c(tinh, huyen, xa, hoso), as.numeric),
    wage_tot = ifelse(wage_tot == 0, NA, wage_tot),
    year = 2002) %>% 
  select(-diaban) 

inc02 <- inner_join(inc02, diaban02, by = c("tinh" = "tinh02", "huyen" = "huyen02", "xa" = "xa02")) %>% 
  rename(diaban = diaban02) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, wage_tot)

inc04 <- ho1_04 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tthu_4) %>% 
  mutate(year = 2004) %>% 
  rename(hhinc = thunhap, 
         wage_tot = tthu_4) %>% 
  mutate(wage_tot = ifelse(wage_tot == 0, NA, wage_tot))

inc06 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tongthu_04) %>% 
  rename(hhinc = thunhap,
         wage_tot = tongthu_04) %>% 
  mutate(year = 2006,
         wage_tot = ifelse(wage_tot == 0, NA, wage_tot))

inc0204 <- bind_rows(inc02, inc04)
inc0206 <- bind_rows(inc02, inc06)

inc_spouse_0204 <- emp0204_p %>% 
  filter(female == 1 & married == 1)
inc_spouse_0206 <- emp0206_p %>% 
  filter(female == 1 & married == 1)

inc_spouse_0204_p <- merge(inc_spouse_0204, inc0204, by = hhid)
inc_spouse_0206_p <- merge(inc_spouse_0206, inc0206, by = hhid)
