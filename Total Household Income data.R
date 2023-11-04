

inc_exp04 <- ho1_04 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tthu_4) %>% 
  mutate(year = 2004) %>% 
  rename(hhinc = thunhap, 
         wage_tot = tthu_4)

inc_exp06 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tongthu_04) %>% 
  rename(hhinc = thunhap,
         wage_tot = tongthu_04)
