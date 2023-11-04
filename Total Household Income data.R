inc_exp04 <- ho1_04 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tchi_1, tchi_2, tchi_3, tchi_12, tchi_17, tchi_18, tthu_4) %>% 
  mutate(year = 2004)

inc_exp06 <- ttchung_06 %>% 
  select(tinh, huyen, xa, hoso, thunhap, tongthu_04, chitieu, m6ac9, m6bc7, m5a2c11, m5b2ct, chids) %>% 
  rename(food = m5a2c11)
