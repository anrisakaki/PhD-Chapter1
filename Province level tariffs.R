provtariffs02 <- provtariff %>%
  select(province, col2_ave_all01) %>% 
  rename(tinh = province,
         tariff = col2_ave_all01) %>% 
  mutate(year = 2002)

provtariffs04 <- provtariff %>%
  select(province, mfn_ave_all04) %>% 
  rename(tinh = province,
         tariff = mfn_ave_all04) %>% 
  mutate(year = 2004)

provtariffs06 <- provtariff %>%
  select(province, mfn_ave_all04) %>% 
  rename(tinh = province,
         tariff = mfn_ave_all04) %>% 
  mutate(year = 2006)

bta0204 <- bind_rows(provtariffs02, provtariffs04)
bta0206 <- bind_rows(provtariffs02, provtariffs06)