phc99 <- phc99 %>%
  filter(age > 15 & age < 65) %>% 
  mutate(tinh = case_when(
           geo1_vn1999 == 219 ~ 104,
           geo1_vn1999 == 223 ~ 106,
           geo1_vn1999 == 503 ~ 505,
           geo1_vn1999 == 505 ~ 507,
           geo1_vn1999 == 507 ~ 503,
           geo1_vn1999 == 703 ~ 607,
           TRUE ~ geo1_vn1999 
         ))

provtariffs <- phc99 %>% 
  mutate(isic2 = as.factor(str_sub(as.character(ind), 1, if_else(nchar(as.character(ind)) < 3, 1, 2))),
         female = ifelse(sex == 2, 1, 0)) %>% 
  group_by(tinh, isic2) %>% 
  summarise(n = sum(perwt),
            f = sum(perwt[female == 1], na.rm = T)) %>% 
  mutate(share_f = f / n) %>% 
  group_by(tinh) %>% 
  mutate(share = n / sum(n)) %>% 
  left_join(tariff, by = "isic2") %>% 
  mutate(
    preBTA = share * col2_ave_all,
    preBTA_f = share_f*share * col2_ave_all,
    postBTA = share * mfn_ave_all,
    postBTA_f = share_f * share * mfn_ave_all
  ) %>%
  group_by(tinh) %>%
  summarise(
    preprov_tariff = sum(preBTA, na.rm = TRUE),
    preprov_tariff_f = sum(preBTA_f, na.rm = TRUE),
    postprov_tariff = sum(postBTA, na.rm = TRUE),
    postprov_tariff_f = sum(postBTA_f, na.rm = TRUE)
  )

tariff02_mccaig <- provtariff_mccaig %>%
  select(province, col2_ave_all01, col2_ave_all98) %>% 
  rename(mccaig_bta = col2_ave_all01,
         mccaig_bta98 = col2_ave_all98,
         tinh_old = province) 

tariff04_mccaig <- provtariff_mccaig %>%
  select(province, mfn_ave_all04) %>% 
  rename(mccaig_bta = mfn_ave_all04,
         tinh_old = province) %>% 
  mutate(mccaig_bta98 = mccaig_bta)

provtariffs02 <- provtariffs %>%
  select(tinh, preprov_tariff, preprov_tariff_f) %>% 
  rename(tinh_old = tinh,
         tariff = preprov_tariff,
         tariff_f = preprov_tariff_f) %>% 
  left_join(tariff02_mccaig, by = "tinh_old") %>% 
  mutate(year = 2002)

provtariffs04 <- provtariffs %>%
  select(tinh, postprov_tariff, postprov_tariff_f) %>% 
  rename(tinh_old = tinh,
         tariff = postprov_tariff,
         tariff_f = postprov_tariff_f) %>% 
  left_join(tariff04_mccaig, by = "tinh_old") %>% 
  mutate(year = 2004)

bta0204 <- bind_rows(provtariffs02, provtariffs04)

