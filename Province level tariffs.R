library(dplyr)

phc99 <- phc99 %>%
  mutate(tinh = case_when(
           geo1_vn1999 == 219 ~ 104,
           geo1_vn1999 == 223 ~ 106,
           geo1_vn1999 == 503 ~ 505,
           geo1_vn1999 == 505 ~ 507,
           geo1_vn1999 == 507 ~ 503,
           geo1_vn1999 == 703 ~ 607,
           TRUE ~ geo1_vn1999 
         ))

traded_n <- phc99 %>% 
  mutate(isic2 = as.double(str_sub(as.character(ind), 1, if_else(nchar(as.character(ind)) < 3, 1, 2)))) %>% 
  left_join(traded, by = "isic2") %>% 
  filter(traded == 1) %>% 
  group_by(tinh) %>% 
  summarise(traded_n = sum(perwt))

provtariffs <- phc99 %>% 
  mutate(isic2 = as.double(str_sub(as.character(ind), 1, if_else(nchar(as.character(ind)) < 3, 1, 2))),
         female = ifelse(sex == 2, 1, 0)) %>% 
  group_by(tinh, isic2) %>% 
  summarise(n = sum(perwt),
            f = sum(perwt[female == 1], na.rm = T)) %>% 
  mutate(share_f = f / n) %>% 
  left_join(traded_n, by = "tinh") %>% 
  mutate(share = n / traded_n) %>% 
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

provtariffs02 <- provtariffs %>%
  select(tinh, preprov_tariff, preprov_tariff_f) %>% 
  rename(tinh_old = tinh,
         tariff = preprov_tariff,
         tariff_f = preprov_tariff_f) %>% 
  mutate(year = 2002)

provtariffs04 <- provtariffs %>%
  select(tinh, postprov_tariff, postprov_tariff_f) %>% 
  rename(tinh_old = tinh,
         tariff = postprov_tariff,
         tariff_f = postprov_tariff_f) %>% 
  mutate(year = 2004)

provtariffs06 <- provtariffs %>%
  select(tinh, postprov_tariff, postprov_tariff_f) %>% 
  rename(tinh_old = tinh,
         tariff = postprov_tariff,
         tariff_f = postprov_tariff_f) %>%
  mutate(year = 2006)

bta0204 <- bind_rows(provtariffs02, provtariffs04)
bta0206 <- bind_rows(provtariffs02, provtariffs06)