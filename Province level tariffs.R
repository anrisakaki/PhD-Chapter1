provtariffs <- vhlss02 %>% 
  filter(work == 1) %>% 
  group_by(tinh, industry) %>% 
  summarise(n = sum(hhwt),
            f = sum(hhwt*(female == 1))) %>% 
  mutate(share_f = f / n) %>% 
  group_by(tinh) %>%
  mutate(share = n / sum(n)) %>% 
  rename(isic2 = industry) %>%
  mutate(isic2 = as.double(isic2)) %>%
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
  rename(tariff = preprov_tariff,
         tariff_f = preprov_tariff_f) %>% 
  mutate(year = 2002)

provtariffs04 <- provtariffs %>%
  select(tinh, postprov_tariff, postprov_tariff_f) %>% 
  rename(tariff = postprov_tariff,
         tariff_f = postprov_tariff_f) %>% 
  mutate(year = 2004)

provtariffs06 <- provtariffs %>%
  select(tinh, postprov_tariff, postprov_tariff_f) %>% 
  rename(tariff = postprov_tariff,
         tariff_f = postprov_tariff_f) %>%
  mutate(year = 2006)

bta0204 <- bind_rows(provtariffs02, provtariffs04)
bta0206 <- bind_rows(provtariffs02, provtariffs06)