##################################################
# PROVINCE-LEVEL TARIFFS USING TOPALOVA'S METHOD #
##################################################
provtariff_weights <- employment_mf_02 %>% 
  filter(m3c2 == 1) %>% 
  select(tinh, industry, hhwt) %>% 
  group_by(tinh, industry) %>% 
  count(industry, tinh, wt = hhwt)

provtariff_weights_prov <- provtariff_weights %>% 
  group_by(tinh) %>% 
  summarise(nworkers = sum(n))

provtariff_weights <- merge(provtariff_weights, provtariff_weights_prov, by = "tinh") %>% 
  mutate(weight_02 = n / nworkers) %>% 
  rename(isic2 = industry)

provtariff_weights$isic2 <-  as.factor(provtariff_weights$isic2) #Joining data on weights and industrial tariffs, by province 
tariff$isic2 <- as.factor(tariff$isic2)

provtariff_weights <- left_join(provtariff_weights, tariff, by = "isic2") 

provtariff_weights$col2_ave_all[is.na(provtariff_weights$col2_ave_all)] <- 0
provtariff_weights$mfn_ave_all[is.na(provtariff_weights$mfn_ave_all)] <- 0

preBTA_provtariff <- provtariff_weights %>%
  group_by(tinh) %>%
  summarise(preprov_tariff = weighted.mean(col2_ave_all, weight_02))

postBTA_provtariff <- provtariff_weights %>% 
  group_by(tinh) %>% 
  summarise(postprov_tariff = weighted.mean(mfn_ave_all, weight_02))

##################################################
# PROVINCE-LEVEL TARIFFS USING KOVAK'S METHOD #
##################################################

for(i in emp020406) {
  assign(i,get(i) %>%
           mutate(
             traded = as.numeric(industry %in% c(16, 17, 25, 33, 23, 26, 28, 31, 29, 20, 24, 34, 19, 93, 22, 21, 15, 27, 1, 2, 14, 12, 74, 5, 92, 23, 11, 10, 40))
           ))
}

provtariff_weights_k <- employment_mf_02 %>% 
  filter(traded == 1) %>% 
  select(tinh, industry, hhwt) %>% 
  group_by(tinh, industry) %>% 
  count(industry, tinh, wt = hhwt)

provtariff_weights_prov_k <- provtariff_weights_k %>% 
  group_by(tinh) %>% 
  summarise(nworkers = sum(n))

provtariff_weights_k <- merge(provtariff_weights_k, provtariff_weights_prov_k, by = "tinh") %>% 
  mutate(weight_02_k = n / nworkers) %>% 
  rename(isic2 = industry)

provtariff_weights_k <- merge(provtariff_weights_k, tariff, by = "isic2")

provtariff_weights_k$tinh <- as.factor(provtariff_weights_k$tinh)

preBTA_provtariff_k <- provtariff_weights_k %>% 
  group_by(tinh) %>% 
  summarise(preprov_tariff_k = weighted.mean(col2_ave_all, weight_02_k))

postBTA_provtariff_k <- provtariff_weights_k %>% 
  group_by(tinh) %>% 
  summarise(postprov_tariff_k = weighted.mean(mfn_ave_all, weight_02_k))

preBTA_textiles <- provtariff_weights %>% 
  mutate(across(isic2, as.character)) %>% 
  filter(isic2 > 16 & isic2 < 20) %>% 
  filter(!isic2 %in% c('2'))

preprovBTA_textiles <- preBTA_textiles %>%
  group_by(tinh) %>%
  summarise(preprov_tariff_textiles = weighted.mean(col2_ave_all, weight_02))

postprovBTA_textiles <- preBTA_textiles %>%
  group_by(tinh) %>%
  summarise(postprov_tariff_textiles = weighted.mean(mfn_ave_all, weight_02))