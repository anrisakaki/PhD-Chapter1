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

###############################################
# PROVINCE-LEVEL TARIFFS USING KOVAK'S METHOD #
###############################################

for(i in emp020406) {
  assign(i,get(i) %>%
           mutate(
             traded = as.numeric(industry %in% c(1, 2, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 3, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 40, 74, 92, 93))
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

preBTA_tariffs <- c("preBTA_provtariff", "preBTA_provtariff_k")

for(i in preBTA_tariffs){
  assign(i, get(i) %>% 
           mutate(across(tinh, as.factor)))
}
