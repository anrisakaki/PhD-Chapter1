##################################################
# PROVINCE-LEVEL TARIFFS USING TOPALOVA'S METHOD #
##################################################
provtariff_weights <- employment_mf_02 %>% 
  filter(work == 1) %>% 
  select(tinh, industry, hhwt) %>% 
  group_by(tinh, industry) %>% 
  count(industry, tinh, wt = hhwt)

provtariff_weights_prov <- provtariff_weights %>% 
  group_by(tinh) %>% 
  summarise(nworkers = sum(n))

provtariff_weights <- left_join(provtariff_weights, provtariff_weights_prov, by = "tinh") %>% 
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

####################################################
# PROVINCE-LEVEL TARIFFS USING AUTOR ET AL. METHOD #
####################################################

## Calculating the female-intensiveness of each sector 

provtariff_weights_a <- provtariff_weights %>% 
  select(tinh, isic2, n) %>% 
  mutate(tinh = as.factor(tinh))

tariff_f_weights <- employment_mf_02 %>% 
  filter(work == 1) %>% 
  select(tinh, industry, hhwt, sex) %>% 
  group_by(tinh, industry, sex) %>% 
  count(industry, sex, tinh, wt = hhwt) %>% 
  filter(sex == "Female") %>% 
  rename(isic2 = industry,
    fintensity = n) %>% 
  mutate(tinh = as.factor(tinh),
         isic2 = as.factor(isic2))

tariff_f_weights <- left_join(provtariff_weights_a, tariff_f_weights, by = c("tinh", "isic2"))

tariff_f_weights <- tariff_f_weights %>% 
  mutate(fweights = fintensity/n) %>% 
  mutate(fweights = ifelse(is.na(fweights), 0, fweights)) %>% 
  select(tinh, isic2, fweights)

# Using Topalova's measure 

provtariff_weights <- provtariff_weights %>% mutate(tinh = as.factor(tinh))

provtariff_weights_f <- left_join(provtariff_weights, tariff_f_weights, by = c("tinh", "isic2"))

provtariff_weights_f <- provtariff_weights_f %>% mutate(fweights_02 = fweights*weight_02)

preBTA_provtariff_f <- provtariff_weights_f %>% 
  group_by(tinh) %>% 
  summarise(preprov_tariff_f = weighted.mean(col2_ave_all, fweights_02))

postBTA_provtariff_f <- provtariff_weights_f %>% 
  group_by(tinh) %>% 
  summarise(postprov_tariff_f = weighted.mean(mfn_ave_all, fweights_02))

# Applying new weights onto Kovak's measure of exposure to trade shock 

provtariff_weights_k <- provtariff_weights_k %>%
  mutate(tinh = as.factor(tinh),
         isic2 = as.factor(isic2))

provtariff_weights_fk <- left_join(provtariff_weights_k, tariff_f_weights, by = c("tinh", "isic2")) 

provtariff_weights_fk <- provtariff_weights_fk %>% mutate(fweights_02 = fweights*weight_02_k)

preBTA_provtariff_fk <- provtariff_weights_fk %>% 
  group_by(tinh) %>% 
  summarise(preprov_tariff_fk = weighted.mean(col2_ave_all, fweights_02))

postBTA_provtariff_fk <- provtariff_weights_fk %>% 
  group_by(tinh) %>% 
  summarise(postprov_tariff_fk = weighted.mean(mfn_ave_all, fweights_02))

# Merging all into single dataframe 

provtariffs <- list(preBTA_provtariff_f, preBTA_provtariff_fk, postBTA_provtariff_f, postBTA_provtariff_fk) %>% 
  reduce(full_join, by = "tinh")

save(provtariffs, file = "provtariffs.rda")

########################
# Put into 1 dataframe #
########################

preBTA_provtariff <- preBTA_provtariff %>% 
  mutate(year = 2002) %>%
  rename(provtariff = preprov_tariff)

preBTA_provtariff_k <- preBTA_provtariff_k %>% 
  mutate(year = 2002) %>% 
  rename(provtariff_k = preprov_tariff_k)

preBTA_provtariff_f <- preBTA_provtariff_f %>%
  mutate(year = 2002) %>%
  rename(provtariff_f = preprov_tariff_f)

preBTA <- list(preBTA_provtariff, preBTA_provtariff_k, preBTA_provtariff_f) %>% 
  reduce(full_join, by = c("tinh", "year"))

postBTA_provtariff <- postBTA_provtariff %>% 
  rename(provtariff = postprov_tariff) %>% 
  mutate(across(tinh, as.factor))

postBTA_provtariff_k <- postBTA_provtariff_k %>% 
  rename(provtariff_k = postprov_tariff_k)

postBTA_provtariff_f <- postBTA_provtariff_f %>%
  rename(provtariff_f = postprov_tariff_f)

postBTA <- list(postBTA_provtariff, postBTA_provtariff_k, postBTA_provtariff_f) %>% 
  reduce(full_join, by = "tinh")

provtariffs0204 <- bind_rows(preBTA, postBTA) %>% 
  mutate(year = ifelse(is.na(year), 2004, year))

provtariffs0206 <- bind_rows(preBTA, postBTA) %>% 
  mutate(year = ifelse(is.na(year), 2006, year))
