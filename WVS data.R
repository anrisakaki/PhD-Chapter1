###################
# PRE-BTA TARIFFS #
###################

provinces <- prov_ams_060402 %>% 
  select(tinh, Tinh)

WVS_01 <- WVS_01 %>% 
  mutate(across(XVIE_V225D, as.numeric),
         tinh = recode(XVIE_V225D,
                       "4" = "Hanoi",
                       "8" = "Ho Chi Minh",
                       "20" = "Lao Cai",
                       "23" = "Lai Chau",
                       "36" = "Thai Binh",
                       "37" = "Thanh Hoa",
                       "39" = "Ha Tinh",
                       "50" = "Dak Lak",
                       "56" = "Binh Dinh",
                       "63" = "Lam Dong",
                       "71" = "Can Tho",
                       "74" = "Tra Vinh",
                       "75" = "Ben Tre",
                       "210" = "Phu Tho",
                       "240" = "Bac Giang",
                       "321" = "Hung Yen",
                       "351" = "Ha Nam",
                       "510" = "Quang Nam",
                       "651" = "Binh Phuoc",
                       "781" = "Bac Lieu"))

WVS_regions_pre <- WVS_01 %>% 
  select(XVIE_V225D, tinh) %>% 
  distinct() %>% 
  rename(Tinh = tinh)

WVS_regions_pre <- merge(WVS_regions_pre, provinces, by = "Tinh")

WVS_preBTA_tariffs <- list(WVS_regions_pre, preBTA_provtariff, preBTA_provtariff_k) %>% 
  reduce(merge, by = "tinh")

####################
# POST-BTA TARIFFS # 
####################

WVS_05 <- WVS_05 %>% 
  mutate(across(V257B, as.numeric),
         tinh = recode(V257B,
                       "704001" = "Hanoi",
                       "704002" = "Hung Yen",
                       "704003" = "Ha Nam",
                       "704004" = "Thai Binh",
                       "704005" = "Lao Cai",
                       "704006" = "Phu Tho",
                       "704007" = "Bac Giang",
                       "704008" = "Lai Chau",
                       "704009" = "Thanh Hoa",
                       "704010" = "Ha Tinh",
                       "704011" = "Quang Nam",
                       "704012" = "Binh Dinh",
                       "704013" = "Dac Lac",
                       "704014" = "Ho Chi Minh",
                       "704015" = "Lam Dong",
                       "704016" = "Binh Phuoc",
                       "704017" = "Dong Thap",
                       "704018" = "An Giang",
                       "704019" = "Ben Tre",
                       "704020" = "Can Tho"))

WVS_regions_post <- WVS_05 %>% 
  select(tinh, V257, V257B) %>% 
  mutate(across(V257, as.numeric),
         region = recode(V257,
                         "704001" = "Red river delta",
                         "704002" = "Northeast",
                         "704003" = "Northwest",
                         "704004" = "North central",
                         "704005" = "Central coast",
                         "704006" = "Central highland",
                         "704007" = "Southeast",
                         "704008" = "Mekong river delta")) %>%
  distinct() %>% 
  rename(Tinh = tinh)

WVS_regions_post <- merge(WVS_regions_post, provinces, by = "Tinh")

WVS_postBTA_tariffs <- list(WVS_regions_post, postBTA_provtariff, postBTA_provtariff_k) %>% 
  reduce(merge, by = "tinh") %>% 
  select(tinh, V257, postprov_tariff, postprov_tariff_k)

load("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/VHLSS Data/WV4_Data_R_v20201117.rdata")

WVS_global <- WV4_Data_R_v20201117 %>% 
  group_by(C_COW_ALPHA) %>% 
  summarise(divorce = mean(V211, na.rm = T)) %>% 
  rename(Country = C_COW_ALPHA) %>% 
  mutate(Country = case_when(
    Country == "DRV" ~ "VIE",
    TRUE ~ Country
  )) %>% 
  filter(Country != "TUR")

ggplot(WVS_global, aes(x = Country, y = divorce, fill = Country)) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) + 
  scale_fill_manual(values = c("VIE" = "red", "Other" = "gray")) +  
  labs(x = "Country", y = "")
ggsave("WVS_global.png", width = 12, height = 7)