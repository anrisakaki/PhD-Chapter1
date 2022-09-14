provtce_0204 <- merge(preBTA_provtariff, postBTA_provtariff, by = "tinh") 

provtce_0204 <- provtce_0204 %>% mutate(tce = postprov_tariff - preprov_tariff)

provtce_0204$tinh <- as.factor(provtce_0204$tinh)

provtce_0204$VARNAME_1 <- forcats::fct_recode(provtce_0204$tinh,
                                              "Hanoi" = "101",
                                              "Hai Phong" = "103",
                                              "Vinh Phuc" = "104",
                                              "Ha Tay" = "105",
                                              "Bac Ninh" = "106",
                                              "Hai Duong" = "107",
                                              "Hung Yen" = "109",
                                              "Ha Nam" = "111",
                                              "Nam Dinh" = "113",
                                              "Thai Binh" = "115",
                                              "Ninh Binh" = "117",
                                              "Ha Giang" = "201",
                                              "Cao Bang" = "203",
                                              "Lao Cai" = "205",
                                              "Bac Can" = "207",
                                              "Lang Son" = "209",
                                              "Tuyen Quyen" = "211",
                                              "Yen Bai" = "213",
                                              "Thai Nguyen" = "215",
                                              "Phu Tho" = "217",
                                              "Bac Giang" = "221",
                                              "Quang Ninh" = "225",
                                              "Lai Chau" = "301",
                                              "Son La" = "303",
                                              "Hoa Binh" = "305",
                                              "Thanh Hoa" = "401",
                                              "Nghe An" = "403",
                                              "Ha Tinh" = "405",
                                              "Quang Binh" = "407",
                                              "Quang Tri" = "409",
                                              "Hue" = "411",
                                              "Da Nang" = "501",
                                              "Quang Nam" = "503",
                                              "Quang Ngai" = "505",
                                              "Binh Dinh" = "507",
                                              "Phu Yen" = "509",
                                              "Khanh Hoa" = "511",
                                              "Kon Tum" = "601",
                                              "Gia Lai" = "603",
                                              "Dac Lac" = "605",
                                              "Lam Dong" = "607",
                                              "Ho Chi Minh" = "701",
                                              "Ninh Thuan" = "705",
                                              "Binh Phuoc" = "707",
                                              "Tay Ninh" = "709",
                                              "Binh Duong" = "711",
                                              "Dong Nai" = "713",
                                              "Binh Thuan" = "715",
                                              "Ba Ria - Vung Tau" = "717",
                                              "Long An" = "801",
                                              "Dong Thap" = "803",
                                              "An Giang" = "805",
                                              "Tien Giang" = "807",
                                              "Vinh Long" = "809",
                                              "Ben Tre" = "811",
                                              "Kien Giang" = "813",
                                              "Can Tho" = "815",
                                              "Tra Vinh" = "817",
                                              "Soc Trang" = "819",
                                              "Bac Lieu" = "821",
                                              "Ca Mau" = "823")

#Creating dataframe for map shapefile and tariffs, by province
tariffmap_0204 <- merge(vnmap1, provtce_0204, by = "VARNAME_1") 

ggplot(tariffmap_0204) + 
  geom_sf(aes(fill = preprov_tariff*100)) +
  scale_fill_gradient(name = "Province-level Tariff", low = "grey90", high = "grey20", na.value = "white") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("Tariff Rate by Province in 2001")
ggsave(file = "tariffmap_2001.png", device = png, width = 7, height = 7)

provincialtariff_map_04 <- ggplot(tariffmap_0204) + 
  geom_sf(aes(fill = postprov_tariff*100)) +
  scale_fill_gradient(name = "Province-level Tariff", low = "grey90", high = "grey20", na.value = "white") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("Tariff Rate by Province in 2003")
ggsave(file = "tariffmap_2003.png", device = png, width = 7, height = 7)

tce_map_0204<- ggplot(tariffmap_0204) + 
  geom_sf(aes(fill = tce *100)) +
  scale_fill_gradient(name = "Tariff Cut Exposure", low = "grey20", high = "grey90", na.value = "white") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("Tariff Cut Exposure by Province in 2003")
ggsave(file = "TCE_2003.png", device = png, width = 7, height = 7)