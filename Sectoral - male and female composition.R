##########################################
# MALE AND FEMALE EMPLOYMENT COMPOSITION #
##########################################

# 2002 
industry_mf_02 <- employment_mf_02 %>%
  select(m1c2, industry, hhwt) %>% 
  replace(is.na(.), 0) %>%  
  group_by(industry, m1c2) %>% 
  count(m1c2, industry, wt = hhwt) %>% 
  pivot_wider(names_from = "m1c2", values_from = "n")

industry_mf_02 <- industry_mf_02 %>% 
  rename("Industry" = "industry") %>% 
  rename("Male" = 2) %>% 
  rename("Female" = 3)

colSums(industry_mf_02[,c(2,3)], na.rm = TRUE)

nm_02 <- 20971525
nf_02 <- 22242964 

industry_mf_02 <- industry_mf_02 %>% 
  mutate(MRatio_02 = Male / (nm_02) * 100,
         FRatio_02 = Female / (nf_02) * 100)

# 2004 
industry_mf_04 <- employment_mf_04 %>% 
  select(m1ac2, m1ac5, industry, hhwt) %>% 
  replace(is.na(.), 0) %>%  
  count(m1ac2, industry, wt = hhwt) %>% 
  pivot_wider(names_from = "m1ac2", values_from = "n")

industry_mf_04 <- industry_mf_04 %>% 
  rename("Industry" = "industry") %>% 
  rename("Male" = 2) %>% 
  rename("Female" = 3)

colSums(industry_mf_04[, c(2, 3)], na.rm=TRUE) 

nm_04 <- 21745627
nf_04 <- 22877119

industry_mf_04 <- industry_mf_04 %>% 
  mutate(MRatio_04 = Male / nm_04 * 100,
         FRatio_04 = Female / nf_04 * 100)

# 2006 
industry_mf_06 <- employment_mf_06 %>% 
  select(sex, industry, hhwt) %>% 
  count(sex, industry, wt = hhwt) %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  rename("Industry" = industry)

colSums(industry_mf_06[, c(2, 3)], na.rm=TRUE) # There are 65,570 men, and 67,316 women in this dataframe 

nm_06 <- 24588676
nf_06 <- 25590580

industry_mf_06 <- industry_mf_06 %>% 
  mutate(MRatio_06 = Male / nm_06 * 100,
         FRatio_06 = Female / nf_06 * 100)

# 2002 - 2006 
industry_lf_020406 <- list(industry_mf_02, industry_mf_04, industry_mf_06) %>% 
  reduce(full_join, by = "Industry") %>% 
  mutate(isic2 = Industry) %>%   
  mutate(MChange = MRatio_06 - MRatio_02) %>% 
  mutate(FChange = FRatio_06 - FRatio_02) %>% 
  mutate(Industry = recode(Industry,
                           "0" = "Unemployed",
                           "1" = "Agriculture, hunting and related service activities",
                           "2" = "Forestry, logging and related service activities",
                           "5" = "Fishing, operation of fish hatcheries and fish farms",
                           "10" = "Mining of coal and lignite" ,
                           "11" = "Extraction of crude petroleum and natural gas",
                           "12" = "Mining of uranium and thorium ores",
                           "13" =  "Mining of metal ores",
                           "14" = "Other mining and quarrying",
                           "15" = "Manufacture of food products and beverages",
                           "16" = "Manufacture of tobacco products",
                           "17" = "Manufacture of textiles",
                           "18" = "Manufacture of wearing apparel",
                           "19" = "Manufacture of leather goods",
                           "20" = "Manufacture of wood and of products of wood",
                           "21" = "Manufacture of paper and paper products",
                           "22" = "Publishing, printing and reproduction of recorded media",
                           "23" = "Manufacture of coke, refined petroleum products and nuclear fuel",
                           "24" = "Manufacture of chemicals and chemical products",
                           "25" = "Manufacture of rubber and plastics products",
                           "26" = "Manufacture of other non-metallic mineral products",
                           "27" = "Manufacture of basic metals",
                           "28" = "Manufacture of fabricated metal products",
                           "29" = "Manufacture of not-yet-classified machinery and equipment",
                           "30" = "Manufacture of office, accounting and computing machinery",
                           "31" = "Manufacture of electrical machinery and apparatus n.e.c.",
                           "32" = "Manufacture of radio, television and communication equipment and apparatus",
                           "33" = "Manufacture of medical, precision and optical instruments, watches and clocks",
                           "34" = "Manufacture of motor vehicles, trailers and semi-trailers",
                           "35" = "Manufacture of other transport equipment",
                           "36" = "Manufacture of furniture",
                           "37" = "Recycling",
                           "40" = "Electricity, gas, steam and hot water supply",
                           "41" = "Collection, purification and distribution of water",
                           "45" = "Construction",
                           "50" = "Sale, maintenance and repair of motor vehicles and motorcycles",
                           "51" = "Wholesale trade and commission trade",
                           "52" ="Retail trade",
                           "55" = "Hotels and restaurants",
                           "60" = "Land transport",
                           "61" = "Water transport",
                           "62" = "Air transport",
                           "63" = "Supporting and auxiliary transport activities",
                           "64" = "Post and telecommunications",
                           "65" = "Financial intermediation",
                           "66" = "Insurance and pension funding, except compulsory social security",
                           "67" = "Activities auxiliary to financial intermediation",
                           "70" = "Scientific and industrial activities",
                           "71" = "Real estate activities",
                           "72" = "Renting of machinery and equipment without operator and of personal and household goods",
                           "73" = "Computer and related activities",
                           "74" = "Other business activities",
                           "75" = "Public administration and defence",
                           "80" = "Education",
                           "85" = "Health and social work",
                           "90" = "Sporting and other recreational activities",
                           "91"= "Activities of the party, organisations and associations",
                           "92" = "Sewage and refuse disposal, sanitation and similar activities",
                           "93" = "Other service activities",
                           "95" = "Private households with employed persons",
                           "99" = "Extra-territorial organizations and bodies"))

industry_lf_020406$isic2 <- as.factor(industry_lf_020406$isic2) 

industry_lf_020406 <- left_join(industry_lf_020406, tariff, by = c("isic2", "Industry"))

industry_lf_020406 <- industry_lf_020406 %>%
  arrange(as.numeric(as.character(FChange, decreasing = TRUE)))

# Bar chart for changes in male and female employment composition between 2001 - 2005 
ggplot(dplyr::filter(industry_lf_020406, MChange > 0.3 | MChange < - 0.06), aes(reorder(Industry, MChange), MChange)) +
  geom_col() +
  labs(x = "Industry",
       y= "Change in MLF composition \nbetween 2001 and 2005") +
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()
ggsave(file = "MLFP-Industry.png", device = png, width = 7, height = 7)

ggplot(dplyr::filter(industry_lf_020406, FChange > 0.2 | FChange < - 0.07), aes(reorder(Industry, FChange), FChange)) +
  geom_col() +
  # geom_text(aes(label = round(FChange, digits = 2))) +  
  labs(x = "Industry",
       y= "Change in FLF composition \nbetween 2001 and 2005") +
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()
ggsave(file = "FLFP-Industry.png", device = png, width = 7, height = 7)

##################################
# SECTORAL COMPOSITION BY GENDER #
##################################

sec_02 <- employment_mf_02 %>%
  select(sex, industry, hhwt) %>% 
  group_by(industry) %>% 
  count(industry, sex, wt = hhwt) %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  mutate(Industry_sum = Male + Female,
         Msec_02 = Male / Industry_sum,
         Fsec_02 = Female / Industry_sum)

sec_04 <- employment_mf_04 %>%
  select(sex, industry, hhwt) %>% 
  group_by(industry) %>% 
  count(industry, sex, wt = hhwt) %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  mutate(Industry_sum = Male + Female,
         Msec_04 = Male / Industry_sum,
         Fsec_04 = Female / Industry_sum)

sec_06 <- employment_mf_06 %>%
  select(sex, industry, hhwt) %>% 
  group_by(industry) %>% 
  count(industry, sex, wt = hhwt) %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  mutate(Industry_sum = Male + Female,
         Msec_06 = Male / Industry_sum,
         Fsec_06 = Female / Industry_sum)

sec020406 <- list(sec_02, sec_04, sec_06) %>% 
  reduce(full_join, by = "industry") %>% 
  mutate(FChange = Fsec_06 - Fsec_02) %>% 
  mutate(industry = recode(industry,
                           "0" = "Unemployed",
                           "1" = "Agriculture, hunting and related service activities",
                           "2" = "Forestry, logging and related service activities",
                           "5" = "Fishing, operation of fish hatcheries and fish farms",
                           "10" = "Mining of coal and lignite" ,
                           "11" = "Extraction of crude petroleum and natural gas",
                           "12" = "Mining of uranium and thorium ores",
                           "13" =  "Mining of metal ores",
                           "14" = "Other mining and quarrying",
                           "15" = "Manufacture of food products and beverages",
                           "16" = "Manufacture of tobacco products",
                           "17" = "Manufacture of textiles",
                           "18" = "Manufacture of wearing apparel",
                           "19" = "Manufacture of leather goods",
                           "20" = "Manufacture of wood and of products of wood",
                           "21" = "Manufacture of paper and paper products",
                           "22" = "Publishing, printing and reproduction of recorded media",
                           "23" = "Manufacture of coke, refined petroleum products and nuclear fuel",
                           "24" = "Manufacture of chemicals and chemical products",
                           "25" = "Manufacture of rubber and plastics products",
                           "26" = "Manufacture of other non-metallic mineral products",
                           "27" = "Manufacture of basic metals",
                           "28" = "Manufacture of fabricated metal products",
                           "29" = "Manufacture of not-yet-classified machinery and equipment",
                           "30" = "Manufacture of office, accounting and computing machinery",
                           "31" = "Manufacture of electrical machinery and apparatus n.e.c.",
                           "32" = "Manufacture of radio, television and communication equipment and apparatus",
                           "33" = "Manufacture of medical, precision and optical instruments, watches and clocks",
                           "34" = "Manufacture of motor vehicles, trailers and semi-trailers",
                           "35" = "Manufacture of other transport equipment",
                           "36" = "Manufacture of furniture",
                           "37" = "Recycling",
                           "40" = "Electricity, gas, steam and hot water supply",
                           "41" = "Collection, purification and distribution of water",
                           "45" = "Construction",
                           "50" = "Sale, maintenance and repair of motor vehicles and motorcycles; retail sale of automotive fuel",
                           "51" = "Wholesale trade and commission trade, except of motor vehicles and motorcycles",
                           "52" ="Retail trade, except of motor vehicles and motorcycles",
                           "55" = "Hotels and restaurants",
                           "60" = "Land transport",
                           "61" = "Water transport",
                           "62" = "Air transport",
                           "63" = "Supporting and auxiliary transport activities",
                           "64" = "Post and telecommunications",
                           "65" = "Financial intermediation",
                           "66" = "Insurance and pension funding, except compulsory social security",
                           "67" = "Activities auxiliary to financial intermediation",
                           "70" = "Scientific and industrial activities",
                           "71" = "Real estate activities",
                           "72" = "Renting of machinery and equipment without operator and of personal and household goods",
                           "73" = "Computer and related activities",
                           "74" = "Other business activities",
                           "75" = "Public administration and defence; compulsory social security",
                           "80" = "Education",
                           "85" = "Health and social work",
                           "90" = "Sporting and other recreational activities",
                           "91"= "Activities of the party, organisations and associations",
                           "92" = "Sewage and refuse disposal, sanitation and similar activities",
                           "93" = "Other service activities",
                           "95" = "Private households with employed persons",
                           "99" = "Extra-territorial organizations and bodies"))

sec020406 <- sec020406 %>%
  arrange(as.numeric(as.character(FChange, decreasing = TRUE)))

#################################################
# PROVINCE-LEVEL SECTORAL COMPOSITION BY GENDER #
#################################################

# 2002 
agri_prov_02 <- employment_mf_02 %>% 
  filter(agri_work == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

manu_prov_02 <- employment_mf_02 %>% 
  filter(manu == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

manu_exc_al_prov_02 <- employment_mf_02 %>% 
  filter(manu == 1) %>%
  filter(tal == 0) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

textiles_prov_02 <- employment_mf_02 %>% 
  filter(industry == 17) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

tal_prov_02 <- employment_mf_02 %>% 
  filter(tal == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

con_prov_02 <- employment_mf_02 %>% 
  filter(industry == 45) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

prov_ams_02 <- c("agri_prov_02", "manu_prov_02", "manu_exc_al_prov_02", "textiles_prov_02", "tal_prov_02", "con_prov_02")

for(i in prov_ams_02){
  
  assign(i, left_join(get(i), provtariff_weights_prov, by = "tinh"))
  
  assign(i, get(i) %>% 
           mutate(n_workers = n/nworkers*100)) # Share of province in sector 
  
  assign(i, get(i) %>% 
           select(tinh, n_workers))
  
  if(i %in% c("agri_prov_02")){
    assign(i, get(i) %>%
             rename(n_agri = n_workers))    
  }  
  
  if(i %in% c("manu_prov_02")){
    assign(i, get(i) %>%
             rename(n_manu = n_workers))    
  }   
  
  if(i %in% c("manu_exc_al_prov_02")){
    assign(i, get(i) %>%
             rename(n_manu_exc_al = n_workers))    
  }
  
  if(i %in% c("textiles_prov_02")){
    assign(i, get(i) %>%
             rename(n_textiles = n_workers))    
  } 
  
  if(i %in% c("tal_prov_02")){
    assign(i, get(i) %>%
             rename(n_tal = n_workers))    
  }  
  
  if(i %in% c("con_prov_02")){
    assign(i, get(i) %>%
             rename(n_con = n_workers))    
  }   
}

prov_ams_02 <- list(agri_prov_02, manu_prov_02, manu_exc_al_prov_02, textiles_prov_02, tal_prov_02, con_prov_02) %>% 
  reduce(full_join, by = "tinh")

# 2004 
agri_prov_04 <- employment_mf_04 %>% 
  filter(agri_work == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

manu_prov_04 <- employment_mf_04 %>% 
  filter(manu == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

manu_exc_al_prov_04 <- employment_mf_04 %>% 
  filter(manu == 1) %>% 
  filter(tal == 0) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

service_prov_04 <- employment_mf_04 %>% 
  filter(service == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

tal_prov_04 <- employment_mf_04 %>% 
  filter(tal == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

con_prov_04 <- employment_mf_04 %>% 
  filter(industry == 45) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

prov_workers_04 <- employment_mf_04 %>% 
  group_by(tinh) %>% 
  count(tinh, wt = hhwt) %>% 
  rename(nworkers = n)

fd_ams_04 <- c("agri_prov_04", "manu_prov_04", "manu_exc_al_prov_04", "service_prov_04", "tal_prov_04", "con_prov_04")

for(i in fd_ams_04){
  
  assign(i, left_join(get(i), prov_workers_04, by = "tinh"))
  
  assign(i, get(i) %>% 
           mutate(n_workers = n/nworkers*100)) # Share of province in sector 
  
  assign(i, get(i) %>% 
           select(tinh, n_workers))
  
  if(i %in% c("agri_prov_04")){
    assign(i, get(i) %>%
             rename(n_agri_04 = n_workers))    
  }  
  
  if(i %in% c("manu_prov_04")){
    assign(i, get(i) %>%
             rename(n_manu_04 = n_workers))    
  }   
  
  if(i %in% c("manu_exc_al_prov_04")){
    assign(i, get(i) %>%
             rename(n_manu_exc_al_04 = n_workers))    
  }  
  
  if(i %in% c("service_prov_04")){
    assign(i, get(i) %>%
             rename(n_service_04 = n_workers))    
  } 
  
  if(i %in% c("tal_prov_04")){
    assign(i, get(i) %>%
             rename(n_tal_04 = n_workers))    
  }   
  
  if(i %in% c("con_prov_04")){
    assign(i, get(i) %>%
             rename(n_con_04 = n_workers))    
  }   
}

prov_ams_04 <- list(agri_prov_04, manu_prov_04, manu_exc_al_prov_04, service_prov_04, tal_prov_04, con_prov_04) %>% 
  reduce(full_join, by = "tinh")

# 2006 
agri_prov_06 <- employment_mf_06 %>% 
  filter(agri_work == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

manu_prov_06 <- employment_mf_06 %>% 
  filter(manu == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

manu_exc_al_prov_06 <- employment_mf_06 %>% 
  filter(manu == 1) %>% 
  filter(tal == 0) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

textiles_prov_06 <- employment_mf_06 %>% 
  filter(industry == 17) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

tal_prov_06 <- employment_mf_06 %>% 
  filter(tal == 1) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

con_prov_06 <- employment_mf_06 %>% 
  filter(industry == 45) %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

prov_workers_06 <- employment_mf_06 %>% 
  group_by(tinh) %>% 
  count(tinh, wt = hhwt) %>% 
  rename(nworkers = n)

fd_ams_06 <- c("agri_prov_06", "manu_prov_06", "manu_exc_al_prov_06", "textiles_prov_06", "tal_prov_06", "con_prov_06")

for(i in fd_ams_06){
  
  assign(i, left_join(get(i), prov_workers_06, by = "tinh"))
  
  assign(i, get(i) %>% 
           mutate(n_workers = n/nworkers*100)) # Share of province in sector 
  
  assign(i, get(i) %>% 
           select(tinh, n_workers))
  
  if(i %in% c("agri_prov_06")){
    assign(i, get(i) %>%
             rename(n_agri_06 = n_workers))    
  }  
  
  if(i %in% c("manu_prov_06")){
    assign(i, get(i) %>%
             rename(n_manu_06 = n_workers))    
  }   
  
  if(i %in% c("manu_exc_al_prov_06")){
    assign(i, get(i) %>%
             rename(n_manu_exc_al_06 = n_workers))    
  }   
  
  if(i %in% c("textiles_prov_06")){
    assign(i, get(i) %>%
             rename(n_textiles_06 = n_workers))    
  } 
  
  if(i %in% c("tal_prov_06")){
    assign(i, get(i) %>%
             rename(n_tal_06 = n_workers))    
  }   
  
  if(i %in% c("con_prov_06")){
    assign(i, get(i) %>%
             rename(n_con_06 = n_workers))    
  }  
}

prov_ams_06 <- list(agri_prov_06, manu_prov_06, manu_exc_al_prov_06, textiles_prov_06, tal_prov_06, con_prov_06) %>% 
  reduce(full_join, by = "tinh")

# Merging 2002 and 2004 data 
prov_ams_060402 <- list(prov_ams_02, prov_ams_04, prov_ams_06) %>% 
  reduce(full_join, by = "tinh") %>% 
  mutate(
    agri_change_0402 = n_agri_04 - n_agri, 
    manu_change_0402 = n_manu_04, n_manu,
    tal_change_0402 = n_tal_04 - n_tal,
    agri_change_0602 = n_agri_06 - n_agri,
    manu_change_0602 = n_manu_06, n_manu,
    textiles_change_0602 = n_textiles_06 - n_textiles,
    tal_change_0602 = n_tal_06 - n_tal,
    con_change_0602 = n_con_06 - n_con,
    manu_exc_al_change_0602 = n_manu_exc_al_06 - n_manu_exc_al,
  )

prov_ams_060402 <- prov_ams_060402 %>% 
  mutate(Tinh = recode(tinh,
                       "101" = "Hanoi",
                       "103" = "Hai Phong",
                       "104" = "Vinh Phuc",
                       "105" = "Ha Tay",
                       "106" = "Bac Ninh",
                       "107" = "Hai Duong",
                       "109" = "Hung Yen",
                       "111" = "Ha Nam",
                       "113" = "Nam Dinh",
                       "115" = "Thai Binh",
                       "117" = "Ninh Binh",
                       "201" = "Ha Giang",
                       "203" = "Cao Bang",
                       "205" = "Lao Cai",
                       "207" = "Bac Can",
                       "209" = "Lang Son",
                       "211" = "Tuyen Quyen",
                       "213" = "Yen Bai",
                       "215" = "Thai Nguyen",
                       "217" = "Phu Tho",
                       "221" = "Bac Giang",
                       "225" = "Quang Ninh",
                       "301" = "Lai Chau",
                       "303" = "Son La",
                       "305" = "Hoa Binh",
                       "401" = "Thanh Hoa",
                       "403" = "Nghe An",
                       "405" = "Ha Tinh",
                       "407" = "Quang Binh",
                       "409" = "Quang Tri",
                       "411" = "Hue",
                       "501" = "Da Nang",
                       "503" = "Quang Nam",
                       "505" = "Quang Ngai",
                       "507" = "Binh Dinh",
                       "509" = "Phu Yen",
                       "511" = "Khanh Hoa",
                       "601" = "Kon Tum",
                       "603" = "Gia Lai",
                       "605" = "Dac Lac",
                       "607" = "Lam Dong",
                       "701" = "Ho Chi Minh",
                       "705" = "Ninh Thuan",
                       "707" = "Binh Phuoc",
                       "709" = "Tay Ninh",
                       "711" = "Binh Duong",
                       "713" = "Dong Nai",
                       "715" = "Binh Thuan",
                       "717" = "Ba Ria - Vung Tau",
                       "801" = "Long An",
                       "803" = "Dong Thap",
                       "805" = "An Giang",
                       "807" = "Tien Giang",
                       "809" = "Vinh Long",
                       "811" = "Ben Tre",
                       "813" = "Kien Giang",
                       "815" = "Can Tho",
                       "817" = "Tra Vinh",
                       "819" = "Soc Trang",
                       "821" = "Bac Lieu",
                       "823" = "Ca Mau"))

#########################################################
# PROVINCE-LEVEL FEMALE AND MALE EMPLOYMENT COMPOSITION #
#########################################################

# 2002 
f_prov_02 <- employment_mf_02 %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt) %>% 
  rename(Female_prov = n)

fagri_prov_02 <- employment_mf_02 %>%
  filter(agri_work == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fmanu_prov_02 <- employment_mf_02 %>% 
  filter(manu == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fmanu_exc_al_prov_02 <- employment_mf_02 %>%
  filter(manu == 1) %>%
  filter(tal == 0) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fservice_prov_02 <- employment_mf_02 %>% 
  filter(service == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

ftal_prov_02 <- employment_mf_02 %>% 
  filter(tal == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

f_ams_02 <- c("fagri_prov_02", "fmanu_prov_02", "fmanu_exc_al_prov_02", "fservice_prov_02", "ftal_prov_02")

for (i in f_ams_02){
  assign(i, left_join(get(i), f_prov_02, by = "tinh"))
  
  assign(i, get(i) %>% 
           mutate(Fsec_02 = n / Female_prov) %>% 
           select(-c("Female_prov", "n")))
  
  if(i %in% c("fagri_prov_02")){
    assign(i, get(i) %>%
             rename(F_Agri_02 = Fsec_02))    
  }
  
  if(i %in% c("fmanu_prov_02")){
    assign(i, get(i) %>%
             rename(F_Manu_02 = Fsec_02))    
  }
  
  if(i %in% c("fmanu_exc_al_prov_02")){
    assign(i, get(i) %>%
             rename(F_Manu_exc_al_02 = Fsec_02))    
  }  
  
  if(i %in% c("fservice_prov_02")){
    assign(i, get(i) %>%
             rename(F_Serv_02 = Fsec_02))
  } 
  
  if(i %in% c("ftal_prov_02")){
    assign(i, get(i) %>%
             rename(F_Tal_02 = Fsec_02))
  }  
  
}

fagri_prov_02$tinh <- as.factor(fagri_prov_02$tinh)
fmanu_prov_02$tinh <- as.factor(fmanu_prov_02$tinh)
fmanu_exc_al_prov_02$tinh <- as.factor(fmanu_exc_al_prov_02$tinh)
fservice_prov_02$tinh <- as.factor(fservice_prov_02$tinh)
ftal_prov_02$tinh <- as.factor(ftal_prov_02$tinh)

f_ams_02 <- list(fagri_prov_02, fmanu_prov_02, fmanu_exc_al_prov_02, fservice_prov_02, ftal_prov_02, preBTA_provtariff) %>% 
  reduce(full_join, by = "tinh")

# 2004 
f_prov_04 <- employment_mf_04 %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt) %>% 
  rename(Female_prov = n)

fagri_prov_04 <- employment_mf_04 %>%
  filter(agri_work == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fmanu_prov_04 <- employment_mf_04 %>%
  filter(manu == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fservice_prov_04 <- employment_mf_04 %>%
  filter(service == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

ftal_prov_04 <- employment_mf_04 %>%
  filter(tal == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

f_ams_04 <- c("fagri_prov_04", "fmanu_prov_04", "fservice_prov_04", "ftal_prov_04")

for (i in f_ams_04){
  assign(i, left_join(get(i), f_prov_04, by = "tinh"))
  
  assign(i, get(i) %>% 
           mutate(Fsec_04 = n / Female_prov) %>% 
           select(-c("Female_prov", "n")))
  
  if(i %in% c("agri_prov_04")){
    assign(i, get(i) %>%
             rename(F_Agri_04 = Fsec_04))    
  }
  
  if(i %in% c("manu_prov_04")){
    assign(i, get(i) %>%
             rename(F_Manu_04 = Fsec_04))    
  }
  
  if(i %in% c("service_prov_04")){
    assign(i, get(i) %>%
             rename(F_Serv_04 = Fsec_04))
  }   
  
  if(i %in% c("tal_prov_04")){
    assign(i, get(i) %>%
             rename(F_Tal_04 = Fsec_04))
  } 
}

f_ams_04 <- list(agri_prov_04, manu_prov_04, service_prov_04, tal_prov_04, postBTA_provtariff) %>% 
  reduce(full_join, by = "tinh")

# 2006 

f_prov_06 <- employment_mf_06 %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt) %>% 
  rename(Female_prov = n)

fagri_prov_06 <- employment_mf_06 %>%
  filter(agri_work == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fmanu_prov_06 <- employment_mf_06 %>%
  filter(manu == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fmanu_exc_al_prov_06 <- employment_mf_06 %>%
  filter(manu == 1) %>%
  filter(tal == 0) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

fservice_prov_06 <- employment_mf_06 %>%
  filter(service == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

ftal_prov_06 <- employment_mf_06 %>%
  filter(tal == 1) %>% 
  filter(sex == "Female") %>% 
  select(tinh, hhwt) %>%   
  group_by(tinh) %>% 
  count(tinh, wt = hhwt)

f_ams_06 <- c("fagri_prov_06", "fmanu_prov_06", "fmanu_exc_al_prov_06", "fservice_prov_06", "ftal_prov_06")

for (i in f_ams_06){
  
  assign(i, left_join(get(i), f_prov_06,  by = "tinh"))
  
  assign(i, get(i) %>% 
           mutate(Fsec_06 = n / Female_prov) %>% 
           select(-c("Female_prov", "n")))
  
  if(i %in% c("fagri_prov_06")){
    assign(i, get(i) %>%
             rename(F_Agri_06 = Fsec_06))    
  }
  
  if(i %in% c("fmanu_prov_06")){
    assign(i, get(i) %>%
             rename(F_Manu_06 = Fsec_06))    
  }
  
  if(i %in% c("fmanu_exc_al_prov_06")){
    assign(i, get(i) %>%
             rename(F_Manu_exc_al_06 = Fsec_06))    
  }  
  
  if(i %in% c("fservice_prov_06")){
    assign(i, get(i) %>%
             rename(F_Serv_06 = Fsec_06))
  }   
  
  if(i %in% c("ftal_prov_06")){
    assign(i, get(i) %>%
             rename(F_Tal_06 = Fsec_06))
  } 
}

f_ams_06<- list(fagri_prov_06, fmanu_prov_06, fservice_prov_06, ftal_prov_06) %>% 
  reduce(full_join, by = "tinh")

# Merging 2002, 2004 and 2006 data 
f_ams_02$tinh <- as.factor(f_ams_02$tinh)
f_ams_04$tinh <- as.factor(f_ams_04$tinh)
f_ams_06$tinh <- as.factor(f_ams_06$tinh)

f_ams_060402 <- list(f_ams_02, f_ams_04, f_ams_06) %>% 
  reduce(full_join, by = "tinh") %>% 
  mutate(
    fd_f_agri_0602 = F_Agri_06 - F_Agri_02,
    fd_f_manu_0602 = F_Manu_06 - F_Manu_02,
    fd_f_service_0602 = F_Serv_06 - F_Serv_02,
    fd_f_tal_0602 = F_Tal_06 - F_Tal_02,
    fd_tariff = postprov_tariff - preprov_tariff,
  )

f_ams_060402 <- f_ams_060402 %>% 
  mutate(Tinh = recode(tinh,
                       "101" = "Hanoi",
                       "103" = "Hai Phong",
                       "104" = "Vinh Phuc",
                       "105" = "Ha Tay",
                       "106" = "Bac Ninh",
                       "107" = "Hai Duong",
                       "109" = "Hung Yen",
                       "111" = "Ha Nam",
                       "113" = "Nam Dinh",
                       "115" = "Thai Binh",
                       "117" = "Ninh Binh",
                       "201" = "Ha Giang",
                       "203" = "Cao Bang",
                       "205" = "Lao Cai",
                       "207" = "Bac Can",
                       "209" = "Lang Son",
                       "211" = "Tuyen Quyen",
                       "213" = "Yen Bai",
                       "215" = "Thai Nguyen",
                       "217" = "Phu Tho",
                       "221" = "Bac Giang",
                       "225" = "Quang Ninh",
                       "301" = "Lai Chau",
                       "303" = "Son La",
                       "305" = "Hoa Binh",
                       "401" = "Thanh Hoa",
                       "403" = "Nghe An",
                       "405" = "Ha Tinh",
                       "407" = "Quang Binh",
                       "409" = "Quang Tri",
                       "411" = "Hue",
                       "501" = "Da Nang",
                       "503" = "Quang Nam",
                       "505" = "Quang Ngai",
                       "507" = "Binh Dinh",
                       "509" = "Phu Yen",
                       "511" = "Khanh Hoa",
                       "601" = "Kon Tum",
                       "603" = "Gia Lai",
                       "605" = "Dac Lac",
                       "607" = "Lam Dong",
                       "701" = "Ho Chi Minh",
                       "705" = "Ninh Thuan",
                       "707" = "Binh Phuoc",
                       "709" = "Tay Ninh",
                       "711" = "Binh Duong",
                       "713" = "Dong Nai",
                       "715" = "Binh Thuan",
                       "717" = "Ba Ria - Vung Tau",
                       "801" = "Long An",
                       "803" = "Dong Thap",
                       "805" = "An Giang",
                       "807" = "Tien Giang",
                       "809" = "Vinh Long",
                       "811" = "Ben Tre",
                       "813" = "Kien Giang",
                       "815" = "Can Tho",
                       "817" = "Tra Vinh",
                       "819" = "Soc Trang",
                       "821" = "Bac Lieu",
                       "823" = "Ca Mau"))
