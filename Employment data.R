###############################################
# SETTING UP DATA FRAME FOR EMPLOYMENT TRENDS #
###############################################

# 2002 
m3_02$m3c7[is.na(m3_02$m3c7)] <- 0

educ_02 <- m2_02 %>% select(tinh, xa, hoso, matv, tinh02, xa02, hoso02, matv02, hhid, ivid, m2c1)

employment_mf_02 <- list(m1_02, m3_02, educ_02) %>% 
  reduce(full_join, by = c("tinh", "xa", "hoso", "matv", "tinh02", "xa02", "hoso02", "matv02", "hhid", "ivid")) %>% 
  rename("hhid02" = hhid,
         "ivid02" = ivid,
         "educ" = m2c1,
         "wage_work" = m3c1a,
         "industry" = m3c7,
         "huyen02" = huyen02.x,
         "diaban02" = diaban02.x) %>%
  filter(m1c5 >= 18) %>%
  filter(m1c5 < 65) %>% 
  select(-c(huyen02.y, diaban02.y)) # N = 185,749

employment_mf_02 <- merge(employment_mf_02, weights_02, by = c("xa02", "diaban02", "huyen02")) # N = 185,749

## Recoding binary variable for sex
employment_mf_02$sex <- factor(employment_mf_02$m1c2,
                               c(1,2),
                               c("Male", "Female"))

## 2002 - 2004 panel 
employment_mf_02p <- employment_mf_02 %>% 
  select(-c(tinh, xa, hoso, matv)) %>% 
  select(-ends_with(".x")) %>% 
  select(-ends_with(".y"))

employment_mf_02p <- merge(ivid0204, employment_mf_02p, by = c("xa02", "hhid02", "ivid02")) %>% 
  distinct() %>% 
  select(-ends_with(".y")) %>% 
  rename(hoso02 = hoso02.x,
         matv02 = matv02.x) # N = 39,988

emp_ivid04 <- employment_mf_02p %>% select("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid")

# 2004 
m4a_04$m4ac5[is.na(m4a_04$m4ac5)] <- 0

employment_mf_04a <- left_join(m123a_04, m4a_04, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv", "hhid", "ivid")) %>%
  distinct() %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhid, ivid, m1ac2, m1ac5, m1ac6, m2c1, m4ac1a, m4ac1c, m4ac2, m4ac5) %>% 
  rename(
    "industry" = m4ac5,
    "wage_work" = m4ac1a
  ) #Merging data on age, sex, marital status, and industry worked in (N = 202,321)

employment_mf_04a <- merge(employment_mf_04a, weights_04, by = c("tinh", "huyen", "xa")) %>% 
  distinct()

employment_mf_04 <- employment_mf_04a %>%
  filter(m1ac5 >= 18) %>%
  filter(m1ac5 < 65) # N = 117,114

#Recoding binary variable for sex
employment_mf_04$sex <- factor(employment_mf_04$m1ac2,
                               c(1,2),
                               c("Male", "Female")) 

employment_mf_04p <- left_join(emp_ivid04, employment_mf_04, by = c("tinh", "huyen", "xa", "hoso", "matv", "hhid", "ivid")) %>% 
  distinct() #N = 40,014

# 2006 
m4a_06$m4ac5[is.na(m4a_06$m4ac5)] <- 0

employment_mf_06 <- merge(m1a_06, m4a_06, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv", "hhid", "ivid"))
employment_mf_06 <- merge(employment_mf_06, weights_06, by = c("tinh", "huyen", "xa"))

employment_mf_06 <- employment_mf_06 %>%
  filter(m1ac5 >= 18,
         m1ac5 < 65) %>% # N = 117,552 
  rename("industry" = m4ac5,
         "wage_work" = m4ac1a) 

## Recoding binary variable for sex
employment_mf_06$sex <- factor(employment_mf_06$m1ac2,
                               c(1,2),
                               c("Male", "Female")) 

employment_mf_06p <- employment_mf_06 %>% 
  rename(ivid06 = ivid)

employment_mf_06p <- left_join(ivid020406, employment_mf_06p, by = "ivid06") %>% 
  distinct() #N = 31,981

#########################################################
# CREATING EMPLOMYMENT DUMMIES AND LABELLING INDUSTRIES #
#########################################################

emp020406 <- c("employment_mf_02", "employment_mf_04", "employment_mf_06", "employment_mf_02p", "employment_mf_04p", "employment_mf_06p")

for(i in emp020406) {
  assign(i,get(i) %>%
           mutate(
             tal = as.numeric(industry %in% c(18, 19)),
             agri_work = as.numeric(industry %in% c(1,2,4,5)),
             service = as.numeric(industry > 50),
             manu = as.numeric(industry > 14 & industry < 38),
             wage_work = as.numeric(wage_work < 2),
             construction = 45
           ))
  
  assign(i,get(i) %>%
           mutate(Industry = recode(industry,
                                    "0" = "Unemployed",
                                    "1" = "Agriculture, hunting and related service activities",
                                    "2" = "Forestry, logging and related service activities",
                                    "5" = "Fishing, operation of fish hatcheries and fish farms; service activities incidental to fishing",
                                    "10" = "Mining of coal and lignite" ,
                                    "11" = "Extraction of crude petroleum and natural gas; service activities incidental to oil and gas extraction excluding surveying",
                                    "12" = "Mining of uranium and thorium ores",
                                    "13" =  "Mining of metal ores",
                                    "14" = "Other mining and quarrying",
                                    "15" = "Manufacture of food products and beverages",
                                    "16" = "Manufacture of tobacco products",
                                    "17" = "Manufacture of textiles",
                                    "18" = "Manufacture of wearing apparel",
                                    "19" = "Manufacture of leather goods (including footwear)",
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
                                    "36" = "Manufacture of furniture; manufacturing n.e.c.",
                                    "37" = "Recycling",
                                    "40" = "Electricity, gas, steam and hot water supply",
                                    "41" = "Collection, purification and distribution of water",
                                    "45" = "Construction",
                                    "50" = "Sale, maintenance and repair of motor vehicles and motorcycles; retail sale of automotive fuel",
                                    "51" = "Wholesale trade and commission trade, except of motor vehicles and motorcycles",
                                    "52" ="Retail trade, except of motor vehicles and motorcycles; repair of personal and household goods",
                                    "55" = "Hotels and restaurants",
                                    "60" = "Land transport; transport via pipelines",
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
                                    "99" = "Extra-territorial organizations and bodies")))
}

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

industry_lf_020406 <- left_join(industry_lf_020406, tariff, by = "isic2") %>% 
  replace(is.na(.), 0)

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