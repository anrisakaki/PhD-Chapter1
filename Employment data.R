########################################################
# SETTING UP DATA FRAME FOR EMPLOYMENT AND INCOME DATA #
########################################################

# 2002 
m3_02$m3c7[is.na(m3_02$m3c7)] <- 0

educ_02 <- m2_02 %>% select(tinh, xa, hoso, matv, tinh02, huyen02, xa02, diaban02, hoso02, matv02, m2c1)

inc02 <- m5a_02 %>% 
  mutate(inc = m5ac6 + m5ac7e) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, inc)

employment_mf_02 <- list(m1_02, m3_02, educ_02) %>% 
  reduce(full_join, by = c("tinh", "xa", "hoso", "matv", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02")) %>% 
  mutate(matv02 = ifelse(nchar(matv02) > 2, substr(matv02, nchar(matv02) - 1, nchar(matv02)), matv02),
         across(matv02, as.numeric),
         year = 2002 ) %>%   
  select(-c(tinh, xa, hoso, matv)) %>%
  select(-matches("\\.x|\\.y")) %>% 
  mutate(work = ifelse(m3c2 == 1, 1, 0),
         married = ifelse(m1c6 == 2, 1, 0),
         housework = ifelse(m3c17 == 1, 1, 0)) %>% 
  rename("educ" = m2c1,
         "industry" = m3c7,
         age = m1c5,
         hours = m3c11,
         sex = m1c2,
         days = m3c10) %>%
  filter(age >= 16) %>%
  filter(age < 65) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, sex, educ, work, industry, age, married, days, hours, year, housework) 

employment_mf_02 <- left_join(employment_mf_02, inc02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02")) %>% distinct()

employment_mf_02 <- merge(employment_mf_02, weights_02, by = c("tinh02", "xa02", "huyen02", "diaban02")) # N = 185,749

## Recoding binary variable for sex
employment_mf_02$sex <- factor(employment_mf_02$sex,
                               c(1,2),
                               c("Male", "Female"))

employment_mf_02 <- employment_mf_02 %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  mutate(female = ifelse(sex == "Female", 1, 0))

# 2004 
m4a_04$m4ac5[is.na(m4a_04$m4ac5)] <- 0

employment_mf_04 <- left_join(m123a_04, m4a_04, by = c("tinh", "huyen", "xa", "hoso", "matv")) %>%
  distinct() %>% 
  rename(age = m1ac5,
         industry = m4ac5,
         days = m4ac7, 
         hours = m4ac8,
         sex = m1ac2,
         educ = m2c1) %>% 
  mutate(work = ifelse(m4ac2 == 1, 1, 0),
         m4ac11 = ifelse(is.na(m4ac11), 0 , m4ac11),
         m4ac12e = ifelse(is.na(m4ac12e), 0 , m4ac12e),
         inc = m4ac11 + m4ac12e,
         housework = ifelse(m4ac26 == 1, 1, 0),
         married = ifelse(m1ac6 == 1, 1, 0),
         year = 2004,
         female = ifelse(sex == 2, 1, 0)) %>% 
  filter(age >= 16) %>%
  filter(age < 65) %>% 
  select(tinh, huyen, xa, hoso, matv, sex, educ, work, industry, age, married, days, hours,inc, year, housework, female) #Merging data on age, sex, marital status, and industry worked in (N = 202,321)

employment_mf_04 <- list(employment_mf_04, diaban04, weights_04) %>% 
  reduce(merge, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

#Recoding binary variable for sex
employment_mf_04$sex <- factor(employment_mf_04$sex,
                               c(1,2),
                               c("Male", "Female")) 

# 2006 
m4a_06$m4ac5[is.na(m4a_06$m4ac5)] <- 0

m1a_06 <- m1a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac5, m1ac6)

educ_06 <- m2a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m2ac1)  

employment_mf_06 <- list(m1a_06, m4a_06, educ_06) %>% 
  reduce(full_join, by = c("tinh", "huyen", "xa", "hoso", "matv"))

employment_mf_06 <- merge(employment_mf_06, weights_06, by = c("tinh", "huyen", "xa")) %>% 
  filter(m1ac5 >= 16,
         m1ac5 < 65) %>% # N = 127,757
  rename(age = m1ac5,
         industry = m4ac5,
         days = m4ac7, 
         hours = m4ac8,
         sex = m1ac2,
         educ = m2ac1) %>% 
  mutate(work = ifelse(m4ac2 == 1, 1, 0),
         m4ac11 = ifelse(is.na(m4ac11), 0 , m4ac11),
         m4ac12f = ifelse(is.na(m4ac12f), 0 , m4ac12f),
         inc = m4ac11 + m4ac12f,
         housework = ifelse(m4ac26 == 1, 1, 0),
         married = ifelse(m1ac6 == 1, 1, 0),
         year = 2006,
         female = ifelse(sex == 2, 1, 0)) %>% 
  select(tinh, huyen, xa, hoso, matv, sex, educ, work, industry, age, married, days, hours, inc, year, housework, female) 

employment_mf_06 <- left_join(employment_mf_06, diaban06, by =c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

employment_mf_06 <- left_join(employment_mf_06, weights_06, by = c("tinh", "huyen", "xa"))

## Recoding binary variable for sex
employment_mf_06$sex <- factor(employment_mf_06$sex,
                               c(1,2),
                               c("Male", "Female")) 

#########################################################
# CREATING EMPLOMYMENT DUMMIES AND LABELLING INDUSTRIES #
#########################################################

emp020406 <- c("employment_mf_02", "employment_mf_06", "employment_mf_04")

for(i in emp020406) {
  assign(i,get(i) %>%
           mutate(
             tal = as.numeric(industry %in% c(18)),
             agri_work = as.numeric(industry %in% c(1,2,4,5)),
             service = as.numeric(industry > 50),
             manu = as.numeric(industry > 14 & industry < 38),
             construction = as.numeric(industry == 45),
             hours = ifelse(work == 1 & hours == 0, NA, hours),
             days = ifelse(work == 1 & days == 0, NA, hours)))
  
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
