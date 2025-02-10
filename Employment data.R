########################################################
# SETTING UP DATA FRAME FOR EMPLOYMENT AND INCOME DATA #
########################################################

# 2002 
educ_02 <- m2_02 %>% select(tinh, xa, hoso, matv, tinh02, huyen02, xa02, diaban02, hoso02, matv02, m2c1)

inc02 <- m5a_02 %>% 
  mutate(inc = m5ac6) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, inc) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, matv02) %>% 
  summarise(inc = sum(inc, na.rm = TRUE), .groups = "drop") %>% 
  mutate(inc = ifelse(inc == 0, NA, inc))

vhlss02 <- list(m1_02, m3_02, educ_02) %>% 
  reduce(full_join, by = c("tinh", "xa", "hoso", "matv", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02")) %>% 
  mutate(matv02 = ifelse(nchar(matv02) > 2, substr(matv02, nchar(matv02) - 1, nchar(matv02)), matv02),
         across(matv02, as.numeric),
         year = 2002 ) %>%   
  select(-c(tinh, xa, hoso, matv)) %>%
  select(-matches("\\.x|\\.y")) %>% 
  mutate(wage_work = ifelse(m3c1a == 1, 1, 0),
         work = ifelse(m3c2 == 1, 1, 0),
         married = ifelse(m1c6 == 2, 1, 0),
         housework = ifelse(m3c17 == 1, 1, 0),
         divorce = ifelse(m1c6 == 4, 1, 0),
         agri = ifelse(m3c7 < 6, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         female = ifelse(m1c2 == 2, 1, 0)) %>% 
  rename(educ = m2c1,
         industry = m3c7,
         age = m1c5,
         hours = m3c11,
         sex = m1c2,
         days = m3c10) %>%
  filter(age > 15 & age < 65) %>%
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, female, divorce, educ, work, wage_work, agri, industry, age, married, days, hours, year, housework) %>% 
  left_join(inc02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02")) %>%
  merge(weights_02, by = c("tinh02", "xa02", "huyen02", "diaban02")) %>% 
  rename(tinh = tinh02, 
         huyen = huyen02,
         xa = xa02,
         diaban = diaban02,
         hoso = hoso02,
         matv = matv02)

# 2004 
vhlss04 <- left_join(m123a_04, m4a_04, by = c("tinh", "huyen", "xa", "hoso", "matv")) %>%
  distinct() %>% 
  rename(age = m1ac5,
         industry = m4ac5,
         days = m4ac7, 
         hours = m4ac8,
         sex = m1ac2,
         educ = m2c1) %>% 
  mutate(work = ifelse(m4ac2 == 1, 1, 0),
         wage_work = ifelse(m4ac1a == 1, 1, 0),
         inc = ifelse(m4ac11 == 0, NA, m4ac11),
         housework = ifelse(m4ac26 == 1, 1, 0),
         married = ifelse(m1ac6 == 1, 1, 0),
         year = 2004,
         female = ifelse(sex == 2, 1, 0),
         agri = ifelse(industry < 6, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         divorce = ifelse(m4ac6 == 4, 1, 0)) %>% 
  filter(age > 15 & age < 65) %>%
  select(tinh, huyen, xa, hoso, matv, divorce, female, educ, wage_work, work, agri, industry, age, married, days, hours,inc, year, housework, female)

vhlss04 <- list(vhlss04, diaban04, weights_04) %>% 
  reduce(merge, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

# 2006 
m1a_06 <- m1a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m1ac2, m1ac3, m1ac5, m1ac6)

educ_06 <- m2a_06 %>% 
  select(tinh, huyen, xa, hoso, matv, m2ac1)  

vhlss06 <- list(m1a_06, m4a_06, educ_06) %>% 
  reduce(full_join, by = c("tinh", "huyen", "xa", "hoso", "matv")) %>% 
  merge(weights_06, by = c("tinh", "huyen", "xa")) %>% 
  rename(age = m1ac5,
         industry = m4ac5,
         days = m4ac7, 
         hours = m4ac8,
         educ = m2ac1) %>% 
  mutate(work = ifelse(m4ac2 == 1, 1, 0),
         inc = ifelse(m4ac11 == 0, NA, m4ac11),
         housework = ifelse(m4ac26 == 1, 1, 0),
         married = ifelse(m1ac6 == 1, 1, 0),
         year = 2006,
         female = ifelse(m1ac2 == 2, 1, 0),
         agri = ifelse(industry < 6, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         divorce = ifelse(m1ac6 == 4, 1, 0)) %>% 
  filter(age > 15 & age < 65) %>%
  select(tinh, huyen, xa, hoso, matv, divorce, female, educ, work, agri, industry, age, married, days, hours, inc, year, housework) %>% 
  left_join(diaban06, by =c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  left_join(weights_06, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban06)

#########################################################
# CREATING EMPLOMYMENT DUMMIES AND LABELLING INDUSTRIES #
#########################################################

emp020406 <- c("vhlss02", "vhlss04", "vhlss06")

for(i in emp020406) {
  assign(i,get(i) %>%
           mutate(
             apparel = ifelse(industry == 18, 1, 0),
             service = ifelse(industry > 50 & industry < 60, 1, 0),
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

