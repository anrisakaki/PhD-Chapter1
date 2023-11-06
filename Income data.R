inc02 <- inc_02 %>% 
  mutate(hhinc = rlincomepc * 12 * hhsize) %>%   
  select(tinh02, huyen02, xa02, diaban02, hoso02, hhinc) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything()) %>% 
  mutate(year = 2002)

inc04 <- inc_04 %>% 
  mutate(hhinc = rlincomepc * 12 * hhsize) %>%   
  select(tinh, huyen, xa, hoso, hhinc) %>% 
  mutate(year = 2004)

inc04 <- merge(inc04, diaban04, by = c("tinh", "huyen", "xa")) %>% 
  rename(diaban = diaban04)

inc06 <- inc_06 %>% 
  mutate(hhinc = rlincomepc * 12 * hhsize) %>%   
  select(tinh, huyen, xa, hoso, hhinc) %>% 
  mutate(year = 2006)

inc06 <- inner_join(inc06, diaban06, by = c("tinh" = "tinh06", "huyen" = "huyen06", "xa" = "xa06")) %>% 
  rename(diaban = diaban06)

hhinc0204 <- bind_rows(inc02, inc04)
hhinc0206 <- bind_rows(inc02, inc06)

hhinc0204_p <- merge(hhinc0204, panel0204, by = hhid)
hhinc0204_p <- merge(hhinc0204_p, provtariffs0204, by = c("tinh", "year"))
hhinc0204_p <- inner_join(hhinc0204_p, weights_02, by = c("tinh" = "tinh02", "huyen" = "huyen02", "xa" = "xa02"))

hhinc0206_p <- merge(hhinc0206, panel0206, by = hhid)
hhinc0206_p <- merge(hhinc0206_p, provtariffs0206, by = c("tinh", "year"))
hhinc0206_p <- merge(hhinc0206_p, weights_06, by = c("tinh", "huyen", "xa"))

emp0204_p <- merge(emp0204_p, hhinc0204, by = hhid) %>%
  mutate(inc_share = inc/hhinc, 
         inc_share = ifelse(inc_share >= 1 | inc_share < 0, NA, inc_share),
         inc_share = ifelse(is.na(inc_share), 0, inc_share)) %>% 
  group_by(hhid, year) %>%
  mutate(total_hhinc_ratio = sum(inc_share)) %>% 
  # If both partners work on field or have own business total_hhinc_ratio will be equals to 0
  mutate(inc_share = ifelse(total_hhinc_ratio == 0 & inc_share == 0, 0.5, inc_share)) %>% 
  select(-total_hhinc_ratio) %>% 
  ungroup()  

emp0206_p <- merge(emp0206_p, hhinc0206, by = hhid) %>%
  mutate(inc_share = inc/hhinc, 
         inc_share = ifelse(inc_share >= 1 | inc_share < 0, NA, inc_share),
         inc_share = ifelse(is.na(inc_share), 0, inc_share))
  
