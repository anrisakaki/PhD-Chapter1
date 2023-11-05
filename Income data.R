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

emp0204_p <- merge(emp0204_p, hhinc0204, by = hhid) %>%
  mutate(inc_share = inc / hhinc, 
         inc_share = ifelse(inc_share >= 1 | inc_share < 0, NA, inc_share))

emp0206_p <- merge(emp0206_p, hhinc0206, by = hhid) %>%
  mutate(inc_share = inc / hhinc, 
         inc_share = ifelse(inc_share >= 1 | inc_share < 0, NA, inc_share))
