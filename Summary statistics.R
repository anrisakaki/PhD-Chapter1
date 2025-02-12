vhlss_sum_fn <- function(i){
  i %>% 
    summarise(
      n = n(),
      agri = weighted.mean(agri, hhwt, na.rm = T),
      manu = weighted.mean(manu, hhwt, na.rm = T),
      service = weighted.mean(service, hhwt, na.rm = T),
      inc = weighted.mean(inc, hhwt, na.rm = T)
    )
}

vhlss_sum <- vhlss %>%
  group_by(year, tinh) %>% 
  vhlss_sum_fn()
  
vhlss_f_sum <- vhlss %>%
  group_by(year, female) %>% 
  summarise(
    n = n(),
    agri = weighted.mean(agri, hhwt, na.rm = T),
    manu = weighted.mean(manu, hhwt, na.rm = T),
    inc = weighted.mean(inc, hhwt, na.rm = T)
  )
