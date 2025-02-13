vhlss_sum_fn <- function(i){
  i %>% 
    filter(age > 15 & age < 65 & !is.na(hhwt)) %>% 
    summarise(
      n = sum(hhwt[work == 1], na.rm = T),
      agri = sum(hhwt[agri == 1], na.rm = T),
      manu = sum(hhwt[manu == 1], na.rm = T),
      service = sum(hhwt[service == 1], na.rm = T),
      inc = weighted.mean(rlinc, hhwt, na.rm = T),
      tariff = mean(tariff, na.rm = T),
      age = weighted.mean(age, hhwt, na.rm = T),
      
    ) %>% 
    mutate(
      agri_share = agri/n,
      manu_share = manu/n,
      service_share = service/n
    )
}

vhlss_sum <- vhlss %>%
  group_by(year) %>% 
  vhlss_sum_fn()

vhlss_prov_sum <- vhlss %>%
  group_by(year, tinh_old) %>% 
  vhlss_sum_fn()
  
vhlss_f_sum <- vhlss %>%
  group_by(year, female) %>% 
  vhlss_sum_fn()