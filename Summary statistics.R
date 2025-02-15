weighted.sd <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    valid <- !is.na(x) & !is.na(w)
    x <- x[valid]
    w <- w[valid]
  }
  mean_x <- sum(x * w) / sum(w)
  variance <- sum(w * (x - mean_x)^2) / sum(w)
  sqrt(variance)
}

employment_sum_fn <- function(i){
  i %>% 
    filter(age > 15 & age < 65 & !is.na(hhwt)) %>% 
    summarise(
      n = sum(hhwt[work == 1], na.rm = TRUE),
      agri = sum(hhwt * (agri == 1), na.rm = TRUE),
      manu = sum(hhwt * (manu == 1), na.rm = TRUE), 
      service = sum(hhwt * (service == 1), na.rm = TRUE),
      hhbus = sum(hhwt * (hhbus == 1), na.rm = TRUE),
      formal_agri = sum(hhwt * (formal_agri == 1), na.rm = TRUE),
      formal_manu = sum(hhwt * (formal_manu == 1), na.rm = TRUE),
      formal_service = sum(hhwt * (formal_service == 1), na.rm = TRUE),
      inc = weighted.mean(rlinc, hhwt, na.rm = TRUE),
      tariff = mean(tariff, na.rm = TRUE)
    ) %>% 
    mutate(
      agri_share = agri/n,
      manu_share = manu/n,
      service_share = service/n,
      hhbus_share = hhbus/n,
      formal_agri_share = formal_agri/agri,  
      formal_manu_share = formal_manu/manu,
      formal_service_share = formal_service/service
    ) %>% 
    mutate(across(where(is.numeric), round, 3))
}

vhlss_sum <- vhlss %>%
  group_by(year) %>% 
  employment_sum_fn() %>% 
  select(year, agri_share, manu_share, service_share, hhbus_share, formal_agri_share, formal_manu_share, formal_service_share)

vhlss_sum <- vhlss %>%
  group_by(year) %>% 
  employment_sum_fn() %>% 
  select(year, agri_share, manu_share, service_share, hhbus_share, formal_agri_share, formal_manu_share, formal_service_share)
  
vhlss_f_sum <- emp0204_p %>%
  group_by(year, female) %>% 
  summarise(
    n = sum(hhwt),
    n_vhlss = n(),
    age = weighted.mean(age, hhwt, na.rm = TRUE),
    educ = weighted.mean(educ, hhwt, na.rm = TRUE),
    work = sum(hhwt[work == 1], na.rm = TRUE),
    agri = sum(hhwt *(agri == 1), na.rm = TRUE),
    manu = sum(hhwt *(manu == 1), na.rm = TRUE), 
    service = sum(hhwt*(service == 1), na.rm = TRUE),
    hhbus = sum(hhwt*(hhbus == 1), na.rm = TRUE),
    formal_agri = sum(hhwt*(formal_agri == 1), na.rm = TRUE),
    formal_manu = sum(hhwt*(formal_manu == 1), na.rm = TRUE),
    formal_service = sum(hhwt*(formal_service == 1), na.rm = TRUE),
    inc = weighted.mean(rlinc, hhwt, na.rm = TRUE),
    urban = weighted.mean(urban, hhwt, na.rm = TRUE)
  ) %>% 
  mutate(
    lfp = work / n,
    agri_share = agri/work,
    manu_share = manu/work,
    service_share = service/work,
    hhbus_share = hhbus/n,
    formal_agri_share = formal_agri/agri,  
    formal_manu_share = formal_manu/manu,
    formal_service_share = formal_service/service
  ) %>% 
  mutate(across(where(is.numeric), round, 2))

reallocated_inc <- emp0204_p %>%
  filter(!is.na(reallocated)) %>% 
  group_by(year, female, reallocated) %>% 
  summarise(
    inc = weighted.mean(rlinc, hhwt, na.rm = TRUE)
  )

kable(vhlss_sum, format = "latex", booktabs = T)
kable(vhlss_f_sum, format = "latex", booktabs = T)
