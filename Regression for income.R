########################################################################################
# SETING UP FOR REGRESSION ON INCOME OF WOMEN AS A SHARE OF HER TOTAL HOUSEHOLD INCOME #
########################################################################################

# Constructing panel data 
## 2002 - 2004 
inc_02_spouse_p <- merge(ivid0204, inc02, by = c("ivid02", "hhid02")) %>% 
  distinct() %>% 
  mutate(across(tinh, as.factor)) %>% 
  mutate(year = 2002) # N = 58,954

inc_panel <- inc_02_spouse_p %>% select(hhid, ivid) %>% distinct()

inc_04_spouse_p <- merge(inc_panel, inc04, by = c("ivid", "hhid")) %>% 
  group_by(ivid) %>% 
  distinct() %>% 
  mutate(year = 2004) %>% 
  mutate(across(tinh, as.factor))

inc_0204_spouse_p <- bind_rows(inc_02_spouse_p, inc_04_spouse_p) %>% 
  filter(married == 2,
         !is.na(hhid)) %>% 
  group_by(hhid, year) %>%
  mutate(total_hhinc_ratio = sum(inc_ratio)) %>% 
  # filter(total_hhinc_ratio >= 0 & total_hhinc_ratio <= 1) %>% 
  mutate(both_nonwage = ifelse(all(inc_ratio == 0), 1, 0),
         inc_ratio = ifelse(both_nonwage == 1, 0.5, inc_ratio)) %>% 
  mutate(total_hhinc_ratio_new = sum(inc_ratio)) %>% 
  ungroup() %>% 
  select(-c(both_nonwage, total_hhinc_ratio, total_hhinc_ratio_new))

## 2002 - 2006 
inc_0602_spouse_p <- merge(hhid020406, inc02, by = "hhid02") %>% 
  mutate(year = 2002,
         across(tinh, as.double))

inc06 <- inc06 %>% rename(hhid06 = hhid)

inc_06_spouse_p <- merge(hhid020406, inc06, by = "hhid06") %>% 
  mutate(year = 2006,
         across(tinh, as.double))

inc_0206_spouse_p <- bind_rows(inc_0602_spouse_p, inc_06_spouse_p) %>% 
  filter(married == 2,
         !is.na(hhid02)) %>% 
  group_by(hhid02, year) %>%
  mutate(total_hhinc_ratio = sum(inc_ratio)) %>% 
  # filter(total_hhinc_ratio >= 0 & total_hhinc_ratio <= 1) %>% 
  mutate(both_nonwage = ifelse(all(inc_ratio == 0), 1, 0),
         inc_ratio = ifelse(both_nonwage == 1, 0.5, inc_ratio)) %>% 
  mutate(total_hhinc_ratio_new = sum(inc_ratio)) %>% 
  ungroup() %>% 
  select(-c(both_nonwage, total_hhinc_ratio, total_hhinc_ratio_new))

save(inc_0204_spouse_p, file = "inc_0204_spouse_p.rda")
save(inc_0206_spouse_p, file = "inc_0206_spouse_p.rda")

#####################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING PANEL DATA #
#####################################################################################

load("inc_0204_spouse_p.rda")
load("inc_0206_spouse_p.rda")

etable(list(
  feols(inc_ratio ~ provtariff | ivid + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~provtariff_f | ivid + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_f | hhid02 + year,
        subset(inc_0206_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

1.363# Accounting for female-intensity  

etable(list(
  feols(inc_ratio ~ provtariff_f | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_fk | hhid + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_f | hhid06 + year,
        subset(inc_0206_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_fk | hhid06 + year,
        subset(inc_0206_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)


#################################################################
# REGRESSION ON SPOUSAL WAGE GAP - BY SECTOR - USING PANEL DATA #
#################################################################

# 2002 - 2004 
etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & agri_work == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & agri_work == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),  
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & manu == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & manu == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),  
  feols(inc_ratio ~ provtariff| hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & tal == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k| hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & agri_work == 1 & year == 2002 | Female == 1 & tal == 1 & year == 2004),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)
