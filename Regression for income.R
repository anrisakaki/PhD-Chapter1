########################################################################################
# SETING UP FOR REGRESSION ON INCOME OF WOMEN AS A SHARE OF HER TOTAL HOUSEHOLD INCOME #
########################################################################################

inc020406 <- c("inc02", "inc04", "inc06")

for(i in inc020406){
  
  assign(i, get(i) %>% 
           mutate(inc_ratio = income/total_income,
                  inc_ratio_1 = income/hhinc))
}

# Constructing panel data 
## 2002 - 2004 
inc_02_spouse_p <- merge(hhid0204, inc02, by = "hhid02") %>% 
  rename(tinh = tinh.x) %>% 
  select(-c("tinh.y")) %>% 
  distinct() %>% 
  mutate(across(tinh, as.factor)) %>% 
  mutate(year = 2002)

inc_04_spouse_p <- merge(hhid0204, inc04, by = c("tinh", "hhid")) %>% 
  distinct() %>% 
  mutate(year = 2004) %>% 
  mutate(across(tinh, as.factor))

inc_0204_spouse_p <- bind_rows(inc_02_spouse_p, inc_04_spouse_p)

## 2002 - 2006 
inc_0602_spouse_p <- merge(hhid020406, inc02, by = "hhid02") %>% 
  mutate(year = 2002)

inc06 <- inc06 %>% rename(hhid06 = hhid)

inc_06_spouse_p <- merge(hhid020406, inc06, by = "hhid06") %>% 
  mutate(year = 2006)

inc_0206_spouse_p <- bind_rows(inc_0602_spouse_p, inc_06_spouse_p)

save(inc_0204_spouse_p, file = "inc_0204_spouse_p.rda")
save(inc_0206_spouse_p, file = "inc_0206_spouse_p.rda")

#####################################################################################
# REGRESSION ON WOMENS INCOME AS A SHARE OF TOTAL HOUSEHOLD INCOME USING PANEL DATA #
#####################################################################################

load("inc_0204_spouse_p.rda")
load("inc_0206_spouse_p.rda")

etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

# Accounting for female-intensity  

etable(list(
  feols(inc_ratio ~ provtariff_f | hhid02 + year,
        subset(inc_0204_spouse_p, female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_fk | hhid02 + year,
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
