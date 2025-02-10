##########################################################
# SETTING UP FOR REGRESSION ON STRUCTURAL TRANSFORMATION #
##########################################################

provrecode_fn <- function(i){
  i %>% 
    mutate(tinh = ifelse(tinh == 302, 301, tinh),
           tinh = ifelse(tinh == 606, 605, tinh),
           tinh = ifelse(tinh == 816, 815, tinh))
}

emp0204_p <- bind_rows(vhlss02, vhlss04) %>%
  merge(ivid0204, by = ivid) %>%
  provrecode_fn() %>%
  left_join(bta0204, by = c("tinh", "year")) %>% 
  mutate(tariff = tariff*-1,
         tariff_f = tariff_f*-1)

emp0206_p <- bind_rows(vhlss02, vhlss06) %>%
  merge(ivid0206, by = ivid) %>%
  provrecode_fn() %>% 
  left_join(bta0206, by = c("tinh", "year")) %>% 
  mutate(tariff = tariff*-1,
         tariff_f = tariff_f*-1)

############################################################
# REGRESSION ON STRUCTURAL TRANSFORMATION USING PANEL DATA #
############################################################

etable(list(
  feols(agri ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(agri ~ i(as.factor(female), tariff) | year + ivid,
        emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(agri ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(agri ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

################################################
# REGRESSION ON FORMALISATION USING PANEL DATA #
################################################

etable(list(
  feols(formal ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal ~ i(as.factor(female), tariff) | year + ivid,
        emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(formal_manu ~ i(as.factor(female), tariff) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_manu ~ i(as.factor(female), tariff) | year + ivid,
        emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_manu ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0204_p,
        weights = ~hhwt,
        vcov = ~tinh),
  feols(formal_manu ~ i(as.factor(female), tariff_f) | year + ivid,
        emp0206_p,
        weights = ~hhwt,
        vcov = ~tinh)
), tex = T)
