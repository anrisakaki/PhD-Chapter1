####################################
# SETTING UP FOR REGRESSION OF WVS #
####################################

WVS_01 <- WVS_01 %>% 
  select(-"tinh")

WVS_01 <- merge(WVS_01, WVS_preBTA_tariffs, by = "XVIE_V225D")

WVS_05 <- WVS_05 %>% 
  select(-"tinh")

WVS_05 <- merge(WVS_05, WVS_postBTA_tariffs, by = "V257")

# Renaming variables 

WVS_01 <- WVS_01 %>% 
  rename(job_scarce = V78,
         working_mother = V115,
         housewife = V116,
         both_contribute = V117)

WVS_05 <- WVS_05 %>% 
  rename(job_scarce = V44,
         housewife = V60)
