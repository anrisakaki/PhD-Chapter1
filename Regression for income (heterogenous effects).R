##########################################################################
# REGRESSION FOR HETEROGENEOUS EFFECT OF BTA ON SPOUSAL WAGE GAP - URBAN #
##########################################################################

# Urban
etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & married == 2 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & married == 2 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & married == 2 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & married == 2 & urban == 1),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

# Rural 
etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & married == 2 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & married == 2 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & married == 2 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & married == 2 & urban == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

##############################################################################
# REGRESSION FOR HETEROGENEOUS EFFECT OF BTA ON SPOUSAL WAGE GAP - EDUCATION #
##############################################################################

# Education 
etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ > 9 & year == 2002 & married == 2 | year == 2004 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ > 9 & year == 2002 & married == 2 | year == 2004 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ > 9 & year == 2002 & married == 2 | year == 2006 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ > 9 & year == 2002 & married == 2 | year == 2006 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ < 10 & educ > 6 & year == 2002 & married == 2 | year == 2004 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ < 10 & educ > 6 & year == 2002 & married == 2 | year == 2004 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ < 10 & educ > 6 & year == 2002 & married == 2 | year == 2006 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ < 10 & educ > 6 & year == 2002 & married == 2 | year == 2006 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)

etable(list(
  feols(inc_ratio ~ provtariff | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ < 7 & year == 2002 & married == 2 | year == 2004 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid02 + year,
        subset(inc_0204_spouse_p, Female == 1 & educ < 7 & year == 2002 & married == 2 | year == 2004 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff| hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ < 7 & year == 2002 & married == 2 | year == 2006 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh),
  feols(inc_ratio ~ provtariff_k | hhid06 + year,
        subset(inc_0206_spouse_p, Female == 1 & educ < 7 & year == 2002 & married == 2 | year == 2006 & Female == 1 & married == 2),
        weights = ~hhwt, 
        vcov = ~tinh)  
), tex = TRUE)
