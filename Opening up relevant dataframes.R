# Opening up relevant packages
library(tidyverse)
library(usethis)
library(haven)
library(gtsummary)
library(gt)
library(sf)
library(fixest)
library(modelsummary)
library(bookdown)
library(kableExtra)
library(ggeffects)
library(xtable)

rm(list=ls())

# Opening up relevant dataframes
setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/VHLSS Data/")

## Tariff data 
tariff <- read_dta("Full VHLSS/Tariffs-by-VLSS-industry-codes.dta")

prov_tariff <- read_dta("Full VHLSS/ProvTariff.dta")

## Map data 
vnmap0 <- read_sf("Full VHLSS/Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Full VHLSS/Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Full VHLSS/Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Full VHLSS/Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")

# 2002 
m3_02a <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc3.dta") #DF containing work 

## 2004 
m4a_04a <- read_dta(file = "Full VHLSS/2004/m4a.dta") #DF containing work 

## 2006 
m4a_06a <- read_dta(file = "Full VHLSS/2006/muc4a.dta") #DF containing employment & housework 

## READ DTA FILES
# 2002 
exp_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/hhexpe02.dta")
inc_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/hhinc02.dta")
m1_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc1.dta") #DF containing age and sex
m2_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc2.dta") #DF containing educational attainment 
m3_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc3.dta") #DF containing work 
m5a_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5a.dta") #DF containing work
m5aho_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5aho.dta")
m5b25_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5b25.dta")
m5b32_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5b32.dta")
m5b4_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5b4.dta")
m5d_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5d.dta")
m6a2_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc6a2.dta")
m6b1_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc6b1.dta")
m6b2_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc6b2.dta")
m6b34_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc6b34.dta")
m7_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc7.dta")

## 2004 
ho1_04 <- read_dta(file = "Full VHLSS/2004/ho1.dta")
ho2_04 <- read_dta(file = "Full VHLSS/2004/ho2.dta")
exp_04 <- read_dta(file = "Full VHLSS/2004/hhexpe04.dta")
inc_04 <- read_dta(file = "Full VHLSS/2004/hhinc04.dta")
m123a_04 <- read_dta(file = "Full VHLSS/2004/m1_2_3a.dta") #DF containing age, sex, and educational attainment 
m1b_04 <- read_dta(file = "Full VHLSS/2004/m1b.dta") # Df containing HH and Individual identifies to construct panel data 
m4a_04 <- read_dta(file = "Full VHLSS/2004/m4a.dta") #DF containing work 
m5a2_04 <- read_dta(file = "Full VHLSS/2004/m5a2.dta")
m5b1_04 <- read_dta(file = "Full VHLSS/2004/m5b1.dta")
m5b2_04 <- read_dta(file = "Full VHLSS/2004/m5b2.dta")
m6a_04 <- read_dta(file = "Full VHLSS/2004/m6a.dta")
m6b_04 <- read_dta(file = "Full VHLSS/2004/m6b.dta")

## 2006
exp_06 <- read_dta(file = "Full VHLSS/2006/hhexpe06.dta")
inc_06 <- read_dta(file = "Full VHLSS/2006/hhinc06.dta")
m1a_06 <- read_dta(file = "Full VHLSS/2006/muc1a.dta") #DF containing age and sex 
m1b_06 <- read_dta(file = "Full VHLSS/2006/muc1b.dta") #DF containing individual panel data  
m2a_06 <- read_dta(file = "Full VHLSS/2006/muc2a.dta") #DF containing educational attainment 
m4a_06 <- read_dta(file = "Full VHLSS/2006/muc4a.dta") #DF containing employment & housework 
m5a2_06 <- read_dta(file = "Full VHLSS/2006/muc5a2.dta")
m5b1_06 <- read_dta(file = "Full VHLSS/2006/muc5b1.dta")
m5b2_06 <- read_dta(file = "Full VHLSS/2006/muc5b2.dta") #DF containing hh consumption patterns
m6a_06 <- read_dta(file = "Full VHLSS/2006/muc6a.dta")
m6b_06 <- read_dta(file = "Full VHLSS/2006/muc6b.dta")
ttchung_06 <- read_dta(file = "Full VHLSS/2006/ttchung.dta")

# WVS
WVS_01 <- read_dta(file = "Full VHLSS/WVS/WVS_Wave_4.dta")
WVS_05 <- read_dta(file = "Full VHLSS/WVS/WVS_Wave_5.dta")