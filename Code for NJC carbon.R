#Building climate resilience, social sustainability and equity in global fisheries
#Code for Blue carbon opportunity cost
#Journal NPJ Ocean Sustainability
#Authors: Raúl Prellezo,  José María Da-Rocha,  Maria L.D. Palomares, U. Rashid Sumaila, Sebastian Villasante
#Last update: 16-05-2023
################################################################################
#Libraries
################################################################################
library(xlsx)
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(rlang)
library("writexl")

################################################################################
#Data
################################################################################
# Data source: Sea Arround us 2010-2018
# https://www.seaaroundus.org/
#Read the data
setwd("C:/Users/rprellezo/OneDrive - AZTI/Carbon_Fish/Submission NJC/Results")
SAU<-readRDS("SAU_data.Rda")

################################################################################
#Paramethers
################################################################################
#Factors for Industrial vs artisanal
F_I<-0.148057 #For Industrial (Source AER)
F_A<-1        #For Artisanal fisheries (sensitivity analysis provided)
#Prices for CO2 equivalents
P_C_ETS<-66    # This can be changed to calculate the results of any price.

################################################################################
#Algorithm
################################################################################
#Adding factor of industrial vs artisanal based on AER calculations
SAU<-SAU %>%
mutate(Factor = if_else(sector == "Industrial", F_I, F_A))

#Adding withdrawal price. Several prices are calaculated.
#If other price is selected use the "P_C_ETS" in Paramethers
#Units are US$ 2021

SAU$Withdrawal_price<-SAU$Carbon_C*SAU$Factor
SAUC<-SAU
################################################################################
#Catches
SAU<-SAU %>%
  mutate(p_0 = if_else(Withdrawal_price<=0, tonnes, 0))
SAU<-SAU %>%
  mutate(p_66 = if_else(Withdrawal_price<=P_C_ETS, tonnes, 0))
SAU<-SAU %>%
  mutate(p_165 = if_else(Withdrawal_price<=165, tonnes, 0))
SAU<-SAU %>%
  mutate(p_203 = if_else(Withdrawal_price<=203, tonnes, 0))
SAU<-SAU %>%
  mutate(p_284 = if_else(Withdrawal_price<=284, tonnes, 0))
SAU<-SAU %>%
  mutate(p_351 = if_else(Withdrawal_price<=351, tonnes, 0))
SAU<-SAU %>%
  mutate(p_543 = if_else(Withdrawal_price<=543, tonnes, 0))
SAU<-SAU %>%
  mutate(p_600 = if_else(Withdrawal_price<=600, tonnes, 0))
SAU<-SAU %>%
  mutate(p_700 = if_else(Withdrawal_price<=700, tonnes, 0))
SAU<-SAU %>%
  mutate(p_800 = if_else(Withdrawal_price<=800, tonnes, 0))
SAU<-SAU %>%
  mutate(p_900 = if_else(Withdrawal_price<=900, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1000 = if_else(Withdrawal_price<=1000, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1006 = if_else(Withdrawal_price<=1006, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1200 = if_else(Withdrawal_price<=1200, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1300 = if_else(Withdrawal_price<=1300, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1400 = if_else(Withdrawal_price<=1400, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1500 = if_else(Withdrawal_price<=1500, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1600 = if_else(Withdrawal_price<=1600, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1700 = if_else(Withdrawal_price<=1700, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1800 = if_else(Withdrawal_price<=1800, tonnes, 0))
SAU<-SAU %>%
  mutate(p_1900 = if_else(Withdrawal_price<=1900, tonnes, 0))
SAU<-SAU %>%
  mutate(p_2000 = if_else(Withdrawal_price<=2000, tonnes, 0))
################################################################################
#Catches value
SAU.V<-SAU
SAU.V<-SAU.V %>%
  mutate(p_0 = if_else(Withdrawal_price<=0, tonnes*Price, 0))
SAU.V.V<-SAU.V %>%
  mutate(p_66 = if_else(Withdrawal_price<=P_C_ETS, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_165 = if_else(Withdrawal_price<=165, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_203 = if_else(Withdrawal_price<=203, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_284 = if_else(Withdrawal_price<=284, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_351 = if_else(Withdrawal_price<=351, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_543 = if_else(Withdrawal_price<=543, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_600 = if_else(Withdrawal_price<=600, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_700 = if_else(Withdrawal_price<=700, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_800 = if_else(Withdrawal_price<=800, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_900 = if_else(Withdrawal_price<=900, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1000 = if_else(Withdrawal_price<=1000, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1006 = if_else(Withdrawal_price<=1006, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1200 = if_else(Withdrawal_price<=1200, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1300 = if_else(Withdrawal_price<=1300, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1400 = if_else(Withdrawal_price<=1400, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1500 = if_else(Withdrawal_price<=1500, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1600 = if_else(Withdrawal_price<=1600, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1700 = if_else(Withdrawal_price<=1700, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1800 = if_else(Withdrawal_price<=1800, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_1900 = if_else(Withdrawal_price<=1900, tonnes*Price, 0))
SAU.V<-SAU.V %>%
  mutate(p_2000 = if_else(Withdrawal_price<=2000, tonnes*Price, 0))
################################################################################
#Carbon
SAUC<-SAUC %>%
  mutate(p_0 = if_else(Withdrawal_price<=0, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_66 = if_else(Withdrawal_price<=P_C_ETS, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_165 = if_else(Withdrawal_price<=165, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_203 = if_else(Withdrawal_price<=203, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_284 = if_else(Withdrawal_price<=284, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_351 = if_else(Withdrawal_price<=351, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_543 = if_else(Withdrawal_price<=543, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_600 = if_else(Withdrawal_price<=600, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_700 = if_else(Withdrawal_price<=700, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_800 = if_else(Withdrawal_price<=800, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_900 = if_else(Withdrawal_price<=900, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1000 = if_else(Withdrawal_price<=1000, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1006 = if_else(Withdrawal_price<=1006, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1200 = if_else(Withdrawal_price<=1200, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1300 = if_else(Withdrawal_price<=1300, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1400 = if_else(Withdrawal_price<=1400, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1500 = if_else(Withdrawal_price<=1500, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1600 = if_else(Withdrawal_price<=1600, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1700 = if_else(Withdrawal_price<=1700, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1800 = if_else(Withdrawal_price<=1800, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_1900 = if_else(Withdrawal_price<=1900, Carbon, 0))
SAUC<-SAUC %>%
  mutate(p_2000 = if_else(Withdrawal_price<=2000, Carbon, 0))
################################################################################
#Carbon value
SAUC.V<-SAU
SAUC.V<-SAUC.V %>%
  mutate(p_0 = if_else(Withdrawal_price<=0, Carbon*0, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_66 = if_else(Withdrawal_price<=P_C_ETS, Carbon*66, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_165 = if_else(Withdrawal_price<=165, Carbon*165, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_203 = if_else(Withdrawal_price<=203, Carbon*203, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_284 = if_else(Withdrawal_price<=284, Carbon_C*284, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_351 = if_else(Withdrawal_price<=351, Carbon*351, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_543 = if_else(Withdrawal_price<=543, Carbon*543, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_600 = if_else(Withdrawal_price<=600, Carbon*600, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_700 = if_else(Withdrawal_price<=700, Carbon*700, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_800 = if_else(Withdrawal_price<=800, Carbon*800, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_900 = if_else(Withdrawal_price<=900, Carbon*900, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1000 = if_else(Withdrawal_price<=1000, Carbon*1000, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1006 = if_else(Withdrawal_price<=1006, Carbon*1006, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1200 = if_else(Withdrawal_price<=1200, Carbon*1200, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1300 = if_else(Withdrawal_price<=1300, Carbon*1300, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1400 = if_else(Withdrawal_price<=1400, Carbon*1400, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1500 = if_else(Withdrawal_price<=1500, Carbon*1500, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1600 = if_else(Withdrawal_price<=1600, Carbon*1600, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1700 = if_else(Withdrawal_price<=1700, Carbon*1700, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1800 = if_else(Withdrawal_price<=1800, Carbon*1800, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_1900 = if_else(Withdrawal_price<=1900, Carbon*1900, 0))
SAUC.V<-SAUC.V %>%
  mutate(p_2000 = if_else(Withdrawal_price<=2000, Carbon*2000, 0))
#Landings after withrawal of carbon

################################################################################
#Landings vs carbon trade off
SAUC.TO<-SAU
SAUC.TO<-SAUC.TO %>%
  mutate(p_0 = if_else(Withdrawal_price<=0, (tonnes*Price*(1-F_I)+(Carbon*0)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_66 = if_else(Withdrawal_price<=P_C_ETS, (tonnes*Price*(1-F_I)+(Carbon*P_C_ETS)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_165 = if_else(Withdrawal_price<=165, (tonnes*Price*(1-F_I)+(Carbon*165)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_203 = if_else(Withdrawal_price<=203, (tonnes*Price*(1-F_I)+(Carbon*203)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_284 = if_else(Withdrawal_price<=284, (tonnes*Price*(1-F_I)+(Carbon*284)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_351 = if_else(Withdrawal_price<=351, (tonnes*Price*(1-F_I)+(Carbon*351)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_543 = if_else(Withdrawal_price<=543, (tonnes*Price*(1-F_I)+(Carbon*543)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_600 = if_else(Withdrawal_price<=600, (tonnes*Price*(1-F_I)+(Carbon*600)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_700 = if_else(Withdrawal_price<=700, (tonnes*Price*(1-F_I)+(Carbon*700)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_800 = if_else(Withdrawal_price<=800, (tonnes*Price*(1-F_I)+(Carbon*800)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_900 = if_else(Withdrawal_price<=900, (tonnes*Price*(1-F_I)+(Carbon*900)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1000 = if_else(Withdrawal_price<=1000, (tonnes*Price*(1-F_I)+(Carbon*1000)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1006 = if_else(Withdrawal_price<=1006, (tonnes*Price*(1-F_I)+(Carbon*1006)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1200 = if_else(Withdrawal_price<=1200, (tonnes*Price*(1-F_I)+(Carbon*1200)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1300 = if_else(Withdrawal_price<=1300, (tonnes*Price*(1-F_I)+(Carbon*1300)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1400 = if_else(Withdrawal_price<=1400, (tonnes*Price*(1-F_I)+(Carbon*1400)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1500 = if_else(Withdrawal_price<=1500, (tonnes*Price*(1-F_I)+(Carbon*1500)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1600 = if_else(Withdrawal_price<=1600, (tonnes*Price*(1-F_I)+(Carbon*1600)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1700 = if_else(Withdrawal_price<=1700, (tonnes*Price*(1-F_I)+(Carbon*1700)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1800 = if_else(Withdrawal_price<=1800, (tonnes*Price*(1-F_I)+(Carbon*1800)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_1900 = if_else(Withdrawal_price<=1900, (tonnes*Price*(1-F_I)+(Carbon*1900)), tonnes*Price))
SAUC.TO<-SAUC.TO %>%
  mutate(p_2000 = if_else(Withdrawal_price<=2000, (tonnes*Price*(1-F_I)+(Carbon*2000)), tonnes*Price))
#Graphs
################################################################################
#Calculate the indicators by year and area of catches
Dat.area.year<-SAU
#Dat.area.year$sector<- NULL
Dat.area.year$catch_type <- NULL
Dat.area.year$gear<- NULL  
Dat.area.year$fishing_entity <- NULL
Dat.area.year$fao_area_id <- NULL
Dat.area.year$fishing_entity_id <- NULL
#Dat.area.year$eez  <- NULL
Dat.area.year$taxon_key<- NULL
Dat.area.year$scientific_name   <- NULL
Dat.area.year$Sea   <- NULL
Dat.area.year.sum<-as.data.frame(Dat.area.year %>%
                                   group_by(year,eez) %>%
                                   summarize("p_0" = sum(p_0,na.rm=TRUE),"p_66" = sum(p_66,na.rm=TRUE),"p_165" = sum(p_165,na.rm=TRUE),"p_203" = sum(p_203,na.rm=TRUE),
                                             "p_284" = sum(p_284,na.rm=TRUE),"p_351" = sum(p_351,na.rm=TRUE),"p_543" = sum(p_543,na.rm=TRUE),"p_600" = sum(p_600,na.rm=TRUE),"p_700" = sum(p_700,na.rm=TRUE),
                                             "p_800" = sum(p_800,na.rm=TRUE),"p_900" = sum(p_900,na.rm=TRUE),"p_1000" = sum(p_1000,na.rm=TRUE),"p_1006" = sum(p_1006,na.rm=TRUE),"p_1200" = sum(p_1200,na.rm=TRUE),
                                             "p_1300" = sum(p_1300,na.rm=TRUE),"p_1400" = sum(p_1400,na.rm=TRUE),"p_1500" = sum(p_1500,na.rm=TRUE),"p_1600" = sum(p_1600,na.rm=TRUE),"p_1700" = sum(p_1700,na.rm=TRUE),
                                             "p_1800" = sum(p_1800,na.rm=TRUE),"p_1900" = sum(p_1900,na.rm=TRUE),"p_2000" = sum(p_2000,na.rm=TRUE)))

Dat.area.mean<-as.data.frame(Dat.area.year.sum %>%
                               group_by(eez) %>%
                               summarize("0" = mean(p_0,na.rm=TRUE),"66" = mean(p_66,na.rm=TRUE),"165" = mean(p_165,na.rm=TRUE),"203" = mean(p_203,na.rm=TRUE),
                                         "284" = mean(p_284,na.rm=TRUE),"351" = mean(p_351,na.rm=TRUE),"543" = mean(p_543),"600" = mean(p_600),"700" = mean(p_700,na.rm=TRUE),
                                         "800" = mean(p_800,na.rm=TRUE),"900" = mean(p_900,na.rm=TRUE),"1000" = mean(p_1000,na.rm=TRUE),"1006" = mean(p_1006,na.rm=TRUE),"1200" = mean(p_1200,na.rm=TRUE),
                                         "1300" = mean(p_1300,na.rm=TRUE),"1400" = mean(p_1400,na.rm=TRUE),"1500" = mean(p_1500),"1600" = mean(p_1600),"1700" = mean(p_1700,na.rm=TRUE),
                                         "1800" = mean(p_1800,na.rm=TRUE),"1900" = mean(p_1900,na.rm=TRUE),"2000" = mean(p_2000,na.rm=TRUE)))

Dat.area.mean<-Dat.area.mean %>%
  adorn_totals("row")                       #To add a total
#Calculate the indicators by year an area of carbon
Dat.area.year.C<-SAUC
#Dat.area.year.C$sector<- NULL
Dat.area.year.C$catch_type <- NULL
Dat.area.year.C$gear<- NULL  
Dat.area.year.sum.C<-as.data.frame(Dat.area.year.C %>%
                                     group_by(year,eez  ) %>%
                                     summarize("p_0" = sum(p_0,na.rm=TRUE),"p_66" = sum(p_66,na.rm=TRUE),"p_165" = sum(p_165,na.rm=TRUE),"p_203" = sum(p_203,na.rm=TRUE),
                                               "p_284" = sum(p_284,na.rm=TRUE),"p_351" = sum(p_351,na.rm=TRUE),"p_543" = sum(p_543,na.rm=TRUE),"p_600" = sum(p_600,na.rm=TRUE),"p_700" = sum(p_700,na.rm=TRUE),
                                               "p_800" = sum(p_800,na.rm=TRUE),"p_900" = sum(p_900,na.rm=TRUE),"p_1000" = sum(p_1000,na.rm=TRUE),"p_1006" = sum(p_1006,na.rm=TRUE),"p_1200" = sum(p_1200,na.rm=TRUE),
                                               "p_1300" = sum(p_1300,na.rm=TRUE),"p_1400" = sum(p_1400,na.rm=TRUE),"p_1500" = sum(p_1500,na.rm=TRUE),"p_1600" = sum(p_1600,na.rm=TRUE),"p_1700" = sum(p_1700,na.rm=TRUE),
                                               "p_1800" = sum(p_1800,na.rm=TRUE),"p_1900" = sum(p_1900,na.rm=TRUE),"p_2000" = sum(p_2000,na.rm=TRUE)))

Dat.area.mean.C<-as.data.frame(Dat.area.year.sum.C %>%
                                 group_by(eez) %>%
                                 summarize("0" = mean(p_0,na.rm=TRUE),"66" = mean(p_66,na.rm=TRUE),"165" = mean(p_165,na.rm=TRUE),"203" = mean(p_203,na.rm=TRUE),
                                           "284" = mean(p_284,na.rm=TRUE),"351" = mean(p_351,na.rm=TRUE),"543" = mean(p_543),"600" = mean(p_600),"700" = mean(p_700,na.rm=TRUE),
                                           "800" = mean(p_800,na.rm=TRUE),"900" = mean(p_900,na.rm=TRUE),"1000" = mean(p_1000,na.rm=TRUE),"1006" = mean(p_1006,na.rm=TRUE),"1200" = mean(p_1200,na.rm=TRUE),
                                           "1300" = mean(p_1300,na.rm=TRUE),"1400" = mean(p_1400,na.rm=TRUE),"1500" = mean(p_1500),"1600" = mean(p_1600),"1700" = mean(p_1700,na.rm=TRUE),
                                           "1800" = mean(p_1800,na.rm=TRUE),"1900" = mean(p_1900,na.rm=TRUE),"2000" = mean(p_2000,na.rm=TRUE)))

Dat.area.mean.C<-Dat.area.mean.C %>%
  adorn_totals("row")                       #To add a total
write_xlsx(Dat.area.mean,"Catches removal_ART_1.xlsx")
write_xlsx(Dat.area.mean.C,"Carbono removal_ART_1.xlsx")
#Calculate the indicators by year an area of trade offs
Dat.area.year.TO<-SAUC.TO
Dat.area.year.TO$sector<- NULL
Dat.area.year.TO$catch_type <- NULL
Dat.area.year.TO$gear<- NULL  
Dat.area.year.sum.TO<-as.data.frame(Dat.area.year.TO %>%
                                      group_by(year,eez  ) %>%
                                      summarize("p_0" = sum(p_0,na.rm=TRUE),"p_66" = sum(p_66,na.rm=TRUE),"p_165" = sum(p_165,na.rm=TRUE),"p_203" = sum(p_203,na.rm=TRUE),
                                                "p_284" = sum(p_284,na.rm=TRUE),"p_351" = sum(p_351,na.rm=TRUE),"p_543" = sum(p_543,na.rm=TRUE),"p_600" = sum(p_600,na.rm=TRUE),"p_700" = sum(p_700,na.rm=TRUE),
                                                "p_800" = sum(p_800,na.rm=TRUE),"p_900" = sum(p_900,na.rm=TRUE),"p_1000" = sum(p_1000,na.rm=TRUE),"p_1006" = sum(p_1006,na.rm=TRUE),"p_1200" = sum(p_1200,na.rm=TRUE),
                                                "p_1300" = sum(p_1300,na.rm=TRUE),"p_1400" = sum(p_1400,na.rm=TRUE),"p_1500" = sum(p_1500,na.rm=TRUE),"p_1600" = sum(p_1600,na.rm=TRUE),"p_1700" = sum(p_1700,na.rm=TRUE),
                                                "p_1800" = sum(p_1800,na.rm=TRUE),"p_1900" = sum(p_1900,na.rm=TRUE),"p_2000" = sum(p_2000,na.rm=TRUE)))

Dat.area.mean.TO<-as.data.frame(Dat.area.year.sum.TO %>%
                                  group_by(eez) %>%
                                  summarize("0" = mean(p_0,na.rm=TRUE),"66" = mean(p_66,na.rm=TRUE),"165" = mean(p_165,na.rm=TRUE),"203" = mean(p_203,na.rm=TRUE),
                                            "284" = mean(p_284,na.rm=TRUE),"351" = mean(p_351,na.rm=TRUE),"543" = mean(p_543),"600" = mean(p_600),"700" = mean(p_700,na.rm=TRUE),
                                            "800" = mean(p_800,na.rm=TRUE),"900" = mean(p_900,na.rm=TRUE),"1000" = mean(p_1000,na.rm=TRUE),"1006" = mean(p_1006,na.rm=TRUE),"1200" = mean(p_1200,na.rm=TRUE),
                                            "1300" = mean(p_1300,na.rm=TRUE),"1400" = mean(p_1400,na.rm=TRUE),"1500" = mean(p_1500),"1600" = mean(p_1600),"1700" = mean(p_1700,na.rm=TRUE),
                                            "1800" = mean(p_1800,na.rm=TRUE),"1900" = mean(p_1900,na.rm=TRUE),"2000" = mean(p_2000,na.rm=TRUE)))

Dat.area.mean.TO<-Dat.area.mean.TO %>%
  adorn_totals("row")                       #To add a total


################################################################################
#Totals of catches
Tot.area.year.sum<-as.data.frame(Dat.area.year %>%
                                   group_by(year,eez  ) %>%
                                   summarize("Catches" = sum(tonnes,na.rm=TRUE)))

Tot.area.mean<-as.data.frame(Tot.area.year.sum %>%
                               group_by(eez) %>%
                               summarize("Total_catches" = mean(Catches,na.rm=TRUE)))
Tot.area.mean<-Tot.area.mean %>%
  adorn_totals("row") 

Tot<-do.call("rbind", replicate(22, Tot.area.mean, simplify = FALSE))

#Totals of catches value
Tot.area.year.sum.V<-as.data.frame(Dat.area.year %>%
                                   group_by(year,eez  ) %>%
                                   summarize("Value" = sum(tonnes*Price,na.rm=TRUE)))

Tot.area.mean.V<-as.data.frame(Tot.area.year.sum.V %>%
                               group_by(eez) %>%
                               summarize("Total_Value" = mean(Value,na.rm=TRUE)))
Tot.area.mean.V<-Tot.area.mean.V %>%
  adorn_totals("row") 

#Totals of carbon
Tot.area.year.sum.C<-as.data.frame(Dat.area.year.C %>%
                                     group_by(year,eez  ) %>%
                                     summarize("Co2" = sum(Carbon,na.rm=TRUE)))

Tot.area.mean.C<-as.data.frame(Tot.area.year.sum.C %>%
                                 group_by(eez) %>%
                                 summarize("Total_Co2" = mean(Co2,na.rm=TRUE)))
Tot.area.mean.C<-Tot.area.mean.C %>%
  adorn_totals("row") 

Tot.C<-do.call("rbind", replicate(22, Tot.area.mean.C, simplify = FALSE))

Total_catches<-cbind(Tot.area.mean,Tot.area.mean.V[,2],Tot.area.mean.C[,2])
colnames(Total_catches)<-c("Area","Catches","Value","Co2")
################################################################################
write_xlsx(Total_catches,"Catches_Carbon_value_ART_1.xlsx")





