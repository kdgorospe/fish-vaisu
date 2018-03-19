rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

setwd("~/Analyses_notGit/fish-vaisu/input") # modify to your folder structure
#LOAD LIBRARY FUNCTIONS ... 

load("TMPwsd-withVatiaFagaalu-SpeciesAbund.Rdata")
shed.info<-read.csv("SITE_MASTER_2016_TUT_Watershed_Sector.csv")

# MERGE fish data with site info (i.e., watershed assignment) data
#common.info<-names(wsd)[names(wsd) %in% names(shed.info)]
#Remove DATE, LAT, LONG
common.info<-c("SITEVISITID", "OBS_YEAR", "REGION", "ANALYSIS_SCHEME", "SEC_NAME", "ISLAND", "SITE", "REEF_ZONE", "DEPTH_BIN")
shed.temp<-subset(shed.info, select=-c(DATE_, LATITUDE, LONGITUDE))
wsd.village<-merge(x=wsd, y=shed.temp, by=common.info, all.x=TRUE)
table(wsd.village$Watershed)

firstcol<-grep("nReps", colnames(wsd.village))+1 # get first fish column number
lastcol<-grep("TotFish", colnames(wsd.village)) # get last fish column number
fish.means<-aggregate(wsd.village[firstcol:lastcol], by=list(wsd.village$Watershed), FUN=mean)
fish.N<-aggregate(wsd.village[firstcol:lastcol], by=list(wsd.village$Watershed), FUN=length)
fish.sd<-aggregate(wsd.village[firstcol:lastcol], by=list(wsd.village$Watershed), FUN=sd)
fish.se<-fish.sd[,-1]/sqrt(fish.N[,-1])
fish.se<-cbind(fish.means[,1],fish.se)
names(fish.se)[1]<-"Group.1"

dp.Shed<-list(Mean=fish.means, PooledSE=fish.se)
save(dp.Shed, file="data_pooled_shed.Rdata")