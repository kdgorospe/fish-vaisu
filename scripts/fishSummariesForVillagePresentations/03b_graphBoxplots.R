rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(ggplot2)

setwd("~/Analyses_notGit/fish-vaisu/input") # modify to your folder structure
#LOAD LIBRARY FUNCTIONS ... 

#load("TMPwsd-withVatiaFagaalu-FamilyAbund.Rdata")
#load("TMPwsd-withVatiaFagaalu-FamilyBiomass.Rdata")
#load("TMPwsd-withVatiaFagaalu-SpeciesAbund.Rdata")
load("TMPwsd-withVatiaFagaalu-SpeciesBiomass.Rdata")

shed.info<-read.csv("SITE_MASTER_2016_TUT_Watershed_Sector.csv")

# MERGE fish data with site info (i.e., watershed assignment) data
#common.info<-names(wsd)[names(wsd) %in% names(shed.info)]
#Remove DATE, LAT, LONG
common.info<-c("SITEVISITID", "OBS_YEAR", "REGION", "ANALYSIS_SCHEME", "SEC_NAME", "ISLAND", "SITE", "REEF_ZONE", "DEPTH_BIN")
shed.temp<-subset(shed.info, select=-c(DATE_, LATITUDE, LONGITUDE))
wsd.village<-merge(x=wsd, y=shed.temp, by=common.info, all.x=TRUE)
table(wsd.village$Watershed)
table(wsd.village$ISLAND)

# For deciding which species to graph:
load("TMPspecies.RData")
acanths<-species_table[species_table$FAMILY=="Acanthuridae",]
acanth.list<-acanths$SPECIES
acanth.list<-as.character(acanth.list)


# GRAPH SETTINGS:::
fishgroups<-acanth.list
#fishgroups<-c("Acanthuridae", "Carangidae", "Scombridae", "Scaridae", "Serranidae", "Lutjanidae", "Mullidae", "Lethrinidae", "TotFish")
places<-c("Tutuila", "Ofu & Olosega", "Tau", "Faga'alu", "Vatia")
yname="Biomass"

for (i in 1:length(fishgroups))
{

oo.dat<-data.frame(group="Ofu & Olosega", value=subset(wsd.village, ISLAND=="Ofu & Olosega", select=fishgroups[i]))
tau.dat<-data.frame(group="Tau", value=subset(wsd.village, ISLAND=="Tau", select=fishgroups[i]))
tut.dat<-data.frame(group="Tutuila", value=subset(wsd.village, ISLAND=="Tutuila", select=fishgroups[i]))
faga.dat<-data.frame(group="Faga'alu", value=subset(wsd.village, Watershed=="Faga'alu", select=fishgroups[i]))
vat.dat<-data.frame(group="Vatia", value=subset(wsd.village, Watershed=="Vatia", select=fishgroups[i]))

graph.dat<-rbind(tut.dat, oo.dat, tau.dat, faga.dat, vat.dat)



p<-ggplot(graph.dat, aes(x=group, y=get(fishgroups[i])))+
  geom_boxplot(outlier.alpha=0, lwd=1.3)+
  geom_jitter(colour="blue", alpha=0.3)+
  labs(y=yname, x="", title=fishgroups[i])+
  theme_light()+
  theme(text = element_text(size=18), 
        axis.text.x=element_text(size=16, colour="black"))

filename=paste("boxplot_", yname, "_", fishgroups[i], ".pdf", sep="")
setwd("~/Analyses_notGit/fish-vaisu/output")
pdf(file=filename)
print(p)
dev.off()
}
