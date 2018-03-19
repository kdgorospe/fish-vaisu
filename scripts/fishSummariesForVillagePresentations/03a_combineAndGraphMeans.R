# Combine island and shed .RData and Graph
rm(list=ls())
library(ggplot2)


setwd("~/Analyses_notGit/fish-vaisu/input") # modify to your folder structure

#load("data_pooled_island-FamilyBiomass.Rdata")
#load("data_pooled_shed-FamilyBiomass.Rdata")

#load("data_pooled_island-FamilyAbund.Rdata")
#load("data_pooled_shed-FamilyAbund.Rdata")

#load("data_pooled_island-SpeciesAbund.Rdata")
#load("data_pooled_shed-SpeciesAbund.Rdata")

load("data_pooled_island-SpeciesBiomass.Rdata")
load("data_pooled_shed-SpeciesBiomass.Rdata")


# For deciding which species to graph:
load("TMPspecies.RData")
acanths<-species_table[species_table$FAMILY=="Acanthuridae",]
acanth.list<-acanths$SPECIES
acanth.list<-as.character(acanth.list)

# ROWBIND the means and standard errors
# Need a dataframe for each family with columns for groupname (Shed or Island), Mean, and SE

# GRAPH SETTINGS:::
fishgroups<-acanth.list
#fishgroups<-c("Acanthuridae", "Carangidae", "Scombridae", "Scaridae", "Serranidae", "Lutjanidae", "Mullidae", "Lethrinidae", "TotFish")
places<-c("Tutuila", "Ofu & Olosega", "Tau", "Faga'alu", "Vatia")
yname="Biomass"


for (i in 1:length(fishgroups))
{
mean.is<-subset(dp$Mean, select=c(ISLAND,get(fishgroups[i])))
mean.sh<-subset(dp.Shed$Mean, select=c(Group.1,get(fishgroups[i])))
colnames(mean.is)[1]<-"Scale"
colnames(mean.sh)[1]<-"Scale"
mean.all<-rbind(mean.is, mean.sh)
mean.graph<-mean.all[(mean.all$Scale %in% places),]

se.is<-subset(dp$PooledSE, select=c(ISLAND,get(fishgroups[i])))
se.sh<-subset(dp.Shed$PooledSE, select=c(Group.1,get(fishgroups[i])))
colnames(se.is)[1]<-"Scale"
colnames(se.sh)[1]<-"Scale"
se.all<-rbind(se.is, se.sh)
se.graph<-se.all[(se.all$Scale %in% places),]

dat.graph<-merge(mean.graph, se.graph, by="Scale")
names(dat.graph)[2:3]<-c("mean", "se")

p<-ggplot(dat.graph, aes(x=Scale, y=mean))+
  geom_bar(stat="identity", color="black", fill="orange")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  labs(y=yname, x="", title=fishgroups[i])+
  theme_light()+
  theme(text = element_text(size=18), 
        axis.text.x=element_text(size=16, colour="black"))

filename=paste("means_", yname, "_", fishgroups[i], ".pdf", sep="")
setwd("~/Analyses_notGit/fish-vaisu/output")
pdf(file=filename)
print(p)
dev.off()
}
