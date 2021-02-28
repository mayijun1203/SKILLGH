
library(tidyverse)

#Information on APIs available at https://www.census.gov/data/developers/data-sets.html

#Pull in data on vacancies by tract, ACS 15-19
Vacancies <- read.csv(file="https://api.census.gov/data/2019/acs/acs5?get=NAME,group(B25004)&for=tract:*&in=state:36%20county:005,047,061,081,085")

#Clean file
names(Vacancies)[names(Vacancies) == "X..NAME"] <- "Name"
names(Vacancies)[names(Vacancies) == "tract."] <- "tract"
Vacancies$Name <- gsub("\\[", "", Vacancies$Name)
Vacancies$tract <- gsub("\\]", "", Vacancies$tract)

#Import crosswalk file for tracts to NTA
Tract2NTA <- read.csv("https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/RTract2010_to_NTA2010.csv")

#Create a variable for boro that matches tract to NTA crosswalk file
Vacancies$Boro <- 0
Vacancies$Boro[which(Vacancies$county==5)] <- 2
Vacancies$Boro[which(Vacancies$county==47)] <- 3
Vacancies$Boro[which(Vacancies$county==61)] <- 1
Vacancies$Boro[which(Vacancies$county==81)] <- 4
Vacancies$Boro[which(Vacancies$county==85)] <- 5

#Create BoroCT2010 variable to match tract to NTA crosswalk file
Vacancies$BoroCT2010 <- paste0(Vacancies$Boro,Vacancies$tract)

#Merge crosswalk file to ACS data
Vacancies <- merge(Vacancies, Tract2NTA, by="BoroCT2010")

#Aggregate ACS data to the NTA level
Vacancies <- aggregate(Vacancies[ ,c("B25004_001E", "B25004_002E", "B25004_003E", "B25004_004E", "B25004_005E", "B25004_006E", "B25004_007E", "B25004_008E")], by=list(Vacancies$NTACode), FUN=sum, na.rm=T)

#Calculate percent of vacants within each type
attach(Vacancies)
Vacancies$RentPer <- B25004_002E/B25004_001E*100
Vacancies$RentedPer <- B25004_003E/B25004_001E*100
Vacancies$SalePer <- B25004_004E/B25004_001E*100
Vacancies$SoldPer <- B25004_005E/B25004_001E*100
Vacancies$SeasonalPer <- B25004_006E/B25004_001E*100
Vacancies$MigrantPer <- B25004_007E/B25004_001E*100
Vacancies$OtherPer <- B25004_008E/B25004_001E*100
detach(Vacancies)

#Create a new dataset with just the variables of interest
VacanciesPer <- Vacancies[ ,c(1, 10:16)]

#Reshape file from wide to long format for graphing
VacanciesLong <- gather(VacanciesPer, Type, Percent, 2:8, factor_key=T)

#Rename variables
names(VacanciesLong) <- c("NTACode", "Type", "Percent")

#Create a variable for the borough by taking first two characters of NTA code
VacanciesLong$Boro <- substr(VacanciesLong$NTACode, 1, 2)

#Create a new dataset for just Manhattan
VacanciesLongMN <- VacanciesLong[which(VacanciesLong$Boro=="MN"),]

#Plot percent of vacants within each type for Manhattan NTAs as a stacked bar chart, NTAs side by side
VacanciesplotMNstacked <- ggplot(data=VacanciesLongMN) +
  geom_bar(aes(x=NTACode, y=Percent, fill=Type), position="stack", stat="identity") +
  scale_y_continuous(breaks=seq(0,100,20)) +
  scale_fill_brewer(palette="Accent", labels=c("For Rent", "Rented, Not Occupied", "For Sale Only", "Sold, Not Occupied", "For Seasonal, Recreational, or Occasional Use", "For Migrant Workers", "Other Vacant")) +
  labs(title="Manhattan: Percent of Vacancies by Type", x="Neighborhood Tabulation Area", y="Percent of All Vacants", fill="Type of Vacant") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, face="bold"), plot.title=element_text(hjust=0.5), legend.position="bottom")

VacanciesplotMNstacked

#Remove MN99
VacanciesLongMN2 <- VacanciesLongMN[VacanciesLongMN$NTACode!="MN99",]

#Plot percent of vacants within each type for Manhattan NTAs, with each NTA displayed separately
VacanciesplotMNFacet <- ggplot(data=VacanciesLongMN2) +
  geom_bar(aes(x=Type, y=Percent, fill=Type), stat="identity") +
  scale_y_continuous(breaks=seq(0,100,20)) +
  scale_fill_brewer(palette="Accent", labels=c("For Rent", "Rented, Not Occupied", "For Sale Only", "Sold, Not Occupied", "For Seasonal, Recreational, or Occasional Use", "For Migrant Workers", "Other Vacant")) +
  facet_wrap(vars(VacanciesLongMN2$NTACode), ncol=7) +
  labs(title="Manhattan: Percent of Vacancies by Type", x="Type of Vacancy", y="Percent of All Vacants", fill="Type of Vacant") +
  theme_bw() +
  theme(axis.text.x=element_blank(), plot.title=element_text(hjust=0.5), legend.position="bottom", axis.ticks.x=element_blank())

VacanciesplotMNFacet




