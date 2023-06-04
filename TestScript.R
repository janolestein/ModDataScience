#load packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, ggrepel, directlabels) 
library(ggrepel)
library(directlabels)
library(tidyr)
install.packages("devtools")
devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr('solarized',type = 'outer')
swatch()

ggthemr_reset()

#import data sets
emix <- import("C:/Users/Jole/Documents/R/DataScience/Data/share-elec-by-source.csv")
euCountData <- import("C:/Users/Jole/Documents/R/DataScience/Data/eu_countries.csv")
euCountOnly <- import("C:/Users/Jole/Documents/R/DataScience/Data/only_eu_countries.csv")
elecProdSource <- import("C:/Users/Jole/Documents/R/DataScience/Data/electricity-prod-source-stacked.csv")
perCapitaElecFossilNuclearRenewables <- import("C:/Users/Jole/Documents/R/DataScience/Data/per-capita-electricity-fossil-nuclear-renewables.csv")
shareElectricityLowCarbon <- import("C:/Users/Jole/Documents/R/DataScience/Data/share-electricity-low-carbon.csv")
nuclearRenewablesRlectricity <- import("C:/Users/Jole/Documents/R/DataScience/Data/nuclear-renewables-electricity.csv")
#import master file
dataMasterFile <- import("C:/Users/Jole/Documents/R/DataScience/Data/dataMasterFile.csv")







#merge Data Sets
dataMasterFile <- merge(x=emix, y=euCountOnly, by="Entity")
dataMasterFile <- merge(x=dataMasterFile, y=euCountData, by=c("Entity","Year"), all.x=TRUE)
dataMasterFile <- merge(x=dataMasterFile, y=elecProdSource, by=c("Entity","Year","Code"), all.x=TRUE)
dataMasterFile <- merge(x=dataMasterFile, y=perCapitaElecFossilNuclearRenewables, by=c("Entity","Year","Code"), all.x=TRUE)
dataMasterFile <- merge(x=dataMasterFile, y=shareElectricityLowCarbon, by=c("Entity","Year","Code"), all.x=TRUE)
dataMasterFile <- merge(x=dataMasterFile, y=nuclearRenewablesRlectricity, by=c("Entity","Year","Code","Nuclear (% electricity)"), all.x=TRUE)
mergeTest2021 <- filter(dataMasterFile, Year == 2021)
mergeTest2020 <- filter(dataMasterFile, Year == 2020)

str(dataMasterFile)

write.csv(dataMasterFile, "C:/Users/Jole/Documents/R/DataScience/Data/dataMasterFile.csv", row.names=FALSE)


########################################plotting########################################



#plot sources of Electricity over time
ggplot(data=dataMasterFile, aes(x=factor(Year), y=`Renewables (% electricity)`, group = 1)) + 
  stat_summary(fun=mean, geom="line", size = 1) + 
  stat_summary(aes(x=factor(Year), y=`Low-carbon electricity (% electricity)`), fun = mean, geom = 'line', size = 1,  group=1, colour="#dc322f") + 
  stat_summary(aes(x=factor(Year), y=100-`Low-carbon electricity (% electricity)`), fun = mean, geom = 'line', size = 1,  group=1, colour="#2aa198") + 
  geom_text(aes(x = 6, y = 20, label = "Erneurbare",  size=20)) + 
  geom_text(aes(x = 6, y = 40, label = "Erneurberare + Nuklear", size=20)) +
  geom_text(aes(x = 15, y = 65, label = "Fossile Energie Träger", size=20)) +
  theme(legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Energie Quelle in der Stromerzeugung aller heutigen EU Mitglieder", subtitle="1985-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


data2021 <- filter(dataMasterFile, Year==2021)

#Compute Correlation between HDI and Low Carbon electricity 
cor(data2021$HDI_2021, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between HDI and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(HDI_2021), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Korrelation zwischen Human Development Index und Co2 armen Energieträgern", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Human Development Index", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between GDP and Low Carbon electricity 
cor(data2021$gdp_2021_milUSD, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between GDP and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(gdp_2021_milUSD), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Bruttoinlandsprodukt und Co2 armen Energieträgern", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Bruttoinlandsprodukt", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between GDP per Capita and Low Carbon electricity 
cor(data2021$gdp_percapita_USD, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between GDP per Capita and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(gdp_percapita_USD), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Bruttoinlandsprodukt pro Person und Co2 armen Energieträgern", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Bruttoinlandsprodukt pro Person", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between latitude and Low Carbon electricity 
cor(data2021$latitude, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between latitude and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(latitude), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen latitude und Co2 armen Energieträgern", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="latitude", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between longitude and Low Carbon electricity 
cor(data2021$longitude, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between longitude and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(longitude), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen longitude und Co2 armen Energieträgern", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="longitude", caption="Quelle: ourWorldInData, BP & Ember")


########################################################Countries############################################################

dataTest <- filter(dataMasterFile, Entity=="Germany")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataNucluear <- filter(dataWide, Type == "Nuclear (% electricity)")
dataBio <- filter(dataWide, Type == "Bioenergy (% electricity)")
#plot sources of Electricity over time
ggplot(data=dataWide, aes(x=factor(Year), y=percent), col=Type) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1) +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1) +
  geom_dl(aes(label = Type), method = list("last.bumpup")) +
  geom_dl(aes(label = Type), method = list("first.bumpup")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.3, 0)) +
  labs(title="Energie Quelle in der Stromerzeugung aller heutigen EU Mitglieder", subtitle="1985-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
