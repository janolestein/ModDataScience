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
dataMasterFile <- import("/Users/jstein/Desktop/R/dataMasterFile.csv")







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


dataAll1990 <- dataMasterFile %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time
ggplot(data=dataAll1990, aes(x=factor(Year), y=`Renewables (% electricity)`, group = 1)) + 
  stat_summary(fun=mean, geom="line", size = 1) + 
  stat_summary(aes(x=factor(Year), y=`Low-carbon electricity (% electricity)`), fun = mean, geom = 'line', size = 1,  group=1, colour="#dc322f") + 
  stat_summary(aes(x=factor(Year), y=100-`Low-carbon electricity (% electricity)`), fun = mean, geom = 'line', size = 1,  group=1, colour="#2aa198") + 
  geom_text(aes(x = 6, y = 20, label = "Erneurbare",  size=20)) + 
  geom_text(aes(x = 6, y = 40, label = "Erneurberare + Nuklear")) +
  geom_text(aes(x = 15, y = 65, label = "Fossile Energie Träger")) +
  theme(legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Energie Quelle in der Stromerzeugung aller heutigen EU Mitglieder", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


data2021 <- filter(dataMasterFile, Year==2021)

#Compute Correlation between HDI and Low Carbon electricity 
cor(data2021$HDI_2021, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between HDI and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(HDI_2021), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$HDI_2021, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Korrelation zwischen Human Development Index und Co2 armen Energieträgern in der Stromerzeugung", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Human Development Index", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between GDP and Low Carbon electricity 
cor(data2021$gdp_2021_milUSD, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between GDP and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(gdp_2021_milUSD), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$gdp_2021_milUSD, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Bruttoinlandsprodukt und Co2 armen Energieträgern in der Stromerzeugung", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Bruttoinlandsprodukt (Millionen USD)", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between GDP per Capita and Low Carbon electricity 
cor(data2021$gdp_percapita_USD, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between GDP per Capita and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(gdp_percapita_USD), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$gdp_percapita_USD, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Bruttoinlandsprodukt pro Person und Co2 armen Energieträgern in der Stromerzeugung", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Bruttoinlandsprodukt pro Person (USD)", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between latitude and Low Carbon electricity 
cor(data2021$latitude, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between latitude and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(latitude), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$latitude, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Breitengrad und Co2 armen Energieträgern in der Stromerzeugung", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Breitengrad (Süden-Norden)", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between longitude and Low Carbon electricity 
cor(data2021$longitude, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between longitude and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(longitude), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$longitude, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Längengrad und Co2 armen Energieträgern in der Stromerzeugung", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Längengrad (Westen-Osten)", caption="Quelle: ourWorldInData, BP & Ember")


########################################################Countries############################################################
#Austria
dataTest <- filter(dataMasterFile, Entity=="Austria")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Austria
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Östereich", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Belgium
dataTest <- filter(dataMasterFile, Entity=="Belgium")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Belgium
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#be4d25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Hydro (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Hydro (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Belgien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Bulgaria
dataTest <- filter(dataMasterFile, Entity=="Bulgaria")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Bulgaria
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Bulgarien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Croatia
dataTest <- filter(dataMasterFile, Entity=="Croatia")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Croatia
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Kroatien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Cyprus
dataTest <- filter(dataMasterFile, Entity=="Cyprus")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Cyprus
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)", Type!="Hydro (% electricity)",Type!="Gas (% electricity)",Type!="Coal (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)", Type!="Hydro (% electricity)",Type!="Gas (% electricity)",Type!="Coal (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Zypern", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


#Czechia
dataTest <- filter(dataMasterFile, Entity=="Czechia")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Czechia
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022",Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in der Tschechischen Republik", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


#Denmark
dataTest <- filter(dataMasterFile, Entity=="Denmark")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Denmark
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)", Type!="Hydro (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)", Type!="Hydro (% electricity)"), aes( label = Type), nudge_x = -2, force = 5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Dänemark", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


#Estonia
dataTest <- filter(dataMasterFile, Entity=="Estonia")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Estonia
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)", Type!="Hydro (% electricity)",Type!="Gas (% electricity)",Type!="Coal (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)", Type!="Hydro (% electricity)",Type!="Gas (% electricity)",Type!="Coal (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Estland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Finland
dataTest <- filter(dataMasterFile, Entity=="Finland")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Finland
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Finnland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#France
dataTest <- filter(dataMasterFile, Entity=="France")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in France
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 10) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2, force = 5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Frankreich", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Deutschland
dataTest <- filter(dataMasterFile, Entity=="Germany")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Germany
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Deutschland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt ", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


#Greece
dataTest <- filter(dataMasterFile, Entity=="Greece")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Greece
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Griechenland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Hungary
dataTest <- filter(dataMasterFile, Entity=="Hungary")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Hungary
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Ungarn", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Ireland
dataTest <- filter(dataMasterFile, Entity=="Ireland")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Ireland
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)",Type!="Solar (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)",Type!="Solar (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Irland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Italy
dataTest <- filter(dataMasterFile, Entity=="Italy")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Italy
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Italien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Latvia
dataTest <- filter(dataMasterFile, Entity=="Latvia")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Latvia
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)",Type!="Coal (% electricity)",Type!="Oil (% electricity)",Type!="Solar (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)",Type!="Coal (% electricity)",Type!="Oil (% electricity)",Type!="Solar (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Lettland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Lithuania
dataTest <- filter(dataMasterFile, Entity=="Lithuania")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Lithuania
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Coal (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Coal (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Litauen", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Luxembourg
dataTest <- filter(dataMasterFile, Entity=="Luxembourg")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Luxembourg
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Coal (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Coal (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Luxemburg", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Malta
dataTest <- filter(dataMasterFile, Entity=="Malta")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Malta
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)", Type!="Hydro (% electricity)",Type!="Nuclear (% electricity)",Type!="Wind (% electricity)",Type!="Bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)", Type!="Hydro (% electricity)",Type!="Nuclear (% electricity)",Type!="Wind (% electricity)",Type!="Bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Malta", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Netherlands
dataTest <- filter(dataMasterFile, Entity=="Netherlands")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Netherlands
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)", Type!="Hydro (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)", Type!="Hydro (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Niederlande", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Poland
dataTest <- filter(dataMasterFile, Entity=="Poland")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Poland
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2, force = 10) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Polen", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Portugal
dataTest <- filter(dataMasterFile, Entity=="Portugal")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Portugal
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Portugal", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Romania
dataTest <- filter(dataMasterFile, Entity=="Romania")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Romania
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Rumänien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Slovakia
dataTest <- filter(dataMasterFile, Entity=="Slovakia")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Slovakia
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Wind (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Wind (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Slowakei", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Slovenia
dataTest <- filter(dataMasterFile, Entity=="Slovenia")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Slovenia
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Wind (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Wind (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Slowenien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Spain
dataTest <- filter(dataMasterFile, Entity=="Spain")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Spain
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Spanien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#Sweden
dataTest <- filter(dataMasterFile, Entity=="Sweden")
dataWide <- gather(dataTest, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time in Sweden
ggplot(data=dataWide, aes(x=factor(Year), y=percent)) + 
  geom_line(data = dataWide %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataWide %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataWide %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataWide %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataWide %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataWide %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataWide %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataWide %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 5) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2, force = 5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Energie Quellen bei der Stromerzeugung in Schweden", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

