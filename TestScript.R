#load packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, ggrepel, directlabels, ggcorrplot, ggalt) 
library(ggrepel)
library(directlabels)
library(tidyr)
library(ggcorrplot)
library(ggalt)
library(forcats)
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
#import master mac
dataMasterFile <- import("/Users/jstein/Desktop/R/dataMasterFile.csv")

#import master Windows
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


dataAll1990 <- dataMasterFile %>% filter(Year %in% (1990:2022) )
#plot sources of Electricity over time(Renewables, Renewables+Nuclar, Fossil )
ggplot(data=dataAll1990, aes(x=factor(Year), y=`Renewables (% electricity)`, group = 1)) + 
  stat_summary(fun=mean, geom="line", size = 1) + 
  stat_summary(aes(x=factor(Year), y=`Low-carbon electricity (% electricity)`), fun = mean, geom = 'line', size = 1,  group=1, colour="#dc322f") + 
  stat_summary(aes(x=factor(Year), y=100-`Low-carbon electricity (% electricity)`), fun = mean, geom = 'line', size = 1,  group=1, colour="#2aa198") + 
  geom_text(aes(x = 6, y = 20, label = "Erneurbare",  size=20)) + 
  geom_text(aes(x = 6, y = 40, label = "Erneurberare + Nuklear")) +
  geom_text(aes(x = 15, y = 65, label = "Fossile Energie Träger")) +
  theme(legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#plot sources of Electricity over time(all seperat, all Eu Countries)
dataWide <- gather(dataMasterFile, Type, percent, `Nuclear (% electricity)`:`Bioenergy (% electricity)`, factor_key=TRUE)
dataWide <- dataWide %>% filter(Year %in% (1990:2022) )
dataGroup <- dataWide %>% group_by(Year, Type) %>% summarise(mean= mean(percent))

ggplot(data=dataGroup, aes(x=factor(Year), y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  geom_line(data = dataGroup %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#dc322f") +
  geom_line(data = dataGroup %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#2aa198") +
  geom_line(data = dataGroup %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#b58900") +
  geom_line(data = dataGroup %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#6c71c4") +
  geom_line(data = dataGroup %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#d33682") +
  geom_line(data = dataGroup %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#49be25") +
  geom_line(data = dataGroup %>% filter(Type == "Bioenergy (% electricity)"), group = 1, col="#be4d25") +
  geom_label_repel(data = filter(dataGroup, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 5, force = 10) +
  geom_label_repel(data = filter(dataGroup, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2,force = 10) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.3, 0)) +
  labs(title="Strommix in allen heutigen EU Ländern", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Nuclear
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Nuclear (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Nuklear Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Coal
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Coal (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Kohle Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Gas
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Gas (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Gas Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Hydro
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Hydro (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Wasserkraft Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Solar
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Solar (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Solarenergie Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Wind
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Wind (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Windenergie Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Oil
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Oil (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Öl Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")
#Bioenergy
ggplot(data=dataGroup, aes(x=Year, y=mean)) + 
  geom_line(data = dataGroup %>% filter(Type == "Bioenergy (% electricity)"),  group = 1, col="#268bd2") +
  labs(title="Bioenergie Anteil im Strommix in allen heutigen EU Ländern", subtitle="1990-2022", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

#plot every Country by Source ordered
data2022 <- filter(dataMasterFile, Year==2022)
data2022$Entity <- factor(data2022$Entity, levels=as.character(data2022$Entity))

#order by Source

#Nuclear
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Nuclear (% electricity)`))
ggplot(data=data2022, aes(x=`Nuclear (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Nuklear Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Coal
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Coal (% electricity)`))
ggplot(data=data2022, aes(x=`Coal (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Kohle Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Gas
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Gas (% electricity)`))
ggplot(data=data2022, aes(x=`Gas (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Gas Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Hydro
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Hydro (% electricity)`))
ggplot(data=data2022, aes(x=`Hydro (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Wasserkraft Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Solar
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Solar (% electricity)`))
ggplot(data=data2022, aes(x=`Solar (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Solarenergie Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Wind
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Wind (% electricity)`))
ggplot(data=data2022, aes(x=`Wind (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Windenergie Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Oil
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Oil (% electricity)`))
ggplot(data=data2022, aes(x=`Oil (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Öl Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")
#Bioenergy
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Bioenergy (% electricity)`))
ggplot(data=data2022, aes(x=`Bioenergy (% electricity)`, y=Entity)) + 
  geom_point() +
  labs(title="Bioenergie Anteil im Strommix in allen heutigen EU Ländern", subtitle="Jahr: 2022", y="Land", x="% Anteil", caption="Quelle: ourWorldInData, BP & Ember")







#order after diff Low-carbon electricity
data2022 <- filter(dataMasterFile, Year==2022)
data2022$Entity <- factor(data2022$Entity, levels=as.character(data2022$Entity))
data2022 <- data2022 %>%
  mutate(diff = `Low-carbon electricity (% electricity)` - data1990$`Low-carbon electricity (% electricity)`)
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, diff))
data1990 <- filter(dataMasterFile, Year==1990)
data1990$Entity <- factor(data1990$Entity, levels=as.character(data1990$Entity))
data1990 <- data1990 %>%
  mutate(diff = `Low-carbon electricity (% electricity)` - data1990$`Low-carbon electricity (% electricity)`)
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, diff))

#plot dumbbell Low-carbon electricity
ggplot(data2022, aes(x=data1990$`Low-carbon electricity (% electricity)`, xend=`Low-carbon electricity (% electricity)`, y=Entity, group=Entity)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                colour_xend = "darkBlue") + 
  labs(x=NULL, y=NULL, title="Veränderung des Anteils von Co2 armen Energieträgern im Strommix", subtitle="1990 Vs. 2022 - Geordnet nach Wachstum (Größte Oben)", caption="Quelle: ourWorldInData, BP & Ember") 

#order after 1990 data Low-carbon electricity
data2022 <- filter(dataMasterFile, Year==2022)
data2022$Entity <- factor(data2022$Entity, levels=as.character(data2022$Entity))
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Low-carbon electricity (% electricity)`))
data1990 <- filter(dataMasterFile, Year==1990)
data1990$Entity <- factor(data1990$Entity, levels=as.character(data1990$Entity))
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Low-carbon electricity (% electricity)`))

#plot dumbbell Low-carbon electricity
ggplot(data2022, aes(x=data1990$`Low-carbon electricity (% electricity)`, xend=`Low-carbon electricity (% electricity)`, y=Entity, group=Entity)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                colour_xend = "darkBlue") + 
  labs(x=NULL, y=NULL, title="Veränderung des Anteils von Co2 armen Energieträgern im Strommix", subtitle="1990 Vs. 2022 - Geordnet nach heutigem Anteil (Größte Oben)", caption="Quelle: ourWorldInData, BP & Ember") 



#order after diff Low-carbon electricity
data2022 <- filter(dataMasterFile, Year==2022)
data2022$Entity <- factor(data2022$Entity, levels=as.character(data2022$Entity))
data2022 <- data2022 %>%
  mutate(diff = `Renewables (% electricity)` - data1990$`Renewables (% electricity)`)
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, diff))
data1990 <- filter(dataMasterFile, Year==1990)
data1990$Entity <- factor(data1990$Entity, levels=as.character(data1990$Entity))
data1990 <- data1990 %>%
  mutate(diff = `Renewables (% electricity)` - data1990$`Renewables (% electricity)`)
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, diff))

#plot dumbbell Low-carbon electricity
ggplot(data2022, aes(x=data1990$`Renewables (% electricity)`, xend=`Renewables (% electricity)`, y=Entity, group=Entity)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                colour_xend = "darkBlue") + 
  labs(x=NULL, y=NULL, title="Veränderung des Anteils von erneuerbaren Energieträgern im Strommix", subtitle="1990 Vs. 2022 - Geordnet nach Wachstum (Größte Oben)", caption="Quelle: ourWorldInData, BP & Ember") 

#order after 1990 data Low-carbon electricity
data2022 <- filter(dataMasterFile, Year==2022)
data2022$Entity <- factor(data2022$Entity, levels=as.character(data2022$Entity))
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Renewables (% electricity)`))
data1990 <- filter(dataMasterFile, Year==1990)
data1990$Entity <- factor(data1990$Entity, levels=as.character(data1990$Entity))
data2022 <- data2022 %>%
  mutate(Entity = fct_reorder(Entity, `Renewables (% electricity)`))

#plot dumbbell Low-carbon electricity
ggplot(data2022, aes(x=data1990$`Renewables (% electricity)`, xend=`Renewables (% electricity)`, y=Entity, group=Entity)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                colour_xend = "darkBlue") + 
  labs(x=NULL, y=NULL, title="Veränderung des Anteils von erneuerbaren Energieträgern im Strommix", subtitle="1990 Vs. 2022 - Geordnet nach heutigem Anteil (Größte Oben)", caption="Quelle: ourWorldInData, BP & Ember") 

#filter year 
data2021 <- filter(dataMasterFile, Year==2021)
dataCorr <- data2021[,-c(1,2,3,11,20:31)]
corr <- cor(dataCorr)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle")
#Compute Correlation between HDI and Low Carbon electricity 
cor(data2021$HDI_2021, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between HDI and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(HDI_2021), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$HDI_2021, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Korrelation zwischen Human Development Index und Co2 armen Energieträgern im Strommix", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Human Development Index", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between GDP and Low Carbon electricity 
cor(data2021$gdp_2021_milUSD, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between GDP and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(gdp_2021_milUSD), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$gdp_2021_milUSD, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Bruttoinlandsprodukt und Co2 armen Energieträgern im Strommix", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Bruttoinlandsprodukt (Millionen USD)", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between GDP per Capita and Low Carbon electricity 
cor(data2021$gdp_percapita_USD, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between GDP per Capita and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(gdp_percapita_USD), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$gdp_percapita_USD, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Bruttoinlandsprodukt pro Person und Co2 armen Energieträgern im Strommix", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Bruttoinlandsprodukt pro Person (USD)", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between latitude and Low Carbon electricity 
cor(data2021$latitude, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between latitude and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(latitude), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$latitude, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Breitengrad und Co2 armen Energieträgern in der im Strommix", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Breitengrad (Süden-Norden)", caption="Quelle: ourWorldInData, BP & Ember")

#Compute Correlation between longitude and Low Carbon electricity 
cor(data2021$longitude, data2021$`Low-carbon electricity (% electricity)`)
#plot Correlation between longitude and Low Carbon electricity 
ggplot(data=data2021, aes(x=factor(longitude), y=`Low-carbon electricity (% electricity)`, group = 1)) + 
  geom_point()+
  geom_smooth() + 
  geom_text_repel(aes(label = Entity)) +
  geom_text(aes(x = 20, y = 5, label = paste( "Korrelation: ", round(cor(data2021$longitude, data2021$`Low-carbon electricity (% electricity)`), digits=4)))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(title="Korrelation zwischen Längengrad und Co2 armen Energieträgern im Strommix", subtitle="Jahr: 2021", y="% Anteil Co2 armen Energieträgern ", x="Längengrad (Westen-Osten)", caption="Quelle: ourWorldInData, BP & Ember")


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
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = 5, force = 10) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)",Type!="Nuclear (% electricity)"), aes( label = Type), nudge_x = -5, force = 10) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Strommix in Östereich", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Belgien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Bulgarien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Kroatien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Zypern", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


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
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2, force = 5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Strommix in der Tschechischen Republik", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


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
  labs(title="Strommix in Dänemark", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


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
  labs(title="Strommix in Estland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Finnland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Frankreich", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Deutschland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt ", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")


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
  labs(title="Strommix in Griechenland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Ungarn", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Irland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Italien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Lettland", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Litauen", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Luxemburg", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Malta", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in den Niederlande", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Polen", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Portugal", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Rumänien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in der Slowakei", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Slowenien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  labs(title="Strommix in Spanien", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

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
  geom_label_repel(data = filter(dataWide, Year=="2022", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = 2, force = 10) +
  geom_label_repel(data = filter(dataWide, Year=="1990", Type!="Other renewables excluding bioenergy (% electricity)"), aes( label = Type), nudge_x = -2, force = 20) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), expand = c(0.2, 0)) +
  labs(title="Strommix in Schweden", subtitle="1990-2022 - Anteilig Vernachlässigbare Energieträger wurden entfernt", y="% Anteil", x="Jahr", caption="Quelle: ourWorldInData, BP & Ember")

