pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 

emix <- import("/Users/jstein/Desktop/R/share-elec-by-source.csv")
euCountData <- import("/Users/jstein/Desktop/R/eu_countries.csv")
euCountOnly <- import("/Users/jstein/Desktop/R/only_eu_countries.csv")
elecProdSource <- import("/Users/jstein/Desktop/R/electricity-prod-source-stacked.csv")
perCapitaElecFossilNuclearRenewables <- import("/Users/jstein/Desktop/R/per-capita-electricity-fossil-nuclear-renewables.csv")
shareElectricityLowCarbon <- import("/Users/jstein/Desktop/R/share-electricity-low-carbon.csv")
nuclearRenewablesRlectricity <- import("/Users/jstein/Desktop/R/nuclear-renewables-electricity.csv")

head(selc)

is.data.frame(selc)
str(selc)

head(hdi)

#filter out Continets
selcwocont <- filter(selc, Code != "")
head(selcwocont)

selc2000 <- filter(selcwocont, Year == 2000)
head(selc2000)

plot(selc2000$Entity, selc2000$`Low-carbon electricity (% electricity)`)

selcAndHdi <- merge(selcwocont, hdi, by="Entity")
head(selcAndHdi)


selcHdi2021 <- filter(selcAndHdi, Year == 2021)
head(selcHdi2000)
plot(selcHdi2000$hdiTier, selcAndHdi$`Low-carbon electricity (% electricity)`)

selcHdi2000 <- filter(selcHdi2000, hdi2021 != "")

write.csv(selcHdi2000, "C:/Users/Jole/Documents/R/DataScience/Data/selcHdi2000.csv", row.names=FALSE)

ggplot(selcHdi2000, aes(as.factor(selcHdi2000$hdiTier), selcHdi2000$`Low-carbon electricity (% electricity)`)) + 
  geom_boxplot() + 
  labs(y = "selc", x = "HDITIER")

ggplot(selcHdi2021, aes(selcHdi2021$hdi2021, selcHdi2021$`Low-carbon electricity (% electricity)`)) + 
  geom_point() + 
  labs(y = "selc", x = "HDITIER")




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



ggplot(mergeTest2021, aes(mergeTest2021$Entity, mergeTest2021$`Coal (% electricity)`)) + 
  geom_point() + 
  labs(y = "Entity", x = "Coal")

ggplot(data=dataMasterFile, aes(x=factor(Year), y=`Renewables (% electricity)`, group = 1)) + 
  stat_summary(fun=mean, geom="line") + 
  labs(y = "Year", x = "Renew")

write.csv(dataMasterFile, "/Users/jstein/Desktop/R/dataMasterFile.csv", row.names=FALSE)
