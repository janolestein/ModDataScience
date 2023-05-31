pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 
selc <- import("C:/Users/Jole/Documents/R/DataScience/Data/share-electricity-low-carbon.csv")
hdi <- import("C:/Users/Jole/Documents/R/DataScience/Data/human-development-index-hdi-by-country-2023.csv")
emix <- import("/Users/jstein/Desktop/R/share-elec-by-source.csv")
euCount <- import("/Users/jstein/Desktop/R/eu_countries.csv")




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

dataMasterFile <- merge(emix, euCount, by="Entity")
mergeTest2021 <- filter(dataMasterFile, Year == 2021)

ggplot(mergeTest2021, aes(mergeTest2021$Entity, mergeTest2021$`Coal (% electricity)`)) + 
  geom_point() + 
  labs(y = "Entity", x = "Coal")

write.csv(dataMasterFile, "/Users/jstein/Desktop/R/dataMasterFile.csv", row.names=FALSE)
