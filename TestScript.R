pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 
selc <- import("C:/Users/Jole/Documents/R/DataScience/Data/share-electricity-low-carbon.csv")
hdi <- import("C:/Users/Jole/Documents/R/DataScience/Data/human-development-index-hdi-by-country-2023.csv")
emix <- import("/Users/jstein/Desktop/R/share-elec-by-source.csv")
euCount <- import("/Users/jstein/Desktop/R/eu_countries.csv")
plot(selc)
hist(selc)

hc <- selc %>%  
      dist    %>%  # Compute distance/dissimilarity matrix
      hclust      # Computer hierarchical clusters

plot(hc)          # Plot dendrogram

plot(selc$Entity)  # Categorical variable
plot(selc$Year)  # Quantitative variable
plot(selc$Entity, selc$Year)  # Cat x quant
plot(iris$Petal.Length, iris$Petal.Width)  # Quant pair
plot(iris)  # Entire data frame

?plot

# Plot with options
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",  # Hex code for datalab.cc red
     pch = 19,         # Use solid circles for points
     main = "Iris: Petal Length vs. Petal Width",
     xlab = "Petal Length",
     ylab = "Petal Width")

describe(selc)

hist(selc)

plot(selc$Entity, selc$Year)

plot(selc$Year, selc$Entity) 

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

mergeTest <- merge(emix, euCount, by="Entity")
mergeTest2021 <- filter(mergeTest, Year == 2021)

ggplot(mergeTest2021, aes(mergeTest2021$Entity, mergeTest2021$`Coal (% electricity)`)) + 
  geom_point() + 
  labs(y = "Entity", x = "Coal")
