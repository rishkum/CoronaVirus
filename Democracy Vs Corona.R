#Democracy Vs Corona
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(coronavirus)
#From the Econmist Times Democracy Index
dfDemocracy <-  read.csv("democracyIndex.csv") 
dfLockdown   <- read.csv("lockdownByCountry.csv")
dfCorona  <-    read.csv("owid-covid-data.csv")


#Visualise the democracy index
summary(dfDemocracy)
ggplot(data=dfDemocracy, aes(x=Regimetype, y=Score, fill=Region)) +
  geom_bar(stat="identity", position=position_dodge()) + 
    theme_classic() + scale_fill_brewer( palette = "Paired" )

#Map the Democracy Index
require(maps)
require(viridis)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

mapDemoData <- dfDemocracy[,c("Country", "Score")]
colnames(mapDemoData) <- c("region", "value")
mapDemoData$region <- as.character(mapDemoData$region)
mapDemoData$region <-  ifelse(mapDemoData$region == "United States", "USA", mapDemoData$region)

##NEED to tody this groph
mapDemoData <- left_join(mapDemoData, world_map, by = "region")
ggplot(mapDemoData, aes(long, lat, group = group))+
  geom_polygon(aes(fill = value ), color = "white")+
  scale_fill_viridis_c(option = "C") + theme_void()

#Start Playing with the data
#Graphs needed
# DemScores vs Corona Casescc
# Dem scores vds deaths
