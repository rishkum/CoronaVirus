#Democracy Vs Corona
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(coronavirus)

#For Ggplot arrange
library(ggpubr)

#From the Econmist Times Democracy Index
dfDemocracy <-  read.csv("democracyIndex.csv") 
dfLockdown   <- read.csv("lockdownByCountry.csv")
dfCorona  <-    read.csv("owid-covid-data.csv")
dfTemperature <- read.csv("data/Average_temperature.csv")
dfMedianAge <- read.csv("data/Median_age.csv")
dfGdpPPP <- read.csv("data/GDP PPP.csv")

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

##NEED to tidy this groph
mapDemoData <- left_join(mapDemoData, world_map, by = "region")
ggplot(mapDemoData, aes(long, lat, group = group))+
  geom_polygon(aes(fill = value ), color = "white")+
  scale_fill_viridis_c(option = "C") + theme_void()

#Start Playing with the data
#Graphs needed
# DemScores vs Corona Casescc
dfCorona$Country <- dfCorona$location
dfDemocracy <- left_join(dfDemocracy, dfCorona, by = "Country")

# Dataset for the first case
# Note the weird rename implementation, location was the old name
dfFirstCase <- dfCorona %>% 
  group_by(Country) %>% 
  filter(total_cases > 0) %>% 
  slice(which.min(total_cases)) %>%  rename(Countries.and.territories = location, FirstDate = date )



dfLockdown <- dfLockdown %>% filter( Level == "National" )
dfLockdown <- left_join(dfLockdown, dfFirstCase, by = "Countries.and.territories")
dfLockdown$DaysPast <- as.POSIXct( dfLockdown$Start.date, format = '%d/%m/%Y' ) - 
                       as.POSIXct( dfLockdown$FirstDate )


dfLockdown <- left_join(dfLockdown, dfDemocracy, by = "Country")

ggplot(dfLockdown, aes(Score, DaysPast)) + geom_line(color = "#FC4E07", size = 2) + theme_classic()


# Cases per million vs the number of days past
dfTotalCases <- dfCorona %>% 
  group_by(Country) %>% 
  slice(which.max(total_cases)) %>% 
  rename( final_total_cases_per_million = total_cases_per_million,
          final_total_cases             = total_cases,
          final_total_deaths_per_million = total_deaths_per_million,
          final_total_deaths             = total_deaths)


#Add the dataset with final cases
dfLockdown <- left_join(dfLockdown, dfTotalCases[,c("Country","final_total_deaths",
                                                    "final_total_cases", "final_total_deaths_per_million",
                                                    "final_total_cases_per_million")], by = "Country")

# Remove duplicated countires
dfLockdown <- dfLockdown[!duplicated(dfLockdown$Countries.and.territories), ]

# Graph as a line chart
ggplot(dfLockdown, aes(DaysPast, final_total_cases_per_million)) +
  geom_line(color = "#FC4E07", size = 2) + theme_classic()


#Graph as a dot plot to show days past vs finanl cases
# This is to see if days past have a significant impact of final cases permillion
#Color was added to see if democracies act differently
# Remove San Marino - outlier
rownames(dfLockdown) <- dfLockdown$Countries.and.territories

dfLockdown <- dfLockdown %>% filter(!Countries.and.territories == "San Marino")
dfLockdown$DaysPast <- as.numeric(dfLockdown$DaysPast)

ggplot(dfLockdown, aes(DaysPast, final_total_cases_per_million, label = rownames(dfLockdown))) +
  geom_point() + geom_smooth(method=lm) +geom_text()

ggplot(dfLockdown, aes(DaysPast, final_total_deaths_per_million, label = rownames(dfLockdown))) +
  geom_point() + geom_smooth(method=lm) +geom_text()


#visualising the relationship between democracies and covid cases
# Graph No. 1 and Graph No 2
G1 <- ggplot(dfLockdown, aes(Score, final_total_deaths_per_million, color = Regimetype )) + geom_point() +
  theme_light() + labs(title = "Total Deaths per mil and Democracy levels",
                       subtitle = "",
                       x = "Democracy Score",
                       y = "Total Deaths per million",
                       colour = "Regime Type")


G2 <- ggplot(dfLockdown, aes(Score, final_total_cases_per_million, color = Regimetype )) + geom_point() +
  theme_light() +  labs(title = "Total cases per mil and Democracy levels",
                           subtitle = "",
                           x = "Democracy Score",
                           y = "Total Cases per million",
                           colour = "Regime Type")

G1.2 <- ggarrange(G1, G2 , common.legend = TRUE, widths = "1080") 

#Add a main Title
G1.2 <- annotate_figure(G1.2, top = text_grob("Are Democracies weak in the time of covid?",
                                              face = "bold", size = 16), 
                        bottom = text_grob("Sources: The Economist, Ocid Data",
                                           face = "italic", size = 9))

#Save it
ggsave(plot = G1.2, filename = "DemoVsCovid.png", device = "png",  path = "Plots/")

library(ggpmisc)
# Do democracies implement lockdown quicker 
dfLockdown %>% filter(!is.na(Regimetype)) %>% ggplot(., aes(Score, DaysPast, color = Regimetype )) + geom_point() +
  geom_smooth(method="lm",  aes(group=1)) + scale_color_brewer(palette="Set1") +
  theme_light() + 
  stat_poly_eq(formula = DaysPast ~ Score,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(title = "Do democracies implement lockdown quicker?",
                        subtitle = "",
                        y = "No of days to lockdown after 1st case ",
                        x = "Democracy Level",
                        colour = "Regime Type")

dfLockdown %>% filter(!is.na(Regimetype)) %>% ggplot(., aes(DaysPast, final_total_cases_per_million, color = Regimetype )) + geom_point() +
  geom_smooth(method="lm", se=F, aes(group=1)) + scale_color_brewer(palette="Accent")+  theme_light() + 
  labs(title = "Total cases per mil and Democracy levels",
       subtitle = "", 
       x = "No of days to lockdown after 1st case ",
       y = "Total Cases per million",
       colour = "Regime Type")

############ Modelling ################
# What things that matter
# Regression with dep var: Cases, Death
# Independent Var: Lock down time, Region, gdp per person, Average Age, Democracy Levels, Temperature?
colnames(dfMedianAge)[1] <- "Country"
colnames(dfGdpPPP)[2] <- "Country"
dfLockdown2 <- left_join(dfLockdown, dfTemperature, by = "Country") %>%
  left_join(., dfMedianAge, by = "Country") %>% left_join(., dfGdpPPP, by = "Country")

dfLockdown2$Int. <- as.numeric(dfLockdown2$Int)
dfLockdown2$Average.yearly.temperature..1961.1990..degrees.Celsius. <- as.numeric(dfLockdown2$Average.yearly.temperature..1961.1990..degrees.Celsius.)



daysPastModel1 <- lm( final_total_cases_per_million ~ Score + DaysPast +`Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        `Median.Years.` + Int. + Score:DaysPast , data = dfLockdown2)

summary(daysPastModel1)
vif(daysPastModel1)


daysPastModel1 <- lm( final_total_cases_per_million ~ Score , data = dfLockdown2)
daysPastModel1 <- lm( final_total_cases_per_million ~ Score + DaysPast , data = dfLockdown2)
daysPastModel1 <- lm( final_total_cases_per_million ~ Score + DaysPast + `Int.`   , data = dfLockdown2)
daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.`   , data = dfLockdown2)

daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  `Int.` +
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.`, data = dfLockdown2)

daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  `Int.` +
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        Score:`Median.Years.` +
                        DaysPast, data = dfLockdown2)

daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  
                        `Int.` + 
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                          Score:`Median.Years.` + Score:DaysPast +
                        DaysPast, data = dfLockdown2)


daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  
                        `Int.` + I(`Median.Years.`^2 )+
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        DaysPast, data = dfLockdown2)
summary(daysPastModel1)
vif(daysPastModel1)


summary(daysPastModel1)
vif(daysPastModel1)

summary(daysPastModel1)
vif(daysPastModel1)






daysPastModel1 <- lm( final_total_deaths_per_million ~ Regimetype + DaysPast +`Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        `Median.Years.` + Int. + Regimetype:DaysPast , data = dfLockdown2)

daysPastModel1 <- lm( final_total_deaths_per_million ~ Regimetype + DaysPast +`Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        `Median.Years.`  + Regimetype:DaysPast , data = dfLockdown2)


daysPastModel1 <- lm( final_total_cases_per_million ~  DaysPast +`Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        `Median.Years.` + `Int.` , data = dfLockdown2)


daysPastModel2 <- lm( final_total_cases_per_million ~ Regimetype+ DaysPast +`Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        `Median.Years.` + `Int.` + `Int.`:Regimetype, data = dfLockdown2)


library("arm")
a <- coefplot(daysPastModel1, xlim=c(-2, 10), col.pts="red",  intercept=F)

daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  `Int.` +
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        Score:`Median.Years.` +
                        DaysPast, data = dfLockdown2)
dfLockdown3 <- dfLockdown2 
dfLockdown3$Score <- dfLockdown3$Score * 10
#dfLockdown3$Median.Years. <- dfLockdown3$Median.Years. / 10
summary(daysPastModel1)



daysPastModel1 <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  `Int.` +
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        Score:`Median.Years.` +
                        DaysPast, data = dfLockdown3)
daysPastModel1 <- lm( final_total_cases_per_million ~ Score   +  `Int.` +
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        Score:`Median.Years.` +
                        DaysPast, data = dfLockdown3)

daysPastModel1 <- lm( final_total_cases_per_million ~ Score   +  `Int.` + `Median.Years.`+
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        Score:`Median.Years.` +
                        DaysPast, data = dfLockdown3)
daysPastModel1 <- lm( final_total_cases_per_million ~ Score   +  `Int.` + `Median.Years.`+
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        Score:`Median.Years.` +
                        DaysPast, data = dfLockdown3)
dfLockdown3$continents

library(GGally)
ggcoef(
  daysPastModel1, exponentiate = F, exclude_intercept = TRUE,
   color = "blue", sort = "ascending", conf.level = .95, 
) + theme_light()



corr(dfLockdown2$Score, dfLockdown2$final_total_cases_per_million, )


cor(dfLockdown2$Score, dfLockdown2$final_total_cases_per_million,
    method = c("pearson", "kendall", "spearman"), use = "pairwise")


cor.test(dfLockdown2$Score, dfLockdown2$final_total_cases_per_million,
    method = c("pearson", "kendall", "spearman"), use = "pairwise")


cor(dfLockdown2$Median.Years., dfLockdown2$final_total_cases_per_million,
    method = c("pearson", "kendall", "spearman"), use = "pairwise")


cor.test(dfLockdown2$Median.Years., dfLockdown2$final_total_cases_per_million,
         method = c("pearson", "kendall", "spearman"), use = "pairwise")

library(simpleboot)
## Resample residuals
lboot2 <- lm.boot(daysPastModel1, R = 1000, rows = FALSE)
lboot2 <- summary(lboot2)
lboot2

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}
library(boot)
results <- boot(data=dfLockdown2, statistic=bs, 
                R=1000, formula=final_total_cases_per_million ~  DaysPast +`Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                  `Median.Years.` + `Int.`)




boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # wt 
boot.ci(results, type="bca", index=5) # disp


coeft

summary(daysPastModel1)
results





# Dem scores vs deaths
# Corona cases vs gdp
# Corona Cases vs Gini



