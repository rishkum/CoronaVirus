#Democracy Vs Corona
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(coronavirus)
library(extrafont)
loadfonts(device = "mac")
theme_rk <- function (base_size = 11, base_family = "Avenir", base_line_size = base_size/22, 
          base_rect_size = base_size/22) 
  
  {half_line <- base_size/2
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey70", size = rel(1)), 
          panel.grid = element_line(colour = "grey87"), 
          panel.grid.major = element_line(size = rel(0.5)), 
          panel.grid.minor = element_line(size = rel(0.25)), 
          axis.ticks = element_line(colour = "grey70", size = rel(0.5)), 
          legend.key = element_rect(fill = "white", colour = NA), 
          strip.background = element_rect(fill = "grey70", 
                                          colour = NA), 
          strip.text = element_text(colour = "white", size = rel(0.8), margin = margin(0.8 * half_line, 
                                                                                       0.8 * half_line, 
                                                                                       0.8 * half_line,
                                                                                       0.8 * half_line)), 
          complete = TRUE,
          plot.title = element_text(color = "black", size = 13, face = "bold", hjust = 0),
          plot.subtitle = element_text(color = "dark grey", hjust = 0),
          plot.title.position = "plot", 
          plot.caption = element_text(color = "grey79", hjust = 1,   face = "italic"))
}


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
G1 <- dfLockdown %>% filter(!is.na(Regimetype)) %>% ggplot(., aes(Score, final_total_deaths_per_million, color = Regimetype )) + geom_point() +
  theme_rk() + labs(title = "Total Deaths per mil and Democracy levels",
                       subtitle = "",
                       x = "Democracy Index",
                       y = "Total Deaths per million",
                       colour = "Regime Type")


G2 <- dfLockdown %>% filter(!is.na(Regimetype)) %>% ggplot(., aes(Score, final_total_cases_per_million, color = Regimetype )) + geom_point() +
  theme_rk() +  labs(title = "Total cases per mil and Democracy levels",
                           subtitle = "",
                           x = "Democracy Index",
                           y = "Total Cases per million",
                           colour = "Regime Type")

G1.2 <- ggarrange(G1, G2 , common.legend = TRUE, widths = 1080) 

#Add a main Title
G1.2 <- annotate_figure(G1.2, top = text_grob("Are Democracies weak in the time of covid?",
                                              face = "bold", size = 16, hjust =.70), 
                        bottom = text_grob("Sources: The Economist, Ocid Data",
                                           face = "italic", size = 9))

#Save it
ggsave(plot = G1.2, filename = "DemoVsCovid.png", device = "png",  path = "Plots/")
library(ggpubr)
library(ggpmisc)
f1 <- (DaysPast ~ Score)

# Do democracies implement lockdown quicker 
plot.lockdown.implementation <-  dfLockdown %>% filter(!is.na(Regimetype)) %>% ggplot(., aes( y =Score, DaysPast, color = Regimetype )) + geom_point() +
  geom_smooth(method="lm", formula = y~x, aes(group=1)) + scale_color_brewer(palette="Set1") +
  theme_rk()+
  stat_cor(aes(group = 1), cor.coef.name = "R", label.x.npc = 0.75, 
           label.y.npc = 0.01, hjust = 0 ) +
  labs(title = "Did democracies took too much time in implementing lockdown?",element_text(colour = "#FFFF"),
                        subtitle = paste("\nDemocracies index on y axis and number of days countries took to impose some kind of lockdown\non y axis. Even though there is some positive relationship among these variables, this relationship \nis weak and statistically insignificant.\n"),
                        x = "No of days to lockdown after 1st case \n",
                        y = "Democracy Index ",
       caption = "\nSee github/rishkum/coronavirus for datasets ",
                        colour = "Regime Type")





plot.lockdown.cases.effects <-   dfLockdown %>% filter(!is.na(Regimetype)) %>% ggplot(., aes(DaysPast, final_total_cases_per_million, color = Regimetype )) + geom_point() +
  geom_smooth(method="lm", se=F, aes(group=1)) + scale_color_brewer(palette="Accent")+  theme_rk() + 
  stat_cor(aes(group = 1), cor.coef.name = "R", label.x.npc = 0.75, 
           label.y.npc = 0.01, hjust = 0 ) +
  labs(title = "A stich in time saves nine? ",
       subtitle = "Total cases per million are seen to be higher in countires that impose lockdown quickly. \nThe correlation is positive but weak as there are countries who imposed the
lockdown later but still had lower cases.\n", 
       x = "No of days to lockdown after 1st case ",
       y = "Total Cases per million",
       caption = "\nSee github/rishkum/coronavirus for datasets",
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

#USE this model for the short version
M.total.cases.just.score <- lm( final_total_cases_per_million ~ Score , data = dfLockdown3)
plot.M.total.cases.just.score <- ggcoef(
  final_total_cases_per_million, exponentiate = F, exclude_intercept = TRUE,
  color = "blue", sort = "ascending", conf.level = .95, 
) + theme_rk()


#Model for the long version
M.total.cases.just.everything <- lm( final_total_cases_per_million ~ Score  + `Median.Years.` +  
                        `Int.` +
                        `Average.yearly.temperature..1961.1990..degrees.Celsius.` +
                        DaysPast + `Median.Years.`:Score, data = dfLockdown3)

plot.M.total.cases.just.everything <- ggcoef(
  M.total.cases.just.everything, exponentiate = F, exclude_intercept = TRUE,
  color = "blue", sort = "ascending", conf.level = .95, 
) + theme_rk()


#Creating the Multi plot
library(coefplot)
 multiplot(M.total.cases.just.everything,
                        M.total.cases.just.score, intercept = F ,title = "In a complex world just by being a democracy does not mean higher cases ",
           horizontal = T, sort = "magnitude",alpha = .05, xlab = "No of COVID-19 cases",
           newNames = c(M.total.cases.just.everything = "With Controls",
             `Median.Years.` = "Median\n Years",
                                  `Int.` = "GDP \nPer \nperson",
                                  `Average.yearly.temperature..1961.1990..degrees.Celsius.` = "Average \nTemperature",
                                  `Score` = "Democracy \nIndex"    , 
                                  `Score:Median.Years.` = "Interaction\n Median\n Age:Democracy \nIndex"  ,  
                                  DaysPast = "No of days \nto lockdown \npost 1st case")) + theme_rk() +
          theme(legend.position = "top") + coord_cartesian(xlim = c(-300, 150)) + coord_flip() + 
   scale_color_manual(values=c("pink","light blue"), labels = c("With Controls", "Just Democracy \nIndex")) +
   labs(subtitle = str_c("\nCovid cases per million and democratic index has a positive relationship but this dies down\n"
                          ,"when other factors are considered. When we consider the interaction between democratic \n",
                          "index and median years then a positive statistically significant effect is found. This implies\n", 
                         "that cases are higher if you in are a democratic nation with older population.\n" ),
        caption = "\nSee github/rishkum/coronavirus for datasets",
        y = "Predicting Variables")





# Dem scores vs deaths
# Corona cases vs gdp
# Corona Cases vs Gini



