#On the move
library(tidyverse)
dataApple <- read.csv("data/applemobilitytrends-2020-05-14.csv")
policy <- read.csv("data/policy.csv")
gdata <- read.csv("data/google_mobility_Data.csv")


df<- dataApple


#Start here
#Overall trend

df <- df %>% select(- c(geo_type , alternative_name)) %>% pivot_longer(- c(region, transportation_type), names_to = "date", values_to = "value")
gdata <- gdata %>% pivot_longer(- c(country_region_code:date), names_to = "move_to", values_to = "value")

df$date <- str_remove(df$date, "X") 
df$date <- gsub(df$date, pattern = "\\.", replacement = "-")
df$date <- as.Date(df$date)


gdata$date <- as.Date(gdata$date)

#basic geom

ggplot(df, aes(date, value)) + geom_line() + geom_smooth()

# Get continent level data for multiple line chart
library(countrycode)
df$continent <- countrycode(sourcevar = df$region,
                            origin = "country.name",
                            destination = "continent")

ggplot(df, aes(date, value, color = continent)) + geom_line() + geom_smooth()

#Checking for UK
df %>% filter(region == "UK") %>%   
  ggplot( ., aes(date, value, color = transportation_type)) + geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) + ggpretty()

df %>% filter(region == "Germany") %>%   
  ggplot( ., aes(date, value, color = transportation_type)) + geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) + ggpretty()


df %>% filter(region == "Sweden") %>%   
  ggplot( ., aes(date, value, color = transportation_type)) + geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) 



#Gdata
gdata %>% filter(country_region == "Sweden") %>%   
  ggplot( ., aes(date, value, color = move_to)) + geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) 

gdata %>% filter(country_region == "United Kingdom") %>%   
  ggplot( ., aes(date, value, color = move_to)) + geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) 


gdata %>% filter(country_region == "United States") %>%   
  ggplot( ., aes(date, value, color = move_to)) + geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) 


gdata %>% filter(country_region == "India") %>%   
  ggplot( ., aes(date, value, color = move_to)) + geom_line() +
    geom_smooth(method = "loess") +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") )) + ggpretty()

gdata %>% 
  ggplot( ., aes(date, value, color = move_to))  +
  geom_smooth() +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-13") ))

+ ggpretty()


temp <- policy %>% filter(CountryName == "United Kingdom" & C4_Restrictions.on.gatherings == 4)
