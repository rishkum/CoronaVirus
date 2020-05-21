#World on the move

# The aim to find out how different movement ways have changed across countries
# We should have 3 different regions
# World, UK, US 
# p is for plot 
# m if for models
# tvl, vst, fcst
#
# The questions that we want to answer
# 1. Are people traveling more and then what ways? - Apple Mobility Data 
# S: Change from baseline in a graph and numbers
# M: Plots x 3,  Change in absolute numbers before lockdown and now
# A: T
# R: T
# T: 1hr
#
# 2. Where are they going? - Google Mobility Data
# S: Change from baseline in a graph and numbers
# M: Plots x 3,  Change in absolute numbers before lockdown and now
# A: T
# R: T
# T: 1hr

# 3. Where are we heading
# 

library(tidyverse)
dataApple <- read.csv("data/applemobilitytrends-2020-05-14.csv")
policy <- read.csv("data/policy.csv")
gdata <- read.csv("data/google_mobility_Data.csv")


df<- dataApple
######## Start here ############
#Overall trend

df <- df %>% select(- c(geo_type , alternative_name)) %>% pivot_longer(- c(region, transportation_type), names_to = "date", values_to = "value")
gdata <- gdata %>% pivot_longer(- c(country_region_code:date), names_to = "move_to", values_to = "value")

df$date <- str_remove(df$date, "X") 
df$date <- gsub(df$date, pattern = "\\.", replacement = "-")
df$date <- as.Date(df$date)


gdata$date <- as.Date(gdata$date)

# basic geom ####

ggplot(df, aes(date, value)) + geom_line() + geom_smooth()

# Get continent level data for multiple line chart####

library(countrycode)
df$continent <- countrycode(sourcevar = df$region,
                            origin = "country.name",
                            destination = "continent")

ggplot(df, aes(date, value, color = continent)) + geom_line() + geom_smooth()


####### cleaning data sets #########
df <-   df %>% mutate(transportation_type = recode(transportation_type, 
                                                                'driving' = 'Driving',
                                                                'transit' = 'Transit',
                                                                'walking' = 'Walking'))

gdata <- gdata %>% mutate(transportation_type = recode(move_to, 
                                                       'retail_and_recreation_percent_change_from_baseline' = 'Retail and recreation',
                                                       'grocery_and_pharmacy_percent_change_from_baseline' = 'Grocery & Essentials',
                                                       'transit_stations_percent_change_from_baseline' = 'Public Trasport',
                                                        'workplaces_percent_change_from_baseline' = 'Workplace',
                                                       'parks_percent_change_from_baseline'       = 'Parks',
                                                         'residential_percent_change_from_baseline' = 'Home'))

####### 1. Are people traveling more and then what ways? - Apple Mobility Data #########

# 1. Checking for UK ###########

p.tvl.uk <- df %>% filter(region == "UK") %>%   
  ggplot( ., aes(as.Date(date), value, color = transportation_type)) + geom_line() +
  geom_smooth() + scale_y_continuous(labels = function(x) paste0(x-100, "%"), limits = c(0, 170)) +
  scale_colour_brewer(palette = "Set2") +
  guides(color=guide_legend(override.aes=list(fill=NA)))

# Pretty it
p.tvl.uk <- ggpretay(p.tvl.uk, title = "Is Britain on the move again?", 
         subtitle = "Data from Apple's mobility data suggests that people have started to travel again.
It does seem like people are shunning public transport usage is recovering at a lower pace.",
        xaxis = "Months", yaxis = "Change from baseline"  )

# Annote it
p.tvl.uk <- p.tvl.uk +
  #Adding vline
  geom_vline(xintercept = as.Date("2020-01-30"), 
             color = "dark gray", linetype = "dashed", show.legend = T) +
  #Curve
  geom_curve(curvature = -0.1, color = "dark gray",
             # Position , no quotes cause continous
             #0.03 is the size, curvature is the shape
             aes(x = as.Date("2020-01-30"), y = 160.00, xend = as.Date("2020-04-15"), yend = 160.00),
             arrow = arrow(length = unit(0.02, "npc"))) + 
  # text
  annotate("text", x = as.Date("2020-04-30"),
           y = 150,
           label = "WHO declares\ncoronavirus a pandemic",
           size = 3.5, color = "dark gray") 

#Save the plot
finalise_plot(p.tvl.uk, source_name = "Source: Apple Mobility Dataset", 
              save_filepath = "Plots/On_the_move/tvl_uk.png",
              logo_image_path = "Data/logo/rk_logo.png")


# 1. Checking for US ##########

p.tvl.us <- df %>% filter(region == "United States") %>%   
  ggplot( ., aes(as.Date(date), value, color = transportation_type)) + geom_line() +
  geom_smooth() + scale_y_continuous(labels = function(x) paste0(x-100, "%"), limits = c(0, 175)) +
  scale_colour_brewer(palette = "Set2") +
  guides(color=guide_legend(override.aes=list(fill=NA))) 

# Preety fy the graph
p.tvl.us <- ggpretay(p.tvl.us, title = "US seems to be on the move too", 
                     subtitle = "Data suggests that Americans have started to travel again.
Like in Britain, Americans are shunning public transport usage",
                     xaxis = "Months", yaxis = "Change from baseline"  )

# Annote the graph
p.tvl.usa <- p.tvl.us +
  #Adding vline
  geom_vline(xintercept = as.Date("2020-01-30"), 
             color = "dark gray", linetype = "dashed", show.legend = T) +
  #Curve
  geom_curve(curvature = -0.15, color = "dark gray",
             # Position , no quotes cause continous
             #0.03 is the size, curvature is the shape
             aes(x = as.Date("2020-01-30"), y = 160.00, xend = as.Date("2020-04-15"), yend = 160.00),
             arrow = arrow(length = unit(0.02, "npc"))) + 
  # text
  annotate("text", x = as.Date("2020-04-30"),
           y = 150,
           label = "WHO declares\ncoronavirus a pandemic",
           size = 3.5, color = "dark gray") 

  

#Save the plot
finalise_plot(p.tvl.usa, source_name = "Source: Apple Mobility Dataset", 
              save_filepath = "Plots/On_the_move/tvl_us.png",
              logo_image_path = "Data/logo/rk_logo.png")


# Checking for World 
p.tvl.wld <- df %>%   
  ggplot( ., aes(as.Date(date), value, color = transportation_type)) +
  geom_smooth() + scale_y_continuous(labels = function(x) paste0(x-100, "%"),  limits = c(0, 175)) +
  scale_colour_brewer(palette = "Set2") +
  guides(color=guide_legend(override.aes=list(fill=NA))) 

# Preety fy the graph
p.tvl.wld <- ggpretay(p.tvl.wld, title = "World is getting back on the move", 
                     subtitle = "Data suggests that globally people have started to travel again.
And the general trend is about shunning public transport",
                     xaxis = "Months", yaxis = "Change from baseline"  )

# Annote the graph
p.tvl.wld1 <- p.tvl.wld +
  #Adding vline
  geom_vline(xintercept = as.Date("2020-01-30"), 
             color = "dark gray", linetype = "dashed", show.legend = T) +
  #Curve
  geom_curve(curvature = -0.15, color = "dark gray",
             # Position , no quotes cause continous
             #0.03 is the size, curvature is the shape
             aes(x = as.Date("2020-01-30"), y = 160.00, xend = as.Date("2020-04-15"), yend = 160.00),
             arrow = arrow(length = unit(0.02, "npc"))) + 
  # text
  annotate("text", x = as.Date("2020-04-30"),
           y = 150,
           label = "WHO declares\ncoronavirus a pandemic",
           size = 3.5, color = "dark gray") 


#Save the plot
finalise_plot(p.tvl.wld1, source_name = "Source: Apple Mobility Dataset", 
              save_filepath = "Plots/On_the_move/tvl_wld.png",
              logo_image_path = "Data/logo/rk_logo.png")



######## Where is the world Traveling too ###########

# 2. Checking for World ######
p.vst.wld <- gdata %>%   
  ggplot( ., aes(as.Date(date), value, color = move_to)) + 
  geom_smooth() + scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_brewer(palette = "Set2") +
  guides(color=guide_legend(override.aes=list(fill=NA))) 

# Preety fy the graph
p.vst.wld <- ggpretay(p.vst.wld, title = "Let's go to the park", 
                      subtitle = "Data suggests that globally people have started to travel again.
And the general trend is about shunning public transport",
                      xaxis = "Months", yaxis = "Change from baseline"  )

# Annote the graph
p.vst.wld1 <- p.vst.wld +
  #Adding vline
  geom_vline(xintercept = as.Date("2020-01-30"), 
             color = "dark gray", linetype = "dashed", show.legend = T) +
  #Curve
  geom_curve(curvature = -0.15, color = "dark gray",
             # Position , no quotes cause continous
             #0.03 is the size, curvature is the shape
             aes(x = as.Date("2020-01-30"), y = 50, xend = as.Date("2020-03-15"), yend = 55),
             arrow = arrow(length = unit(0.02, "npc"))) + 
  # text
  annotate("text", x = as.Date("2020-04-30"),
           y = 50,
           label = "WHO declares\ncoronavirus a pandemic",
           size = 3.5, color = "dark gray") 


#Save the plot
finalise_plot(p.vst.wld1, source_name = "Source:Apple Mobility Dataset", 
              save_filepath = "Plots/On_the_move/tvl_wld.png",
              logo_image_path = "Data/logo/rk_logo.png")




# 1. Checking for US ########
p.vst.us <- gdata %>% filter(country_region_code == "US") %>%    
  ggplot( ., aes(as.Date(date), value, color = move_to)) + 
  geom_smooth() + scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_brewer(palette = "Set2") +
  guides(color=guide_legend(override.aes=list(fill=NA))) 

# Preety fy the graph
p.vst.us <- ggpretay(p.vst.us, title = "Let's go to the park", 
                      subtitle = "Data suggests that globally people have started to travel again.
And the general trend is about shunning public transport",
                      xaxis = "Months", yaxis = "Change from baseline"  )

# Annote the graph
p.vst.us1 <- p.vst.us +
  #Adding vline
  geom_vline(xintercept = as.Date("2020-01-30"), 
             color = "dark gray", linetype = "dashed", show.legend = T) +
  #Curve
  geom_curve(curvature = -0.15, color = "dark gray",
             # Position , no quotes cause continous
             #0.03 is the size, curvature is the shape
             aes(x = as.Date("2020-01-30"), y = 50, xend = as.Date("2020-03-15"), yend = 55),
             arrow = arrow(length = unit(0.02, "npc"))) + 
  # text
  annotate("text", x = as.Date("2020-04-30"),
           y = 50,
           label = "WHO declares\ncoronavirus a pandemic",
           size = 3.5, color = "dark gray") 


#Save the plot
finalise_plot(p.vst.us1, source_name = "Source:Apple Mobility Dataset", 
              save_filepath = "Plots/On_the_move/vst_us.png",
              logo_image_path = "Data/logo/rk_logo.png")

# Checking for UK 
p.vst.uk <- gdata %>% filter(country_region_code == "UK") %>%    
  ggplot( ., aes(as.Date(date), value, color = move_to)) + 
  geom_smooth() + scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_brewer(palette = "Set2") +
  guides(color=guide_legend(override.aes=list(fill=NA))) 

# Preety fy the graph
p.vst.uk <- ggpretay(p.vst.uk, title = "Let's go to the park", 
                     subtitle = "Data suggests that globally people have started to travel again.
And the general trend is about shunning public transport",
                     xaxis = "Months", yaxis = "Change from baseline"  )

# Annote the graph
p.vst.uk1 <- p.vst.uk +
  #Adding vline
  geom_vline(xintercept = as.Date("2020-01-30"), 
             color = "dark gray", linetype = "dashed", show.legend = T) +
  #Curve
  geom_curve(curvature = -0.15, color = "dark gray",
             # Position , no quotes cause continous
             #0.03 is the size, curvature is the shape
             aes(x = as.Date("2020-01-30"), y = 50, xend = as.Date("2020-03-15"), yend = 55),
             arrow = arrow(length = unit(0.02, "npc"))) + 
  # text
  annotate("text", x = as.Date("2020-04-30"),
           y = 50,
           label = "WHO declares\ncoronavirus a pandemic",
           size = 3.5, color = "dark gray") 


#Save the plot
finalise_plot(p.vst.uk1, source_name = "Source:Apple Mobility Dataset", 
              save_filepath = "Plots/On_the_move/vst_uk.png",
              logo_image_path = "Data/logo/rk_logo.png")
