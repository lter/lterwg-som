# 
# title: "LTERWG SOM Data 101"
# author: "Derek Pierson"
# date: "November 9, 2018"
# 

### Getting Started
#To start, let's first load up a few of the libraries we'll be using here.  
#Much of the data processing workflow here is done with the dplyr package. 

#Load libraries
library(dplyr)
library(ggplot2)

### Loading The Data
########################################

  #Change path to wherever you have the tarball.csv stored locally
  data.all <- read.csv("E:/Box/Box Sync/SOM_tarball/tarball_10-18-2018.csv", stringsAsFactors = FALSE)  
  
  #Print out tarball column names
  colnames(data.all)[1:30]  #only the first 30 in this case
  
  # Unite site codes and location names
  data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
  data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings
  
  #Print list of location names
  unique(data$full_name)


### Filtering The Data
#######################################

## By network
  #View network names  
  unique(data.all$network)
  
  #Create a new dataframe with data from a specific network
  data.neon <- data.all %>% filter(network == "NEON")   
  data.dirt <- data.all %>% filter(network == "DIRT")  
  unique(data.dirt$full_name)
  
## By depth 
  #Filter for 0-10 cm data
  data.surf <- data.all %>% filter(layer_top == 0 & layer_bot == 10)

  
## By treatment level
  #Take a look at names of level 1 treatments
  unique(data.all$tx_L1)[1:30]  #only showing the first 30
  
  #Create vector of names to select for control or undisturbed sample locations
  ctls <- c("c","Undisturbed","Unmanaged","CTL","CO","Control")
  
  #Filter data by L1 treatment values = CONTROL or UNDISTURBED
  data.ctl <- data.all %>% filter(tx_L1 %in% ctls)


### Data Exploration Examples
#######################################
  
## Data inventory tables

  #Create a dataframe table to show the number of values for 'soc' and 'c_tot' by location ('full_name')
  data.ctbl <- data.all %>% group_by(full_name) %>% summarize(count.ctot = n_distinct(c_tot, na.rm = TRUE),
                                                            count.soc = n_distinct(soc, na.rm = TRUE))
  #Show the first 10 rows of the table
  data.ctbl[1:10,]



## Take a look at the number of experimental levels by location

  #Create summary table for soc by level
  data.explvls <- data.all %>% group_by(full_name) %>% summarize(L1_n = n_distinct(L1, na.rm = TRUE),
  L2_n = n_distinct(L2, na.rm = TRUE),
  L3_n = n_distinct(L3, na.rm = TRUE),
  L4_n = n_distinct(L4, na.rm = TRUE),
  L5_n = n_distinct(L5, na.rm = TRUE))
  
  #Show the first 10 rows of the table
  data.explvls[1:10,]


### Plot Examples
#######################################
  
##Filter data and create box plot by location

  #Define the treatment classes to include. Only control type data in this case.
  ctls <- c("c","Undisturbed","Unmanaged","CTL","CO","Control")
  
  #Filter data by treatment and depth 
  data.plot <- data.all %>% filter(tx_L1 %in% ctls) %>% filter(layer_top == 0 & layer_bot == 10) %>% 
  select(full_name,location_name, site_code, soc, layer_top, layer_bot)                                                                                                            
  
  #Create boxplot  (...may also by piped after the code above)
  soc.ctl.bxplot <- ggplot(data.plot, aes(x=full_name, y=soc)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #Rotate x-axis labels 
  soc.ctl.bxplot  #display plot



### Map Examples
#######################################
  
##Currently we have to hotfix the latitude and longitude values to decimal degrees

  #Bring in clean coordinate csv
  clean.coords <- read.csv("SOM_clean_coords.csv", stringsAsFactors = FALSE)
  
  #Remove duplicate lat & long values to avoid adding rows in join step
  lat.clean <- clean.coords %>% distinct(lat, lat.clean)
  long.clean <- clean.coords %>% distinct(long, long.clean)
  
  #Join clean coordinate columns to tarball dataframe
  data.all <- left_join(data.all, lat.clean, by="lat") %>% left_join(., long.clean, by="long")


##Simple point map
  library(maps)
  
  map('world')
  points(data.all$long.clean, data.all$lat.clean, col=2, pch=19)


##Filter data and create map of mean 0-10 cm SOC for undistrubed soils only

  library('ggmap')
  
  #Starting with the merged clean lat, long dataframe from above (...still named data.all)
  #Also using the defined 'ctls' from above to select undisturbed soils only
  data.map <- data.all %>% filter(tx_L1 %in% ctls) %>% filter(layer_top == 0 & layer_bot == 10) %>% 
    select(full_name,location_name, site_code, soc, layer_top, layer_bot, lat.clean, long.clean)
  
  #Summarize data to find n, mean, st dev, min, and max soc values by location
  data.map.soc <- data.map %>% distinct(full_name,soc,lat.clean,long.clean) %>%  
    filter(!is.na(soc)) %>%  
    group_by(full_name) %>%                                                                                       
    summarise(soc.count = n(),                                                                                            
              soc.mn = mean(soc, na.rm = T),  
              soc.stdv = sd(soc, na.rm = T),
              soc.min = min(soc, na.rm = T),
              soc.max = max(soc, na.rm = T),
              lat = first(lat.clean),                                                                                                  
              long = first(long.clean))
  
  #Map it
  mapWorld <- borders("world", colour="gray50", fill="gray50") 
  ggplot() + mapWorld + geom_point(data = data.map.soc, aes(x=long, y = lat, color=soc.mn), size=3) + 
    coord_fixed(1.3) +  
    scale_colour_gradientn(colours=rainbow(4))  


  
#
#
#
#
### More maps
#######################################
  
## Map locations by NETWORK
  
  data.map.network <- data.all %>% distinct(full_name,network,lat.clean,long.clean) %>% mutate(network = if_else(is.na(network), 'Other', network))
  
  #World map
  mapWorld <- borders("world", colour="gray40", fill="gray50") 
  ggplot() + mapWorld + 
    geom_point(data = data.map.network, aes(x=long.clean, y = lat.clean, color=network), size=2, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    coord_fixed(1.3) 

  #USA map
  mapUSA <- borders("usa", colour="gray40", fill="gray50") 
  ggplot() + mapUSA + 
    geom_point(data = data.map.network, aes(x=long.clean, y = lat.clean, color=network), size=4, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    xlim(-130,-65) + ylim(20,55) +
    coord_fixed(1.3) 
  
## Map locations with treatment data
  
  data.map.tx_L1 <- data.all %>% distinct(full_name,network,lat.clean,long.clean, tx_L1) %>% filter(!is.na(tx_L1))  
  
  #World map
  mapWorld <- borders("world", colour="gray40", fill="gray50") 
  ggplot() + mapWorld + 
    geom_point(data = data.map.tx_L1, aes(x=long.clean, y = lat.clean, color='Locations with \n treatment data'), size=3, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    coord_fixed(1.3) + theme(legend.title=element_blank()) 
  
  #USA map
  mapUSA <- borders("usa", colour="gray40", fill="gray50") 
  ggplot() + mapUSA + 
    geom_point(data = data.map.tx_L1, aes(x=long.clean, y = lat.clean, color='Locations with \n treatment data'), size=4, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    xlim(-130,-65) + ylim(20,55) +
    coord_fixed(1.3) + theme(legend.title=element_blank()) 
  