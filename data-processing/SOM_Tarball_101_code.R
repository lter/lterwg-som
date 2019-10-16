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
 data.all <- readRDS("/Users/wwieder/Desktop/lter_homogenized/somCompositeData_2019-10-15.rds")  

  #Print out tarball column names
  colnames(data.all)[1:30]  #only the first 30 in this case
  
  # Unite site codes and location names
  data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
  data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings
  
  #Print list of location names
  unique(data.all$full_name)
  length(unique(data.all$full_name))
  length(unique(data.all$site_code))
  length(unique(data.all$location_name))
  
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
  data.ctbl <- data.all %>% group_by(full_name) %>% summarize(count.ctot = n_distinct(lyr_c_tot, na.rm = TRUE),
                                                            count.soc = n_distinct(lyr_soc, na.rm = TRUE))
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
  select(full_name,location_name, site_code, lyr_soc, layer_top, layer_bot)                                                                                                            
  
  #Create boxplot  (...may also by piped after the code above)
  soc.ctl.bxplot <- ggplot(data.plot, aes(x=full_name, y=lyr_soc)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #Rotate x-axis labels 
  soc.ctl.bxplot  #display plot



### Map Examples
#######################################
  
##Simple point map
  library(maps)
  
  map('world')
  points(data.all$long, data.all$lat, col=2, pch=19)

  map('world')
  points(data.all$long, data.all$lat, col=2, pch=19)
  
##Filter data and create map of mean 0-10 cm SOC for undistrubed soils only

  library('ggmap')
  library(fiftystater)
  
  #Starting with the merged clean lat, long dataframe from above (...still named data.all)
  #Also using the defined 'ctls' from above to select undisturbed soils only
  data.map <- data.all %>% filter(tx_L1 %in% ctls) %>% filter(layer_top == 0 & layer_bot == 10) %>% 
    select(full_name,location_name, site_code, lyr_soc, layer_top, layer_bot, lat, long)
  
  #Summarize data to find n, mean, st dev, min, and max soc values by location
  data.map.soc <- data.map %>% distinct(full_name,lyr_soc,lat,long) %>%  
    filter(!is.na(lyr_soc)) %>%  
    group_by(full_name) %>%                                                                                       
    summarise(soc.count = n(),                                                                                            
              soc.mn = mean(lyr_soc, na.rm = T),  
              soc.stdv = sd(lyr_soc, na.rm = T),
              soc.min = min(lyr_soc, na.rm = T),
              soc.max = max(lyr_soc, na.rm = T),
              lat = first(lat),                                                                                                  
              long = first(long))
  
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
  
  data.map.network <- data.all %>% distinct(full_name,network,lat,long) %>% 
    mutate(network = if_else(is.na(network), 'Other', network))

    #World map
  mapWorld <- borders("world", colour="gray40", fill="gray50") 
  ggplot() + mapWorld + 
    geom_point(data = data.map.network, aes(x=long, y = lat, color=network), 
               size=2, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    coord_fixed(1.3) 

  #USA map
  mapUSA <- borders("state", colour="black", fill="white") 
#  mapUSA <- borders("usa", colour="gray40", fill="gray50") 
  ggplot() + mapUSA + 
    geom_point(data = data.map.network, aes(x=long, y = lat, color=network), 
               size=2, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    xlim(-125,-67) + ylim(25,50) +
    coord_fixed(1.3) 
  
  #USA map
  mapUSA <- borders("state", colour="black", fill="white") 
  ggplot() + mapUSA + 
    geom_point(data = data.map.network, aes(x=long, y = lat, color='brickred'), 
               size=3, alpha=0.9) + #position=position_jitter(h=0.7,w=0.7)) + 
    xlim(-125,-67) + ylim(25,50) +
    coord_fixed(1.3) + theme(legend.title=element_blank()) 
  
  library(ggalt)     # coord_proj
  library(albersusa) # devtools::install_github("hrbrmstr/albersusa")
  library(ggthemes)  # theme_map
  library(rgeos)     # centroids
  library(dplyr)
  us <- usa_composite()
  us_map <- fortify(us, region="name")
  
  gg <- ggplot()
  gg <- gg + geom_map(data=us_map, map=us_map,
                      aes(x=long, y=lat, map_id=id),
                      color="#2b2b2b", size=0.1, fill=NA)
  gg <- gg + theme_map()
  
  gg + coord_map()
gg

# yet another attempt
# https://stackoverflow.com/questions/52911837/add-points-to-usmap-with-ggplot-in-r

states_centers <- as.data.frame(state.center)
states_centers$name <- state.name
#states_centers <- states_centers[!(states_centers$name %in% c("Alaska", "Hawaii")),]
coordinates(states_centers) <- ~x+y
proj4string(states_centers) <- CRS(us_longlat_proj)
states_centers <- spTransform(states_centers, CRSobj = CRS(us_aeqd_proj))
states_centers <- as.data.frame(coordinates(states_centers))
us_map <- fortify(us, region="name")

ggplot() +
  geom_map(
    data = us_map, map = us_map,
    aes(x = long, y = lat, map_id = id),
    color = "#2b2b2b", size = 0.1, fill = NA
  ) +
  geom_point(
    data = states_centers, aes(x, y), size = 4, color = "steelblue"
  ) +
  coord_equal() + # the points are pre-projected
  ggthemes::theme_map()



  df <- data.map.network[complete.cases(data.map.network), ]
  coordinates(df) <- ~long+lat
  proj4string(df) <- CRS(us_longlat_proj)
  df <- points_elided(df)
  df <- spTransform(df, CRSobj = CRS(us_aeqd_proj))
  summary(df)
  df <- as.data.frame(coordinates(df))

  us <- usa_composite(proj = "aeqd")
  us_map <- fortify(us, region="name")
    
    ggplot() +
      geom_map(
        data = us_map, map = us_map,
        aes(x = long, y = lat, map_id = id),
        color = "#2b2b2b", size = 0.1, fill = NA
      ) +
#      geom_point(
#        data = df, aes(x, y), size = 4, color = "steelblue"
#      ) +
      coord_equal() + # the points are pre-projected
      ggthemes::theme_map()
    
    gg <- ggplot()
    gg <- gg + geom_map(data=usa_map, map=usa_map,
                        aes(long, lat, map_id=id),
                        color="#2b2b2b", size=0.1, fill=NA)
    gg <- gg + coord_proj(us_laea_proj)
    gg <- gg + geom_point(data = df, 
                          aes(long, lat), 
                          size=3, alpha=0.9)
    gg <- gg + theme_map()
gg

# Try 50 states with insets
# https://github.com/wmurphyrd/fiftystater
  data("fifty_states") # this line is optional due to lazy data loading
  crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  
  p <- ggplot(crimes, aes(map_id = state)) + 
    # map points to the fifty_states shape data
  #  geom_map(aes(fill = 'Assault'), map = fifty_states) + 
    geom_map(aes(), map = fifty_states) + 
  #  geom_point(data = data.map.network, aes(x=long, y = lat, color=network), 
  #             size=2, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") +
    theme(legend.position = "bottom", 
          panel.background = element_blank())
  
  p
  
  ggplot(crimes, aes(map_id = state)) + 
    # map points to the fifty_states shape data
  #  geom_point(data = data.map.network, aes(x=long, y = lat, color=network), 
   #            size=2, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") +
    theme(legend.position = "bottom", 
          panel.background = element_blank())
  
  p
  
# ---- another try here ----  
# https://stackoverflow.com/questions/49523375/graphing-lat-long-data-points-on-us-map-50-states-including-alaska-hawaii
  library(devtools)
  library(githubinstall)
  githubinstall("bloom")
  library(albersusa)
  library(sp)
  library(raster)
  library(ggplot2)
  library(broom)
  

  # set up reference coordinate system and convert to spatial points
  df <- data.map.network[complete.cases(data.map.network), ]


  coordinates(df) <- ~lon + lat
  latlong = "init=epsg:4326"
  proj4string(df) = CRS(latlong)
  df <- as.data.frame(points_elided(df))
  
  
  USA <- usa_composite(proj="laea")  # creates map projection 
  USA_MAP <- tidy(USA, region="name")
  
  q <- ggplot() + 
    geom_map(data = USA_MAP, map = USA_MAP, aes(map_id = id), fill=NA, size = 0.1)#+
    geom_point(data = df, alpha = 0.5, aes(x=df$lon, y=df$lat, size=df$manual_prm, color=df$wcng_score)) +
    theme_void() +
    coord_map()
  
  
## Map locations with treatment data
  
  data.map.tx_L1 <- data.all %>% distinct(full_name,network,lat,long, tx_L1) %>% filter(!is.na(tx_L1))  
  
  #World map
  mapWorld <- borders("world", colour="gray40", fill="gray50") 
  ggplot() + mapWorld + 
    geom_point(data = data.map.tx_L1, aes(x=long, y = lat, color='Locations with \n treatment data'), size=3, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    coord_fixed(1.3) + theme(legend.title=element_blank()) 
  
  #USA map
  mapUSA <- borders("usa", colour="gray40", fill="gray50") 
  ggplot() + mapUSA + 
    geom_point(data = data.map.tx_L1, aes(x=long, y = lat, color='Locations with \n treatment data'), size=4, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
    xlim(-130,-65) + ylim(20,55) +
    coord_fixed(1.3) + theme(legend.title=element_blank()) 
  
  