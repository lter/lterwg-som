#Script to map SOM tarball data
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/28/2019

library(dplyr)
library(tidyverse)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv", as.is=T)
tarball <- tarball %>% filter(google_dir != "NA")

#Select the data columns reqd
map.df <- tarball %>% select(c("site_code","network","lat", "long","lyr_soc"))

#make sure coordinates are numeric
map.df$lat <- as.numeric(map.df$lat)
map.df$long <- as.numeric(map.df$long)

#strip tarball of all data without coordinates
map.df <- map.df %>% filter(lat != "NA") %>% filter (long != "NA")

#round the coordinates to two decimal places
  # 1 decimal place is ~10 km resolution
  # 2 decimal places is ~1 km resolution
  # 3 decimal places is ~100 m resolution
map.df <- map.df %>% mutate_at(vars(long,lat), round, 1)

#remove duplicate values by creating unified lat-long column and removing duplicates
simplmap.df <- map.df
simplmap.df$latlong <- paste0(map.df$lat,map.df$long) 
simplmap.df <- simplmap.df[!duplicated(simplmap.df$latlong),]


###########################
##Simple point map
library(maps)

map('world')
points(simplmap.df$long, simplmap.df$lat, col=2, pch=19)


########################################
#ggplot data by Network map

#World map
mapWorld <- borders("world", colour="gray40", fill="gray50") 
#ggplot() + mapWorld + 
#  geom_point(data = simplmap.df, aes(x=long, y = lat, color=network), size=3, alpha=0.5) + #position=position_jitter(h=0.7,w=0.7)) + 
#  coord_fixed(1.3) 

#USA map
mapWorld <- borders("world", colour="gray40", fill="gray50") 
mapUSA <- borders("usa", colour="gray40", fill="white") 
ggplot() + mapWorld +#mapUSA + 
  geom_point(data = simplmap.df, aes(x=long, y = lat, color=network, shape=network), size=4, alpha=0.5) + #position=position_jitter(h=0.7,w=0.7)) + 
  #xlim(-130,-65) + ylim(20,55) +
  coord_fixed(1.3) 


########################################
#ggplot data by lyr_soc

#World or USA map
mapWorld <- borders("world", colour="gray40", fill="white") 
mapUSA <- borders("usa", colour="gray40", fill="white") 
ggplot() + mapWorld +#mapUSA + 
  geom_point(data = map.df, aes(x=long, y = lat, color=lyr_soc, shape=network), size=4, alpha=0.5) + #position=position_jitter(h=0.7,w=0.7)) + 
  #xlim(-130,-65) + ylim(20,55) +
  coord_fixed(1.3) 

