# Script to map SOM tarball data
# Derek Pierson, piersond@oregonstate.edu
# Created: 8/28/2019
# Modified and updated 
#   Will Wieder, wwieder@ucar.edu
#   5/1/2020

rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#get tarball for different users

if (file.exists('C:/github/lterwg-som')){
  #for Derek
  tarball <- readRDS("C:/GitHub/lterwg-som-shiny/shiny_SOM/somCompositeData_2020-05-12.rds")  
  tarball <- tarball %>% filter(google_dir != "NA")
} else {
  #for Will
#  tarball <- readRDS("/Users/wwieder/Will/git_repos_local/lterwg-som/somCompositeData_2020-05-12.rds")  
  tarball <- read.csv("/Users/wwieder/Will/git_repos_local/lterwg-som/521_soils_data_harmonization_415ab64c9ef9f80c3bd65fd45441e8e8.csv")  
  tarball <- tarball %>% filter(google_dir != "NA")
}

#Select the data columns reqd
map.df <- tarball %>% select(c("site_code","network","lat", "long","lyr_soc"))

#make sure coordinates are numeric
map.df$lat <- as.numeric(map.df$lat)
map.df$long <- as.numeric(map.df$long)

#strip tarball of all data without coordinates
map.df <- map.df %>% 
  filter(lat != "NA") %>% 
  filter (long != "NA") %>% 
  filter(network!="none")

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

#map('world')
#points(simplmap.df$long, simplmap.df$lat, col=2, pch=19)


########################################
#ggplot data by Network map

#style for maps
map_style = theme(
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "cm"),
                  panel.background = element_blank()) #<-- Removes the map panel box


#World map
mapWorld <- borders("world", colour="gray40", fill="white") 
mapWorld <- ggplot() + mapWorld + 
  geom_point(data = simplmap.df, aes(x=long, y = lat, color=network), 
             size=3, alpha=0.5, show.legend = FALSE) + #position=position_jitter(h=0.7,w=0.7)) + 
  ylim(-54,85) +
  coord_fixed(1.3) +
  map_style

  mapWorld

#USA map
mapUSA <- borders("usa", colour="gray40", fill="white") 
mapUSA <- ggplot()  + mapUSA + 
  geom_point(data = simplmap.df, aes(x=long, y = lat, color=network), 
             size=3, alpha=0.6) + #position=position_jitter(h=0.7,w=0.7)) + 
  borders("state") +
  xlim(-125,-67) + ylim(25,50) +
  coord_fixed(1.3) +
  scale_size_area() +
  coord_quickmap() +
  scale_color_discrete(name = "Network") +
  map_style

  #mapUSA

# pdf(file = "fig3.pdf",   # The directory you want to save the file in
#     width = 6, # The width of the plot in inches
#     height = 4) # The height of the plot in inches
# 
# mapWorld
# dev.next()
# 
# mapUSA
# dev.off()

#from ggpubr package
combined_maps <- ggarrange(mapWorld, mapUSA, ncol = 1, nrow = 2)
combined_maps

ggsave(plot=combined_maps, filename = "fig4.jpg", width = 8, height = 6 , dpi = 300)
ggsave(plot=combined_maps, filename = "fig4.pdf", width = 8, height = 6 , dpi = 300)



