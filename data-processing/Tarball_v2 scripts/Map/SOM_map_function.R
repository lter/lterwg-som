#DIY map function for SOM tarball data
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/29/2019

library(dplyr)
library(tidyverse)
library(maps)
library(ggplot2)

#Debug
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv", as.is=T)
#tarball <- tarball %>% filter(google_dir != "NA")
#colnames(tarball)


### Function parameters ###
# data = SOM tarball dataframe or subset of tarball (dataframe)
# scale = Two options: "world" or "usa" (string)
# res = plot single point for all points with approx 10, 1, 0.1 km radius (inputs = 1, 2, 3 respectively) (numeric)
# color = data column to color points by (string)
# symbol = data column to symbolize points by (string)
# pt.sz = size of map points (numeric, try 1-6)
# export_map_dataframe = TRUE/FALSE export dataframe used for map from function (default is FALSE)
# var.min/max = confine range of values for plot (color) variable (numeric)
# depth.min/max = confine map data to specific soil depth limits 

som_map <- function(data, scale=NULL, res=NULL, color=NULL, symbol=NULL, pt.sz=NULL, 
                    export_map_dataframe=F, var.min=NULL, var.max=NULL, 
                    depth.min=NULL, depth.max=NULL) {
  
  #debug
  # data <- tarball
  # scale <- NULL
  # res <- NULL
  # color <- "lyr_soc"
  # symbol <- NULL
  # pt.sz <- NULL
  # export_map_dataframe <- F
  # var.min <- NULL
  # var.max <- NULL
  # depth.min <- NULL
  # depth.max <- NULL
  
  
  #Set defaults if ftn inputs are blank
  if(is.null(scale)) {scale <- "usa"}
  if(is.null(res)) {res <- 1}
  if(is.null(color)) {color <- "lyr_soc"}
  if(is.null(pt.sz)) {pt.sz <- 4}
  
  #strip data of all rows without coordinates
  data <- data %>% filter(lat != "NA") %>% filter (long != "NA")
  
  #strip data of all rows with NA data for plot var
  data <- data[!is.na(data[,color]),]
  
  #Confine plot data by analyte (color) variable limits
  if(!is.null(var.min)){data <- data[which(data[,color] > var.min),]}
  if(!is.null(var.max)){data <- data[which(data[,color] < var.max),]}
  
  #Confine plot data by soil depth limits
  if(!is.null(depth.min)){data <- data %>% filter(layer_top > depth.min)}
  if(!is.null(depth.max)){data <- data %>% filter(layer_bot < depth.max)}
  
  #make sure coordinates are numeric
  data$lat <- as.numeric(data$lat)
  data$long <- as.numeric(data$long)
  
  #make sure plot data is numeric
  #data[,color] <- as.numeric(data[,color])
  
  #Set data resolution by rounding decimal degree for lat,long
  data <- data %>% mutate_at(vars(long,lat), round, res)
  
  #Eliminate duplicate spatial points
  data$map_dups <- paste0(data$lat, data$long, data[,color])
  if(!is.null(symbol)) {data$map_dups <- paste0(data$map_dups, data[,color])}
  data <- data[!duplicated(data$map_dups),]
  data <- select(data, -map_dups)
  
  #Set map scale
  if(scale == "world"){
    map_scale <- borders("world", colour="gray40", fill="white") 
  } else {
    map_scale <- borders("usa", colour="gray40", fill="white")
  }

  #Add option to get map dataframe out of function
  if(export_map_dataframe){
    return(data) 
  }
  
  #Print notice of number of mapped points
  print(paste0("Mapping ",nrow(data), " points for ",color))
  
  # Create map
  ggplot(data = data, aes(x=long, y=lat, color=data[,color])) + 
    map_scale + coord_fixed(1.3) + geom_point(size=pt.sz, alpha=0.5) +
    {if(!is.null(symbol))geom_point(aes(shape=data[,symbol]))} + 
    {if(scale == "usa")xlim(-130,-65)} +
    {if(scale == "usa")ylim(20,55)} +
    scale_colour_gradientn(colours=rainbow(4)) + 
    ggtitle(paste0("SOM Data Map: ", as.character(color))) #Improve later to include other map options, e.g. "... by depth and network"
}

#Examples of function use:
som_map(data=tarball, color="lyr_soc")

#som_map(data=tarball, color="lyr_soc", var.max=20)

#som_map(data=tarball, scale="world", color="lyr_soc", var.max=20)

#som_map(data=tarball, scale="world", color="lyr_soc", var.max=20, pt.sz = 1, depth.max=11)

#som_map(data=tarball, color="ph_h2o", symbol="network", pt.sz = 2)

#map.data <- som_map(data=tarball, scale="world", color="lyr_soc", var.max=20, export_map_dataframe = TRUE)





