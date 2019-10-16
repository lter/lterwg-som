#DIY map function for SOM tarball data
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/29/2019

#Required packages (loaded by ftn script below), make sure you have them installed
# library(dplyr)
# library(tidyverse)
# library(maps)
# library(ggplot2)

#set workign drive to folder where this script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load SOM map function from script from working drive
source("SOM_map_function.R")

#Load your SOM data
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv", as.is=T)

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


#Examples of function use:
som_map(data=tarball, color="lyr_soc")

som_map(data=tarball, color="lyr_soc", var.max=20, pt.sz=4)

som_map(data=tarball, scale="world", color="lyr_soc", var.max=20, pt.sz=4)

som_map(data=tarball, scale="world", color="lyr_soc", var.max=20, pt.sz = 4, depth.max=11)

som_map(data=tarball, color="ph_h2o", symbol="network", pt.sz = 2)

map.data <- som_map(data=tarball, scale="world", color="lyr_soc", var.max=20, export_map_dataframe = TRUE)