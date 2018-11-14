
#Load libraries
library(dplyr)
library(ggplot2)

### Loading The Data
########################################

#Change path to wherever you have the tarball.csv stored locally
#data.all <- read.csv("E:/Box/Box Sync/SOM_tarball/tarball_10-18-2018.csv", stringsAsFactors = FALSE)  
data.all <- read.csv("../../somCompositeData_2018-11-09T17_34_23.csv", stringsAsFactors = FALSE)  
#data.all <- readRDS("../somCompositeData_2018-11-09T17_34_23.rds")   

#Print out tarball column names
colnames(data.all)[1:30]  #only the first 30 in this case

# Unite site codes and location names
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

mat_num <- as.numeric(data.all$mat)
map_num <- as.numeric(data.all$map)

ggplot(data.all) + geom_point(aes(x=mat_num,y=map_num,color=network,shape='.',alpha=0.5))
ggplot(data.all) + geom_point(aes(x=mat_num,y=clay,color=network,alpha=0.5))
ggplot(data.all) + geom_point(aes(x=clay,y=silt,color=network,alpha=0.5))

ggplot(data.all) + geom_density(aes(x=as.numeric(layer_bot),color=network))


