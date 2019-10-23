# title: "ataAssessment"
# author: "Will Wieder, modified from Derek P."
# date: "November 21, 2018"
# 

### How much ANPP data de we have? 
#    (ANPP, aboveground biomass for grasslands, litterfall + increment growth for forests)
#    = columns: npp, anpp
#Load libraries
library(dplyr)
library(ggplot2)

### Loading The Data
########################################

#Change path to wherever you have the tarball.csv stored locally
data.all <- readRDS("/Users/wwieder/Desktop/lter_homogenized/somCompositeData_2019-10-15.rds")  

# Unite site codes and location names
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

#Print out tarball column names
colnames(data.all)[1:30]  #only the first 30 in this case

 
#Create summary table for distonct npp, anpp and agb values by site name
data.npp <- data.all %>% group_by(full_name) %>% summarize(npp_n = n_distinct(npp, na.rm = TRUE),
                                                           lit_n = n_distinct(lit_c, na.rm = TRUE),
                                                           agb_n = n_distinct(agb, na.rm = TRUE),
                                                           anpp_n = n_distinct(anpp, na.rm = TRUE))
data.npp <- data.all %>% filter(npp >0)
unique(data.npp$full_name)
unique(data.all$npp)

data.anpp <- data.all %>% filter(anpp >0)
unique(data.anpp$full_name)
unique(data.all$anpp)

data.lit_c <- data.all %>% filter(lit_c >0)
unique(data.lit_c$full_name)
unique(data.all$lit_c)

