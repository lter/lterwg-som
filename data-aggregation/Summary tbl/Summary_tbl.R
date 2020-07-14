# SoDaH Summary Table creator 
# More details @ https://github.com/lter/lterwg-som/issues/72
# Created: July 2020, Derek Pierson

# Load libraries
library(tidyverse)

# Get SoDaH database from EDI script
source("edi.521.1.r")

# Filter som for control only
som <- dt1 %>% filter(control_sample == TRUE)

#############################
### FOCUSING ON SOIL C FIRST, expect we'll later be able to go back and to pull MAP, MAT, NPP 
#############################

#FOR THE INITIAL PURPOSE OF READABILITY AND QA...
#Slim down the dataframe to req'd columns only
som <- som %>% select(site_code, location_name, lat, long, layer_top, layer_bot, lyr_soc)  ### Q1: use lyr_soc or lyr_soc_stock_calc? Greater number of sites with % soc, but the stock is preferred value, yes?

# Group by site, location, depth
som_grp <- som %>% mutate(site_code = as.character(site_code),
                          location_name = as.character(location_name)) %>%
                   group_by(site_code, location_name, lat, long, layer_top, layer_bot)


#Summarize data
som_smrz <- som_grp %>% summarize(lyr_soc_avg = mean(lyr_soc, na.rm=TRUE),
                                  lyr_soc_n = n())

  ### Q2: How to sum lyr data across depth increments?
    # First thought, a loop crawler that goes 1 by 1 through the unique lat-longs, with built in checks to make sure layers sum to certain depth without gaps.
    # Other ideas?








