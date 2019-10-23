# siteAggregation
# try aggregating w/in nested experimental levels
# Will Wieder
# Oct 16, 2019
#########################
rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)

#get tarball
source('/Users/wwieder/Will/git_repos_local/lterwg-som/data-processing/get_latest_som.R')
tarball <- get_latest_som()
#tarball <- readRDS("/Users/wwieder/Will/git_repos_local/lterwg-som/somCompositeData_2019-10-17.rds")  
tarball <- tarball %>% filter(google_dir != "NA")

#-- get control only --  
source('/Users/wwieder/Will/git_repos_local/lterwg-som/data-processing/Tarball_v2 scripts/Filter/Control data only/SOM_control_data_filter.R')
dsets <- na.omit(as.character(unique(tarball$google_dir)))
data.all <- do.call(rbind, lapply(dsets, ctl_filter, tarball))

# -- identify unique datasets -- 
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
#data.all$full_name <- (data.all$google_dir)  
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings
unique(data.all$eco_region)
site.id  <- as.character(unique(data.all$google_dir))
site.id[1:3]
site.id[40]
names(data.all$land_cover)
vars <- c('google_dir','site_code','full_name', 'map', 'mat', 'lat','long',
          'layer_top', 'layer_bot', 'layer_mid', 'layer_thick_calc','land_cover', 'eco_region',
          'lyr_soc', 'lyr_c_tot','lyr_n_tot', 'lyr_c_to_n', 'lyr_soc_stock_calc',
          'ph_h2o', 'ph_cacl','clay','sand','silt','hzn', 'bd_samp',
          'L1','L2','L3','L4','L5',
          'L1_level','L2_level','L3_level','L4_level','L5_level')
#  no treatment data here    'tx_L1','tx_L2','tx_L3','tx_L4','tx_L5','tx_L6',
#                            'tx_L1_level','tx_L2_level','tx_L3_level',
#                            'tx_L4_level','tx_L5_level','tx_L6_level') ))
data.sub <- select(data.all, one_of(vars))
# look at data levels for ssczo, #390-393, what level do we want to aggregate up to?
# here we can look at some NEON data (google drive #40)
df <- data.sub %>% filter(google_dir == site.id[40])
names(df)
unique(df$eco_region)
lev <- unique(c(df$L1_level, df$L2_level,df$L3_level,df$L4_level,df$L5_level))
lev
unique(df$L1)

#drop empty colums of data
df <- df %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)

data.all$layer_thick_calc

# This will aggregate all data up to L1, for each unique upper soil layer.
# mess around with NEON init soil data
df %>% 
  distinct(full_name, L1_level, L1, layer_top, mat,map) %>%
  inner_join(df %>%
               group_by(L1, layer_top) %>%
               summarize_if(is.numeric, list(~mean(., na.rm=T), ~sd(., na.rm=T))), 
                by=c('L1','layer_top') ) 


# This will aggregate all data
# there are all linds of problems with doing this, as many of the profiles are << 100 cm deep, 
# with lots of missing data
NEON_init <- df %>%
  distinct(site_code, L1_level, map, mat) %>%
  inner_join(df %>%
               group_by(site_code, L1) %>%
               drop_na() %>%
               summarize(prof_soc_stock = sum(lyr_soc_stock_calc) * 1e-3, #kgC/m2
                         depth_min= min(layer_bot),
                         depth_max= max(layer_bot),
                         thick=sum(layer_thick_calc)) %>%
               summarize_if(is.numeric, list(~mean(., na.rm=T), ~sd(., na.rm=T))), 
             by=c('site_code') ) 

par(mfrow=c(1,2))
plot(NEON_init$mat, NEON_init$prof_soc_stock_mean, 
     ylab='SOC stock (kgC/m2, 100cm)', xlab='mat')
plot(NEON_init$map, NEON_init$prof_soc_stock_mean, 
     ylab='SOC stock (kgC/m2, 100cm)', xlab='map')

NEON_init$thick_mean

mean$layer_top
df$lyr_soc


unique(df$L3_level)
remove(df)

