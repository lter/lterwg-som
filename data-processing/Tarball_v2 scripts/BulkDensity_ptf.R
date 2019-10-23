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
data.all$eco
# -- identify unique datasets -- 
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
#data.all$full_name <- (data.all$google_dir)  
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings
(data.all$eco_region)
site.id  <- as.character(unique(data.all$google_dir))
site.id[1:3]
site.id[40]
names(data.all$land_cover)
vars <- c('google_dir','site_code','full_name', 'map', 'mat', 'lat','long',
          'layer_top', 'layer_bot', 'layer_mid', 'layer_thick_calc','land_cover',
          'lyr_soc', 'lyr_c_tot','lyr_n_tot', 'lyr_c_to_n', 'lyr_soc_stock_calc',
          'ph_h2o', 'ph_cacl','clay','sand','silt','hzn', 'bd_samp',
          'L1','L2','L3','L4','L5',
          'L1_level','L2_level','L3_level','L4_level','L5_level', 'eco_region')
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

names(df)
ggplot(filter(df, lyr_soc<10), aes(x=lyr_soc, y=log10(bd_samp), colour=site_code)) + 
  geom_point() +
  geom_smooth(method='lm') 

names(df)
ggplot(filter(df, lyr_soc<12), aes(x=sand, y=(bd_samp))) + 
  geom_point() +
  geom_smooth(method='lm') 

## MLR-A Multiple linear regression bd = a+b·sand + c·sand^2 + d + e·log depth
max(df$lyr_soc, na.rm=T)
df$lyr_soc[df$lyr_soc>12] = NA   #mask out high C soils
(df$hzn)
df2 <- df[,1:25]
df2$lyr_soc[df2$lyr_soc<=0] = NA   #mask out zero C soils
names(df2)

df2 <- df2[complete.cases(df2), ]

df2$sand2 <- df2$sand^2
df2$clay2 <- df2$clay^2
bd_m0  <- lm(bd_samp~lyr_soc+log(layer_mid), data=df2)
bd_m1a <- lm(bd_samp~sand+sand2+log(layer_mid), data=df2)
bd_m1b <- lm(bd_samp~clay+clay2+log(layer_mid), data=df2)
bd_m2a <- lm(bd_samp~sand+log(lyr_soc)+sand2+log(layer_mid), data=df2)
bd_m2b <- lm(bd_samp~clay+log(lyr_soc)+clay2+log(layer_mid), data=df2)
bd_m2c <- lm(bd_samp~silt+log(lyr_soc)+sand2+log(layer_mid), data=df2)
bd_m3a <- lm(bd_samp~log(lyr_soc)*sand2+log(layer_mid), data=df2)
bd_m3b <- lm(bd_samp~log(layer_mid)+clay2+log(lyr_soc)+sand+ph_h2o, data=df2)
summary(bd_m0)
summary(bd_m1a)
summary(bd_m1b)
summary(bd_m2a)
summary(bd_m2b)
summary(bd_m2c)
summary(bd_m3a)
summary(bd_m3b)

m0_pred <- predict(bd_m0)
m1a_pred <- predict(bd_m1a)
m1b_pred <- predict(bd_m1b)
m2a_pred <- predict(bd_m2a)
m2b_pred <- predict(bd_m2b)
m3a_pred <- predict(bd_m3a)
m3b_pred <- predict(bd_m3b)

m1a_resid <- resid(bd_m1a)
m1b_resid <- resid(bd_m1b)
m2a_resid <- resid(bd_m2a)
m2b_resid <- resid(bd_m2b)
m3a_resid <- resid(bd_m3a)
m3b_resid <- resid(bd_m3b)

par(mfrow=c(1,1))
lim=c(0.4,2)
plot(df2$lyr_soc, df2$bd_samp)
plot(m1a_pred,df2$bd_samp, pch=16, cex=0.4, xlim=lim, ylim=lim,
     ylab='observed BD', xlab = 'predicted BD')
#points(m0_pred,df2$bd_samp, pch=16, cex=0.4, col=7)
points(m1b_pred,df2$bd_samp, pch=16, cex=0.4, col=4)
points(m2a_pred,df2$bd_samp, pch=16, cex=0.4, col=2)
points(m2b_pred,df2$bd_samp, pch=16, cex=0.4, col=5)
points(m3a_pred,df2$bd_samp, pch=16, cex=0.4, col=7)
points(m3b_pred,df2$bd_samp, pch=16, cex=0.4, col=3)
abline(0,1, lty=2)

#Still a significant trend in the residuals.  Why?
plot(m1a_resid ~ df2$bd_samp)
plot(m1b_resid ~ df2$bd_samp)
plot(m2a_resid ~ df2$bd_samp)
plot(m2b_resid ~ df2$bd_samp)
plot(m3a_resid ~ df2$bd_samp)
plot(m3b_resid ~ df2$bd_samp)
abline(h=0)
boxplot(m3b_resid ~ df2$land_cover)

names(df)
# This will aggregate all data
# 
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

