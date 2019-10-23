# Bulk Density pedotransfer function
# Will Wieder
# Oct, 2019
#########################
rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)

# get tarball, if needed
#source('/Users/wwieder/Will/git_repos_local/lterwg-som/data-processing/get_latest_som.R')
#tarball <- get_latest_som()

#Read tarball, if already in working directory
tarball <- readRDS("/Users/wwieder/Will/git_repos_local/lterwg-som/somCompositeData_2019-10-17.rds")  
tarball <- tarball %>% filter(google_dir != "NA")
#NEON    <- tarball %>% filter(network == 'NEON')
ds_init <- tarball %>% filter(google_dir == 'NEON_initialChar')

vars <- c('google_dir','site_code','map', 'mat', 'lat','long',
          'layer_top', 'layer_bot', 'layer_mid', 'layer_thick_calc','land_cover',
          'lyr_soc', 'lyr_c_tot','lyr_n_tot', 'lyr_c_to_n', 'lyr_soc_stock_calc',
          'ph_h2o', 'ph_cacl','clay','sand','silt','hzn', 'bd_samp',
          'L1','L1_level','eco_region')
df <- select(ds_init, one_of(vars))
names(df)
unique(df$eco_region)

#drop empty colums of data
df <- df %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)

df$layer_thick_calc

# This will aggregate all data up to L1, for each unique upper soil layer.
# mess around with NEON init soil data
df %>% 
  distinct(site_code, L1_level, L1, layer_top, mat,map) %>%
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
df2 <- df
df2$lyr_soc[df2$lyr_soc<=0] = NA   #mask out zero C soils
#df2$bd_samp[df2$bd_samp<0.7] = NA   # as in Tranter paper, not sure this is warranted
#df2$bd_samp[df2$bd_samp>1.8] = NA   
names(df2)


df2 <- df2[complete.cases(df2), ]

# easier to define this function with nls than lm
# MLR from Tranter et al. 2007 Soil Use and Management
# formula are not consistent in table 2, also won't converge using all sand...?
# modified here to get convergence, but likely not correct
m1   <- nls(bd_samp ~ a + b*clay + (c-sand)^2 * d + e*log(layer_mid), data=df2,  
            start=list(a=1.35,b=4.5e-3, c=44.7, d=-6e-5, e = 6e-2))
summary(m1)
m1b  <- nls(bd_samp ~ a +  (c-sand)^2 * d + e*log(layer_mid), data=df2,  
            start=list(a=1.35, c=44.7, d=-6e-5, e = 6e-2))
summary(m1b)

# from MLR_B1
m2   <- nls(bd_samp ~ a + b*clay + c*lyr_soc + (d+sand)^2 * e + f*log(layer_mid), data=df2,  
            start=list(a=1.2,b=2.1e-3, c=-0.143, d=-47.95, e = 6e-5, f=-0.043))
# remove insignificant terms
m2b   <- nls(bd_samp ~ a +  c*lyr_soc + (d+sand)^2 * e + f*log(layer_mid), data=df2,  
            start=list(a=1.2, c=-0.143, d=-47.95,e = 6e-5, f=-0.043))
summary(m2b)

# Pm for conceptual model
m3   <- nls(bd_samp ~ a + b*clay + ((c-sand)^2) + e*log(layer_mid), data=df2,  
            start=list(a=1.35,b=4.5e-3, c=44.7, e = 6e-2))
summary(m3)
# now predict residuals with OM
df2$r1 <- resid(m1)
df2$r1b <- resid(m1b)
m4 <- nls(r1 ~ a + b*log(lyr_soc) + c*log(layer_mid), data=df2, 
          start=list(a=-0.217, b=-0.114, c=-0.077) )
m4b <- nls(r1b ~ a + b*log(lyr_soc) + c*log(layer_mid), data=df2, 
          start=list(a=-0.217, b=-0.114, c=-0.077) )


# texture controls
m0a   <- nls(bd_samp ~ a + b*sand + c*sand^2,  data=df2,  
             start=list(a=1.35,b=4.5e-3, c=44.7))
df2$r0a <- resid(m0a)
# depth effects
m0b     <- nls(r0a ~ d + e*log(layer_mid),  data=df2,  
             start=list(d=-6e-5, e = 6e-2))
df2$r0b <- resid(m0b)
# OM controls
m0c <- nls(r0b ~ a + b*log(lyr_soc) + c*log(layer_mid), data=df2, 
           start=list(a=-0.217, b=-0.114, c=-0.077) )

summary(m0a)
summary(m0b)
summary(m0c)
df2$m0sum <- predict(m0a) + predict(m0b) + predict(m0c)
lim=c(0.4,2)
plot(df2$m0sum, df2$bd_samp, ylim=lim, xlim=lim,pch=16, cex=0.6)
abline(0,1)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m4b)

pm1 <- predict(m1)
pm1b <- predict(m1b)
pm2 <- predict(m2)
pm2b <- predict(m2b)
pm3 <- predict(m3)
pm4 <- predict(m4)
pm4b <- predict(m4b)

bd_hat <- pm1 + pm4
bd_hatb <- pm1b + pm4b
lim=c(0.4,2)
plot(bd_hat, df2$bd_samp, ylim=lim, xlim=lim,pch=16, cex=0.6)
points(bd_hatb,df2$bd_samp, col = 2, pch=16, cex=0.6)
points(df2$m0sum, df2$bd_samp, col=4 ,pch=16, cex=0.6)
points(pm2, df2$bd_samp, col=3 ,pch=16, cex=0.6)
points(pm2b, df2$bd_samp, col=5 ,pch=16, cex=0.6)
abline(0,1)

r0 <-df2$bd_samp -df2$m0sum  
plot(r0~df2$bd_samp)
abline(v=c(0.7,1.8))
abline(h=0)
max(bd_hat)

plot(pm1, df2$bd_samp, ylim=lim, xlim=lim)
points(pm2, df2$bd_samp,pch=16, col=2)


# try here with simple linear regressions
bd_m0  <- lm(bd_samp~lyr_soc+log(layer_mid)|, data=df2)
bd_m1a <- lm(bd_samp~sand+I(sand^2)+log(layer_mid), data=df2)
bd_m1b <- lm(bd_samp~clay+I(clay^2)+log(layer_mid), data=df2)
bd_m2a <- lm(bd_samp~sand+log(lyr_soc)+I(clay^2)+log(layer_mid), data=df2)
bd_m2b <- lm(bd_samp~clay+log(lyr_soc)+I(clay^2)+log(layer_mid), data=df2)
bd_m2c <- lm(bd_samp~silt+log(lyr_soc)+I(sand^2)+log(layer_mid), data=df2)
bd_m3a <- lm(bd_samp~log(lyr_soc)*I(sand^2)+log(layer_mid), data=df2)
bd_m3b <- lm(bd_samp~log(layer_mid)+I(clay^2)+log(lyr_soc)+ph_h2o, data=df2)
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

m0_resid <- resid(bd_m0)
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
plot(m0_resid ~ df2$bd_samp)
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

