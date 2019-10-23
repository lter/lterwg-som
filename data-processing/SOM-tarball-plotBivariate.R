# 
# title: "LTERWG SOM Data 101"
# author: "Derek Pierson"
# date: "November 9, 2018"
# 

### Getting Started
#To start, let's first load up a few of the libraries we'll be using here.  
#Much of the data processing workflow here is done with the dplyr package. 

#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(devtools)
library(reshape2)
#devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraphExtra)

### Loading The Data
########################################

#Change path to wherever you have the tarball.csv stored locally
data.all <- readRDS("/Users/wwieder/Desktop/lter_homogenized/somCompositeData_2018-11-09T17_34_23.rds")  

#Print out tarball column names
colnames(data.all)[1:30]  #only the first 30 in this case
# Unite site codes and location names
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

#Print list of location names
unique(data.all$full_name)
unique(as.numeric(data.all$map))
unique(data.all$hzn)

boxplot(data.all$map ~ data.all$full_name)
boxplot(data.all$`15n` ~ data.all$full_name)
hist(data.all$n_tot)
plot(data.all$c_to_n, data.all$`15n`, ylim=c(-15,15),xlim=c(0,80), pch=16, cex=0.5)
plot(data.all$n_tot, data.all$`15n`, ylim=c(-15,15),xlim=c(0,80), pch=16, cex=0.5)
m  <- (lm(data.all$`15n` ~ data.all$n_tot ))
m1 <- (lm(data.all$`15n` ~ data.all$c_to_n ))
#group by MAT here, because site names not working
m2 <- lm(data.all$`15n` ~ data.all$c_to_n  + data.all$mat)
summary(m2)
        
abline(m1)

require(ggiraph)
require(ggiraphExtra)
require(plyr)
data.all$MAT <- as.numeric(data.all$mat)
data.all$MAT2 <- data.all$mat
data.all$MAT2[data.all$MAT<= 5]  <- '<5'
data.all$MAT2[data.all$MAT > 5]  <- '>5'
#data.all$MAT2[data.all$MAT > 15] <- 'hot'
data.all$MAT2 <- as.factor(data.all$MAT2)

data.all$MAP <- as.numeric(data.all$map)
data.all$MAP2 <- data.all$map
data.all$MAP2[data.all$MAP<= 1000] <- 'dry'
data.all$MAP2[data.all$MAP > 1000] <- 'wet'

data.all$PH <- data.all$ph_h2o
data.all$PH[data.all$ph_h2o<= 7] <- 'acid'
data.all$PH[data.all$ph_h2o > 7] <- 'base'

data.neon <- data.all %>% 
  filter(c_to_n    > 0) %>% 
#  filter(layer_top >= 0)%>%
  filter(network == "NEON")%>%
  filter(!is.na(mat)) %>%
  filter(!is.na(`15n`)) %>%
  filter(!is.na(n_tot))
as.factor(data.neon$mat)

ggplot(data.neon, aes(x=1/c_to_n, y=`15n`)) + 
  geom_point() +
  #  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  xlim(1/75, 0.15) +
  geom_point(aes(colour=MAT2)) +
  geom_smooth(aes(color=MAT2), 
              method=lm, se=T, fullrange=TRUE) 
#  geom_smooth(aes(), 
#              method=lm, se=FALSE, fullrange=TRUE) 
#  scale_colour_gradient2(low = "blue", mid="white",high = "red", midpoint = 10) +
#  geom_point(aes(color=MAP2)) +
#  ggPredict(m2,interactive=TRUE) +

ggplot(data.neon, aes(x=n_tot, y=`15n`)) + 
  geom_point() +
  #  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
#  xlim(1/75, 0.15) +
  geom_point(aes(colour=MAT2)) +
  geom_smooth(aes(color=MAT2), 
              method=lm, se=T, fullrange=TRUE) 

# ---  look at changes of 15n through profiles --- 
Summary.df <- data.neon %>% # Start by defining the original dataframe, AND THEN...
  group_by(mat) %>% 
  do(min15n = min(.$`15n`, na.rm=T),
     max15n = max(.$`15n`, na.rm=T),
     minCN = min(.$c_to_n, na.rm=T),
     maxCN = max(.$c_to_n, na.rm=T),
     minN = min(.$n_tot, na.rm=T),
     maxN = max(.$n_tot, na.rm=T),
     MAP = mean(.$MAP, na.rm=T),
     MAT = mean(.$MAT, na.rm=T))

data.neon$Layer_top <- as.numeric(data.neon$layer_top)
range(data.neon$Layer_top)

# quick and dirty changes in 15N and %N (using min and max values for each site)
# this ignores how the data were actually collected (e.g. plot-level measurements)
delta.df <- 
  data.neon %>%  #Start by defining the original dataframe, AND THEN...
  group_by(L1) %>% 
  do(e = .$`15n` - min(.$`15n`, na.rm=T),
     f = log(.$n_tot/ max(.$n_tot, na.rm=T)),
     MAT = .$MAT) %>%
  unnest()      #saves headache later?
dput(head(delta.df))
summary(delta.df)
dim(delta.df)


ggplot(delta.df, aes(x=(f), y=(e))) +
  labs(x = 'ln f', 
       y = expression(delta*s - delta*s0)) +
  geom_point(aes(colour=L1))  +
  geom_smooth(aes(colour = L1), method=lm, formula = y ~ x-1, 
              se=FALSE, fullrange=FALSE) + 
  geom_smooth(aes(colour = 'black'), method=lm, formula = y ~ x-1, 
              se=TRUE, fullrange=TRUE, weight=3) 


# accounts for structure of data (plots)
# uses surface samples to calculate differences
site.df <- 
  data.neon %>%  #Start by defining the original dataframe, AND THEN...
  group_by(L2) %>% 
  do(e = .$`15n` - min(.$`15n`[.$Layer_top == 0], na.rm=T),
     f = log(.$n_tot/ max(.$n_tot[.$Layer_top == 0], na.rm=T)),
     L1  = .$L1, 
     MAT = .$MAT) %>%
  unnest()      #saves headache later?

names(site.df)
summary(site.df)

(site.df$L1)
site.df$L1 <- as.factor(site.df$L1)

range(site.df$f)
plot((site.df$f), site.df$e, col=as.numeric(site.df$L1))
m <- lm(e~f-1, data=site.df)
abline(m)
summary(m)

ggplot(site.df, aes(x=(f), y=(e))) +
  labs(x = 'ln f', 
       y = expression(delta*s - delta*s[0])) +
  geom_point(aes(colour=L1))  +
#  geom_smooth(aes(colour = L1), method=lm, formula = y ~ x-1, 
#              se=FALSE, fullrange=FALSE) + 
  geom_smooth(aes(colour = 'black', weight=5), method=lm, formula = y ~ x-1, 
              se=TRUE, fullrange=TRUE) 


xlim = range(site.df$f)
ylim = range(site.df$e)
plot(site.df$e[site.df$L1 == levels(site.df$L1)[1] ]~
     site.df$f[site.df$L1 == levels(site.df$L1)[1] ], 
     ylim=ylim, xlim=xlim)
for (i in 1:length(Site.df$L2)) {
  points(site.df$e[site.df$L1 == levels(site.df$L1)[i] ]~
         site.df$f[site.df$L1 == levels(site.df$L1)[i] ], 
         col=i)
  m <- lm(site.df$e[site.df$L1 == levels(site.df$L1)[i] ]~
          site.df$f[site.df$L1 == levels(site.df$L1)[i] ] -1)
  print(paste( levels(site.df$L1)[i],summary(m)[[4]][[1]], summary(m)[[9]])) 
  abline(m, col=i)
}



#-----------------------------------------

range  <- as.numeric(Summary.df$max15n) - as.numeric(Summary.df$min15n)
min15n <- as.numeric(Summary.df$min15n)
maxCN  <- as.numeric(Summary.df$maxCN)
MAT    <- as.numeric(Summary.df$MAT)
MAP    <- as.numeric(Summary.df$MAP)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(range~min15n, ylab='range 15N', xlab="min 15N")
abline(lm(range~min15n))

plot(range~maxCN, ylab='range 15N', xlab="maxCN")
abline(lm(range~maxCN))

plot(min15n~maxCN, ylab='min 15N', xlab="maxCN")
abline(lm(min15n~maxCN))

plot(min15n~MAT, ylab='min 15N', xlab="MAT")
abline(lm(min15n~MAT))

plot(maxCN~MAT)
abline(lm(maxCN~MAT))

range_15n <- tapply(min(data.neon$`15n`), data.neon$mat, na.rm=T)

summary(data.all$mat)

summary(data.all$soc) 
org <- c('org','Organic','Oa','Oi','Oe') #etc
data.all$mat <- as.numeric(data.all$mat)
data.all %>%
  filter(!hzn %in% org) %>% 
  filter(bd_samp > 0.5) %>%   # also masks out NA values for pools
  filter(layer_bot > 0) %>%
  filter(soc > 0) %>% filter(soc < 25) %>%
  ggplot(aes(x = as.numeric(mat), y = soc)) +
  #   geom_point(alpha = 0.3, position = position_jitter()) 
  geom_boxplot() +
  facet_wrap(~full_name, scale="free")


DATA %>%
  filter(DATA$ph_h2o <= 7 & !is.na(DATA$layer)) %>%
  ggplot(aes(x= al_ox, y = oc, color = as.factor(layer), shape = as.factor(layer) )) +
  ggtitle('Acidic, log10(oc) ~ log10(al_ox)') +
  scale_x_log10() +
  scale_y_log10() +
  ylim(range(DATA$oc, na.rm=T))  +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
               formula = formula, parse = TRUE)+
  geom_point(alpha = 0.3, position = position_jitter()) 

### Filtering The Data
#######################################

## By network
#View network names  
unique(data.all$network)

#Create a new dataframe with data from a specific network
data.neon <- data.all %>% filter(network == "NEON")   
data.dirt <- data.all %>% filter(network == "DIRT")  
unique(data.dirt$full_name)

## By depth 
#Filter for 0-10 cm data
data.surf <- data.all %>% filter(layer_top == 0 & layer_bot == 10)


## By treatment level
#Take a look at names of level 1 treatments
unique(data.all$tx_L1)[1:30]  #only showing the first 30

#Create vector of names to select for control or undisturbed sample locations
ctls <- c("c","Undisturbed","Unmanaged","CTL","CO","Control")

#Filter data by L1 treatment values = CONTROL or UNDISTURBED
data.ctl <- data.all %>% filter(tx_L1 %in% ctls)


### Data Exploration Examples
#######################################

## Data inventory tables

#Create a dataframe table to show the number of values for 'soc' and 'c_tot' by location ('full_name')
data.ctbl <- data.all %>% group_by(full_name) %>% summarize(count.ctot = n_distinct(c_tot, na.rm = TRUE),
                                                            count.soc = n_distinct(soc, na.rm = TRUE))
#Show the first 10 rows of the table
data.ctbl[1:10,]



## Take a look at the number of experimental levels by location

#Create summary table for soc by level
data.explvls <- data.all %>% group_by(full_name) %>% summarize(L1_n = n_distinct(L1, na.rm = TRUE),
                                                               L2_n = n_distinct(L2, na.rm = TRUE),
                                                               L3_n = n_distinct(L3, na.rm = TRUE),
                                                               L4_n = n_distinct(L4, na.rm = TRUE),
                                                               L5_n = n_distinct(L5, na.rm = TRUE))

#Show the first 10 rows of the table
data.explvls[1:10,]


### Plot Examples
#######################################

##Filter data and create box plot by location

#Define the treatment classes to include. Only control type data in this case.
ctls <- c("c","Undisturbed","Unmanaged","CTL","CO","Control")

#Filter data by treatment and depth 
data.plot <- data.all %>% filter(tx_L1 %in% ctls) %>% filter(layer_top == 0 & layer_bot == 10) %>% 
  select(full_name,location_name, site_code, soc, layer_top, layer_bot)                                                                                                            

#Create boxplot  (...may also by piped after the code above)
soc.ctl.bxplot <- ggplot(data.plot, aes(x=full_name, y=soc)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #Rotate x-axis labels 
soc.ctl.bxplot  #display plot



### Map Examples
#######################################

##Currently we have to hotfix the latitude and longitude values to decimal degrees

#Bring in clean coordinate csv
clean.coords <- read.csv("SOM_clean_coords.csv", stringsAsFactors = FALSE)

#Remove duplicate lat & long values to avoid adding rows in join step
lat.clean <- clean.coords %>% distinct(lat, lat.clean)
long.clean <- clean.coords %>% distinct(long, long.clean)

#Join clean coordinate columns to tarball dataframe
data.all <- left_join(data.all, lat.clean, by="lat") %>% left_join(., long.clean, by="long")


##Simple point map
library(maps)

map('world')
points(data.all$long.clean, data.all$lat.clean, col=2, pch=19)


##Filter data and create map of mean 0-10 cm SOC for undistrubed soils only

library('ggmap')

#Starting with the merged clean lat, long dataframe from above (...still named data.all)
#Also using the defined 'ctls' from above to select undisturbed soils only
data.map <- data.all %>% filter(tx_L1 %in% ctls) %>% filter(layer_top == 0 & layer_bot == 10) %>% 
  select(full_name,location_name, site_code, soc, layer_top, layer_bot, lat.clean, long.clean)

#Summarize data to find n, mean, st dev, min, and max soc values by location
data.map.soc <- data.map %>% distinct(full_name,soc,lat.clean,long.clean) %>%  
  filter(!is.na(soc)) %>%  
  group_by(full_name) %>%                                                                                       
  summarise(soc.count = n(),                                                                                            
            soc.mn = mean(soc, na.rm = T),  
            soc.stdv = sd(soc, na.rm = T),
            soc.min = min(soc, na.rm = T),
            soc.max = max(soc, na.rm = T),
            lat = first(lat.clean),                                                                                                  
            long = first(long.clean))

#Map it
mapWorld <- borders("world", colour="gray50", fill="gray50") 
ggplot() + mapWorld + geom_point(data = data.map.soc, aes(x=long, y = lat, color=soc.mn), size=3) + 
  coord_fixed(1.3) +  
  scale_colour_gradientn(colours=rainbow(4))  



#
#
#
#
### More maps
#######################################

## Map locations by NETWORK

data.map.network <- data.all %>% distinct(full_name,network,lat.clean,long.clean) %>% mutate(network = if_else(is.na(network), 'Other', network))

#World map
mapWorld <- borders("world", colour="gray40", fill="gray50") 
ggplot() + mapWorld + 
  geom_point(data = data.map.network, aes(x=long.clean, y = lat.clean, color=network), size=2, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
  coord_fixed(1.3) 

#USA map
mapUSA <- borders("usa", colour="gray40", fill="gray50") 
ggplot() + mapUSA + 
  geom_point(data = data.map.network, aes(x=long.clean, y = lat.clean, color=network), size=4, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
  xlim(-130,-65) + ylim(20,55) +
  coord_fixed(1.3) 

## Map locations with treatment data

data.map.tx_L1 <- data.all %>% distinct(full_name,network,lat.clean,long.clean, tx_L1) %>% filter(!is.na(tx_L1))  

#World map
mapWorld <- borders("world", colour="gray40", fill="gray50") 
ggplot() + mapWorld + 
  geom_point(data = data.map.tx_L1, aes(x=long.clean, y = lat.clean, color='Locations with \n treatment data'), size=3, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
  coord_fixed(1.3) + theme(legend.title=element_blank()) 

#USA map
mapUSA <- borders("usa", colour="gray40", fill="gray50") 
ggplot() + mapUSA + 
  geom_point(data = data.map.tx_L1, aes(x=long.clean, y = lat.clean, color='Locations with \n treatment data'), size=4, alpha=0.7) + #position=position_jitter(h=0.7,w=0.7)) + 
  xlim(-130,-65) + ylim(20,55) +
  coord_fixed(1.3) + theme(legend.title=element_blank()) 
