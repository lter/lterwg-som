# QuickSOM_dataCoverage"
# Wil Wieder
# Built on work from Derek Pierson & Jessica Moore
# April 3, 2020

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(tidyverse)

data.all <- readRDS("/Users/wwieder/Will/git_repos_local/lterwg-som/somCompositeData_2020-02-11.rds")  

data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

#get first four characters from observation_date
#first, make sure all are characters 
data.all$site_code <- str_replace(data.all$site_code, "HRF", "HFR")
data.all$observation_date <- str_replace(data.all$observation_date, "Aug-Sep 2011", "2011")
data.all$year<-substr(data.all$observation_date, start=1, stop=4)
data.all$year_all<-as.numeric(data.all$year)

#check for unique 4 digit values in year
print(unique(data.all$year_all))

#-------------------------------------------
## SOC Percents
### Clean up SOC percent values
#filter out negative soc%
data.soc.percent<-data.all %>% 
  filter(!is.na(lyr_soc)) #%>% filter(lyr_soc>0) %>% filter(lyr_soc<100)

soc.per<-ggplot(data.all %>% 
                  filter(network!="none")  %>%
                  filter(layer_bot>0), 
                aes(x=site_code, y=as.numeric(year)), na.omit=T)+
  geom_point(aes(color=network), show.legend = FALSE) +
  #  geom_point() +
  xlab("Site") + ylab("Year") +
  scale_y_continuous(breaks=seq(1975,2019,5),na.value=2020)+
  scale_x_discrete()+#labels = abbreviate) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6)) +
  facet_grid(.~network, scales = "free_x", space='free_x', drop=FALSE)

soc.per
# Not using %SOC here, should we be consistent?

soc.dep<-ggplot(data.all %>% 
                  filter(network!="none")  %>%
                  filter(layer_bot>0), 
                aes(x=site_code, y=layer_bot), na.omit=T)+
  geom_point(aes(color=network), show.legend = FALSE, size=1) +
  #  geom_point() +
  xlab("Site") + ylab("Depth") +
  scale_y_reverse() +
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6)) +
  facet_grid(.~network, scales = "free_x", space='free_x', drop=FALSE) 

soc.dep

pdf(file = "fig4.pdf",   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 3) # The height of the plot in inches

soc.per
dev.next()

soc.dep
dev.off()

temp = data.all %>% filter(network ==  "NEON") 
print(unique(temp$site_code ))

