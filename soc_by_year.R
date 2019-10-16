#Load libraries
library(dplyr)
library(ggplot2)

### Loading The Data
########################################

# Change path to wherever you have the tarball.csv stored locally
data.all <- read.csv("C:/Google Drive/Code/R/SOM_tarball_11-9-2018.csv", stringsAsFactors = FALSE)  

# Unite site codes and location names 
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

# NEON full_name and site_code fix
data.all <- data.all %>% mutate(full_name = replace(full_name,full_name==" ", "NEON"))
data.all <- data.all %>% mutate(site_code = replace(site_code,full_name=="NEON", "NEON"))


# Create year column from observation_date & observation_data_1

data.all$year <- substr(data.all$observation_date, start = 1, stop = 4)
data.all$year_1 <- substr(data.all$observation_date_1, start = 1, stop = 4)

data.all$year <- as.numeric(data.all$year)
data.all$year_1 <- as.numeric(data.all$year_1)

data.all <- data.all %>% mutate(year.all = coalesce(year, year_1))

# Add years from exp levels
  #Data check part a
  data.before <- data.all %>% filter(!is.na(year.all))

data.all$yr_L1 <- as.numeric(data.all$L1)
data.all <- data.all %>% mutate(yr_L1 = "is.na<-"(yr_L1, yr_L1 < 1900)) %>% 
                     mutate(yr_L1 = "is.na<-"(yr_L1, yr_L1 > 2020)) %>% 
                     mutate(year.all = coalesce(year.all, yr_L1))

  #Check unique values added from L1
  unique(data.all$yr_L1)
  sum(!is.na(data.all$yr_L1))

data.all$yr_L2 <- as.numeric(data.all$L2)
data.all <- data.all %>% mutate(yr_L2 = "is.na<-"(yr_L2, yr_L2 < 1900)) %>% 
  mutate(yr_L2 = "is.na<-"(yr_L2, yr_L2 > 2020)) %>% 
  mutate(year.all = coalesce(year.all, yr_L2))

  #Check unique values added from L2
  unique(data.all$yr_L2)
  sum(!is.na(data.all$yr_L2))
  
data.all$yr_L3 <- as.numeric(data.all$L3)
data.all <- data.all %>% mutate(yr_L3 = "is.na<-"(yr_L3, yr_L3 < 1900)) %>% 
  mutate(yr_L3 = "is.na<-"(yr_L3, yr_L3 > 2020)) %>% 
  mutate(year.all = coalesce(year.all, yr_L3))
  
  #Check unique values added from L3
  unique(data.all$yr_L3)  
  sum(!is.na(data.all$yr_L3))
  
data.all$yr_L4 <- as.numeric(data.all$L4)
data.all <- data.all %>% mutate(yr_L4 = "is.na<-"(yr_L4, yr_L4 < 1900)) %>% 
  mutate(yr_L4 = "is.na<-"(yr_L4, yr_L4 > 2020)) %>% 
  mutate(year.all = coalesce(year.all, yr_L4))
  
  #Check unique values added from L4
  unique(data.all$yr_L4)  
  sum(!is.na(data.all$yr_L4))
  
  #Data check part b
  data.after <- data.all %>% filter(!is.na(year.all))




#filter to get only soc data > 0
data.soc <- data.all %>% filter(!is.na(soc)) %>% filter(soc > 0)  ###Why are some values less than zero?

#Filter proper data range for plot
data.soc <- data.all %>% filter(soc >0) %>% filter(soc < 50) 

#plot
ggplot(data.soc, aes(x=year.all, y=soc, colour=site_code)) + geom_point()



