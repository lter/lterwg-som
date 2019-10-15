library(dplyr)
library(ggplot2)
library(lubridate)

# Load SOM database tarball rds

  # Latest version may be downloaded from Github @:
  #  https://github.com/lter/lterwg-som-shiny/tree/master/shiny_SOM

  # Also available from Google Drive @:
  #  https://drive.google.com/drive/folders/1bQcTB8lZtE0vjuZ_kvPvDbHCyv0PLbt6?usp=sharing

  tarball <- readRDS("somCompositeData_2019-10-13.rds")
  
  #Filter for times series only
  df <- tarball %>% filter(., time_series == "YES" | time_series == "Yes")
  
#format observation dates    

  #Pull in observation_date_1 if observation_date is NA
  df <- df %>% mutate(
    observation_date_fix = case_when(
      is.na(observation_date) ~ observation_date_1,
      !is.na(observation_date) ~ observation_date
      ))
  
### END SOM DAY 1 - COWEETA FIX NEEDS TESTING, NEED FIX FOR DIRT-BOUSSON etc.  
  #Fix Coweeta
  #df <- df %>% mutate(
  #  observation_date_fix = case_when(
  #    location_name == "Coweeta basin" ~ paste0(observation_date_1,"-",observation_date_2,"-01"),
  #   location_name != "Coweeta basin" ~ observation_date
  #  ))
  
  #Lubridate auto-convert to date
  df$obs_date_lub <- as_date(df$observation_date_fix)
  
  #Fill in dates lubridate could not convert with raw values from observation_date
  df <- df %>% mutate(
    observation_date_fix = case_when(
      !is.na(obs_date_lub) ~ as.character(obs_date_lub),
      is.na(obs_date_lub) ~ paste0(observation_date_fix)
    )
  )
  
  #Convert date formats manually, using string length (not fool proof, but it works for now...)
  df <- df %>% mutate(
    observation_date_fix = case_when(
      nchar(observation_date_fix) == 10 ~ observation_date_fix,
      nchar(observation_date_fix) == 4 ~ paste0(observation_date_fix, "-01-01"),
      nchar(observation_date_fix) == 3 ~ "NA",
      nchar(observation_date_fix) == 8 ~ paste0(substr(observation_date_fix,1,4),"-",
                                                substr(observation_date_fix,5,6),"-01")
      )
    )

  #Convert column data type to date 
  df$observation_date_fix <- as_date(df$observation_date_fix) 
 
  na.omit(df$observation_date_fix)
  
 

#graph
  ggplot(df, aes(y=observation_date_fix, x=location_name, color=network)) + geom_point(size=3) +
    theme(axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(size = 14),
          legend.text=element_text(size=16)) +
    ggtitle("SOM Database Time Series Coverage") +
    xlab("Location Name") + ylab("Year") 
    
    


  

  



