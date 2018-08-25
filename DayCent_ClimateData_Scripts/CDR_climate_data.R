####################################################
# NCEAS LTER/lter-wg-scicomp                      ##
# Climate data processing for DAYCENT model       ##
# NCEAS repo: LTER/lter-som-climate_data          ##
# Margaux Sleckman, Cristina Sparks, Julien Brun  ##
# scicomp@nceas.ucsb.edu                          ##
####################################################
# Data Site: CDR_climate_data                     ##
####################################################

# niwot_forest.wth
# The actual format of a DayCent weaher file, a text file with a ".wth" extension.  
# 
# There are 7 required columns:
#   1) Day of Month
# 2) Month (1-12)
# 3) Year (4 digits)
# 4) Day of Year (1-366) Not that on leap years 366 days are required
# 5) Maximum daily temperature (C)
# 6) Minimum daily temperature (C)
# 7) Precipitation (cm/day) --> in email written in mm (precip usually in mm...right?)
# 
# Missing temperature or precipitation data is expressed as -99.9.
# Maximum temperature is the highest temperature that occurred during the day
# Minimum temperature is the lowest temperature that occurred during the day
# Precipitation is the sum of precipitation events during the day.
# 
# OBSclimate_1999-2013.csv
# Example of a 30-minute file from Niwot Ridge Forest
# 
# format_Niwot_Forest_DayCent3.R
# R script that reads the 30-minute file (OBSclimate_1999-2013.csv) and creates two daily files:
#   met_niwot_forest.csv 
# - daily meteorological variables, without day 366 (day 366 averaged with 365 on leap years)
# met_niwot_forest_daycent.csv 
# - daily variables that will be inserted into DayCent weather file 
# (shortwave radiation and vpd are optional, so are relative humidity and windspeed).

###---libraries---
library(googledrive)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
 #install.packages("weathermetrics", "measurements", "chron", "hydrostats")
library(chron)
library(weathermetrics)
library(measurements)
library(hydrostats)

###---read Daily text file - noMetadata version

#---function read_delim_gdive (text delimited) from gdrive (derived from utils.R - brun)
  #' adapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R

read_df_gdrive <- function(file_id_gdrive,
                           skipper=0, nrows=0,
                           gdrive_url="https://drive.google.com/uc?export=download&id="){
  # Add some checks
  stopifnot(is.character(file_id_gdrive))
  if(grepl("id|http", file_id_gdrive)) {
    stop("please ckeck your id. You should pass only the id hash, not the full URL.\nTo get the id, right-click on the file in Google Drive and 'Get shareable link'")
  }
  # Create the full URL for the files
  download_link <- paste0(gdrive_url,file_id_gdrive)
  # Import the csv as Data frame
  data_df <- read.delim(file = download_link, header = TRUE, skip = skipper, nrows = nrows, stringsAsFactors = FALSE)
  return(data_df)
}

google_id <- "1vj5fZ1UVKtEOL_UeYlIVwKpALFW9rRFk"
e080_DailyClimateData <- read_df_gdrive(google_id, skipper = 47, nrows = 19874)
head(e080_DailyClimateData)
tail(e080_DailyClimateData)

#rename specific columns
colnames(e080_DailyClimateData)[which(
  colnames(e080_DailyClimateData) %in% c("MaxTemp.degF.","MinTemp.degF.", "Precip.inches.")
  )] <- c("MaxTemp_degF","MinTemp_degF", "Precip_inches")
class(e080_DailyClimateData$Date)
summary(e080_DailyClimateData) # NO NAs =)

#---To Do: 
# 0. Remove columns such as name and etc
# 1. convert dates
# 2. convert precip from inches to mm
# 3. Convert temp from F to C

#----New_DF
CDR_processed <- e080_DailyClimateData %>% 
  select(Date, MaxTemp_degF, MinTemp_degF, Precip_inches) %>% 
  mutate(Month = sapply(strsplit(Date, "/") , "[", 1)) %>% 
  mutate(Day = sapply(strsplit(Date, "/") , "[", 2)) %>% 
  mutate(Year = sapply(strsplit(Date, "/") , "[", 3)) %>% 
  mutate(`Day of Year` = yday(as.Date(Date, format = "%m/%d/%y")))
head(CDR_processed)

# #Fix two digit year to 4 digit year - #(not necessary anymore, read from drive does this automatically)
# CDR_processed$Year <- ifelse(CDR_processed$Year>=62,
#                              paste0("19",CDR_processed$Year),
#                              paste0("20",CDR_processed$Year))

#---df_with conversions
CDR_processed <- CDR_processed %>%
  select(Day, Month, Year, `Day of Year`,
         MaxTemp_degF, MinTemp_degF, Precip_inches) %>%
  mutate(MaxTemp_degC = weathermetrics::fahrenheit.to.celsius(MaxTemp_degF)) %>% 
  mutate(MinTemp_degC = weathermetrics::fahrenheit.to.celsius(MinTemp_degF)) %>% 
  mutate(Precip_mm = measurements::conv_unit(Precip_inches, "inch", "mm")) %>% 
  select(Day, Month, Year, `Day of Year`,
         MaxTemp_degC, MinTemp_degC, Precip_mm)


#--verification
summary(CDR_processed)
# View(CDR_processed)
sample_n(CDR_processed, 50)
# sample_n(CDR_processed, 50)

#---write
write.csv(CDR_processed,
          file = file.path("./CDR_Climate_Data/", "CDR_processed.csv"),
          na = "-99.9", row.names = F)


  