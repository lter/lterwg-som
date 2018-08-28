####################################################
# NCEAS LTER/lter-wg-scicomp                      ##
# Climate data processing for DAYCENT model       ##
# NCEAS repo: LTER/lter-som-climate_data          ##
# Margaux Sleckman, Cristina Sparks, Julien Brun  ##
# scicomp@nceas.ucsb.edu                          ##
####################################################
# Data Site: AND_climate_data                     ##
####################################################


# There are 7 required columns:
#   1) Day of Month
# 2) Month (1-12)
# 3) Year (4 digits)
# 4) Day of Year (1-366) Not that on leap years 366 days are required
# 5) Maximum daily temperature (C)
# 6) Minimum daily temperature (C)
# 7) Precipitation (cm/day)
# 
# Missing temperature or precipitation data is expressed as -99.9.
# Maximum temperature is the highest temperature that occurred during the day
# Minimum temperature is the lowest temperature that occurred during the day
# Precipitation is the sum of precipitation events during the day.

#---libraries
library(googledrive)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
#source("utils.R")

#---read_csv_gdive function to read from gdrive
  #'  (derived from utils.R - brun) 
  #' adapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R
 
read_csv_gdrive <- function(file_id_gdrive, skipper=0, nrows=0, gdrive_url="https://drive.google.com/uc?export=download&id="){
  # Add some checks
  stopifnot(is.character(file_id_gdrive))
  if(grepl("id|http", file_id_gdrive)) {
    stop("please ckeck your id. You should pass only the id hash, not the full URL.\nTo get the id, right-click on the file in Google Drive and 'Get shareable link'")
  }
  # Create the full URL for the files
  download_link <- paste0(gdrive_url,file_id_gdrive)
  # Import the csv as Data frame
  data_df <- read.csv(file = download_link, header = TRUE, skip = skipper, nrows=nrows, stringsAsFactors = FALSE)
  return(data_df)
}

#---read from drive
google_id <- "1NLgVTqohUOXUdmwzuINwEBVv9SPNTqW5" # get this id by right-clicking on the file in Google Drive and "Get shareable link"
AND_airtemp_94to16 <- read_csv_gdrive(google_id)

names(AND_airtemp_94to16)
summary(AND_airtemp_94to16)
length(which(is.na(AND_airtemp_94to16$DATE))) 

#--To Do:
# 1. Select relevant columns
# 2. Split date and create day of year

#---New_df
AND_processed <- AND_airtemp_94to16 %>% 
  select(DATE, AIRTEMP_MAX_DAY, AIRTEMP_MIN_DAY)

#---Spliting dates columns into yr, month, day, and yday

class(AND_airtemp_94to16$DATE) #should be character

# Splitting day, month year, applied on DATE in character format
AND_processed$Month <- sapply(strsplit(AND_processed$DATE, "/") , "[", 1)
AND_processed$Day <- sapply(strsplit(AND_processed$DATE, "/") , "[", 2)
AND_processed$Year <- sapply(strsplit(AND_processed$DATE, "/") , "[", 3)

# as_Date
AND_processed$DATE <- as.Date(AND_processed$DATE, "%m/%d/%Y") 
class(AND_processed$DATE) #should be date
length(which(is.na(AND_processed$DATE))) 
# day of year - can only apply on DATE in date format
AND_processed$`Day of Year` <- yday(AND_processed$DATE)

#---finalizing_df
AND_processed <- AND_processed %>%
  select(Day, Month, Year, `Day of Year`,
         AIRTEMP_MAX_DAY, AIRTEMP_MIN_DAY) %>% 
  mutate(precip = NA) %>% 
  magrittr::set_colnames(c("Day of month", "Month", "Year",
                 "Day of Year",
                 "Maximum daily temperature (C)",
                 "Minimum daily temperature (C)", "Precipitation (cm/day)"))

#---replace_NAs
# AND_processed[is.na(AND_processed)] <- -99.9  # --> decided to fix NA when writing csv.

#---Verification
# View(AND_processed)
summary(AND_processed)
sample_n(AND_processed, 50)

#---write CSV 
write.csv(AND_processed,
          file = file.path("./Climate_data_processed/", "AND_processed.csv"),
          na = "-99.9", row.names = F)

  # if want specific folders

  # write.csv(AND_processed,
  #         file = file.path("./AND_climate_data/", "AND_processed.csv"),
  #         na = "-99.9", row.names = F)


