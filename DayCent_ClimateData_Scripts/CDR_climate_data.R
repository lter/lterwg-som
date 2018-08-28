####################################################
# NCEAS LTER/lter-wg-scicomp                      ##
# Climate data processing for DAYCENT model       ##
# NCEAS repo: LTER/lter-som-climate_data          ##
# Margaux Sleckman, Cristina Sparks, Julien Brun  ##
# scicomp@nceas.ucsb.edu                          ##
####################################################
# Data Site: CDR_climate_data                     ##
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
 #install.packages("weathermetrics", "measurements", "chron", "hydrostats")
library(chron)
library(weathermetrics)
library(measurements)
library(hydrostats)

#---read Daily text file

  #' read_df_gdrive function for text delimited files from gdrive (derived from utils.R - brun)
  #' adapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R

read_df_gdrive <- function(file_id_gdrive, skipper=0, nrows=0,
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


#---read from drive
google_id <- "1vj5fZ1UVKtEOL_UeYlIVwKpALFW9rRFk"
e080_DailyClimateData <- read_df_gdrive(google_id, skipper = 47, nrows = 19874)

#---Rename_Columns
colnames(e080_DailyClimateData)[which(
  colnames(e080_DailyClimateData) %in% c("MaxTemp.degF.","MinTemp.degF.", "Precip.inches.")
  )] <- c("MaxTemp_degF","MinTemp_degF", "Precip_inches")
class(e080_DailyClimateData$Date)
summary(e080_DailyClimateData) # NO NAs =)

#---To Do 
# 0. Remove columns such as name and etc
# 1. Convert dates
# 2. Convert precip from inches to cm
# 3. Convert temp from F to C

#----New_DF
CDR_processed <- e080_DailyClimateData %>% 
  select(Date, MaxTemp_degF, MinTemp_degF, Precip_inches) %>% 
  mutate(Month = sapply(strsplit(Date, "/") , "[", 1)) %>% 
  mutate(Day = sapply(strsplit(Date, "/") , "[", 2)) %>% 
  mutate(Year = sapply(strsplit(Date, "/") , "[", 3)) %>% 
  mutate(`Day of Year` = yday(as.Date(Date, format = "%m/%d/%y")))
head(CDR_processed)

## Fix two digit year to 4 digit year - #(not necessary anymore, read from drive does this automatically)
# CDR_processed$Year <- ifelse(CDR_processed$Year>=62,
#                              paste0("19",CDR_processed$Year),
#                              paste0("20",CDR_processed$Year))

#---df_with conversions
CDR_processed <- CDR_processed %>%
  select(Day, Month, Year, `Day of Year`,
         MaxTemp_degF, MinTemp_degF, Precip_inches) %>%
  mutate(MaxTemp_degC = weathermetrics::fahrenheit.to.celsius(MaxTemp_degF)) %>% 
  mutate(MinTemp_degC = weathermetrics::fahrenheit.to.celsius(MinTemp_degF)) %>% 
  mutate(Precip_cm = measurements::conv_unit(Precip_inches, "inch", "cm")) %>% 
  select(Day, Month, Year, `Day of Year`,
         MaxTemp_degC, MinTemp_degC, Precip_cm)


#--verification
summary(CDR_processed)
# View(CDR_processed)
sample_n(CDR_processed, 50)
# sample_n(CDR_processed, 50)

#---write_csv

ProcessedData_folder <-  "./Climate_data_processed/"
  dir.create(path = ProcessedData_folder, showWarnings = F)

write.csv(CDR_processed,
          file = file.path(ProcessedData_folder, "CDR_processed.csv"),
          na = "-99.9")

# If want site-specific specific folders
# write.csv(CDR_processed,
#           file = file.path("./CDR_Climate_Data/"),
#           na = "-99.9", row.names = F)
  