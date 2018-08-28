####################################################
# NCEAS LTER/lter-wg-scicomp                      ##
# Climate data processing for DAYCENT model       ##
# NCEAS repo: LTER/lter-som-climate_data          ##
# Margaux Sleckman, Cristina Sparks, Julien Brun  ##
# scicomp@nceas.ucsb.edu                          ##
####################################################
# Data Site: CWT_climate_data                     ##
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
library(purrr)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
#install.packages("weathermetrics", "measurements", "chron", "hydrostats")
#library(chron)
library(weathermetrics)
library(measurements)
library(hydrostats)

#---read_csv_gdive (text delimited) from gdrive (derived from utils.R - brun)
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

#---read_from_drive
google_id <- "1z01CluhJzi0N1CXwZRU55icLyveU7j6i"

CWT_climatestation <- read_csv_gdrive(google_id, skipper = 5)   #to reintroduce colnames
colnames(CWT_climatestation) <- c("Year", "Month", "Air_temp_mean", "Air_temp_max_C", "Air_temp_min_C",
                                  "Precipitation_RG19.MM.", "Solar_Radiation", "Precipitation_RG31.mm.", "Total_evaporation")
head(CWT_climatestation)
summary(CWT_climatestation)
  # NOTE: no day column

#To Do: 
# 0. Remove columns such as name and etc
# 1. Make yday - can't no day column
# 2. Precip data converted from mm to cm
# 3. Sum precip data 

#----New DF
CWT_processed <- CWT_climatestation %>% 
  select(Year, Month, Air_temp_max_C, Air_temp_min_C, Precipitation_RG19.MM., Precipitation_RG31.mm.) %>% 
  mutate(Sum_Precip_cm = Precipitation_RG19.MM. + Precipitation_RG31.mm.) %>% 
  mutate(Day = NA, `Day of Year`= NA) 

CWT_processed <- CWT_processed %>% 
  mutate(Sum_Precip_cm = measurements::conv_unit(Sum_Precip_cm, "mm", "cm")) %>% 
  select(Year, Month, Day, `Day of Year`, Air_temp_max_C, Air_temp_min_C, Sum_Precip_cm) %>% 
  magrittr::set_colnames(c("Year", "Month", "Day", "Day of Year", "Max_temp_C_monthly", "Min_temp_C_monthly", "Sum_Precip_cm"))

#--verify 
summary(CWT_processed)
# sample_n(CWT_processed, 50)


#---write-------------
write.csv(CWT_processed, file = file.path("./Climate_data_processed/", "CWT_processed.csv"),
          na = "-99.9", row.names = F)

# if want site-specific specific folders
# write.csv(CWT_processed, file = file.path("./CWT_climate_data/", "CWT_processed.csv"),
#           na = "-99.9", row.names = F)
