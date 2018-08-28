####################################################
# NCEAS LTER/lter-wg-scicomp                      ##
# Climate data processing for DAYCENT model       ##
# NCEAS repo: LTER/lter-som-climate_data          ##
# Margaux Sleckman, Cristina Sparks, Julien Brun  ##
# scicomp@nceas.ucsb.edu                          ##
####################################################
# Data Site: UMBS_climate_data                    ##
####################################################

# There are 7 required columns:
# 1) Day of Month
# 2) Month (1-12)
# 3) Year (4 digits)
# 4) Day of Year (1-366) Note that on leap years 366 days are required
# 5) Maximum daily temperature (C)
# 6) Minimum daily temperature (C)
# 7) Precipitation (cm/day)

# Missing temperature or precipitation data is expressed as -99.9.
# Maximum temperature is the highest temperature that occurred during the day
# Minimum temperature is the lowest temperature that occurred during the day
# Precipitation is the sum of precipitation events during the day.

#-----libraries
library(googledrive)
library(purrr)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
#install.packages("anytime")
library(anytime)

#---read_csv_gdive from gdrive 
    #' (derived from utils.R - brun)
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

#-----read-----
google_id <- "1K0xui5aTT7ZGvsibRc31d78VqUdb6it7"  #https://drive.google.com/open?id=1K0xui5aTT7ZGvsibRc31d78VqUdb6it7
UMBS_04to14 <- read_csv_gdrive(google_id)
names(UMBS_04to14)

#-----modify-----
UMBS_04to14$date <- paste(UMBS_04to14$YEAR,"-",UMBS_04to14$MONTH,"-",UMBS_04to14$DAY, sep = "")    #Create a column in a date format
UMBS_04to14$date <- as.Date(UMBS_04to14$date)

UMBS_processed <- UMBS_04to14 %>% 
  mutate(dayofyear = lubridate::yday(UMBS_04to14$date)) %>%                 #Add day of year column
  select(YEAR, MONTH, DAY, MIN_TEMP, MAX_TEMP, SUM_PREC, dayofyear) %>%     #Select relevant columns
  mutate(SUM_PREC = measurements::conv_unit(SUM_PREC, "mm", "cm")) %>% 
  set_colnames(c("Year", "Month", "Day of Month", 
                 "Minimum Daily Temperature (C)",
                 "Maximum Daily Temperature (C)",
                 "Sum of Precipitation-cm",
                 "Day of Year"))

UMBS_processed <- UMBS_processed[,c(1,2,3,7,4,5,6)] # Reorder columns so dayofyear is adjacent to other date columns

#-----Verification-----
summary(UMBS_processed)
# sample_n(UMBS_processed, 50)
# sample_n(UMBS_processed, 50)
# sample_n(UMBS_processed, 50)

#---write CSV 
write.csv(UMBS_processed, file = file.path("./Climate_data_processed/", "UMBS_processed.csv"),
          na = "-99.9", row.names = F)

# if want site-specific specific folders
write.csv(UMBS_processed, file = file.path("./UMBS_DIRT_Climate_Data/", "UMBS_processed.csv"),
          na = "-99.9", row.names = F)
