####################################################
# NCEAS LTER/lter-wg-scicomp                      ##
# Climate data processing for DAYCENT model       ##
# NCEAS repo: LTER/lter-som-climate_data          ##
# Margaux Sleckman, Cristina Sparks, Julien Brun  ##
# scicomp@nceas.ucsb.edu                          ##
####################################################
# Data Site: HBR_climate_data                    ##
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

#-----libraries-----
library(googledrive)
library(purrr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
#install.packages("anytime")
library(anytime)

#-----read-----

#---function read_delim_gdive (text delimited) from gdrive (derived from utils.R - brun)
#' adapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R

read_df_gdrive <- function(file_id_gdrive,
                           skipper=0, nrows=0, sep = ",",
                           gdrive_url="https://drive.google.com/uc?export=download&id="){
  # Add some checks
  stopifnot(is.character(file_id_gdrive))
  if(grepl("id|http", file_id_gdrive)) {
    stop("please ckeck your id. You should pass only the id hash, not the full URL.\nTo get the id, right-click on the file in Google Drive and 'Get shareable link'")
  }
  # Create the full URL for the files
  download_link <- paste0(gdrive_url,file_id_gdrive)
  # Import the csv as Data frame
  data_df <- read.delim(file = download_link, header = TRUE, skip = skipper, nrows = nrows, stringsAsFactors = FALSE, sep = sep)
  return(data_df)
}

# Read temperature file

google_id_temp <- "1gePzJ4uVODX0z7C1XgjVLlUrP38Qdabl" # get this id by right-clicking on the file in Google Drive and "Get shareable link"
HBR_temp_55to14 <- read_df_gdrive(google_id_temp, sep = ",")
head(HBR_temp_55to14)
tail(HBR_temp_55to14)

# Read precip file

google_id_precip <- "1gn5E-0GiQV3a24fZ7T4c6IbYBQzEw0ik" # get this id by right-clicking on the file in Google Drive and "Get shareable link"
HBR_precip_56to14 <- read_df_gdrive(google_id_precip, sep = ",")
head(HBR_precip_56to14)
tail(HBR_precip_56to14)

#-----merge-----

# Focus on only Station 1 for now, drop other columns

HBR_temp_55to14 <- rename(HBR_temp_55to14, "Date" = "DATE") # Make sure both Date columns are written the same
HBR_merge <- merge(HBR_temp_55to14, HBR_precip_56to14, by = "Date", all.x = TRUE) # Merge datasets based on Date column
HBR_processed <- HBR_merge %>% 
  select("Date", "STA1_MAX", "STA1_MIN", "WS_1") # Only need these columns


#-----modify-----

# Only need to create date columns

HBR_processed$Date <- as.character(HBR_processed$Date)

HBR_processed <- HBR_processed %>% 
  mutate(Year = sapply(strsplit(Date, "-") , "[", 1)) %>% 
  mutate("Month" = sapply(strsplit(Date, "-") , "[", 2)) %>% 
  mutate("Day of Month" = sapply(strsplit(Date, "-") , "[", 3)) %>% 
  mutate("Day of Year" = lubridate::yday(as.Date(HBR_processed$Date))) %>% # Add day of year column
  select(-Date) %>% 
  set_colnames(c("Maximum Daily Temperature (C)", "Minimum Daily Temperature (C)", "Sum of Precipitation", "Year", "Month", "Day of Month", "Day of Year")) 

HBR_processed <- HBR_processed[,c(4,5,6,7,2,1,3)] # Reorder columns so dayofyear is adjacent to other date columns

#---write CSV 

ProcessedData_folder <-  "./Climate_data_processed/"
dir.create(path = ProcessedData_folder, showWarnings = F) #returns true/false

write.csv(HBR_processed,
          file = file.path(ProcessedData_folder, "HBR_processed.csv"),
          na = "-99.9", row.names = F)


# if want individual folder
#write.csv(HBR_processed, file = file.path("./HBR_Climate_Data/", "HBR_processed.csv"),
#          na = "-99.9", row.names = F)

