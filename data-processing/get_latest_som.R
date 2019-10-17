
# README ------------------------------------------------------------------

# The function get_latest_som will download the latest SOM data tarball into the
# current directory, and load it into memory. Run as:
# 1. source('get_latest_som.R')
# 2. somTarball <- get_latest_som()

# The user should recall that the tarball has been downloaded to the working
# directory.

# libraries ---------------------------------------------------------------

library(googledrive)
library(tidyverse)
library(lubridate)


# get_latest_som() --------------------------------------------------------

get_latest_som <- function() {

# access content of homoged_and_bound google dir --------------------------

# boundHomogedContent <- drive_ls(path = 'https://drive.google.com/open?id=1bQcTB8lZtE0vjuZ_kvPvDbHCyv0PLbt6')
boundHomogedContent <- drive_ls(path = 'homoged_and_bound_output')
boundHomogedContentSummary <- boundHomogedContent$drive_resource %>% 
  {
    tibble(
      googleID = map_chr(., "id"),
      webLink = map_chr(., "webContentLink"),
      dataset = map_chr(., "name"),
      createdTime = as_datetime(map_chr(., "createdTime"))
    )
  }


# download rds to current directory using google id -----------------------

tarballLatestGoogleId <- boundHomogedContentSummary %>% 
  filter(createdTime == max(createdTime)) %>% 
  pull(googleID)

drive_download(as_id(tarballLatestGoogleId))


# read downloaded rds into memory -----------------------------------------

tarballLatestFilename <- boundHomogedContentSummary %>% 
  filter(createdTime == max(createdTime)) %>% 
  pull(dataset )

somTarball <- readRDS(tarballLatestFilename)


# delete downloaded file --------------------------------------------------

if (file.exists(tarballLatestFilename)) { file.remove(tarballLatestFilename) }


# return tarball ----------------------------------------------------------

return(somTarball)


} # close function
