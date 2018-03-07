# libraries

library(googledrive)
library(googlesheets)
library(tidyverse)


# todos and notes ----

# check header_row, 1 if NA
# for header_name to var consider rename_at, rename_all, or purrr::set_names
# check out select(matches) if select_one poses a problem

# generic function ----


#' @title data_homogonization
#'
#' @description data_homogonization standardizes data files in a Google Drive
#'   directory according to a user-supplied template loosely based on the Powell
#'   Center Template
#'
#' @param directoryName Path to a Google Drive directory where data and a key
#'   file (the template are housed)
#'
#' @import tidyverse
#' @import googlesheets
#' @import googledrive
#'
#' @return Returns a homogonized data file for each input data file
#'
#' @examples
#' \dontrun{
#' data_homogonization('CAP_LTER')
#' }
#'
#' @export

# helper functions

# batch googlesheets file ingestion function
batch_load <- function(fileName) {
  token <- gs_title(fileName)
  dataFile <- gs_read(token)
}


data_homogonization <- function(directoryName) {
  
  # access google dir id for reference
  googleID <- drive_get(directoryName) %>% 
    pull(id)
  
  # list files in google dir
  dirFileList <- drive_ls(path = directoryName) 
  
  # isolate names from google dir
  dirFileNames <- dirFileList %>% 
    select(name) %>% 
    pull(name)
  
  # import files from google dir
  googleDirData <- lapply(dirFileNames, batch_load)
  
  # add filenames
  names(data) <- dirFileNames 
  
  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", capFileNames, ignore.case = T, value = T)
  keyFileToken <- gs_title(keyFileName)
  
  locationData <- gs_read(keyFileToken, ws = 1) %>% 
    filter(!is.na(value))
  
  profileData <- gs_read(keyFileToken, ws = 2)
    filter(!is.na(header_name))
  
}
