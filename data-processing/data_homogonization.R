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
batch_load <- function(fileName, skipRows, missingValueCode) {
  token <- gs_title(fileName)
  dataFile <- gs_read(token, skip = skipRows, na = missingValueCode)
}


data_homogonization <- function(directoryName) {
  
  # Google Drive directory
  
  # access Google directory id for reference
  googleID <- drive_get(directoryName) %>% 
    pull(id)
  
  # list files in Google directory 
  dirFileList <- drive_ls(path = directoryName) 
  
  # isolate names from Google directory 
  dirFileNames <- dirFileList %>% 
    select(name) %>% 
    pull(name)
  
  
  # Key File  
  
  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", dirFileNames, ignore.case = T, value = T)
  keyFileToken <- gs_title(keyFileName)
  
  locationData <- gs_read(keyFileToken, ws = 1) %>% 
    filter(!is.na(Value))
  
  profileData <- gs_read(keyFileToken, ws = 2) %>% 
    filter(!is.na(header_name))
  
  # isolate rows to skip from locationData for data import
  if(length(locationData[locationData$var == 'header_row',]$var) == 1) { 
    skipRows = locationData[locationData$var == 'header_row',]$Value
  } else {
    skipRows = 0
  }
 
  # isolate missing value codes from locationData for data import
  if (length(locationData[locationData$var == 'NA_1',]$var) == 1) {
    mvc1 = locationData[locationData$var == 'NA_1',]$Value }
  if (length(locationData[locationData$var == 'NA_2',]$var) == 1) {
    mvc2 = locationData[locationData$var == 'NA_2',]$Value }
  
  missingValueCode = "NA"
  if (exists('mvc1')) { missingValueCode = mvc1}
  if (exists('mvc2')) { missingValueCode = mvc2}
  if (exists('mvc1') && exists('mvc2')) { missingValueCode = c(paste(mvc1, mvc2))}

  
  # Data Files
  
  # import all (data + key) files from google dir
  googleDirData <- lapply(dirFileNames, 
                          batch_load, 
                          missingValueCode = missingValueCode,
                          skipRows = skipRows)
  
  # add filenames
  names(googleDirData) <- dirFileNames
  
  # as key file is already loaded, remove it from the list of data frames 
  googleDirData <- googleDirData[-grepl("key", names(googleDirData), ignore.case = T)]

  # generate a vector of dataframe columns to keep from key file input to
  # header_name
  varsToKeep <- profileData %>% 
    select(header_name) %>% 
    pull()

  # pull targeted vars from each data frame based on header_names in key file
  googleDirData <- map(googleDirData, select, one_of(varsToKeep))
  
  # rename investigator names to key file names
  googleDirData <- lapply(googleDirData, function(frame) { setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })
  
  # generate wide data frame of location data
  locationDataWide <- locationData %>% 
    select(var, Value) %>% 
    spread(key = var, value = Value)

  # merge location data with each data frame
  listCopy <- lapply(listCopy, function(frame) { merge(locationDataWide, frame, all = T) })
  
  
  
  
  # temporary returns for code development
  return(list(locationData, 
              profileData,
              skipRows,
              missingValueCode))

}
