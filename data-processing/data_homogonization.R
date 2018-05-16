# libraries

library(googledrive)
library(googlesheets)
library(tidyverse)


# todos and notes ----

# check header_row, 1 if NA - done!
# for header_name to var consider rename_at, rename_all, or purrr::set_names - done!
# check out select(matches) if select_one poses a problem - done!
# location data are being sorted alphabetically, not a huge problem but need to correct
# warnings to file
# on-run warns for: 
  # multiple header_names of same name

#Derek's notes
  #5-14-18: There's a bug in the header row names.


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


#DEBUG input folders
#dbg.folders <- c('AND_10YR_CN','AND_10YR_DenseFrac','AND_15YR_CN')


data_homogonization <- function(directoryName) {
  
  #DEBUG line for stepping through a specific folder
  #directoryName <- dbg.folders[2]
  
  # access Google directory id for reference
  googleID <- drive_get(directoryName) %>% 
    pull(id)
  
  # list files in Google directory 
  dirFileList <- drive_ls(path = directoryName) 
  
  # isolate names from Google directory 
  # REPLY: could purge duplicates here but is that wise - maybe this should be done manually?
  dirFileNames <- dirFileList %>% 
    select(name) %>% 
    pull(name)
  
  
  # Key File  
  
  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", dirFileNames, ignore.case = T, value = T)
  keyFileToken <- gs_title(keyFileName)
  
  locationData <- gs_read(keyFileToken, ws = 1) %>% 
    filter(!is.na(Value)) %>% 
    add_row(Value = googleID, var = 'google_id', .before = 1)
  
  profileData <- gs_read(keyFileToken, ws = 2) %>% 
    filter(!is.na(header_name))
  
  # isolate rows to skip from locationData for data import
  if(length(locationData[locationData$var == 'header_row',]$var) == 1) { 
    skipRows = as.numeric(locationData[locationData$var == 'header_row',]$Value)-1
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

  ### Remove duplicate files, for some reason I lose the filename if I do this any other way than an if else
  # REPLY: I think it is too late here, this may have to be done at the 'dirFileList' stage
  # why just the first entity?
  if(length(googleDirData) > 1 | length(unique(googleDirData)) == 1) {
    file.nm <- names(googleDirData[1])
    googleDirData <- googleDirData[1]
    names(googleDirData) <- file.nm
  } else {
    print("Multiple File Error)")
  }
  
  
  # generate a vector of dataframe columns to keep from key file input to
  # header_name
  varsToKeep <- profileData %>% 
    select(header_name) %>% 
    pull()

  # pull targeted vars from each data frame based on header_names in key file
  googleDirData <- map(googleDirData, select, one_of(varsToKeep))
  
  # rename investigator names to key file names
  googleDirData <- lapply(googleDirData, function(frame) { setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })
  
  #Cut out second 'header row'
  #NOTE: Maybe we should do this more carefully?
  locationData <- locationData[!duplicated(locationData$var),]
  
  # generate wide data frame of location data
  locationDataWide <- locationData %>% 
    select(var, Value) %>% 
    spread(key = var, value = Value)

  # merge location data with each data frame
  googleDirData <- lapply(googleDirData, function(frame) { merge(locationDataWide, frame, all = T) })
  
  # rename files to include base name + indication of homogenization 
  names(googleDirData) <- paste0(str_extract(names(googleDirData), "^[^\\.]*"), "_HMGZD")
  
  #Notes
  #Grab location data references and notes
  loc.notes <- locationData %>% mutate_all(as.character) %>% filter(!is.na(var_notes)) %>% select(-Value, -Unit)
  
  #Grab profile data notes and comments
  prof.notes <- profileData %>% filter(!is.na(Notes) | !is.na(Comment)) %>% select(Var_long,var,Level,Notes,Comment) %>% rename(var_notes=Notes)
  
  #Merge notes
  notes <- full_join(loc.notes,prof.notes)
  
  #Add filename and site code
  # getting errors with notes$File
  notes$File <- names(googleDirData)
  # why hard-code [16]?
  notes$Site <- locationData$Value[16]
  
  #Arrange filename first
  notes <- notes %>% select(File, Site, everything())
  
  
  #Data and file output 
  #This is person specific for now...
  
  #Stevan
  # write files to a temporary location
  googleDirData %>%
    names(.) %>%
    # map(~ write_csv(googleDirData[[.]], paste0("~/Desktop/temp_som_outspace/", .)))
    map(~ write_csv(googleDirData[[.]], paste0("~/Desktop/temp_som_outspace/", ., ".csv")))
 
  #Derek 
  write.csv(googleDirData[1], paste0(names(googleDirData),".csv"))
  write.csv(notes, paste0(names(googleDirData),"_NOTES.csv"))
}


#HAVEN'T TESTED GD UPLOAD YET, DPierson 05-14-2018

tempToGoogleDrive <- function() {
  
  # identify directory with files (not full.names=T)
  filesToUpload <- list.files(path="~/Desktop/temp_som_outspace/",
                              full.names=F,
                              recursive=FALSE)

  filesToUpload %>% 
    names(.)
  drive_upload('event1513rsvps.csv', path = "temp_som_hmgzd_output/", name = "", type = "spreadsheet")
  
}