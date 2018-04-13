# libraries

library(googledrive)
library(googlesheets)
library(tidyverse)

getwd()
setwd("C:/Users/Derek/Google Drive/Code/GitHub/lterwg-som/HMGZD exports")

# todos and notes ----

# check header_row, 1 if NA - done!
# for header_name to var consider rename_at, rename_all, or purrr::set_names - done!
# check out select(matches) if select_one poses a problem - done!
# location data are being sorted alphabetically, not a huge problem but need to correct
# warnings to file
# on-run warns for: 
  # multiple header_names of same name


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
                  
#FINDING KEY KEY FILE                                   
###################                  
  #CURRENTLY STARTING WITH FOLDER NAME TO FIND THE KEY KEY FILE

#I've added these print lines to help track ftn progress and debug errors    
print("New data homogenization start")
  
  ###TEST LINE for stepping through one file
    #directoryName <- hja[1]
  
  # access Google directory id for reference
  googleID <- drive_get(directoryName) %>% 
    pull(id)
  
  # list files in Google directory 
  dirFileList <- drive_ls(path = directoryName) 
  
  # isolate names from Google directory 
  dirFileNames <- dirFileList %>% 
    select(name) %>% 
    pull(name)
  
  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", dirFileNames, ignore.case = T, value = T)
  keyFileToken <- gs_title(keyFileName)  #<--Could we start here if we had list of key file names?

print("Google Drive files located")
  
#READING THE KEY KEY FILE  
#############################

#Read in the location data from the key key 
  locationData <- gs_read(keyFileToken, ws = 1) %>% 
    filter(!is.na(Value)) %>% 
    add_row(Value = googleID, var = 'google_id', .before = 1)
  
#Read in the profile data from key key  
  profileData <- gs_read(keyFileToken, ws = 2) %>% 
    filter(!is.na(header_name))  #<- Only pulls data vars if specified, blank vars removed

print("Key Key data retrieved")
    
#READ DATA FILES
###########################  
  
#Find and set number of rows to skip at top of data file  
  # isolate rows to skip from locationData for data import      # <-- We're getting the 'header_row' value twice, since it's in the sheet twice, Ok?
  if(length(locationData[locationData$var == 'header_row',]$var) == 1) { 
    skipRows = as.numeric(locationData[locationData$var == 'header_row',]$Value)-1
  } else {
    skipRows = 0
  }
 
  
#Find and set missing value codes  
  # isolate missing value codes from locationData for data import
  if (length(locationData[locationData$var == 'NA_1',]$var) == 1) {
    mvc1 = locationData[locationData$var == 'NA_1',]$Value }
  if (length(locationData[locationData$var == 'NA_2',]$var) == 1) {
    mvc2 = locationData[locationData$var == 'NA_2',]$Value }
  
  missingValueCode = "NA"
  if (exists('mvc1')) { missingValueCode = mvc1}
  if (exists('mvc2')) { missingValueCode = mvc2}
  if (exists('mvc1') && exists('mvc2')) { missingValueCode = c(paste(mvc1, mvc2))}


  
  # import all (data + key) files from google dir
  #Uses batch_load function
  googleDirData <- lapply(dirFileNames, 
                          batch_load, 
                          missingValueCode = missingValueCode,
                          skipRows = skipRows)
  
  # add filenames
  names(googleDirData) <- dirFileNames
  
  # as key file is already loaded, remove it from the list of data frames 
  googleDirData <- googleDirData[!grepl("key", names(googleDirData), fixed = T)]

  ### Remove duplicate files, for some reason I lose the filename if I do this any other way than an if else
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
  googleDirData <- map(googleDirData, select, one_of(varsToKeep))  # <- Spits a warning for non-matching vars 
  
  # rename investigator names to key file names
    #??? DNP not sure whate happens here...
  googleDirData <- lapply(googleDirData, function(frame) { 
    setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })
  
  # generate wide data frame of location data
  
  ###DNP FIX FOR DUPLICATE 'header row' row in locationDataWide
      #I think we can just cut out the second 'header row' since we already brought in the value above
      locationData <- locationData[!duplicated(locationData$var),]
        #If !duplicated doesn't work, just cut by row #, or name...etc.
  
  locationDataWide <- locationData %>%   # <-- Spits error since 'header row' is in there twice
    select(var, Value) %>% 
    spread(key = var, value = Value)

  # merge location data with each data frame
  googleDirData <- lapply(googleDirData, function(frame) { merge(locationDataWide, frame, all = T) })

print("Data file homogenization complete")  
    
  # rename files to include base name + indication of homogenization 
  names(googleDirData) <- paste0(str_extract(names(googleDirData), "^[^\\.]*"), "_HMGZD")
  
#EXPORT HMGZD DATA FILE
###########################
  
  # write files to a temporary location (working drive) --> Should we upload to single "HMGZD" folder?
  googleDirData %>%
    names(.) %>%
    # map(~ write_csv(googleDirData[[.]], paste0("~/Desktop/temp_som_outspace/", .)))
    map(~ write_csv(googleDirData[[.]], paste0( ., ".csv")))

print("Homogenized Data File Exported")


#COMPILE & EXPORT NOTES
################################

#Grab location data references and notes
loc.notes <- locationData %>% mutate_all(as.character) %>% filter(!is.na(var_notes)) %>% select(-Value, -Unit)

#Grab profile data notes and comments
prof.notes <- profileData %>% filter(!is.na(Notes) | !is.na(Comment)) %>% select(Var_long,var,Level,Notes,Comment) %>% rename(var_notes=Notes)

#Merge notes
notes <- full_join(loc.notes,prof.notes)

#Add filename and site code
notes$File <- names(googleDirData)
notes$Site <- locationData$Value[16]

#Arrange filename first
notes <- notes %>% select(File, Site, everything())

#Export notes
  ### -> Change to GDrive upload in next revision

  write.csv(notes, paste0(names(googleDirData),"_NOTES.csv"))
  
print("Notes Exported")
print("Success! Data file homogenization complete.")

}

### TEST RUN OF HOMOG FTN
#Test run of AND folders through homogenization ftn
  
  #List of Andrews DIRT folder location
  hja <- c('AND_10YR_CN','AND_10YR_DenseFrac','AND_15YR_CN')
  
  #Pass vector of Andrews DIRT folder names to homogenization ftn
    ### Right now file output goes to working drive
  lapply(hja,data_homogonization)

