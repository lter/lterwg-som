
# README ------------------------------------------------------------------

# data harvest

# Workflow to harvest and merge homogenized data sets. The script searches the
# user's Google Drive for files with 'HMGZD' in the filename, excluding notes
# and QAQC output (.html). The sheet_download.R script from the
# soilHarmonization package reads each file, coalescing each into a list. Only
# unique names are passed to sheet_download.R so only one version of file of
# which there are multiple copies will be read - the user does not have any
# control over which version is ready so duplicates should be addressed outside
# of this workflow and generally avoided.

# data manipulation

# The guess_date function in this workflow attempts to standardize the format of
# the modification_date field in each homogenized data set to faciliate merging.
# Some variables (e.g., L1, L2, tx_L1) are too diverse across data sets to
# standardize so are convereted to type character with the cols_to_character
# function so that they do not prevent merging. As new data sets are
# homogenized, add the column/variable name of any fields that are creating a
# merge issue to the targets list - any columns with a name in the target list
# will be converted to type character.

# data output

# After standardization, the list of data sets is merged into a single data
# frame. This data frame is then written to Aurora and the Google Drive as both
# .csv and .rds files with the time stamp of when the data were harvested from
# Google Drive in the file name.

# notes

# This workflow could be automated or, at least, sourced but because of the
# frequency of errors (e.g., new data sets with columns that have to be
# converted to character), it should generally be run manually.


# libraries ---------------------------------------------------------------

library(soilHarmonization)
library(tools)
library(googlesheets)
library(googledrive)
library(tidyverse)
library(sqldf)
library(lubridate)


# options -----------------------------------------------------------------

options(httr_oob_default=TRUE) # create out-of-band oauth token in server env


# load resources ----------------------------------------------------------

# Google Sheet download script. This will be available from the
# soilHomogenization package but can be loaded from this repository if desired
# for whatever reason.

# source("/ ... path .../sheet_download.R")


# identify files to harvest -----------------------------------------------

myDrive <- gs_ls() # file inventory

# harvest names of homogenized data files
homogenizedDataFileNames <- myDrive %>%
  filter(grepl("HMGZD", sheet_title)) %>% # hmgzd files only 
  filter(!grepl("HMGZD_NOTES", sheet_title)) %>% # cull notes
  select(sheet_title) %>% 
  # distinct(sheet_title) %>%
  mutate(
    baseName = file_path_sans_ext(sheet_title),
    extension = file_ext(sheet_title),
    extension = replace(extension, extension == "", NA)
  ) 

# identify duplicates. these can be addressed manually in Google Drive.
# Alternatively, duplicates will be ignored in the download but the user will
# not have control over the file(s) downloaded.
homogenizedDataFileNames %>%
  group_by(baseName) %>% 
  summarise(
    numberFiles = n()
  ) %>% 
  filter(numberFiles >= 2)

# vector of the names of UNIQUE homogenized data files
uniqueHomogenizedDataFileNames <- sqldf('
SELECT
  hdfn.sheet_title,
  hdfn.baseName,
  hdfn.extension
FROM
  homogenizedDataFileNames hdfn
WHERE
  hdfn.baseName IN
(
  SELECT
    hdfn.baseName
  FROM
    homogenizedDataFileNames hdfn
  GROUP BY
    hdfn.baseName
  HAVING
    COUNT(hdfn.sheet_title) > 1
) AND
hdfn.extension IS NULL
UNION
SELECT
  hdfn.sheet_title,
  hdfn.baseName,
  hdfn.extension
FROM
  homogenizedDataFileNames hdfn
WHERE
  hdfn.baseName NOT IN
(
  SELECT
    hdfn.baseName
  FROM
    homogenizedDataFileNames hdfn
  GROUP BY
    hdfn.baseName
  HAVING
    COUNT(hdfn.sheet_title) > 1
)
ORDER BY hdfn.sheet_title
;') %>% 
  select(sheet_title) %>% 
  pull(sheet_title) 


# harvest data ------------------------------------------------------------

# Use the sheet_download script to harvest all of the (unique) homogenized data
# files from Google Drive. Address this in a function so that we can document
# the date and time of data harvest.

harvest_hmgzd_data <- function() {
  
  harvestedData <- lapply(uniqueHomogenizedDataFileNames, sheet_download)
  dataHarvestTime <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S")
  return(list(hmgzdData = harvestedData, harvestTime = dataHarvestTime))
  
}

# Assign function output to object
homogenizedDataAndHarvestTime <- harvest_hmgzd_data()

# Assign list of downloads to homogenizedData object
homogenizedData <- homogenizedDataAndHarvestTime[['hmgzdData']]
# homogenizedData <- lapply(uniqueHomogenizedDataFileNames, sheet_download)

# Access the date and time of download (we can call this directly when needed so
# no need to store as an object)
# dataHarvestTime <- homogenizedDataAndHarvestTime[['harvestTime']]

format(object.size(homogenizedData), units = "Mb") # check object size


# data backup -------------------------------------------------------------

# temporary, in-memory backup to avoid rerunning the downloading + binding
# operation (during development only)

# homogenizedDataBak <- homogenizedData
# homogenizedData <- homogenizedDataBak 


# standardize data format -------------------------------------------------

# function guess_date: for a given data entity (e.g., data frame in a
# list of data frames), identify any columns that are in the list of variables
# (i.e., targets) that need to be standardized with regard to data type to
# facilitate binding.
guess_date <- function(entity) {
  
  if (!is.null(entity[["modification_date"]]) & !inherits(entity[["modification_date"]], 'Date')) {
    
    entity %>%
      mutate(modification_date = parse_date_time(modification_date, c("mdY", "mdy", "m-d-Y")))
    
  } else if (!is.null(entity[["modification_date"]]) & inherits(entity[["modification_date"]], 'Date')) {
    
    entity %>%
      mutate(modification_date = parse_date_time(modification_date, c("Y-m-d")))
    
  } else {
    
    entity
    
  }
  
} # close guess_date

homogenizedData <- lapply(homogenizedData, guess_date)


# conflicting column types to character -----------------------------------

# function cols_to_character: for a given data entity (e.g., data frame in a
# list of data frames), identify any columns that are in the list of variables
# (i.e., targets) that need to be standardized with regard to data type to
# facilitate binding.

cols_to_character <- function(entity) {
  
  subTargets <- intersect(colnames(entity), targets)
  
  entity %>% 
    mutate_at(vars(subTargets), as.character)
  
}

# vector of data type conflicts
targets <- c('lat',
             'long',
             'L1',
             'L2',
             'L3',
             'L4',
             'layer_top',
             'layer_bot',
             'bd_samp',
             'NA_1',
             'NA_2',
             'tx_start',
             'bgb_upperdiam',
             'elevation',
             'map',
             'tx_L1',
             'tx_L2',
             'tx_L3',
             'tx_L4',
             'observation_date',
             'observation_date_1',
             'observation_date_2',
             'mat',
             'wood_lit_c',
             'slope') 

homogenizedData <- lapply(homogenizedData, cols_to_character)

boundData <- bind_rows(homogenizedData)


# write bound data to file ------------------------------------------------

format(object.size(boundData), units = "Mb") # check object size

# Aurora

saveRDS(boundData, paste0('/home/shares/lter-som/', 'somCompositeData_', homogenizedDataAndHarvestTime[['harvestTime']], '.rds'))
write_csv(boundData, paste0('/home/shares/lter-som/', 'somCompositeData_', homogenizedDataAndHarvestTime[['harvestTime']], '.csv'))

# Google Drive

homogedAndBoundOutputID <- googledrive::drive_get('homoged_and_bound_output') %>%
  dplyr::pull(id)

googledrive::drive_upload(media = paste0('/home/shares/lter-som/', 'somCompositeData_', homogenizedDataAndHarvestTime[['harvestTime']], '.csv'),
                          path = as_id(homogedAndBoundOutputID),
                          name = paste0('somCompositeData_', homogenizedDataAndHarvestTime[['harvestTime']], '.csv'))

googledrive::drive_upload(media = paste0('/home/shares/lter-som/', 'somCompositeData_', homogenizedDataAndHarvestTime[['harvestTime']], '.rds'),
                          path = as_id(homogedAndBoundOutputID),
                          name = paste0('somCompositeData_', homogenizedDataAndHarvestTime[['harvestTime']], '.rds'))


# preliminary data assessment ---------------------------------------------

# boundData %>%
#   filter(
#     !is.na(soc),
#     soc >= 0 & soc < 100
#   ) %>% 
#   ggplot(aes(x = soc)) +
#   geom_histogram() +
#   ggtitle(
#     label = "SOM across all data sets",
#     subtitle = "range constrained to 0-100"
#   )


# scratch space -----------------------------------------------------------

# files that had to be renamed
# AND_DIRT_10YR_CN_densefrac_HMGZD
# AND_DIRT_10YR_CN_raw_HMGZD

