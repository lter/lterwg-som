
# README ------------------------------------------------------------------


# libraries ---------------------------------------------------------------

library(tools)
library(googlesheets)
library(googledrive)
library(tidyverse)
library(sqldf)


# options -----------------------------------------------------------------

options(httr_oob_default=TRUE) # create out-of-band oauth token in server env


# load resources ----------------------------------------------------------

source(here::here("localRepos", "lterwg-som", "data-processing", "sheet_download.R"))


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

homogenizedData <- lapply(uniqueHomogenizedDataFileNames, sheet_download)

format(object.size(homogenizedData), units = "Mb")

lapply(homogenizedData, function(dataFrame) { if ("lat" %in% colnames(dataFrame)) { dataFrame %>% mutate(lat = as.character(lat)) } })

asCharacter <- lapply(homogenizedData, function(dataFrame) { dataFrame %>% mutate_all(as.character) })

# toCharacter <- lapply(homogenizedData, function(dataFrame) { if ("L1" %in% colnames(dataFrame)) { dataFrame %>% mutate(L1 = as.character(L1)) } })
# toCharacter <- lapply(homogenizedData, function(dataFrame) { if ("L1" %in% colnames(dataFrame)) { dataFrame$L1 <- as.character(dataFrame$L1) } })
# 
# toCharacter <- lapply(toCharacter, function(dataFrame) { if ("L3" %in% colnames(dataFrame)) { dataFrame %>% mutate(L3 = as.character(L3)) } })
# toCharacter <- lapply(toCharacter, function(dataFrame) { if ("L4" %in% colnames(dataFrame)) { dataFrame %>% mutate(L4 = as.character(L4)) } })
# toCharacter <- lapply(toCharacter, function(dataFrame) { if ("modification_date" %in% colnames(dataFrame)) { dataFrame %>% mutate(modification_date = as.character(modification_date)) } })

# boundData <- bind_rows(toCharacter)
boundData <- bind_rows(asCharacter)

boundData <- bind_rows(homogenizedData)

###

# files that had to be renamed
AND_DIRT_10YR_CN_densefrac_HMGZD
AND_DIRT_10YR_CN_raw_HMGZD

###
###
###
###
###

homogenizedDataFiles <- myDrive %>%
  filter(grepl("HMGZD", sheet_title)) %>% 
  # filter(file_ext(sheet_title) == "csv") %>% # cull csv
  filter(!grepl("HMGZD_NOTES", sheet_title)) %>% 
  select(sheet_title) %>% 
  distinct(sheet_title) %>% 
  mutate(
    baseName = file_path_sans_ext(sheet_title),
    extension = file_ext(sheet_title),
    extension = replace(extension, extension == "", NA)
  ) %>% 
  group_by(baseName) %>%
  filter(any(!is.na(extension))) %>%
  View()


distinct(sheet_title) %>% 
pull(sheet_title)

myDrive %>%
  filter(grepl("HMGZD", sheet_title)) %>% 
  filter(!grepl("HMGZD_NOTES", sheet_title)) %>% 
  write_csv("~/Desktop/filenames.csv")

sheet_download <- function(fileName) {
  
  token <- googlesheets::gs_title(fileName)
  
  dataFile <- googlesheets::gs_read(token)
  
}

homogenizedData <- lapply(homogenizedDataFiles, sheet_download)
