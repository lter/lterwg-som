### SOM re-homog crawler
## Derek Pierson, July 26, 2019
## piersond@oregonstate.edu

#Load libraries
library(googledrive)
library(tidyverse)


# Get top level list of folders found in SOM GDrive "Data_downloads"
data_downloads <- drive_ls(path="Data_downloads", trashed=FALSE, type="folder") #,recursive = TRUE)

# Create vector of top level folder names
top_folders <- NULL
for (j in 1:nrow(data_downloads)) {top_folders[j] <- as.character(data_downloads[j,1])}
top_folders <- as.list(top_folders)


## Ignore list
  # Removed from top_folders
top_ignore <- c("1_Climate_data","2_soilMoisture_Temp","3_NPP_Biomass",
                "NEON_periodicSoil", "NEON_initialCharacterizationSoil", "NEON_megapitSoil",
                "NEON_megapitRoots", "NEON_periodicRoots", "NEON_site-climate", "NEON_litterfall")

top_folders = top_folders[-which(top_folders %in% top_ignore)]

## Function to find all nested folders under GDrive folder (also returns the input folder name)
find_nested_folders <- function(top_folder) {
  
  # Convert tibble input to character
  top_folder <- as.character(top_folder)
  
  # Get all *nested* folder names 
  folders <- drive_ls(path=as.character(top_folder), trashed=FALSE, type="folder", recursive = TRUE)
  
  # Create list of folder names to check for keyV2, including top folder
  # Needs to be looped to avoid added characters
  folder_names <- NULL
  folder_names[1] <- as.character(top_folder)
  if(nrow(folders) > 0) {
    for (j in 1:nrow(folders)) {folder_names[j+1] <- as.character(folders[j,1])}}
  
  return(folder_names)
}

# Create list of top level folders and their nested folders
  # First folder in each list element is the the top level folder
all_folders_list <- lapply(top_folders, find_nested_folders)

# Master vector of all GDrive folder names
all_folders <- as.character(unique(unlist(all_folders_list)))

## Ignore list
  # Removed from all_folders
folders_ignore <- c("Extra files", "Not used or duplicate")
all_folders_subset = all_folders[-which(all_folders %in% folders_ignore)]

## Function to check if folder contains a keykey
keykey_check <- function(folder) {
  
  # Get filename in GDrive folder
  files <- drive_ls(path=folder, trashed=FALSE)
  
  # Create vector log
  v1 <- "0"
  v2 <- "0"
  
  if(grepl("KEY_V2", files)) {
    v1 <- "Key present"
    v2 <- "KeyV2 Complete"
  }
  
  if(grepl("key", files, ignore.case = TRUE)) {
    v1 <- "Key present"
  }
  
  return(c(v1,v2))
}

# Run check for keys
  # FUTURE: Add re-homog function inside keykey_check function
key_log_list <- sapply(all_folders_subset, keykey_check)

# Create keykey log dataframe and export csv
log <- as.data.frame(key_log_list)
key_log_df <- as.data.frame(colnames(key_log_list))
colnames(key_log_df)[1] <- c("Folder")
key_log_df$key_v1 <- key_log_list[1,]
key_log_df$key_v2 <- key_log_list[2,]
key_log_df <- arrange(key_log_df, desc(key_v2), desc(key_v1), Folder)
date <- format(Sys.Date(), "%m-%d-%y")
write.csv(key_log_df, paste0("SOM_key_log_",date,".csv"), row.names=FALSE)

