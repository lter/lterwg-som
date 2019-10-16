### SOM Rehomoger script
# Derek Pierson, July 29, 2019
# piersond@oregonstate.edu

# Load libraries
library(googledrive)
library(tidyverse)
library('devtools')
devtools::install_github("srearl/soilHarmonization")
library('soilHarmonization')

# SoilHarmonization temp_path
hmg_temp <- "C:/Users/Derek/Desktop/tmp/SOM"

# Get date as day-month
today <- format(Sys.Date(), "%d%b")

# Function: copy GDrive files using a Google ID string
gd_copy <- function(gid, dest) {
  gfile <- as_id(gid)
  drive_cp(gfile, path=dest)
}

# Function rename HMGZD files
HMG_rename <- function(filename, dir) {
  new_name <- paste0(dir, "_", gsub("Copy of ", "", filename), "_", today)
  drive_rename(filename, name = new_name)
}


# Function: rehomog folder contents in new nested folder
rehomog<- function(folder, homog_temp_path) {
  
  # debug vars
  #folder <- "CDR_E141_BioCON"
  #homog_temp_path <- hmg_temp
  
  # Get list of folder contents
  files <- as.data.frame(drive_ls(path=folder, trashed=FALSE))
  
  # Create simple dataframe from tibble with filenames, type and Google ID
  file_info <- as.data.frame(as.character(files[,1]))
  colnames(file_info) <- "item"
  file_info$type <- NA
  for(i in 1:nrow(file_info)) {
    file_info$type[i] <- files[,3][[i]][[4]] %>% gsub("application/","",.) %>% gsub("vnd.google-apps.","",.)
  }
  file_info$ID <- NA
  for(i in 1:nrow(file_info)) {
    file_info$ID[i] <- files[,3][[i]][[2]] %>% gsub("application/","",.) %>% gsub("vnd.google-apps.","",.)
  }
  
  # Find keyv2 name
  keyv2 <- grep("key_v2",file_info$item,ignore.case=TRUE,value=TRUE)
  
  # Only proceed if keyV2 found
  if(length(keyv2) > 0) {
    
    # Find the raw data files by process of elimination
      # Bit of a manual catch all...update as needed
    data_files <- filter(file_info, !grepl("HMG",item)) %>% 
                    filter(., !grepl("folder",type)) %>% 
                    filter(., !grepl("document",type)) %>% 
                    filter(., !grepl("key",item,ignore.case=TRUE)) %>% 
                    filter(., !grepl("zip",type,ignore.case=TRUE)) %>%
                    filter(., !grepl(".txt",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".rtf",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".doc",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".pdf",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".html",item,ignore.case=TRUE))
    
    # Set Google Drive directory for temp files
    temp_folder <- "Homoger_temp"
    
    # Create new_homog directory
    drive_mkdir(paste0(folder,"_rehomoger"), parent = temp_folder)
    
    # Copy keyV2 and data_files to the new homoger folder
     drive_cp(keyv2, path=paste0(folder,"_rehomoger/"))
     sapply(data_files$ID, gd_copy, dest=paste0(folder,"_rehomoger/"))
     
    ### Re-homog 
     data_homogenization(directoryName = paste0(folder,"_rehomoger"), 
                         temporaryDirectory = hmg_temp)
    
    ### Filename housekeeping
    # Identify names of new HMGZD files
     new_files <- as.data.frame(drive_ls(path=paste0(folder,"_rehomoger"), trashed=FALSE))
     new_HMGZD_names <- grep("HMGZD", as.character(new_files[,1]), ignore.case=TRUE, value=TRUE)
    
    # Change HMGZD data filename(s) to folder_filename_HMGZD_date  
     new_HMGZD_data <- new_HMGZD_names[!grepl("HMGZD_NOTES",new_HMGZD_names)]
     sapply(new_HMGZD_data, HMG_rename, dir=folder) 
      
     # Remove "rehomoger" from HMGZD notes filename and add date stamp 
     new_HMGZD_notes <- new_HMGZD_names[grepl("HMGZD_NOTES",new_HMGZD_names)]
     drive_rename(new_HMGZD_notes, name = gsub("rehomoger_HMGZD_NOTES.pdf",paste0("_HMGZD_NOTES_",today,".pdf"),new_HMGZD_notes)) 
     
     #Update HMGZD filenames 
     new_HMGZD_names <- paste0(folder, "_", gsub("Copy of ", "", new_HMGZD_data), "_", today)
     new_HMGZD_names <- c(new_HMGZD_names,  gsub("rehomoger_HMGZD_NOTES.pdf",paste0("_HMGZD_NOTES_",today,".pdf"), new_HMGZD_notes))
     
    ### Move new HMGZD files to folder destination   
    # Specify GDrive folder to store new HMGZD files  
     HMGZD_store <- "Auto-Homog-Aug6/"   #Created manually...switch to programmatic? place folder creation at top of script, outside ftn
     
    # Move new HMGZD files to specified folder 
     sapply(new_HMGZD_names, drive_mv, path=HMGZD_store)
    
    ### Clean up  
    # Delete temp directory
     drive_rm(paste0(folder,"_rehomoger"))
     
  } else {
      print(paste0("No keyV2 found for ",folder))
  }
}


# Bring all keyV2 folder names
 #Requires csv output from homog_crawler.R
keyV2s <- read.csv("SOM_key_log_07-27-19.csv") %>% filter(key_v2 != "0" ) 

# Run rehomoger function on all keyV2 folders
sapply(as.character(keyV2s[,1]), rehomog, homog_temp_path=hmg_temp) 

# For resuming homoger at specific folder row 
#sapply(as.character(keyV2s[21:48,1]), rehomog, homog_temp_path=hmg_temp)


# For running rehomoger function on a single folder, by name
#rehomog(folder="CWTgradient_roots", homog_temp_path=hmg_temp)




