### SOM Rehomoger script
# Derek Pierson, July 29, 2019
# piersond@oregonstate.edu

#Load libraries
library(googledrive)
library(tidyverse)
library('devtools')
devtools::install_github("srearl/soilHarmonization")
library('soilHarmonization')

#SoilHarmonization temp_path
hmg_temp <- "C:/Users/Derek/Desktop/tmp/SOM"

#Function: copy GDrive files using a Google ID string
gd_copy <- function(gid, dest) {
  gfile <- as_id(gid)
  drive_cp(gfile, path=dest)
}

#Function: rehomog folder contents in new nested folder
rehomog<- function(folder, homog_temp_path) {
  
  #Get list of folder contents
  files <- as.data.frame(drive_ls(path=folder, trashed=FALSE))
  
  #Create simple dataframe from tibble with filenames, type and Google ID
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
  
  #Find keyv2 name
  keyv2 <- grep("key_v2",file_info$item,ignore.case=TRUE,value=TRUE)
  
  #Only proceed if keyV2 found
  if(length(keyv2) > 0) {
    
    #find the raw data files by process of elimination
      #bit of a manual catch all...update as needed
    data_files <- filter(file_info, !grepl("HMG",item)) %>% 
                    filter(., !grepl("folder",type)) %>% 
                    filter(., !grepl("key",item,ignore.case=TRUE)) %>% 
                    filter(., !grepl("zip",type,ignore.case=TRUE)) %>%
                    filter(., !grepl(".txt",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".rtf",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".doc",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".pdf",item,ignore.case=TRUE)) %>%
                    filter(., !grepl(".html",item,ignore.case=TRUE))
      
    #identify files and folders to move out before homog
    #move_files <- filter(file_info, !grepl(keyv2,item)) %>% subset(., !(item %in% data_files[,1])) %>% filter(., !grepl("zip",type))
    
    #Create new_homog directory
    drive_mkdir(paste0(folder,"_rehomoger"), parent = folder)
    
    #Copy keyV2 and data_files to the new homoger folder
     drive_cp(keyv2, path=paste0(folder,"_rehomoger/"))
     sapply(data_files$ID, gd_copy, dest=paste0(folder,"_rehomoger/"))
     
    #Re-homog 
     data_homogenization(directoryName = paste0(folder,"_rehomoger"), 
                         temporaryDirectory = hmg_temp)
    
    #Move new homog files back into main folder?
      #Delete old versions of HMG files?
  } else {
      print(paste0("No keyV2 found for ",folder))
    }
}


### Run rehomoger function using folder name
rehomog(folder="CWTgradient_roots", homog_temp_path=hmg_temp)



