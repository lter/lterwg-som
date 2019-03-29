## Use to install soilharmonization package from github
#library('devtools')
#devtools::install_github("srearl/soilHarmonization")

library('soilHarmonization')

# USER INPUT
###############################################
#Set path to temp folder
temp_path <- 'C:/temp/SOM'

#Set name of keykey to update
data_folder <- 'AND_10YR_CN'
key_file_name <- 'AND_DIRT_10YR_CN_key_Key'

###############################################


#Delete all files in temp folder
f <- list.files(temp_path, include.dirs = F, full.names = T, recursive = T)
file.remove(f)

#Run keyV2 update function
key_update_v2(sheetName = key_file_name,
              keyFileDownloadPath = paste0(temp_path,'/key_file_download/'),
              keyFileArchivePath = paste0(temp_path,'/key_file_archive/'),
              keyFileUploadPath = paste0(temp_path,'/key_file_upload/'))


### After successful keyV2 update, go into key and update new vars 
#   (last 7 rows of keykey location tab sheet & treatment levels in the profile tab sheet)

#Then, re-homog
data_homogenization(directoryName = data_folder, 
                    temporaryDirectory = temp_path)

### Check QC log and fix erors, re-homog as needed (delete homog and QC files before re-homog)


