## Use to install soilharmonization package from github
library('devtools')
library(tidyverse)
<<<<<<< Updated upstream
=======
devtools::install_github("srearl/soilHarmonization")
>>>>>>> Stashed changes

#install from master
devtools::install_github("srearl/soilHarmonization")
library('soilHarmonization')

#install from branch
#devtools::install_github("srearl/soilHarmonization", ref = "fix-biomass-units-conv")

#units conversion table
utbl <- unitsConversions %>% print(n = Inf)


# USER INPUT
###############################################
#Set path to temp folder
<<<<<<< Updated upstream
#temp_path <- 'C:/temp/SOM'
temp_path <- "C:/R_temp/SOM"

#Set name of keykey to update
data_folder <- 'UMBS_DIRT_C_N_by_Plot_2004_2014'
key_file_name <- 'UMBS_DIRT_C_N_2004_2014_Key_Key'
=======
temp_path <- 'C:/Users/Derek/Desktop/tmp'

#Set name of keykey to update
data_folder <- 'Wisconsin DIRT'
key_file_name <- 'Wisconsin_DIRT_KEY_V2'
>>>>>>> Stashed changes

###############################################


#Delete all files in temp folder
f <- list.files(temp_path, include.dirs = F, full.names = T, recursive = T)
file.remove(f)

#Run keyV2 update function
  #These folders need to be created in the temp directory
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

#Sys.getenv("R_ENVIRON")
#Sys.getenv("R_HOME")
#Sys.getenv("PATH")
#Sys.which("pdflatex")


<<<<<<< Updated upstream
digest      
ellipsis    
fs          
fuzzyjoin   
gargle      
ggplot2     
googledrive 
hms         
knitr       
lifecycle   
markdown    
pkgconfig   
rmarkdown   
sys         
tidyr       
tinytex    
xfun        
xml2        
zip         

=======
#unitsConversions %>% print(n = Inf)
>>>>>>> Stashed changes
