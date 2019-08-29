#Script takes tarball and creates table with the number of unique values, by site, for each column in the tarball
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/27/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv")
tarball <- tarball %>% filter(google_dir != "NA")

#identify unique datasets
data.sets <- as.character(unique(tarball$google_dir))

#Function to count unique data values by column for a specific site
var_mean <- function(site, tar) {
  
  #debug
  #site <- "Shale Hills CZO"
  #tar <- tarball
  
  #Extract site data from tarball
  df <- tar %>% filter(google_dir == site)
  
  #Count number of unique values
  var.mean <- as.data.frame(df %>% summarise_if(is.numeric, mean)) %>% mutate_if(is.numeric, round, 1)
  
  #Set "Site" as column name
  row.names(var.mean) <- site
  
  #return the dataframe
  return(var.mean)
}

#Create table of unique column var values by tarball site (site = google_dir) 
site.data_means <- do.call(rbind, lapply(data.sets, var_mean, tarball))

#Export summary table as csv
date <- format(Sys.Date(), "%m-%d-%y")
write.csv(site.data_means, paste0("SOM_Site_data_means_",date,".csv"), row.names=TRUE)



