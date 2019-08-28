#Script takes tarball and creates table with the number of unique values, by site, for each column in the tarball
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/27/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-04-29.csv")
tarball <- tarball %>% filter(google_dir != "NA")

#identify unique datasets
data.sets <- as.character(unique(tarball$google_dir))

#Function to count unique data values by column for a specific site
var_counter <- function(site, tar) {
  
  #Extract site data from tarball
  df <- tarball %>% filter(google_dir == site)
  
  #Count number of unique values
  var.unique <- as.data.frame(apply(df, 2, function(x) length(unique(x[!is.na(x)]))))
  
  #Set "Site" as column name
  colnames(var.unique) <- site
  
  #return the dataframe
  return(var.unique)
}

#Create table of unique column var values by tarball site (site = google_dir) 
site.data.summary <- do.call(cbind, lapply(data.sets, var_counter, tarball))

#Export summary table as csv
date <- format(Sys.Date(), "%m-%d-%y")
write.csv(site.data.summary, paste0("SOM_Site_data_count_",date,".csv"), row.names=TRUE)



