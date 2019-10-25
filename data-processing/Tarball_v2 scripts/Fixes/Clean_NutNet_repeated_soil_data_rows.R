#Script to fix repeat data rows in NutNet-join
#Author: Derek Pierson
#Date: 10-23-2019
#Email: piersond@oregonstate.edu


#Libraries
library(dplyr)

#read in latest tarball
tarball <- readRDS("somCompositeData_2019-10-18.rds")

  #for testing, read local NutNet_HMGZD csv instead
  #tarball <- read.csv("C:/R_temp/SOM/nnCombinedDensityRoots_HMGZD - nnCombinedDensityRoots_HMGZD.csv")

#NutNet columns with duplicate data across years. The data provided should only be listed for the first year before treatment.
NN_columns_to_clean <- c("bd_samp", "lyr_soc", "lyr_n_tot", "p_ex_1", "k", "ca", "na", "fe_HCL", "al", "ph_h2o")

#Loop through columns, row by row, and replace improper duplicate NutNet values with NA
  #Leaves the data in place for treatment year 0, as it should be.
  #FYI Takes a minute to run the loops...
for(j in 1:length(NN_columns_to_clean)){
  for(i in 1:nrow(tarball)){
    if(!is.na(tarball$network[i]) & !is.na(tarball$observation_date[i]) & !is.na(tarball$tx_start[i])) {
      if(tarball$network[i] == "NutNet" & tarball$observation_date[i] >= tarball$tx_start[i]) {
        tarball[i,NN_columns_to_clean[j]] <- NA
      }
    }
  }
}
  
#Save local copy of tarball output to verify intended result
#write.csv(tarball, "C:/R_temp/SOM/nn_test1.csv")
