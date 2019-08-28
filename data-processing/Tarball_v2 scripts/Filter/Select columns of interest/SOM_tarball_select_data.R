#Script for selecting SOM data
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/28/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv")
tarball <- tarball %>% filter(google_dir != "NA")

### The tarball is huge, so it's often helpful to "select" out the data we need
  ## Dplyr makes this easy, we can use select to specify the data columns we want
    # FYI The "var" column in the raw keyV2 provides info for all the SOM tarball columns 
  cols_of_interest <- c("google_dir", "network", "site_code", "location_name", "lat", "long",  #Site info 
                        "time_series", "gradient", "experiments", "control_id",  #Experiment type info
                        "L1", "L2", "L3", "L4", "L5", "tx_L1", "tx_L2", "tx_L3", "tx_L4", "tx_L5", "tx_L6", #Levels
                        "lyr_soc", "lyr_soc_stock", "lyr_n_tot", "lyr_n_tot_stock", "layer_top", "layer_bot", #Analytes and descriptors
                        "layer_mid", "hzn", "bd_samp", "bd_tot", "ph_h2o", "clay") #More analytes and descriptors

  #Create a minimized dataframe with the columsn of interest
    #Reduces dataframe size by >80%
  mini.df <- tarball %>% select(cols_of_interest)
  
  
  #Going one step further to filter out rows with no soc data, then removing any columns that are entirely NA 
    #Reduces dataframe size by >98%, this dataframe "might" now be viewable as table in RStudio or workable in Excel
  mini.df <- tarball %>% select(cols_of_interest) %>% filter(!is.na(lyr_soc)) %>% select_if(~sum(!is.na(.)) > 0)

  
## To export the selected data:
  write.csv(mini.df, "...path/filename.csv")






