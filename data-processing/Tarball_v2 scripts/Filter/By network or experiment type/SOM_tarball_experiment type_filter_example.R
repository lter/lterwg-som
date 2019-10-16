#Script for filtering SOM data by network and experiment type
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/28/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv", as.is=T)
tarball <- tarball %>% filter(google_dir != "NA")

### The tarball contains a few columns which are helpful for filtering the dataset by experiment type, network, etc.

##############################################################################################
## To filter by experimental network, use the 'network' column

  # See what networks have data in the tarball
  unique(tarball$network)

  # Subset tarball to only contain DIRT and NutNet data
  subset.df <- tarball %>% filter(network == c("DIRT", "NutNet"))

  
  
##############################################################################################  
## To filter for all time series experiments, use the 'time_series' column
  #Note: same can be done for gradient studies using the 'gradient' column
  
  # Subset tarball to only contain time series data
  subset.df <- tarball %>% filter(time_series == "YES")

  
  
##############################################################################################
## To filter for a specific type of treatment experiment, use the treatment levels
  
  #See what treatment types exist
  unique(tarball$tx_L1_level)
  
  # Subset tarball to only contain litter manipulation experiments
    #Note, since the "litter_manip" value may exist across the treatment levels, we use filter_at to 
    # filter across all columns with names ending in "_level"
  subset.df <- tarball %>% filter_at(vars(matches("_level$")), any_vars(. == "litter_manip"))

  
