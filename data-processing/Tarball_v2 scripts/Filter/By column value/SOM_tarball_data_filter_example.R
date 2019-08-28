#Script for filtering SOM data
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/28/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv")
tarball <- tarball %>% filter(google_dir != "NA")

### To subset the SOM tarball data, we'll use dplyr filter() in a few different ways

#######################################################################################
## Option 1: Filter by list of elements, e.g. list of sites 
  #Looking at the tarball column fields, you'll find that the dataset names are in "google_dir"
  #  So we'll make a list of the sites we want
  site_list <- c("AND_10YR_CN", "AND_10YR_DenseFrac", "AND_15YR_CN", "ARC1", "ARC2", "Barre Woods Warming", 
               "BcCZO", "BES", "BNZ1", "BNZ2", "BNZ3")

  #dplyr filter to subset dataframe to only those rows that match a name "in" the list
  subset.df <- tarball %>% filter(google_dir %in% site_list)


##########################################################################################
## Option 2: Let's further filter our Site subset data from above to only contain rows with soc values
  #To do this, we'll remove all rows with NA values
  subset.soc.df <- subset.df %>% filter(!is.na(lyr_soc))

  #We could then go further and confine the dataset to a range of soc values, and limit the layer depth
  subset.soc.df <- subset.df %>% filter(!is.na(lyr_soc)) %>% 
                                  filter(lyr_soc < 20 & lyr_soc > 0.2) %>%
                                  filter(layer_bot > 11)





  
  
