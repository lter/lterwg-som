# Description: Filter Tarball-V2 data for 'control' data only 
# Date: May 8, 2019
# Author: Derek Pierson

# Libraries
library(dplyr)

# Tarball "Control Data Only" filter function
ctl_filter <- function(dataset_name, tar) {
  
  ### DEBUG step thru vars
  #tar <- tarball
  #dataset_name <- "NEON_megapitSOIL_a-master-database"
  #dataset_name <- dsets[15]
  
  # Isolate rows for the dataset
  cfilt.df <- filter(tar, google_dir == dataset_name)
  
  # Identify "control" value
  cfilt.ctl_val <- droplevels(as.factor(unique(cfilt.df$control_id)))
  
  #Split control value into vector elements if comma separator present
  cfilt.ctl_val <- gsub(", ",",",cfilt.ctl_val)
  cfilt.ctl_val <- unlist(strsplit(cfilt.ctl_val , split=","))   

  #Open var for later
  cfilt.rows <- NULL

  # Print warning if no control value exists
  if(is.na(cfilt.ctl_val)) {
    cat(paste0("Note! No control_id found for ",dataset_name,"\n"))
    
    #Add all rows if NO manipulation experiment was performed (Uses Location tab var in keykey)
    if("NO" %in% cfilt.df$experiments == TRUE) {
      cfilt.rows <- seq(1:nrow(cfilt.df))
      cat(paste0("Included all data for ",dataset_name,", no experimental manipulation was perfomed ","\n"))
      }
  } else {
  
    #Create ordered list of data levels
    key.lvls <- c("L1","tx_L1","L2","tx_L2","L3","tx_L3","L4","tx_L4","L5","tx_L5","L6","tx_L6")
    
    
        #df <- data.frame("L1" = 1:4, "tx_L1" = c(21,15,16,12), "L2" = c("UNT","CTL","UNT","CTL"))
        #dataset_name <- "DATASET NAME"
        #cfilt.ctl_val <- "CTL"
    
    for (i in 1:length(key.lvls)) {
      if(any(cfilt.df[[key.lvls[i]]] %in% cfilt.ctl_val)) {
        cfilt.rows <- which(cfilt.df[[key.lvls[i]]] %in% cfilt.ctl_val)
        
        #Print console message of column where control values were found
        cat(paste0("Dataset: ",dataset_name,"\n"))
        cat(paste0("Control_id value '",cfilt.ctl_val,"' found in data level column: ", key.lvls[i],"\n"))
        break
      }
      
      #Print warning if no control data found
      if(i == length(key.lvls)) {
        cat(paste0("Warning! No control_id matches found for ",dataset_name,"\n"))
      } 
    }
  }
    
    #Print console message for number of control data rows found
    if(length(cfilt.rows) > 0) {
      cat(paste0("Merged ",length(cfilt.rows)," rows with control_id matches out of ",
               nrow(cfilt.df)," data rows in ",dataset_name,"\n"))
    } else {
      cat(paste0("ALERT! NO DATA INCLUDED FOR ",dataset_name,"\n"))
    }
    
    
    
    # Print dataset separator for easier console reading
    cat("---------------------------------------\n")
    
    # Return dataframe with only control data rows 
      # ...for use with do.call() to construct dataframe from lapply
    return(cfilt.df[cfilt.rows,])
}

# Get the tarball
tarball <- readRDS("somCompositeData_2019-10-15.rds")

# Identify the individual dataset names
dsets <- na.omit(as.character(unique(tarball$google_dir))) #Remove som_testing_env and som_testing_envII ???

# Run filter ftn on each dataset and create dataframe with all control data rows
tarball_ctl_data <- do.call(rbind, lapply(dsets, ctl_filter, tarball))

# Write tarball control data to .csv
  # ...set appropriate filepath and name
#write.csv(tarball_ctl_data, "DONT USE_prelim_SOM-CompositeData_Control-Only.csv")

#For log of console notes, copy-paste console output to .txt file









