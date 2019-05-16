# Description: Filter Tarball-V2 data for 'control' data only 
# Date: May 8, 2019
# Author: Derek Pierson

# Libraries
library(dplyr)

# Tarball "Control Data Only" filter function
ctl_filter <- function(dataset_name) {
  
  # Isolate rows for the dataset
  cfilt.df <- filter(tar, google_dir == dataset_name)
  
  # Identify "control" value
  cfilt.ctl_val <- droplevels((unique(cfilt.df$control_id)))
  
  # Print warning if no control value exists
  if (length(cfilt.ctl_val) < 1) {
    cat(paste0("WARNING! No control_id found for ",dataset_name,"\n"))
  }
  
  #################
  ### WHAT TO DO WHEN CONTROL VALUE IS NA? KEEP ALL ROWS?
  #################
  
  ###Two ways to go about finding data matching the control value
  ####################################################################
    ## Stevan, Will? Which is better?
  
  ### OPTION 1: Use first level column which contains values that match with the control value
  
    #Create ordered list of data levels
    key.lvls <- c("L1","tx_L1","L2","tx_L2","L3","tx_L3","L4","tx_L4","L5","tx_L5","L6","tx_L6")
    
    
        #df <- data.frame("L1" = 1:4, "tx_L1" = c(21,15,16,12), "L2" = c("UNT","CTL","UNT","CTL"))
        #dataset_name <- "DATASET NAME"
        #cfilt.ctl_val <- "CTL"
    
    cfilt.rows <- NULL
    
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
        cat(paste0("WARNING! No control_id matches found for ",dataset_name,"\n"))
      } 
    }
  
    #Print console message for number of control data rows found
    cat(paste0("Merged ",length(cfilt.rows)," rows with control_id matches out of ",
               nrow(cfilt.df)," data rows in ",dataset_name,"\n"))
  
    # Print dataset separator for easier console reading
    cat("---------------------------------------\n")
    
    # Return dataframe with only control data rows 
      # ...for use with do.call() to construct dataframe from lapply
    return(cfilt.df[cfilt.rows,])
    
  
  ### OPTION 2: Find all matches in all data and treatment levels
  
    # Find rows with the control value in any of the keykey level columns
    #key.lvls <- c("L1","L2","L3","L4","L5","L6","tx_L1","tx_L2","tx_L3","tx_L4","tx_L5","tx_L6")
    #cfilt.rows <- data.frame(unlist(apply(subset(cfilt.df, select = c(key.lvls[(key.lvls %in% colnames(cfilt.df))])), 2, 
    #                                      function(x) which(x %in% cfilt.ctl_val))))
  

    # Check the columns where the control_id was found
      # Uses row names to provide the filtered column names
    #cfilt.row.nms <- ifelse(grepl("tx",row.names(cfilt.rows)),substr(row.names(cfilt.rows), 1, 5),row.names(cfilt.rows))
    #cfilt.row.nms <- ifelse(!grepl("_",cfilt.row.nms),substr(cfilt.row.nms, 1, 2),cfilt.row.nms)
  
    #Print details for manual review 
    #cat(paste0("Dataset: ",dataset_name,"\n"))
    #cat(paste0("Control_id value '",cfilt.ctl_val,"' found in data level columns: ", unique(cfilt.row.nms),"\n"))

    #Print warning if no control data found
    #if(length(unique(cfilt.rows[,1])) < 1) {
    #  cat(paste0("WARNING! No control_id matches found for ",dataset_name,"\n"))
    #} else {
    #  cat(paste0("Merged ",length(unique(cfilt.rows[,1]))," rows with control_id matches out of ",
    #            nrow(cfilt.df)," data rows in ",dataset_name,"\n"))
    #}

    # Print dataset separator for easier console reading
    #cat("---------------------------------------\n")
  
    # Return dataframe with only control data rows 
      # ...for use with do.call() to construct dataframe from lapply
    #return(cfilt.df[unique(cfilt.rows[,1]),])
}

# Get the tarball
tar <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-04-29.csv")

# Identify the individual dataset names
dsets <- unique(tar$google_dir) #Remove som_testing_env and som_testing_envII ???

# Run filter ftn on each dataset and create dataframe with all control data rows
tarball_ctl_data <- do.call(rbind, lapply(dsets, ctl_filter))

# Write tarball control data to .csv
  # ...set appropriate filepath and name
write.csv(tarball_ctl_data, "DONT USE_prelim_SOM-CompositeData_Control-Only.csv")











