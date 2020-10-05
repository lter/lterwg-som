#script info...



# Load libraries
library(tidyverse)

### Function to create dataframe with analyte values by location and depth
  # Creates summary table from SoDaH database (input parameter) with the... 
  # ...average analyte (input parameter) value for each unique location and depth layer
  # Returns a dataframe

analyte_location_summary <- function(database, analyte) {
  
  #DEBUG
  #database <- RC
  #analyte <- "lyr_soc"
  
  database$latlong <- paste0(database$lat, ", ", database$long)
  
  #Narrow database to smaller df
  df <- database %>% select(site_code, location_name, lat, long, latlong, layer_top, layer_bot, analyte) #<-- lyr_soc (i.e. analyte of interest) needs to come from function input of dataframe column 
  
  #rename analyte column for subsequent use in function
  colnames(df)[ncol(df)] <- "analyte"
  
  #Remove rows with analyte NA
  df <- df[complete.cases(df[,ncol(df)]),]
  
  #Set column data types
  df <- type.convert(df, as.is=T)
  
  # Group by site code, location, depth then summarize
  df_smry <- df %>% mutate(site_code = as.character(site_code),
                           location_name = as.character(location_name)) %>%
    group_by(site_code, location_name, latlong, layer_top, layer_bot) %>%
    summarize(analyte_avg = mean(analyte, na.rm=TRUE),
              analyte_stdev = sd(analyte, na.rm=TRUE),
              analyte_n = n())
  
  return(df_smry)
}


#Function to calc analyte stock for a single location
stocks_by_loc <- function(location, analyte_data, max_depth, trim) {
  
  #DEBUG
  #location = "45.804414, -90.052165"
  #analyte_data = my_df
  #max_depth = 30
  #trim = "cut"
  
  #Separate lat long values
  lat <- as.numeric(str_split(location,",")[[1]][1])
  long <- as.numeric(str_split(location,",")[[1]][2])

  print(paste0("Processing: ", location))
  
  #Filter data by location
  df <- analyte_data %>% filter(latlong == location) %>% arrange(layer_top)
  
  #filter depth to below max
  df <- df %>% filter(layer_top < max_depth)
  
  #record number of rows
  lyr_n <- nrow(df)
  
  if(nrow(df) < 1) {
    return()
  } else if(nrow(df) < 2) {
    output <- data.frame(site_code=unique(df$site_code),
                         location_name=unique(df$location_name), 
                         lat=lat,
                         long=long,
                         gap_num = NA,
                         gap_size = NA,
                         gaps = NA,
                         depth = df$layer_bot[nrow(df)],
                         analyte_avg = df$analyte_avg[nrow(df)],
                         lyr_n = lyr_n,
                         calc_notes = "Single depth increment",
                         stringsAsFactors=FALSE)
    
  } else if(any(duplicated(df$layer_top))) {
    stock_calc_error = "Mixed depth increments"
    output <- data.frame(site_code=unique(df$site_code),
                         location_name=unique(df$location_name), 
                         lat=lat,
                         long=long,
                         gap_num = NA,
                         gap_size = NA,
                         gaps = NA,
                         depth = df$layer_bot[nrow(df)],
                         analyte_avg = NA,
                         lyr_n = lyr_n,
                         calc_notes = "Mixed depth increments",
                         stringsAsFactors=FALSE)
  } else {  
    
    #find gaps in depth increments
    #Create empty gaps list
    gaps <- list() 
    
    #check if top layer starts at 1 or less
    if(df$layer_top[1] > 1) {
      gaps[[1]] <- c(0,df$layer_top[1])
    }
    
    #Find gaps between layers
    for(x in 1:(nrow(df)-1)) {
      if(df$layer_bot[x] != df$layer_top[x+1]) {
        gaps[[length(gaps) + 1]] <- c(df$layer_bot[x], df$layer_top[x+1])
      }
    }
    
    #Store gap and depth info for output
    num_of_gaps <- length(gaps)
    bot_depth <- df$layer_bot[nrow(df)] 
    
    if(num_of_gaps > 0) {
      gaps <- as.data.frame(unlist(t(as.data.frame(gaps))))
      gaps$gaps_size <- paste0(gaps[,1], "-", gaps[,2])
      gaps$sum <- gaps[,2]-gaps[,1]
      gap_lyrs <- paste(gaps$gaps_size, collapse = ",")
      gap_sum <- sum(gaps$sum)
    } else {
      gap_lyrs <- NA
      gap_sum <- 0
    }
    
    #Calc analyte concentration (average)
    df$analyte_amount <- (df$layer_bot-df$layer_top)*df$analyte_avg
    analyte_avg <- sum(df$analyte_amount)/bot_depth
    
    calc_notes <- ""
    
    
  if(trim != "NO") {
    if(df$layer_bot[nrow(df)] != max_depth) {
      if(trim == "cut") {
        layer_max <- df$layer_bot[nrow(df)]
        df$layer_bot[nrow(df)] <- max_depth
        df$analyte_amount[nrow(df)] <- df$analyte_amount[nrow(df)] * ((max_depth - df$layer_top[nrow(df)])/(layer_max-df$layer_top[nrow(df)]))
        calc_notes <- paste0("trim: cut to ", max_depth, " cm")
        bot_depth <- max_depth
      } else if(trim == "extrapolate") {
          #NOT implemented yet
          print("missing calc")
      }
    }
  }
    
    #Create the output dataframe
    output <- data.frame(site_code=unique(df$site_code),
                         location_name=unique(df$location_name), 
                         lat=lat,
                         long=long,
                         gap_num = num_of_gaps,
                         gap_size = gap_sum,
                         gaps = gap_lyrs,
                         depth = bot_depth,
                         analyte_avg = analyte_avg,
                         lyr_n = lyr_n,
                         calc_notes = calc_notes,
                         stringsAsFactors=FALSE)
    
    #return values as dataframe with 1 row
    return(as.data.frame(output))
  }
}
  



################################################################################################
# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get SoDaH database from EDI script
#source("edi.521.1.r") #incoming database var = dt1 as dataframe
dt1 <- readRDS("C:/github/lterwg-som-shiny/shiny_SOM/somCompositeData_2020-05-12.rds")

# Filter EDI SoDaH database "dt1" for control data only
som <- dt1 %>% filter(control_sample == TRUE)

# Get RC SoDaH
RC <- readRDS("C:/GitHub/RC-som-shiny/shiny_SOM/RC_database_current.rds")


###############################################################################################################

### USER SPECIFIED VALUES ###
target_database <- RC  #RC or som, or any SoDaH style database
target_analyte_to_avg <- "lyr_sic"  #Analyte column form SoDaH style database
target_depth_cm <- 30  #Max soil depth for analyte summary

#################################################################################################################


# Get summary table for all locations and depth layers
my_df <- analyte_location_summary(database=target_database, analyte=target_analyte_to_avg)

# Get summary for a specific location
#single_loc_test2 <- stocks_by_loc(location = "44.95597, -110.54196",   #63.880708, -149.239569
#                                  analyte_data = my_df, 
#                                  max_depth = 300)

## Get analyte summary for all unique locations
# Run functiion across all locations and rbind output to dataframe
get_loc_stocks <- lapply(unique(my_df$latlong), stocks_by_loc, analyte_data = my_df, max_depth=target_depth_cm, trim="cut")
stocks <- do.call(rbind, get_loc_stocks)

#data set that covers at least 0-30 cm, w/ gaps if < 5 cm
gaps_lt5cm_df <- stocks %>% drop_na(analyte_avg) %>% filter(gap_size <= 5)

#data set that covers at least 0-30 cm, w/o gaps
covers_df <- stocks %>% filter(gap_num == 0) %>% drop_na(analyte_avg)

#data set with perfect 0-30 analyte averages
perfect_df <- stocks %>% filter(depth == 30) %>% 
                       filter(gap_num == 0) %>% 
                       drop_na(analyte_avg)



