# SoDaH Summary Table creator 
# More details @ https://github.com/lter/lterwg-som/issues/72
# Created: July 2020, Derek Pierson


# TO DO as of 7/27/20
#######################################################################################
# 1) Implement max depth parameter
# 2) Better handling of locs withonly one layer
# 3) Add a top layer only option to ftn
# 4) Add a stock option which uses bulk density

# A) Find why some sites have overlapping increments, add column to differentiate
# B) Remove -1 to 0 cm layers from profile calc, add to separate column in output
# C) Decide if filling gaps is worth doing

# NEXT NEW FTN: Build a ftn for locational averages of stocks
#   - Sequential cluster avgs to a specified separation distance
#   - Or avg by close proximity and similar elevation (pull elevation by lat long from USGS)
#   - Plot/map results to look for errors

# LOOKING AHEAD: Can ftns also get us sand, silt, clay?
#   - Or pull in USGS soil classification values by lat long

#######################################################################################

# Load libraries
library(tidyverse)

#set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get SoDaH database from EDI script
source("edi.521.1.r")

# Filter EDI SoDaH database "dt1" for control data only
som <- dt1 %>% filter(control_sample == TRUE)

#Function to create dataframe with analyte values by location and depth
analyte_location_summary <- function(database, analyte) {

  database$latlong <- paste0(database$lat, ", ", database$long)
  
  #Narrow database to smaller df
  df <- database %>% select(site_code, location_name, lat, long, latlong, layer_top, layer_bot, analyte) #<-- lyr_soc (i.e. analyte of interest) needs to come from function input of dataframe column 
  
  #rename analyte column for subsequent use in function
  colnames(df)[ncol(df)] <- "analyte"
  
  #Remove rows with analyte NA
  df <- df[complete.cases(df[,ncol(df)]),]
  
  # Group by site code, location, depth then summarize
  df_smry <- df %>% mutate(site_code = as.character(site_code),
                            location_name = as.character(location_name)) %>%
                    group_by(site_code, location_name, latlong, layer_top, layer_bot) %>%
                    summarize(analyte_avg = mean(analyte, na.rm=TRUE),
                              analyte_n = n())

  return(df_smry)
}

### Test function output
tbl1 <-   analyte_location_summary(database = som, analyte = "lyr_soc")#pre-function implementation values

#Create blank dataframe to rbind stocks from stocks_by_loc function
stocks_df <- data.frame(site_code=character(),
                        location_name=character(), 
                        latlong=character(),
                        gaps = numeric(),
                        depth = numeric(),
                        analyte_profile_conc = numeric(),
                        calc_notes = character(),
                        stringsAsFactors=FALSE) 

#Function to calc analyte stock for a single location
stocks_by_loc <- function(location, analyte_data, max_depth) {
  
  #DEBUG
  #location = "44.95597, -110.54196"
  #analyte_data = tbl1
  #max_depth = 300
    
  #Filter data by location
  df <- analyte_data %>% filter(latlong == location) %>% arrange(layer_top)
  
  if(nrow(df) < 2) {
    stock_calc_error = "Single depth increment"
    #output <- data.frame(site_code=unique(df$site_code),
    #                     location_name=unique(df$location_name), 
    #                     latlong=unique(df$latlong),
    #                     gaps = NA,
    #                     depth = df$layer_bot[nrow(df)],
    #                     analyte_profile_conc = df$analyte_avg[nrow(df)],
    #                     calc_notes = stock_calc_error,
    #                     stringsAsFactors=FALSE)
    
  } else if(any(duplicated(df$layer_top))) {
    stock_calc_error = "Mixed depth increments"
    #output <- data.frame(site_code=unique(df$site_code),
    #                     location_name=unique(df$location_name), 
    #                     latlong=unique(df$latlong),
    #                     gaps = NA,
    #                     depth = df$layer_bot[nrow(df)],
    #                     analyte_profile_conc = NA,
    #                     calc_notes = stock_calc_error,
    #                     stringsAsFactors=FALSE)
  } else {  
    
    ###for error tracking
    #print(df$latlong)
    
    #filter depth to below max
    df <- df %>% filter(layer_top < max_depth)
    
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
    
    #Calc analyte stocks ###<--- NOTE, not using bulk denisty, not really a stock...)
    df$analyte_amount <- (df$layer_bot-df$layer_top)*df$analyte_avg
    
    analyte_prof_conc <- sum(df$analyte_amount)/bot_depth
    
    output <- data.frame(site_code=unique(df$site_code),
                         location_name=unique(df$location_name), 
                         latlong=unique(df$latlong),
                         gaps = num_of_gaps,
                         depth = bot_depth,
                         analyte_profile_conc = analyte_prof_conc,
                         calc_notes = " ",
                         stringsAsFactors=FALSE)
    
    #return values as dataframe with 1 row
    return(output)
  }
  
  
  
}

#test the function for one location
single_loc_test2 <- stocks_by_loc(location = "44.968096, -110.492027",   #63.880708, -149.239569
                      analyte_data = tbl1, 
                      max_depth = 300)

#Run functiion across all locations and rbind output to dataframe
get_loc_stocks <- sapply(unique(tbl1$latlong), stocks_by_loc, analyte_data = tbl1, max_depth=500)
stocks <- do.call(rbind, get_loc_stocks)


str(get_loc_stocks)

#Remove the lat-long row names
rownames(stocks) <- c()


#QA gaps, how many locs with depth increment gaps?
qa_df <- stocks %>% mutate(gaps = as.numeric(gaps)) %>% filter(gaps > 0)




#save stocks tbl
#write.csv(stocks, "lyr-soc_stocks_output.csv")




