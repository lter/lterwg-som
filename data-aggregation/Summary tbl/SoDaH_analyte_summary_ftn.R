### Function to create dataframe with analyte values by location and depth
# Creates summary table from SoDaH database (input parameter) with the
# average analyte (input parameter) value for each unique location and depth layer
# Returns a dataframe

analyte_summary <- function(database, analyte) {
  
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