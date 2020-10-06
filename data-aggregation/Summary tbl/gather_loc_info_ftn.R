# Function to get SoDaH var value for a specific location (lat long), 
# or within specified km distance of the location

gather_loc_info <- function(db, info_var, target_lat, target_long, area_km=0) {
  
  #debug
  #db = som 
  #info_var = 'map'
  #target_lat = 44.23100 
  #target_long = -122.2200
  #area_km = 100
  
  if(area_km == 0) {
    df <- db %>% filter(lat == as.numeric(target_lat)) %>% filter(long == as.numeric(target_long)) 
  } else {
    range_dd <- area_km * (1/111)
    df <- db %>% drop_na(lat) %>% drop_na(long) %>%
                 filter(lat < (as.numeric(target_lat)+range_dd)) %>%
                 filter(lat > (as.numeric(target_lat)-range_dd)) %>%
                 filter(long < (as.numeric(target_long)+range_dd)) %>%
                 filter(long > (as.numeric(target_long)-range_dd))
  }
  target_val <- as.character(unique(df[info_var]))
  output <- data.frame(
             lat = target_lat,
             long = target_long,
             approx_area_km = area_km,
             info = as.character(target_val))
  output[,4] <- as.character(output[,4])
  colnames(output)[4] <- info_var
  return(output)
  
}

###############################################
# EXAMPLE USAGE

# ## Bring in SoDaH
# # Get SoDaH .RDS from local path
# dt1 <- readRDS("C:/github/lterwg-som-shiny/shiny_SOM/somCompositeData_2020-06-22.rds")
# 
# ## Filter EDI SoDaH database "dt1" for control data only
# som <- dt1 %>% filter(control_sample == TRUE)
# 
# # Run ftn for specific location
# my_df <- gather_loc_info(db = som,
#             info_var = 'map',
#             target_lat = 44.23100,
#             target_long = -122.2200,
#             area_km = 0)

