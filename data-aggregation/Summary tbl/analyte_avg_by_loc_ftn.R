#Function to calc analyte average for a single location, to a specified depth
#Requires summarized SoDaH data table from analyte_summary_ftn for input var "analyte data"
#Trim options are "cut" or "loess" (If loess not possible, cut will be used, see notes in outpur dataframe)
#Returns a dataframe
analyte_avg_by_loc <- function(location, analyte_data, max_depth, trim) {
  
  #DEBUG
  #location = "68.63333, -149.7167"
  #analyte_data = my_df
  #max_depth = 30
  #trim = "loess"
  
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
    output <- data.frame(site_code=NA,
                         location_name=NA, 
                         lat=lat,
                         long=long,
                         gap_num = NA,
                         gap_size = NA,
                         gaps = NA,
                         depth = NA,
                         analyte_avg = NA,
                         soil_lyrs_n = lyr_n,
                         calc_notes = "No analyte data",
                         stringsAsFactors=FALSE)
  } else if(max(df$layer_bot, na.rm=T) < max_depth ) {
    output <- data.frame(site_code=unique(df$site_code),
                         location_name=unique(df$location_name), 
                         lat=lat,
                         long=long,
                         gap_num = NA,
                         gap_size = NA,
                         gaps = NA,
                         depth = df$layer_bot[nrow(df)],
                         analyte_avg = df$analyte_avg[nrow(df)],
                         soil_lyrs_n = lyr_n,
                         calc_notes = "Profile data depth < max requested",
                         stringsAsFactors=FALSE)
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
                         soil_lyrs_n = lyr_n,
                         calc_notes = "Single depth increment",
                         stringsAsFactors=FALSE)
  } else if(any(duplicated(df$layer_top))) {
    stock_calc_error = "Mixed depth increments"
    
    #Loess across all the points? Need to know why loc has many different profile values, treatments? 
    
    output <- data.frame(site_code=unique(df$site_code),
                         location_name=unique(df$location_name), 
                         lat=lat,
                         long=long,
                         gap_num = NA,
                         gap_size = NA,
                         gaps = NA,
                         depth = df$layer_bot[nrow(df)],
                         analyte_avg = NA,
                         soil_lyrs_n = lyr_n,
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
        if(trim == "cut" || nrow(df) < 3) {
          layer_max <- df$layer_bot[nrow(df)]
          df$layer_bot[nrow(df)] <- max_depth
          df$analyte_avg[nrow(df)] <- df$analyte_avg[nrow(df)] * ((max_depth - df$layer_top[nrow(df)])/(layer_max-df$layer_top[nrow(df)]))
          calc_notes <- paste0("trim: cut to ", max_depth, " cm")
          bot_depth <- max_depth
        } else if(trim == "loess") {
          if(df$analyte_avg[nrow(df)] == df$analyte_avg[nrow(df)-1]) {
            missing_xval <- df$analyte_avg[nrow(df)]
            loess_note <- paste0("trim: cut to ", max_depth, " cm")
          } else if(df$analyte_avg[1] == df$analyte_avg[2] & nrow(df) == 3) {
            layer_max <- df$layer_bot[nrow(df)]
            df$layer_bot[nrow(df)] <- max_depth
            missing_xval <- df$analyte_avg[nrow(df)] * ((max_depth - df$layer_top[nrow(df)])/(layer_max-df$layer_top[nrow(df)]))
            loess_note <- paste0("trim: cut to ", max_depth, " cm")
            bot_depth <- max_depth
          } else if(df$analyte_avg[1] == df$analyte_avg[3] & nrow(df) == 3) {
            layer_max <- df$layer_bot[nrow(df)]
            df$layer_bot[nrow(df)] <- max_depth
            missing_xval <- df$analyte_avg[nrow(df)] * ((max_depth - df$layer_top[nrow(df)])/(layer_max-df$layer_top[nrow(df)]))
            loess_note <- paste0("trim: cut to ", max_depth, " cm")
            bot_depth <- max_depth
          } else {
            x <- df$analyte_avg
            y <- ((df$layer_bot-df$layer_top)/2)+df$layer_top
            formula <- loess(y~x)
            missing_xval <- approx(x = formula$fitted, y = formula$x, xout = ((max_depth-df$layer_top[nrow(df)])/2)+df$layer_top[nrow(df)])$y
            loess_note <- paste0("trim: loess approx to ", max_depth, " cm") 
          }
          
          #plot(-x,-y)        
          #lines(x=-x, y=-predict(formula),
          #      col = "red",
          #      lwd = 3)
          
          layer_max <- df$layer_bot[nrow(df)]
          df$layer_bot[nrow(df)] <- max_depth
          df$analyte_avg[nrow(df)] <- missing_xval
          calc_notes <- loess_note
          bot_depth <- max_depth
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
                         soil_lyrs_n = lyr_n,
                         calc_notes = calc_notes,
                         stringsAsFactors=FALSE)
    
    #return values as dataframe with 1 row
    return(as.data.frame(output))
  }
}

