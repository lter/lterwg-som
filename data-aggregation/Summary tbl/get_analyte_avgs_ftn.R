# Load libraries
library(tidyverse)

# Load analyte summary ftn
source("SoDaH_analyte_summary_ftn.R")

# Load analyte avg ftn
source("analyte_avg_by_loc_ftn.R")

get_analyte_avgs <- function(target_database, target_analyte_to_avg, target_depth_cm) {
  
  # Create summary table for all locations and depth layers
  analyte_avgs_all <- analyte_summary(database=target_database, analyte=target_analyte_to_avg)
  
  ## Get analyte summary for all unique locations
  # Run functiion across all locations and rbind output to dataframe
  get_loc_stocks <- lapply(unique(analyte_avgs_all$latlong), analyte_avg_by_loc, analyte_data = analyte_avgs_all, max_depth=target_depth_cm, trim="loess")
  analyte_avgs_raw <- do.call(rbind, get_loc_stocks)
  
  #data set that covers at least 0-30 cm, w/o gaps
  analyte_avgs_no_gaps <- analyte_avgs_raw %>% filter(gap_num == 0) %>% drop_na(analyte_avg)
  
  assign(paste0("avg_all_",target_analyte_to_avg), analyte_avgs_all, envir = .GlobalEnv)
  assign(paste0("avg_toDepth_",target_analyte_to_avg,"_raw"), analyte_avgs_raw, envir = .GlobalEnv)
  assign(paste0("avg_toDepth_",target_analyte_to_avg,"_clean"), analyte_avgs_no_gaps, envir = .GlobalEnv)
  
}