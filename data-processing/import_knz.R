#---------------------#
# Import KNZ soil data
#---------------------#
# Julien Brun, NCEAS
# SciComp@nceas.ucsb.edu

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)

# devtools::install_github("ropenscilabs/emldown")
library("emldown")



## CONSTANTS ----

file_name <- "data-processing/NSC011.csv"
eml_name <- "data-processing/knb-lter-knz.47.7.xml"


## FUNCTIONS ----

categorical_frequency <- function(df){
  # Select the character columns
  cat_data <- Filter(is.character, site_data)
  # compute the unique values and frequencies
  cat_freq_list <- map(cat_data, janitor::tabyl)
  # Create a data frame out of that
  cat_freq <- bind_rows(cat_freq_list, .id = "attribute")
  names(cat_freq)[[2]] <- "category"
  
  cat_freq
}


## MAIN ----

# Render a human readable version of the metadata
render_eml(eml_name)

# Read the data in
site_data <- read_csv(file_name)

# Some data exploration
cat_freq <- categorical_frequency(site_data)

cat_data <- Filter(is.character, site_data)
cat_freq_list <- map(cat_data, janitor::tabyl)
cat_freq <- bind_rows(cat_freq_list, .id = "attribute")
