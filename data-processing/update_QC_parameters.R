
# README ------------------------------------------------------------------

# The soil data homogenization process includes some data quality checking. One
# check checks the investigator-provided values against a reaonable range of
# values for that given variable. Expected ranges are outlined in a Google Sheet
# titled 'Key_Key_V2_lookup'
# (https://docs.google.com/spreadsheets/d/1l3u2bPgzxotFao3GHp_JOGK9YA_NELdP7dkAztPnyqk/edit#gid=1798806153)
# in the LTER-SOM/LTER-SOM-keyfile directory. So that these values do not have
# to be loaded evertime the homogenization function is run, the data are
# included in the package as data/locationQC.rda and data/profileQC/rda, and,
# thus, available to the package soilHarmonization package functions. The
# initial ranges identified at early stages of the project were in some cases
# not appropriate (e.g., a range provided for a unit other than the expected
# unit). This workflow documents the initial creation of the *.rda files, which
# is the same workflow used to update values as changes are required.

# libraries ---------------------------------------------------------------


library(tidyverse)
library(usethis)


# options -----------------------------------------------------------------

options(scipen = 999)


# load data and store a temporary (backup) --------------------------------

# load data from package
data('locationQC', package = 'soilHarmonization')
data('profileQC', package = 'soilHarmonization')

# store a temporary copy of the current data
write_csv(locationQC, "~/Desktop/locationQC_OE.csv")
write_csv(profileQC, "~/Desktop/profileQC_OE.csv")


# load data from drive and add to package ---------------------------------

# here downloading data from Google Drive as csv files instead of reading
# through goooglesheets, which woule work just as well

# location

locationQC <- read_csv('~/Desktop/Key_Key_V2_lookup - Location_data.csv')
usethis::use_data(locationQC, overwrite = TRUE)

# profile

profileQC <- read_csv('~/Desktop/Key_Key_V2_lookup - Profile_data (Key-Key).csv')
usethis::use_data(profileQC, overwrite = TRUE)

