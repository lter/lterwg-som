
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
# unit). This workflow documents updates to the QC values. Note that, in this
# workflow, we are updating the data in the package directly rather than
# updating the file on Google Drive thus the data in the Google Drive and in
# this package are out-of-step for some variables.

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


# update 2019-05-31 -------------------------------------------------------

# update 2019-05-31: update lit_c to reflect units of mg/g (not %)
locationQC <- locationQC %>%
  mutate(
    minValue = replace(minValue, var == "lit_c", 0.1),
    maxValue = replace(minValue, var == "lit_c", 600),
    givenUnit = replace(givenUnit, var == "lit_c", "mg g-1")
  )

usethis::use_data(locationQC, overwrite = TRUE)

# update 2019-05-31: update Ca, Mg, K, Na to reflect units of cmol kg-1 (not %)
profileQC <- profileQC %>%
  mutate(
    maxValue = replace(maxValue, var == "Ca", 1000),
    maxValue = replace(maxValue, var == "Mg", 1000),
    maxValue = replace(maxValue, var == "K", 1000),
    maxValue = replace(maxValue, var == "Na", 1000)
  )

usethis::use_data(profileQC, overwrite = TRUE)

# update 2019-05-31: update layer_bot min to -1 to reflect different O horizons

profileQC <- profileQC %>%
  mutate(minValue = replace(minValue, var == "layer_bot", -1))

usethis::use_data(profileQC, overwrite = TRUE)
