
# README ------------------------------------------------------------------

# Part of the key file version 2 update included changing the names of
# variables, specifically the 'var' column, such that they are unique. Much of
# the duplication in the names stemmed from the same name being used for both
# bulk and fractional equivalents of a variable. For example, there may have
# been a 'c_tot' variable that corresponded to bulk measurements and another
# 'c_tot' that corresponded to soil fractions. As a result, much of the renaming
# centered on this distinction such that new names for the unique c_tot
# variables are now something along the lines of lyr_c_tot and frc_c_tot for
# layer and fraction, respectively. A consequence of these name changes is that
# the old variable names (e.g., c_tot) are referenced in the data files that
# hold the units conversion details. To address this, the variable names in the
# data file holding the units conversion details had to be updated to match the
# key file version 2 variable names. The script here addresses that requirement.


# libraries ---------------------------------------------------------------

library(soilHarmonization)
library(tidyverse)


# set up ------------------------------------------------------------------

# data('newNamesLocation', package = 'soilHarmonization')
# data('unitsConversionLocation', package = 'soilHarmonization')

data('newNamesProfile', package = 'soilHarmonization')
data('unitsConversionProfile', package = 'soilHarmonization')

head(unitsConversionProfile)
head(newNamesProfile)


# profile -----------------------------------------------------------------

# update existing unitsConversionProfile with Var_long from key V2
unitsConversionProfile <- unitsConversionProfile %>% 
  mutate(
    Var_long = replace(Var_long, Var_long == 'Bulk Layer Total Carbon', 'Bulk Layer Total Carbon, not acid treated to remove inorganic C'),
    Var_long = replace(Var_long, Var_long == 'Bulk Layer Organic Carbon (CN analyzer) concentration', 'Bulk Layer Organic Carbon (CN analyzer) concentration, inorganic C removed or not present')
  )

# merge existing units conversion with new names given for key V2
unitsConversionProfile <- unitsConversionProfile %>% 
  filter(!is.na(unitConversionFactor)) %>%
  left_join(newNamesProfile, by = c('Var_long')) %>% 
  filter(!grepl("fraction", Level, ignore.case = TRUE)) %>% 
  mutate(
    var = case_when(
      is.na(var_new_name) ~ var,
      !is.na(var_new_name) ~ var_new_name
    )
  ) %>% 
  select(unit_levels, Var_long, var, givenUnit, unitConversionFactor)


# location ----------------------------------------------------------------

# location data are okay as we did not change the names of any location
# variables that also have a units conversion

# inner_join(unitsConversionLocation, newNamesLocation, by = c('var' = 'var_old_name')) %>% 
#   arrange(var) %>% 
#   select(-contains('Var_long')) %>% 
#   filter(!is.na(unitConversionFactor)) %>% 
#   View()