# README ------------------------------------------------------------------

# overview

# This workflow largely mirrors harvest_HMGZD_drive.R, which was the workflow
# developed to aggregate homogenized data files but differs in that the worflow
# here works on a downloaded, unzipped directory of the content in
# `Data_downloads` (as opposed to pulling homogenized data files directly from
# Google Drive). This modified workflow became necessary as accessing the entire
# contents of `Data_downloads` became no longer feasible. The error message
# suggests a memory issue but some SO comments suggest that there is too much
# recursion. Regardless, harvesting via Google Drive seems no longer feasible.
# Another difference is that we are generally moving away from Aurora owing to
# the curl issue, so note that the output here is written to local file then
# uploaded to the project folder on Google Drive. Further, the modification_date
# formatting in the previous workflow has not been implemented here, and the
# list of columns to convert to character to facilitate binding of different
# variable types is specific to the data downloaed. Finally, because downloading
# the data occurs outside of R (i.e., is not a manual process), the time of
# harvest is no longer recorded. Instead, the time of aggregated file generation
# is noted, but careful that this time reflects the time that the the aggregated
# data were written to file NOT the time they were harvested.

# data harvest

# Workflow to harvest and merge homogenized data sets. The workflow starts
# outside of R with the user downloading and unzipping the `Data_downloads`
# directory from Google Drive to local file. For efficiency, the
# `Data_downloads` directory within the unzipped directory should be moved
# outside of the unzipped directory. The workflow identified directories
# containing a version 2 of the key file, then harvests the data files from
# those data files.

# data manipulation

# NOT IMPLEMENTED: The guess_date function in this workflow attempts to
# standardize the format of the modification_date field in each homogenized data
# set to faciliate merging.

# Some variables (e.g., L1, L2, tx_L1) are too diverse across data sets to
# standardize so are convereted to type character with the cols_to_character
# function so that they do not prevent merging. As new data sets are
# homogenized, add the column/variable name of any fields that are creating a
# merge issue to the targets list - any columns with a name in the target list
# will be converted to type character.

# data output

# After standardization, the list of data sets is merged into a single data
# frame. This data frame is then written to file and the Google Drive as both
# .csv and .rds files with the time stamp of when the data were aggregated.

# notes

# This workflow could be automated or, at least, sourced but because of the
# frequency of errors (e.g., new data sets with columns that have to be
# converted to character), it should generally be run manually.

# libraries ---------------------------------------------------------------

library(tools)
library(tidyverse)
library(readxl)
library(googledrive)


# options -----------------------------------------------------------------

options(httr_oob_default = TRUE) # create out-of-band oauth token in server env
options(scipen = 999) # no sci. notation

# identify homogenized data files -----------------------------------------

# build a reference of directories containing version 2 of the key file

key_v2_dirs <- tibble(
  fullPath = list.files(path = "Data_downloads/",
                        recursive = TRUE,
                        full.names = TRUE)
) %>% 
  filter(grepl("KEY_V2", fullPath)) %>%
  mutate(key_v2_dir = str_extract(fullPath, ".*\\/")) %>% 
  pull(key_v2_dir)


# harvest and format data files created with key file V2 ------------------

# function to harvest HMGZD data files (*.HMGZD.xlsx) from directories
# identified above in which there is a version 2 of the key file present
harvest_hmgzd_data <- function(directory) {

  map(.x = list.files(path = directory,
      pattern = "HMGZD\\.",
      full.names = TRUE),
    ~ read_excel(path = .x,
      na = c("", "NA"),
      guess_max = Inf)
  )

}

# harvest HMGZD data files, compact will remove any empty list items (e.g.,
# where there is a key file but the files were not homogenized)
homogenizedData <- map(.x = key_v2_dirs, .f = harvest_hmgzd_data) %>%
  compact()

# unlist will flatten the list such that data sets with multiple HMGZD data
# files will be unnested so that all list items are of the same depth
homogenizedData <- unlist(homogenizedData, recursive = FALSE)


# conflicting column types to character -----------------------------------

# function cols_to_character: for a given data entity (e.g., data frame in a
# list of data frames), identify any columns that are in the list of variables
# (i.e., targets) that need to be standardized with regard to data type to
# facilitate binding.

# vector of data type conflicts
targets <- c(
  # "lat",
  # "long",
  "L1",
  "L2",
  "L3",
  "L4",
  # "layer_top",
  # "layer_bot",
  # "bd_samp",
  "NA_1",
  "NA_2",
  "number_treatments",
  # "tx_start",
  # "bgb_upperdiam",
  # "elevation",
  # "map",
  "tx_L1",
  "tx_L2",
  "tx_L3",
  "tx_L4",
  "observation_date",
  "observation_date_1",
  "observation_date_2",
  "control_id" #,
  # "modification_date",
  # "agb"
  # "mat",
  # "wood_lit_c",
  # "slope"
)

cols_to_character <- function(entity) {

  subTargets <- intersect(colnames(entity), targets)

  entity %>%
    mutate_at(vars(subTargets), as.character)

}

homogenizedData <- map(.x = homogenizedData, .f = cols_to_character)


# aggregate data ----------------------------------------------------------

boundData <- bind_rows(homogenizedData)


# tarball bulk processing -------------------------------------------------

# the following are a series of steps for calculating new variables and minor
# formatting of existing data

boundData <- boundData %>%
  mutate(
    lyr_c_to_n = case_when(
      is.na(lyr_c_to_n) & lyr_n_tot != 0 ~ lyr_soc / lyr_n_tot,
      TRUE ~ lyr_c_to_n 
    ),
    layer_mid = case_when(
      is.na(layer_mid) ~ (layer_bot + layer_top) / 2,
      TRUE ~ layer_mid 
    ),
    layer_thick_calc = case_when(
      !is.na(layer_bot) & !is.na(layer_top) & layer_bot >= 0 & layer_top >= 0 ~ (layer_bot - layer_top)
    ),
    lyr_soc_stock_calc = case_when(
      is.na(lyr_soc_stock) & !is.na(lyr_soc) & !is.na(bd_samp) & !is.na(layer_thick_calc) & layer_thick_calc >= 0 ~ lyr_soc * bd_samp * layer_thick_calc * 100,
      TRUE ~ lyr_soc_stock),
    lyr_n_stock_calc = case_when(
      is.na(lyr_n_tot_stock) & !is.na(lyr_n_tot) & !is.na(bd_samp) & !is.na(layer_thick_calc) & layer_thick_calc >= 0 ~ lyr_n_tot * bd_samp * layer_thick_calc * 100,
      TRUE ~ lyr_n_tot_stock),
    lit_cn = case_when(
      is.na(lit_cn) & lit_n != 0 ~ lit_c / lit_n,
      TRUE ~ lit_cn  
    ),
    bgb_cn = case_when(
      is.na(bgb_cn) & bgb_n != 0 ~ bgb_c / bgb_n,
      TRUE ~ bgb_cn  
    ),
    # salt, silt, clay calcs
    sand = case_when(
      is.na(sand) & !is.na(clay) & !is.na(silt) ~ 100 - (clay + silt),
      TRUE ~ sand
    ),
    silt = case_when(
      is.na(silt) & !is.na(clay) & !is.na(sand) ~ 100 - (clay + sand),
      TRUE ~ silt 
    ),
    clay = case_when(
      is.na(clay) & !is.na(silt) & !is.na(sand) ~ 100 - (silt + sand),
      TRUE ~ clay  
    ),
    time_series = toupper(time_series),
    gradient = toupper(gradient),
    merge_align = toupper(merge_align),
    experiments = toupper(experiments)
  ) %>%  # close mutate
  rename("bd_methods_notes" = `^bd_`)


# identify controls -------------------------------------------------------

# identify controls based on the presence of the prescribed control identifer in
# any of the experiment or treatment levels

# experimentTreatmentVarSet <- c('L1','L2','L3','L4','L5','L6','tx_L1','tx_L2','tx_L3','tx_L4','tx_L5','tx_L6')

boundData <- boundData %>% 
  mutate(control_id = gsub(" ", "", control_id)) %>% 
  rowwise() %>% 
  mutate(control_id_vec = str_split(string = control_id, pattern = ",")) %>% 
  mutate(
    control_sample  = as.logical(NA),
    control_sample = case_when(
      (
        (
          L1 %in% control_id_vec |
            tx_L1 %in% control_id_vec |
            L2 %in% control_id_vec |
            tx_L2 %in% control_id_vec |
            L3 %in% control_id_vec |
            tx_L3 %in% control_id_vec |
            L4 %in% control_id_vec |
            tx_L4 %in% control_id_vec |
            L5 %in% control_id_vec |
            tx_L5 %in% control_id_vec
        ) &
          !all(is.na(control_id_vec))
      ) | grepl("no", experiments, ignore.case = T) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  ungroup() %>% 
  select(-control_id_vec)

# the NWT snow fence study is a special case when the control identifiers are
# spread across two columns, and only the combination of the two identifers
# signifies a control. This as opposed to the above function, which identifies a
# control if the control identifer is identified in any of the experiment or
# treatment fields.

boundData <- boundData %>% 
  mutate(
    control_sample = replace(control_sample, grepl("nwt_snowfence", google_dir, ignore.case = T), FALSE),
    control_sample = case_when(
      grepl("nwt_snowfence", google_dir, ignore.case = T) & grepl("no_snow", tx_L1, ignore.case = T) & grepl("cc", tx_L2, ignore.case = T) ~ TRUE,
      TRUE ~ control_sample 
    )
  )


# replaced NutNet data to NA ----------------------------------------------

# data for the following vars are duplicated across years but are only relevant
# to the year of collection (i.e., the first year of the study); convert
# duplicated data to NA

NN_columns_to_clean <- c("bd_samp", "lyr_soc", "lyr_n_tot", "p_ex_1", "k", "ca", "na", "fe_HCl", "al", "ph_h2o")

boundData <- boundData %>% 
  mutate_at(.vars = NN_columns_to_clean, .funs = ~replace(., grepl("nutnet", network, ignore.case = T) & observation_date >= tx_start, NA))


# additional standardization ---------------------------------------------------

# Initial efforts to publish these data revealed a suite of variables that were
# added by users that were not part of the template key file. Basically, these
# were identified as variables in tarball but no in the key file template. Some
# of these were addressed by rehomoging the data. Some became new variables
# (e.g., litterfall_anpp). Some were removed as there was no way to standardize
# them (e.g., n_replicates) and some were recoded into the variable where the
# information should reside (e.g., mg).

varsToRemove <- c(
  "layer",            # copied to clay
  "dominant_species", # user added / not standardized
  "comment_location", # copied to loc_comments
  "doi_number",       # copied to data_doi
  "aspect",           # copied to aspect_class
  "comments",         # copied to loc_comments
  "loi",              # copied to lyr_loi
  "no3_n",            # user added / not standardized
  "nh4_n",            # user added / not standardized
  "align",            # copied to align_1
  "eco_type",         # copied to eco_region
  "al",               # user added / not standardized
  "k",                # copied to K
  "ca",               # copied to Ca
  "na",               # copied to NA
  "mg",               # copied to Mg
  "fe_HCl",           # user added / not standardized
  "n_replicates",     # user added / not standardized
  "c_tot_se",         # user added / not standardized
  "n_tot_se"          # user added / not standardized
)

boundData <- boundData %>%
  mutate(
    # AND misidentified layer as clay
    clay = case_when(
      !is.na(layer) ~ layer,
      TRUE ~ clay
      ),
    # doi_number should be data_doi
    data_doi = case_when(
      !is.na(doi_number) ~ doi_number,
      TRUE ~ data_doi
      ),
    # comment_location should be loc_comments
    loc_comments = case_when(
      !is.na(comment_location) ~ comment_location,
      TRUE ~ loc_comments
      ),
    # aspect should be aspect_class
    aspect_class = case_when(
      !is.na(aspect) ~ aspect,
      TRUE ~ aspect_class
      ),
    # comments should be loc_comments
    loc_comments = case_when(
      !is.na(comments) ~ comments,
      TRUE ~ loc_comments
      ),
    # loi should be lyr_loi
    lyr_loi = case_when(
      !is.na(loi) ~ loi,
      TRUE ~ lyr_loi
      ),
    # ca should be Ca
    Ca = case_when(
      !is.na(ca) ~ ca,
      TRUE ~ Ca
      ),
    # mg should be Mg
    Mg = case_when(
      !is.na(mg) ~ mg,
      TRUE ~ Mg
      ),
    # na should be Na
    Na = case_when(
      !is.na(na) ~ na,
      TRUE ~ Na
      ),
    # k should be K
    K = case_when(
      !is.na(k) ~ k,
      TRUE ~ K
      ),
    # align should be align_1 but first remove boolean align from
    # NEON_initialChar
    align = case_when(
      google_dir == "NEON_initialChar" ~ NA_character_,
      TRUE ~ align
      ),
    align_1 = case_when(
      !is.na(align) ~ align,
      TRUE ~ align_1
      ),
    # eco_type should be eco_region
    eco_region = case_when(
      !is.na(eco_type) ~ eco_type,
      TRUE ~ eco_region
    )
    ) %>%
select(-all_of(varsToRemove))

  
# write aggregated data to file -------------------------------------------

saveRDS(boundData, paste0('somCompositeData_', Sys.Date(), '.rds'))
# write_csv(boundData, paste0('somCompositeData_', Sys.Date(), '.csv'))


# upload aggregated data to Google Drive ----------------------------------

homogedAndBoundOutputID <- googledrive::drive_get('homoged_and_bound_output') %>%
  dplyr::pull(id)

googledrive::drive_upload(media = paste0('somCompositeData_', Sys.Date(), '.rds'),
                          path = as_id(homogedAndBoundOutputID),
                          name = paste0('somCompositeData_', Sys.Date(), '.rds'))

# googledrive::drive_upload(media = paste0('somCompositeData_', Sys.Date(), '.csv'),
#                           path = as_id(homogedAndBoundOutputID),
#                           name = paste0('somCompositeData_', Sys.Date(), '.csv'))
