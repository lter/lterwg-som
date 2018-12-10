
# README ------------------------------------------------------------------

# Early work with the SOM data indicated that additional details about the data
# sets are required. To accomodate more detail, new additions to the key file
# are needed. The key_version2 function addresses desired changes to the key
# files. It is critical that information already entered into key files was not
# lost, so the new key file features had to be added to existing key file
# without information loss.

# workflow:
# (1) download project key file with googledrive
# (2) load downloaded (now xlsx) into R with openxlsx::loadWorkbook **
# (3) additions to the workbook object as needed
# (4) prescribe validations with openxlsx::dataValidation
# (5) fix styling as needed with openxlsx::createSytle/addStyle
# (6) write workbook back to file with openxlsx::write.xlsx (or saveWorkbook)
# (7) upload workbook back to project directory to be followed by a re-homog

# ** note
# the workflow was getting hung up at step 2. The solution was to use the
# development branch of the openxlsx package (see
# https://github.com/awalker89/openxlsx/issues/386)
# devtools::install_github("awalker89/openxlsx") Rcpp package is a dependency

# The workflow here is specific to a key file version 2. New features include:

# 1. several new metadata fields in the location tab ('time_series', 'gradient',
# 'experiments', 'control_id', 'number_treatments', 'merge_align',
# 'key_version').

# 2. A new 'logical' column in the Units tab to facilitate a YES, NO drop-down
# option for several of the new metadata fields added to the location tab.

# 3. Revised options in the Units tab for the list of drop-down options in the
# treatment rows of the Profile tab.

# 4. Add pull-down menu for units field of lit_lig in location tab.

# 5. Clarify meanings (Var_long field) of profle tab c_tot and soc (bulk, not
# fraction)

# Though specific to version 2 features, this workflow could be modified to
# implement new features for future versions.

# ** note
# this workflow is specific to being run on Aurora

# run as...
# key_version2('621_Key_Key_test')
# key_version2('cap.557.Key_Key_master')


# libraries ---------------------------------------------------------------

library(googlesheets)
library(googledrive)
library(openxlsx) # development version required, install via GitHub
library(tidyverse)
library(purrr)
library(tools)


# establish or reset log --------------------------------------------------

# create template for logging details of key file upversions. caution: this code
# will overwrite an existing log

# tibble(
#   keyFileName = as.character(NA),
#   keyFileDirectory = as.character(NA),
#   timestamp = as.POSIXct(NA)
# ) %>% 
#   write_csv(path = '/home/shares/lter-som/key_file_update_log.csv',
#             append = FALSE)


# key_version2 function ---------------------------------------------------

key_version2 <- function(sheetName) {
  
  
  # access Google Drive sheet -----------------------------------------------
  
  # keyFileDownloadPath is a intermediary directory to where the key file from
  # Google Drive may be downloaded for importing into the R environment
  
  keyFileDownloadPath <- '/home/shares/lter-som/key_file_download/'
  
  drive_download(file = sheetName,
                 path = paste0(keyFileDownloadPath, sheetName, '.xlsx'),
                 overwrite = TRUE)
  
  
  # openxlsx workbook and access sheets -------------------------------------
  
  # currently set to a local directory but this should eventually direct to the
  # lter-som directory on Aurora
  
  # load downloaded key file as a openxlsx workbook
  keyfileWorkbook <- loadWorkbook(file = paste0(keyFileDownloadPath, sheetName, '.xlsx'))
  
  # import location tab sheet
  sheetLocation <- read.xlsx(xlsxFile = keyfileWorkbook,
                             sheet = 'Location_data')
  
  # before proceeding, check for key file version 2 features and stop if they are
  # present
  key_v2_location_additions <- c('time_series',
                                 'gradient',
                                 'experiments',
                                 'control_id',
                                 'number_treatments',
                                 'key_version')
  
  if(all(key_v2_location_additions %in% sheetLocation$var)) {
    stop("the key file in this data set seems to already be at or abover version 2")
  }
  
  # import profile tab sheet
  sheetProfile <- read.xlsx(xlsxFile = keyfileWorkbook,
                            sheet = 'Profile_data (Key-Key)')
  
  
  # DEV only feature: import profile tab sheet - not needed until later imported
  # at this point for dev only
  sheetUnits <- read.xlsx(xlsxFile = keyfileWorkbook,
                          sheet = 'Units')
  
  
  # write location and profile sheets to file for archiving -----------------
  
  write_csv(x = sheetLocation,
            path = paste0("/home/shares/lter-som/key_file_archive/", sheetName, "_location.csv"),
            append = FALSE)
  write_csv(x = sheetProfile,
            path = paste0("/home/shares/lter-som/key_file_archive/", sheetName, "_profile.csv"),
            append = FALSE)
  
  
  # add new drop down options to units sheet --------------------------------
  
  # revised treatment level options
  revisedTreatmentOptions <- c(
    'nutrients',
    'litter_manip',
    'warming',
    'precip',
    'fire',
    'forest_harvest',
    'ag_harvest',
    'times-series',
    'tillage',
    'CO2',
    'other (add notes)')
  
  writeData(wb = keyfileWorkbook,
            sheet = 'Units',
            x = revisedTreatmentOptions,
            startCol = 2,
            startRow = 2)
  
  # logical for new location metadata
  
  # append logical column to end of Units sheet
  numColsModifiedUnits <- ncol(read.xlsx(xlsxFile = keyfileWorkbook,
                                         sheet = 'Units'))
  
  newLogical <- tibble(
    logical = c('YES', 'NO')
  )
  
  writeData(wb = keyfileWorkbook,
            sheet = 'Units',
            x = newLogical,
            startCol = numColsModifiedUnits + 1,
            startRow = 1,
            colNames = TRUE)
  
  
  # new location sheet metadata ---------------------------------------------
  
  # set keyVersion to 2. older code to update conditionally not relevant since we
  # are noting key versions (i.e., not key input iterations)
  
  if (sheetLocation %>% filter(var == "key_version") %>% nrow() == 0) {
    keyVersion <- 2
  } # else {
  #   keyVersion <- as.integer(sheetLocation[sheetLocation$var == "key_version",]$Value + 1)
  # }
  
  # get max row of location sheet
  locationMaxRow <- as.integer(nrow(sheetLocation))
  
  # build tibble of new metadata to add to location tab
  newLocationMetadata <- tibble(
    Value = c(NA, NA, NA, NA, NA, NA, keyVersion),
    Unit = c(NA, NA, NA, NA, NA, NA, NA),
    Var_long = c('includes time-series data',
                 'is a gradient study',
                 'includes experimental manipulations',
                 'control samples identifier',
                 'number of treatments',
                 'merging datafiles required? please add details to alignment notes',
                 'key file version (do not edit)'),
    var = c('time_series',
            'gradient',
            'experiments',
            'control_id',
            'number_treatments',
            'merge_align',
            'key_version'),
    Level = c('location',
              'location',
              'location',
              'location',
              'location',
              'location',
              'location')
  )
  
  # add tibble of new metadata to location tab; note `startRow = locationMaxRow +
  # 2` is to account for the next row and the header row, which R does not
  # recognize as a data row
  writeData(wb = keyfileWorkbook,
            sheet = 'Location_data',
            x = newLocationMetadata,
            startCol = 1,
            startRow = locationMaxRow + 2,
            colNames = FALSE)
  
  
  # new profile sheet validations -------------------------------------------
  
  # key file units sheet can be different so we must reference columns by name
  
  # helper function to make a spreadsheet-style vector of letters
  letterwrap <- function(n, depth = 1) {
    args <- lapply(1:depth, FUN = function(x) return(LETTERS))
    x <- do.call(expand.grid, args = list(args, stringsAsFactors = F))
    x <- x[, rev(names(x)), drop = F]
    x <- do.call(paste0, x)
    if (n <= length(x)) return(x[1:n])
    return(c(x, letterwrap(n - length(x), depth = depth + 1)))
  }
  
  # access revised sheetUnits to coordinate columns for validation
  sheetUnits <- read.xlsx(xlsxFile = keyfileWorkbook,
                          sheet = 'Units')
  
  # tibble of column names and corresponding column ids (e.g. D, AB)
  spreadsheetLetters <- tibble(
    column = letterwrap(length(colnames(sheetUnits))),
    columnName = colnames(sheetUnits)
  )
  
  
  # add validation to treatments 
  
  # identify range of treatment level cells in profile sheet; add one to account
  # for header row, which is not seen as a row of data by R
  trtMinCell <- min(grep("Treatment_", sheetProfile$Var_long)) + 1
  trtMaxCell <- max(grep("Treatment_", sheetProfile$Var_long)) + 1
  
  # get spreadsheet id of treatment column
  treatmentColID <- spreadsheetLetters %>% 
    filter(grepl('treatment', columnName)) %>% 
    select(column) %>% 
    pull()
  
  # add validation to treatment input
  dataValidation(wb = keyfileWorkbook, 
                 sheet = "Profile_data (Key-Key)", 
                 cols = 2,
                 rows = trtMinCell:trtMaxCell,
                 type = "list",
                 value = paste0("'Units'!$", treatmentColID, "$2:$", treatmentColID, "$12"))
  
  
  # add validation to new location tab metadata
  
  # access revised sheetLocation to coordinate columns for validation
  sheetLocation <- read.xlsx(xlsxFile = keyfileWorkbook,
                             sheet = 'Location_data')
  
  # access revised sheetUnits to coordinate columns for validation
  sheetUnits <- read.xlsx(xlsxFile = keyfileWorkbook,
                          sheet = 'Units')
  
  # tibble of column names and corresponding column ids (e.g. D, AB)
  spreadsheetLetters <- tibble(
    column = letterwrap(length(colnames(sheetUnits))),
    columnName = colnames(sheetUnits)
  )
  
  # get the column id of the new logical column in the Units sheet
  logicalColID <- spreadsheetLetters %>% 
    filter(grepl('logical', columnName)) %>% 
    select(column) %>% 
    pull()
  
  # get the column id of the soil.C,.soil.N column in the Units sheet
  soilCNColID <- spreadsheetLetters %>% 
    filter(grepl('soil', columnName)) %>% 
    select(column) %>% 
    pull()
  
  # add Units::logical validation to Location::time_series
  
  # add validation to treatment input
  dataValidation(wb = keyfileWorkbook, 
                 sheet = "Location_data", 
                 cols = 1,
                 rows = grep("time_series", sheetLocation$var) + 1,
                 type = "list",
                 value = paste0("'Units'!$", logicalColID, "$2:$", logicalColID, "$11"))
  
  # add Units::logical validation to Location::gradient
  dataValidation(wb = keyfileWorkbook, 
                 sheet = "Location_data", 
                 cols = 1,
                 rows = grep("gradient", sheetLocation$var) + 1,
                 type = "list",
                 value = paste0("'Units'!$", logicalColID, "$2:$", logicalColID, "$11"))
  
  # add Units::logical validation to Location::experiments
  dataValidation(wb = keyfileWorkbook, 
                 sheet = "Location_data", 
                 cols = 1,
                 rows = grep("experiments", sheetLocation$var) + 1,
                 type = "list",
                 value = paste0("'Units'!$", logicalColID, "$2:$", logicalColID, "$11"))
  
  # add Units::logical validation to Location::merge_align
  dataValidation(wb = keyfileWorkbook, 
                 sheet = "Location_data", 
                 cols = 1,
                 rows = grep("merge_align", sheetLocation$var) + 1,
                 type = "list",
                 value = paste0("'Units'!$", logicalColID, "$2:$", logicalColID, "$11"))
  
  # add Units::soil C, soil N#1-5 validation to Location::lit_lig
  dataValidation(wb = keyfileWorkbook, 
                 sheet = "Location_data", 
                 cols = 2,
                 rows = grep("lit_lig", sheetLocation$var) + 1,
                 type = "list",
                 value = paste0("'Units'!$", soilCNColID, "$2:$", soilCNColID, "$6"))
  
  
  # change c_tot & soc Var_long ---------------------------------------------
  
  if (!is.null(grep("Bulk Layer Total Carbon", sheetProfile$Var_long))) {
    
    writeData(wb = keyfileWorkbook,
              sheet = 'Profile_data (Key-Key)',
              x = 'Bulk Layer Total Carbon, not acid treated to remove inorganic C',
              startCol = 3,
              startRow = grep("Bulk Layer Total Carbon", sheetProfile$Var_long) + 1,
              colNames = TRUE)
    
  }
  
  if (!is.null(grep("Bulk Layer Organic Carbon \\(CN analyzer\\) concentration", sheetProfile$Var_long))) {
    
    writeData(wb = keyfileWorkbook,
              sheet = 'Profile_data (Key-Key)',
              x = 'Bulk Layer Organic Carbon (CN analyzer) concentration, inorganic C removed or not present',
              startCol = 3,
              startRow = grep("Bulk Layer Organic Carbon \\(CN analyzer\\) concentration", sheetProfile$Var_long) + 1,
              colNames = TRUE)
    
  }
  
  
  # fix formatting imposed by openxlsx --------------------------------------
  
  # create styles
  bodyStyle <- createStyle(fontSize = 10,
                           fontName = 'Arial')
  
  headerStyle <- createStyle(fontSize = 10,
                             fontName = 'Arial',
                             textDecoration = 'bold')
  
  
  # function to apply styles
  updateStyle <- function(uniqueSheet) {
    
    numRowsModified <- nrow(read.xlsx(xlsxFile = keyfileWorkbook,
                                      sheet = uniqueSheet))
    numColsModified <- ncol(read.xlsx(xlsxFile = keyfileWorkbook,
                                      sheet = uniqueSheet))
    
    # body style to body
    addStyle(wb = keyfileWorkbook,
             sheet = uniqueSheet,
             style = bodyStyle,
             rows = 1:numRowsModified + 1,
             cols = 1:numColsModified + 1,
             gridExpand = TRUE,
             stack = FALSE)
    
    # body style to first column (not sure why this is not caught above)
    addStyle(wb = keyfileWorkbook,
             sheet = uniqueSheet,
             style = bodyStyle,
             rows = 1:numRowsModified + 1,
             cols = 1,
             gridExpand = TRUE,
             stack = FALSE)
    
    # header style to first row
    addStyle(wb = keyfileWorkbook,
             sheet = uniqueSheet,
             style = headerStyle,
             rows = 1,
             cols = 1:numColsModified,
             gridExpand = TRUE,
             stack = FALSE)
    
  }
  
  # apply styles to all sheets
  walk(c('Location_data', 'Profile_data (Key-Key)', 'Units'), ~updateStyle(.x))
  
  
  # save modified workbook to file ------------------------------------------
  
  # keyFileDownloadPath is a intermediary directory to where the new key
  # workbook from the R environment can be saved for eventual upload to Google
  # Drive
  
  keyFileUploadPath <- '/home/shares/lter-som/key_file_upload/'
  
  # save workbook to file
  saveWorkbook(wb = keyfileWorkbook,
               file = paste0(keyFileUploadPath, sheetName, '_KEY_V2.xlsx'),
               overwrite = TRUE)
  
  
  # upload revised key file to google drive --------------------------------
  
  # retrieve key file details
  keyFileDetails <- drive_get(sheetName)
  
  # retrieve key file parent directory
  keyFileParent <- keyFileDetails[["drive_resource"]][[1]][["parents"]][[1]]
  
  # upload to google drive
  drive_upload(media = paste0(keyFileUploadPath, sheetName, '_KEY_V2.xlsx'),
               path = as_id(keyFileParent),
               type = "spreadsheet")
  
  
  # log upversion -----------------------------------------------------------
  
  tibble(
    keyFileName = sheetName,
    keyFileDirectory = drive_get(as_id(keyFileParent))[['name']],
    timestamp = Sys.time()
  ) %>% 
    write_csv(path = '/home/shares/lter-som/key_file_update_log.csv',
              append = TRUE)
  
}