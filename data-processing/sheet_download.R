
# README ------------------------------------------------------------------

# helper function to facilitate accessing Google Sheets


# function: sheet_download ------------------------------------------------

sheet_download <- function(fileName) {
  
  token <- googlesheets::gs_title(fileName)
  
  dataFile <- googlesheets::gs_read(token)
  
}
