#QC function for SOM homogenization
#Author: Derek Pierson
#Created: 12/10/18
#Last version edit: 12/11/18

# Description: QC function to check that LTER-SOMWG homogenized data columns meet requested 
# format and value limits specified in "HMG_QC_constraints" sheet on GDrive. 


#Load libraries
library(tidyverse)
library(googledrive)
library(googlesheets)


### NOTE: QC fail breaks are currently disabled

#Homogenized data QC function
homog.QC <- function(hmg.df, QC.tbl) {

 ### Ensure conversion of googlesheet tibble to dataframe
  hmg.df <- as.data.frame(hmg.df)
  QC.tbl <- as.data.frame(QC.tbl)
  
  #Verify all dataframe column names exist in QC constraint table  
  if(!all(as.character(colnames(hmg.df)) %in% QC.tbl$var)) 
    {print("Warning: QC constraints do not exist for all homogenized data columns")}
      
  #Cycle through hmg.df columns
  for(i in 1:ncol(hmg.df)) {
    
    #Check if hmg.df column exists in QC.tbl, if not then go to next column
    ## This step is needed as we build up the QC table
    if(!colnames(hmg.df)[i] %in% QC.tbl$var) {
      #If last column, print message QC complete
      if(i == ncol(hmg.df)) {print("QC complete")}
      next}  #Jump to next loop iteration
    
    #Isolate QC restraints for specified column
    qc.var <- NULL
    qc.var <- filter(QC.tbl,var == colnames(hmg.df)[i])  #qc.var should be exactly one row
    
    ### CHARACTER QC CHECKS
    #If data class is "character", then...
    if(qc.var$class == "character") {
      
      #Verify all column values are standard chatacters
      if(!all(is.character(na.omit(hmg.df[,i])))) {
        print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," is not character format"))
        #break
      }
      
      
      #Verify character string does not exceed max length set by QC.tbl
      if( all(nchar(na.omit(hmg.df[,i])) > qc.var$range_max)) {
        print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," character length exceeds max value of ",qc.var$range_max))
        #break
      }
    }
    
    ### NUMERIC QC CHECKS
    #If data class is "numeric", then...
    if(qc.var$class == "numeric") {
      
      #Verify all column values are numeric
      if(!all(is.numeric(na.omit(hmg.df[,i])))) 
        {print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," is not numeric format"))
        #break
      }
      
      #Verify numeric values do not exceed max value set by QC.tbl
      if(all(na.omit(hmg.df[,i]) > qc.var$range_max)) 
        {print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," has numeric value exceeding QC max value of ",qc.var$range_max))
        #break
      }
      
      
      #Verify numeric values do not exist below min value set by QC.tbl
      if(all(na.omit(hmg.df[,i]) < qc.var$range_min)) 
        {print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," has numeric value below QC min value of ",qc.var$range_min))
        #break
      }
      
      
      #If unit type is "percent", check to see if all values occupy range from 0-1
      if(qc.var$hardUnit == "percent") {
        if(all(na.omit(hmg.df[,i]) >= 0 && na.omit(hmg.df[,i]) <= 1)) 
          {print(paste0("Column ",colnames(hmg.df)[i]," contains all numeric percent values between 0-1. Check if percent values should be x100"))}
          #Fail QC??? This may need to be an option in the function parameters, values all below 1 could be correct in some rare cases
      }
    }
    
    ### DATE QC CHECKS
    # ???
    
    #Print column QC check finished
    #print(paste0("QC complete for column ",colnames(hmg.df)[i]))
    
    #Print message for completion of QC
    if(i == ncol(hmg.df)) {print("QC complete")}
  }
}  
  

#Example use:

  ### NOTE: Perhaps these file inputs could be added to the function...?

#Load QC constraints table from Google Drive
QC.get <- gs_read(gs_title("HMG_QC_constraints"))

#Trim QC by QC.ready column = "YES"
  ## This is a simplifying step as we build up the QC table
QC.trim <- QC.get %>% filter(QC.ready == "YES")

#Load homogenized dataframe by name
homog.file <- gs_read(gs_title("Calhoun CZO data_HMGZD"))

#Run QC function  
homog.QC(hmg.df = homog.file, QC.tbl = QC.trim)  
  
  
  
  
  
    
  