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
homog.QC <- function(hmg.df) {

 ### Ensure conversion of googlesheet tibble to dataframe
  hmg.df <- as.data.frame(hmg.df)
  
  #Load QC constraint tables from Google Drive
  QC.loc <- gs_read(gs_title("Key_Key_V2_lookup"),ws="Location_data")
  QC.prof <- gs_read(gs_title("Key_Key_V2_lookup"),ws="Profile_data (Key-Key)")
  
  #Combine var QC values into one table, only bringing in the columns needed for this QC
  QC.tbl <- as.data.frame(bind_rows(
    select(QC.loc, var, minValue, maxValue, class, Level, givenUnit), 
    select(QC.prof,var, minValue, maxValue, class, Level, givenUnit)))
  
  #Verify all dataframe column names exist in QC constraint table  
  if(!all(as.character(colnames(hmg.df)) %in% QC.tbl$var)) 
    {print("Warning: QC constraints do not exist for all homogenized dataframe columns")}
      
  #Cycle through hmg.df columns
  for(i in 1:ncol(hmg.df)) {
    
    #Check if hmg.df column exists in QC.tbl, if not then go to next column
    if(!colnames(hmg.df)[i] %in% QC.tbl$var) {
     #If last column, print message QC complete
      if(i == ncol(hmg.df)) {print("QC complete")}
      next}  #Jump to next loop iteration
    
    #Isolate QC restraints for specified column
    qc.var <- NULL
    qc.var <- filter(QC.tbl,var == colnames(hmg.df)[i])  #qc.var should be exactly one row
    qc.var <- qc.var[1,] #Take only the first column if two rows are created from value existing in location and profile sheets, QC should be identical
    
    #Check if class value exists in QC.tbl
    if(is.na(qc.var$class)) {
      print(paste0(qc.var[1,1], " has no class label in QC sheet"))
    } else {
      ### CHARACTER QC CHECKS
      #If data class is "character", then...
      if(qc.var$class == "character") {
        
        #Verify all column values are standard chatacters
        if(!all(is.character(na.omit(hmg.df[,i])))) {
          print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," is not character class"))
          #break
        }
        
        #Check if max character length is set
        if(!is.na(qc.var$maxValue) && qc.var$maxValue > 0) {
        
          #Verify character string does not exceed max length set by QC.tbl
          if(all(nchar(na.omit(hmg.df[,i])) > qc.var$maxValue)) {
            print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," character length exceeds max value of ",qc.var$maxValue))
            #break
        }
        }
      }
    
      
      ### NUMERIC QC CHECKS
      #If data class is "numeric", then...
      if(qc.var$class == "numeric") {
        
        #Verify all column values are numeric
        if(!all(is.numeric(na.omit(hmg.df[,i])))) 
          {print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," is not numeric class"))
          #break
        }
        
        #Verify max value is set
        if(is.na(qc.var$maxValue)) {
          print(paste0(qc.var[1,1], " has no max value set in QC sheet"))
        } else {
        
          #Verify numeric values do not exceed max value set by QC.tbl
          if(all(na.omit(hmg.df[,i]) > qc.var$maxValue)) 
            {print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," has numeric value exceeding QC max value of ",qc.var$maxValue))
            #break
          }
        }
      
        #Verify max value is set
        if(is.na(qc.var$minValue)) {
          print(paste0(qc.var[1,1], " has no min value set in QC sheet"))
        } else {
        
          #Verify numeric values do not exist below min value set by QC.tbl
          if(all(na.omit(hmg.df[,i]) < qc.var$minValue)) 
            {print(paste0("QC FAIL: Column ",colnames(hmg.df)[i]," has numeric value below QC min value of ",qc.var$minValue))
            #break
          }
        }
        
        #Verify max value is set
        if(is.na(qc.var$givenUnit)) {
          print(paste0(qc.var[1,1], " has no unit type set in QC sheet"))
        } else {
        
          #If unit type is "percent", check to see if all values occupy range from 0-1
          if(qc.var$givenUnit == "percent") {
            if(all(na.omit(hmg.df[,i]) >= 0 && na.omit(hmg.df[,i]) <= 1)) 
              {print(paste0("Column ",colnames(hmg.df)[i]," contains all numeric percent values between 0-1. Check if percent values should be x100"))}
              #Fail QC??? This may need to be an option in the function parameters, values all below 1 could be correct in some rare cases
          }
        }
      }
    
    ### DATE QC CHECKS
    # ???
    
    }  #Closes bracket on if "class" exists  
      
      
    #Print column QC check finished
    #print(paste0("QC complete for column ",colnames(hmg.df)[i]))
    
    #Print message for completion of QC
    if(i == ncol(hmg.df)) {print("QC complete")}
  }
}  
  

#Examples of QC function use:
########################

### OPTION 1
  #Load homogenized dataframe by name
  homog.file <- gs_read(gs_title("Calhoun CZO data_HMGZD"))
  
  #Run QC function  
  homog.QC(hmg.df = homog.file)


### OPTION 2
  # Same as above, but simplified QC call
  #homog.QC(gs_read(gs_title("Calhoun CZO data_HMGZD")))
  
  
  
  
    
  