#########
# EXAMPLE: HOW TO RUN SoDaH analyte avg ftn 
#########

# Load SoDaH analyte avg ftn (brings in the three required ftns)
source("get_analyte_avgs_ftn.R")

## Load SoDaH database

# Get SoDaH database from EDI script
#source("edi.521.1.r") #incoming database var = dt1 as dataframe

# Get SoDaH .RDS from local path
dt1 <- readRDS("C:/github/lterwg-som-shiny/shiny_SOM/somCompositeData_2020-06-22.rds")

## Filter EDI SoDaH database "dt1" for control data only
som <- dt1 %>% filter(control_sample == TRUE)

## Get RC SoDaH from local path
#RC <- readRDS("C:/GitHub/RC-som-shiny/shiny_SOM/RC_database_current.rds")


#Example: Remove NEON from SoDaH database object
SOM_wo_NEON <- som %>% filter(network != "NEON")


# Run ftn to fet analyte avgs for all locations and depths, 
# as well as avgs to the specified depth
get_analyte_avgs(target_database = SOM_wo_NEON,
                 target_analyte_to_avg = "lyr_n_tot", 
                 target_depth_cm = 30)




