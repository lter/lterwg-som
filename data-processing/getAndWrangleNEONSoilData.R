### CODE FOR DOWNLOADING SOIL DATA FROM NEON DATA PORTAL ###
### Demo of two options, plus data wrangling
### Feb 28th, 2018
### S. Weintraub

# Add working directory for additional user as needed
if (file.exists('/Users/sweintraub/')){
  dir <- ("/Users/sweintraub/Documents/Conferences_Meetings/2018_LTER-SOM/data-files")
  setwd(dir)
}

### OPTION 1: NEON Data Stacker (neonDataStackR)
# library(devtools) # load devtools if needed
# install_github("NEONScience/NEON-utilities/neonDataStackR", dependencies=TRUE) # install package if needed
library (neonDataStackR) # load package
# download all data for a given product, then unzip and stack by table
# Soil physical properties (Megapit), DP1.00096.001
{
  zipsByProduct(dpID="DP1.00096.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack00096"), folder=T)
}


### OPTION 2, NEON-API direct interaction
# Install packages (if needed)
#install.packages("httr")
#install.packages("jsonlite")
#devtools::install_github("NEONScience/NEON-utilities/neonDataStackR") # for stacking monthly files
#devtools::install_github("NEONScience/NEON-geolocation/geoNEON") # for calculating geolocations, if desired

# Load packages
library(httr)
library(jsonlite)
library(neonDataStackR)
#library(geoNEON)
# Get list, Soil physical properties (Megapit)
get.MP <- GET("http://data.neonscience.org/api/v0/products/DP1.00096.001") 
# List of site-months where this DP is available, flattened & in text format 
MP.avail <- fromJSON(content(get.MP, as = "text"), simplifyDataFrame=T, flatten=T) 
# Get urls for available data
MP.urls <- unlist(MP.avail$data$siteCodes$availableDataUrls) 
# Get list of files for UKFS
MP <- GET(MP.urls[grep("UKFS", MP.urls)]) 
# Flatten & put in text format 
MP.files <- fromJSON(content(MP, as="text")) 
# View just the available data file names
MP.files$data$files$name 
# Get the actual files - by table type and basic vs expanded
# permegapit
MP.permegapit <- read.delim(MP.files$data$files$url
                            [intersect(grep("permegapit", MP.files$data$files$name),
                                       grep("basic", MP.files$data$files$name))], sep=",")
# perhoriozn
MP.perhorizon <- read.delim(MP.files$data$files$url
                            [intersect(grep("perhorizon", MP.files$data$files$name),
                                       grep("basic", MP.files$data$files$name))], sep=",")
# perbulksample
MP.perbulksample <- read.delim(MP.files$data$files$url
                            [intersect(grep("perbulksample", MP.files$data$files$name),
                                       grep("basic", MP.files$data$files$name))], sep=",")
# perbiogeosample
MP.perbiogeosample <- read.delim(MP.files$data$files$url
                            [intersect(grep("perbiogeosample", MP.files$data$files$name),
                                       grep("basic", MP.files$data$files$name))], sep=",")
# perarchivesmaple
MP.perarchivesample <- read.delim(MP.files$data$files$url
                            [intersect(grep("perarchivesample", MP.files$data$files$name),
                                      grep("basic", MP.files$data$files$name))], sep=",")

#############################################################################
### USING OPTION 1 (neonDataStackR) plus handy function from NNEO package 
### to download all soil observational data
#############################################################################
# Install nneo if needed
#install.packages("nneo") # cran version
# devtools::install_github("ropenscilabs/nneo") # development version
library(nneo)
# make a table of all NEON Data Products (DPs)
products <- nneo_products()
# subset for soil DPs only
soilProds <- subset(products, grepl("soil", products$productName, ignore.case = TRUE), select=c("productCode", "productName")) 
soilProds 
# Pick out the DPs you want, then down below
# Soil chemical properties (Megapit), DP1.00097.001
{
  zipsByProduct(dpID="DP1.00097.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack00097"), folder=T)
}

# Soil chemical properties (Distributed periodic), DP1.10078.001
{
  zipsByProduct(dpID="DP1.10078.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10078"), folder=T)
}  
# Soil physical properties (Distributed periodic), DP1.10086.001
{
  zipsByProduct(dpID="DP1.10086.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10086"), folder=T)
}  
# Soil chemical properties (Distributed initial characterization), DP1.10008.001
{
  zipsByProduct(dpID="DP1.10008.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10008"), folder=T)
}  
# Soil physical properties (Distributed initial characterization), DP1.10047.001
{
  zipsByProduct(dpID="DP1.10047.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10047"), folder=T)
}  
# Soil stable isotopes (Distributed periodic), DP1.10100.001
{
  zipsByProduct(dpID="DP1.10100.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10100"), folder=T)
}  
# Soil inorganic nitrogen pools and transformations, DP1.10080.001
{
  zipsByProduct(dpID="DP1.10080.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10080"), folder=T)
}  

#############################################################################
### Get physiographic data about the plots (slope, aspect) 
### Use geoNEON package (Git)
#############################################################################
library(devtools) # load devTools if needed
install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE) # install geoNeon
library(geoNEON) # load geoNEON
library(tidyverse)
# read in one of the megapit files
mgp_permegapit <- read.csv(paste
                          (dir, "filesToStack00096/stackedFiles/mgp_permegapit.csv", 
                          sep = "/"), header = T)
# one of the initial characterization files
spc_perplot <- read.csv(paste
                        (dir, "filesToStack10047/stackedFiles/spc_perplot.csv", 
                        sep = "/"), header = T)
# one of the distributed plot files
sls_soilCoreCollection <- read.csv(paste
                        (dir, "filesToStack10086/stackedFiles/sls_soilCoreCollection.csv", 
                        sep = "/"), header = T)
# use def.extr.geo.os function in geoNEON to get plot-level metadata
mgp <- def.extr.geo.os(mgp_permegapit, 'pitNamedLocation')
spc <- def.extr.geo.os(spc_perplot, 'namedLocation')
# for distributed plots (sls), make a smaller df with just locations first
slsLocations <- as.data.frame(levels(sls_soilCoreCollection$namedLocation))
colnames(slsLocations) <- "namedLocation"
sls <- def.extr.geo.os(slsLocations, 'namedLocation')
# export csvs
write.csv(mgp, paste(dir, "spatial_mgp.csv", sep = "/"), row.names = F)
write.csv(spc, paste(dir, "spatial_spc.csv", sep = "/"), row.names = F)
write.csv(sls, paste(dir, "spatial_sls.csv", sep = "/"), row.names = F)

#############################################################################
### Download other input data for Soil C modeling - litterfall and roots
#############################################################################
# Litterfall and fine woody debris sampling, DP1.10033
{
  zipsByProduct(dpID="DP1.10033.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10033"), folder=T)
}
# Root sampling tower plots, DP1.10067
{
  zipsByProduct(dpID="DP1.10067.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10067"), folder=T)
}
# Root sampling (Megapit), DP1.10066
{
  zipsByProduct(dpID="DP1.10066.001", site="all", package="basic", check.size=T)
  stackByTable(paste0(getwd(), "/filesToStack10066"), folder=T)
}

#############################################################################
### Unpack zip files for climate data, downloaded from the portal 
#############################################################################
stackByTable(paste0(getwd(), "/NEON_temp-air-single"), folder=T)
stackByTable(paste0(getwd(), "/NEON_precipitation"), folder=T)

#############################################################################
### Make 'master' chemistry files with predictor variables attached
#############################################################################
library(tidyverse)
### Load Climate - will need to change file paths
MAP <- read.csv('/Users/sweintraub/Documents/Conferences_Meetings/2018_LTER-SOM/climate_precip.csv', header = T, stringsAsFactors = F)
MAT <- read.csv('/Users/sweintraub/Documents/Conferences_Meetings/2018_LTER-SOM/climate_temp.csv', header = T, stringsAsFactors = F)
### Megapit - start with Chemistry (mgc), then add on physical (mgp) + other
mgc_perbiogeo <- read.csv(paste
                         (dir, "filesToStack00097/stackedFiles/mgc_perbiogeosample.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
mgp_perbiogeo <- read.csv(paste
                         (dir, "filesToStack00096/stackedFiles/mgp_perbiogeosample.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
mgp_perbulksample <- read.csv(paste
                         (dir, "filesToStack00096/stackedFiles/mgp_perbulksample.csv", 
                         sep = "/"), header = T, stringsAsFactors = F)
mgc_permegapit <- read.csv(paste
                          (dir, "filesToStack00096/stackedFiles/mgp_permegapit.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
names(mgc_perbiogeo)
names(mgp_perbiogeo)
# Join chem to texture, by includes all common columns
megpit_biogeo_1 <- left_join(x = mgc_perbiogeo, y = mgp_perbiogeo, 
      by = c("domainID", "siteID","pitNamedLocation","pitID", "horizonID", "biogeoID", "horizonName","biogeoHorizonProportion", 
             "biogeoSampleType", "setDate", "collectDate", "laboratoryName", 
             "labProjID", "biogeoTopDepth", "biogeoBottomDepth", "biogeoCenterDepth"))
# Join both to bulk density, by includes all common columns
names(megpit_biogeo_1)
names(mgp_perbulksample)
megpit_biogeo_2 <- left_join(x = megpit_biogeo_1, y = mgp_perbulksample, 
      by = c("domainID", "siteID","pitNamedLocation","pitID", "horizonID", "horizonName",
      "setDate", "collectDate", "laboratoryName", "labProjID"))
# Join both to pit-level metadata
names(megpit_biogeo_2)
names(mgc_permegapit)
megpit_biogeo_3 <- left_join(x = megpit_biogeo_2, y = mgc_permegapit, 
      by = c("domainID", "siteID","pitNamedLocation", "pitID", "setDate", "collectDate"))
# Add climate - from PRISM, mean of 30-yr normals for entire site
megpit_biogeo_3$MAT_C <- MAT$Temp_C[match(megpit_biogeo_3$siteID,MAT$siteID)]
megpit_biogeo_3$MAP_mm <- MAP$Precip_mm[match(megpit_biogeo_3$siteID,MAP$siteID)]
# Write the file, customize path
write.csv(megpit_biogeo_3, '/Users/sweintraub/Documents/Conferences_Meetings/2018_LTER-SOM/megapit_all.csv', row.names = F)

### Initital Characterization - start with chemistry (perbiogeo), then add on physical (bd, texture) + other
spc_perbiogeo <- read.csv(paste
                          (dir, "filesToStack10008/stackedFiles/spc_biogeochem.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
spc_bulkdense <- read.csv(paste
                          (dir, "filesToStack10047/stackedFiles/spc_bulkdensity.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
spc_particlesize <- read.csv(paste
                          (dir, "filesToStack10047/stackedFiles/spc_particlesize.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
spc_perplot <- read.csv(paste
                          (dir, "filesToStack10047/stackedFiles/spc_perplot.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
spc_spatial <- read.csv(paste(dir, "spatial_spc.csv", sep = "/"), 
                        header = T, stringsAsFactors = F) # spatial data
names(spc_perbiogeo)
names(spc_bulkdense)
# Join chem to bulk density, by includes all common columns
spc_biogeo_1 <- left_join(x = spc_perbiogeo, y = spc_bulkdense, 
        by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID",
        "collectDate", "horizonID", "horizonName", "laboratoryName", "dataQF"))
# Join both to particle size, by includes all common columns 
names(spc_biogeo_1)
names(spc_particlesize)
spc_biogeo_2 <- left_join(x = spc_biogeo_1, y = spc_particlesize, 
        by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID",
        "collectDate", "horizonID", "horizonName",  "biogeoIDnrcs", "biogeoSampleType",
        "biogeoTopDepth", "biogeoBottomDepth", "biogeoCenterDepth", "laboratoryName", "dataQF"))
# Join to plot-level metadata, by includes all common columns 
names(spc_biogeo_2)
names(spc_perplot)
spc_biogeo_3 <- left_join(x = spc_biogeo_2, y = spc_perplot, 
                by = c("namedLocation", "domainID", "siteID", "plotID", "dataQF"))
# Add spatial (slope/aspect) - from NEON api
spc_biogeo_3$plotAspect <- spc_spatial$api.slopeAspect[match(spc_biogeo_3$namedLocation, spc_spatial$namedLocation)]
spc_biogeo_3$plotSlope <- spc_spatial$api.slopeGradient[match(spc_biogeo_3$namedLocation, spc_spatial$namedLocation)]
# Add climate (MAT/MAP) - from PRISM, mean of 30-yr normals for entire site
spc_biogeo_3$MAT_C <- MAT$Temp_C[match(spc_biogeo_3$siteID,MAT$siteID)]
spc_biogeo_3$MAP_mm <- MAP$Precip_mm[match(spc_biogeo_3$siteID,MAP$siteID)]
# Write the file, customize path
write.csv(spc_biogeo_3, '/Users/sweintraub/Documents/Conferences_Meetings/2018_LTER-SOM/intitalChar_all.csv', row.names = F)

### Periodic - start with % C and N (soilChem), then add on isotopes, moisture, pH, other
sls_soilChem <- read.csv(paste
                        (dir, "filesToStack10078/stackedFiles/sls_soilChemistry.csv", 
                        sep = "/"), header = T, stringsAsFactors = F)
sls_soilStableIso <- read.csv(paste
                        (dir, "filesToStack10100/stackedFiles/sls_soilStableIsotopes.csv", 
                        sep = "/"), header = T, stringsAsFactors = F)
sls_CoreCollect <- read.csv(paste
                        (dir, "filesToStack10086/stackedFiles/sls_soilCoreCollection.csv", 
                        sep = "/"), header = T, stringsAsFactors = F)
sls_moisture <- read.csv(paste
                        (dir, "filesToStack10086/stackedFiles/sls_soilMoisture.csv", 
                        sep = "/"), header = T, stringsAsFactors = F)
sls_pH <- read.csv(paste
                        (dir, "filesToStack10086/stackedFiles/sls_soilpH.csv", 
                        sep = "/"), header = T, stringsAsFactors = F)
sls_spatial <- read.csv(paste(dir, "spatial_sls.csv", sep = "/"), 
                        header = T, stringsAsFactors = F)
# Each dataframe has few actual data variables, with lots of metadata
# Will use mostly 'match' instead of 'join' to pick and choose vars
# Need primary key for soil CN data, accounting for acidTreatment & repNumber
sls_soilChem$primaryKey <- paste(sls_soilChem$sampleID, sls_soilChem$analyticalRepNumber, sls_soilChem$acidTreatment, sep = "_")
sls_soilStableIso$primaryKey <- paste(sls_soilStableIso$sampleID, sls_soilStableIso$analyticalRepNumber, sls_soilStableIso$acidTreatment, sep = "_")
# Add isotope data to %C and N table
sls_soilChem$d15N <- sls_soilStableIso$d15N[match(sls_soilChem$primaryKey, sls_soilStableIso$primaryKey)]
sls_soilChem$d13C <- sls_soilStableIso$d13C[match(sls_soilChem$primaryKey, sls_soilStableIso$primaryKey)]
# Condense this table so each sampleID only has 1 row, means for reps
sls_soilChem <- sls_soilChem %>%
  group_by(sampleID) %>% 
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
# New indicator variable for whether C-data came from acid treatment
sls_soilChem$acidTreatedforC <- ifelse(is.na(sls_soilChem$CNratio), "Y", "N")
# Add on soil moisture vars
sls_soilChem$soilMoisture <- sls_moisture$soilMoisture[match(sls_soilChem$sampleID, sls_moisture$sampleID)]
sls_soilChem$dryMassFraction <- sls_moisture$dryMassFraction[match(sls_soilChem$sampleID,  sls_moisture$sampleID)]
# Add on pH vars
sls_soilChem$soilInWaterpH <- sls_pH$soilInWaterpH[match(sls_soilChem$sampleID, sls_pH$sampleID)]
sls_soilChem$soilInCaClpH <- sls_pH$soilInCaClpH[match(sls_soilChem$sampleID, sls_pH$sampleID)]
sls_soilChem$waterpHRatio <- sls_pH$waterpHRatio[match(sls_soilChem$sampleID, sls_pH$sampleID)]
sls_soilChem$caclpHRatio <- sls_pH$caclpHRatio[match(sls_soilChem$sampleID, sls_pH$sampleID)]
# Add on field collection metadata
names(sls_soilChem)
names(sls_CoreCollect)
sls_all <- left_join(x = sls_soilChem, y = sls_CoreCollect, 
          by = c("domainID", "siteID", "plotID", "plotType","sampleID", "namedLocation", "collectDate"))
# Add spatial (slope/aspect) and soil order - from NEON api
sls_all$plotAspect <- sls_spatial$api.slopeAspect[match(sls_all$namedLocation, sls_spatial$namedLocation)]
sls_all$plotSlope <- sls_spatial$api.slopeGradient[match(sls_all$namedLocation, sls_spatial$namedLocation)]
sls_all$soilOrder <- sls_spatial$api.soilTypeOrder[match(sls_all$namedLocation, sls_spatial$namedLocation)]
# Add climate (MAT/MAP) - from PRISM, mean of 30-yr normals for entire site
sls_all$MAT_C <- MAT$Temp_C[match(sls_all$siteID,MAT$siteID)]
sls_all$MAP_mm <- MAP$Precip_mm[match(sls_all$siteID,MAP$siteID)]
# Remove non-relevant variables
sls_all_clean <- subset(sls_all, select = -c(acidTreatment, analyticalRepNumber, 
primaryKey, utmZone, northing, easting))
# Write the file, customize path
write.csv(sls_all_clean, '/Users/sweintraub/Documents/Conferences_Meetings/2018_LTER-SOM/periodic_all.csv', row.names = F)



