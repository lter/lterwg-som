##########################################################################
### CODE FOR DOWNLOADING AND WRANGLING SOIL DATA FROM NEON DATA PORTAL
### Created Feb 28th, 2018
### Updated June 15, 2018
### Edited for programmatic upload to Google Drive, Jan 7 2019
### More edits to interact with Google Drive + add other vars, July 2019
### S. Weintraub
##########################################################################

### Reset workspace
rm(list = ls())

### Install packages
library(tidyverse) # includes googledrive package
library(devtools) # for interacting with Git packages
# Install git version of neonUtilities - for downloading products with the API
devtools::install_github("NEONScience/NEON-utilities/neonUtilities", 
                         dependencies=T, force = T)
library(neonUtilities)
# Install git version of geoNEON - for getting spatial data about sampling sites
devtools::install_github('NEONScience/NEON-geolocation/geoNEON', 
                         dependencies=TRUE, force = T)
library(geoNEON)
# Install neonNTrans - to work up net rates from raw data
devtools::install_github("NEONScience/NEON-Nitrogen-Transformations/neonNTrans", 
                         dependencies=TRUE, force = T)
library(neonNTrans) 
### Add working directory for additional user as needed
# If you've run this code before, make a new folder with the date to put
# new data-files into and change path below
# Code below won't run if old files present in the working dir
if (file.exists('/Users/sweintraub/')){
  dir <- ("/Users/sweintraub/Box/Conferences_Meetings/2018_LTER-SOM/data/2019_08") 
  setwd(dir)
}

### READ ME!
### Code to add Soil-Relevant NEON Data to LTER-SOM google drive
## Not all potentially relevant DPs are included, code can be expanded as desired
## Info about NEON DPs can be found at http://data.neonscience.org/data-product-catalog
## The Code chunks below do the following: 
# First, get rid of older versions of files on google drive (googledrive::drive_trash and drive_mkdir)
# Next 2 lines (zipsBy and stackBy) download all data for a DP, then unzip, stack by table, keep 'stacked' outputs (neonUtilities)
# Last 2 lines (file.list and map) push to google drive in NEON folders (googledrive in tidyverse)
# Run code chunks below (each takes a few mins), then review on google drive to make sure files appear

## MEGAPIT, SOIL - 9 tables
{
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_megapitSoil/data-files/")
googledrive::drive_mkdir("~/LTER-SOM/Data_downloads/NEON_megapitSoil/data-files/")
# Soil physical properties (Megapit), DP1.00096.001
  zipsByProduct(dpID="DP1.00096.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack00096"), folder=T, saveUnzippedFiles = F)
  file.list1 <- list.files(path = paste0(dir, "/filesToStack00096/stackedFiles"), pattern="mgp", full.names = TRUE)
  map(file.list1, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_megapitSoil/data-files/", verbose = FALSE)
# Soil chemical properties (Megapit), DP1.00097.001
  zipsByProduct(dpID="DP1.00097.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack00097"), folder=T, saveUnzippedFiles = F)
  file.list2 <- list.files(path = paste0(dir, "/filesToStack00097/stackedFiles"), pattern="mgc", full.names = TRUE)
  map(file.list2, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_megapitSoil/data-files/", verbose = FALSE)
}
## MEGAPIT, ROOT BIOMASS - 3 tables
{
  googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_megapitRoots/data-files/")
  googledrive::drive_mkdir("~/LTER-SOM/Data_downloads/NEON_megapitRoots/data-files/")
# DP1.10066
  zipsByProduct(dpID="DP1.10066.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10066"), folder=T, saveUnzippedFiles = F)
  file.list3 <- list.files(path = paste0(dir, "/filesToStack10066/stackedFiles"), pattern="mpr", full.names = TRUE)
  map(file.list3, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_megapitRoots/data-files/", verbose = FALSE)
}
## INITIAL CHAR, SOIL - 5 tables
{
  googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/data-files/")
  googledrive::drive_mkdir("~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/data-files/")
# Soil chemical properties (Distributed initial characterization), DP1.10008.001
  zipsByProduct(dpID="DP1.10008.001", site="all", package="expanded", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10008"), folder=T, saveUnzippedFiles = F)
  file.list4 <- list.files(path = paste0(dir, "/filesToStack10008/stackedFiles"), pattern="spc", full.names = TRUE)
  map(file.list4, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/data-files/", verbose = FALSE)
# Soil physical properties (Distributed initial characterization), DP1.10047.001
  zipsByProduct(dpID="DP1.10047.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10047"), folder=T, saveUnzippedFiles = F)
  file.list5 <- list.files(path = paste0(dir, "/filesToStack10047/stackedFiles"), pattern="spc", full.names = TRUE)
  map(file.list5, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/data-files/", verbose = FALSE)
}
## PERIODIC, SOIL - 10 tables; this one is large, give it time
{
  googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/")
  googledrive::drive_mkdir("~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/")
# Soil chemical properties (Distributed periodic), DP1.10078.001
  zipsByProduct(dpID="DP1.10078.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10078"), folder=T, saveUnzippedFiles = F)
  file.list6 <- list.files(path = paste0(dir, "/filesToStack10078/stackedFiles"), pattern="sls", full.names = TRUE)
  map(file.list6, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/", verbose = FALSE)
# Soil physical properties (Distributed periodic), DP1.10086.001 - large, takes a while
  zipsByProduct(dpID="DP1.10086.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10086"), folder=T, saveUnzippedFiles = F)
  file.list7 <- list.files(path = paste0(dir, "/filesToStack10086/stackedFiles"), pattern="sls", full.names = TRUE)
  map(file.list7, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/", verbose = FALSE)
# Soil stable isotopes (Distributed periodic), DP1.10100.001
  zipsByProduct(dpID="DP1.10100.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10100"), folder=T, saveUnzippedFiles = F)
  file.list8 <- list.files(path = paste0(dir, "/filesToStack10100/stackedFiles"), pattern="sls", full.names = TRUE)
  map(file.list8, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/", verbose = FALSE)
  # Soil N transformations, DP1.10080.001
  zipsByProduct(dpID="DP1.10080.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10080"), folder=T, saveUnzippedFiles = F)
  file.list9 <- list.files(path = paste0(dir, "/filesToStack10080/stackedFiles"), pattern="ntr", full.names = TRUE)
  map(file.list9, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/", verbose = FALSE)
}
## PERIODIC, ROOT BIOMASS + ALL ROOT CHEM - 6 tables
{
  googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_periodicRoots/data-files/")
  googledrive::drive_mkdir("~/LTER-SOM/Data_downloads/NEON_periodicRoots/data-files/")
# Root sampling tower plots = DP1.10067
  zipsByProduct(dpID="DP1.10067.001", site="all", package="basic", check.size=F) +
  stackByTable(paste0(dir, "/filesToStack10067"), folder=T, saveUnzippedFiles = F)
  file.list10 <- list.files(path = paste0(dir, "/filesToStack10067/stackedFiles"), pattern="bbc", full.names = TRUE)
  map(file.list10, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicRoots/data-files/", verbose = FALSE)
# Root chemical properties = DP1.10102
  zipsByProduct(dpID="DP1.10102.001", site="all", package="basic", check.size=F) +
  stackByTable(paste0(dir, "/filesToStack10102"), folder=T, saveUnzippedFiles = F)
  file.list11 <- list.files(path = paste0(dir, "/filesToStack10102/stackedFiles"), pattern="bbc", full.names = TRUE)
  map(file.list11, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicRoots/data-files/", verbose = FALSE)
# Root stable isotopes = DP1.10099
  zipsByProduct(dpID="DP1.10099.001", site="all", package="basic", check.size=F) +
  stackByTable(paste0(dir, "/filesToStack10099"), folder=T, saveUnzippedFiles = F)
  file.list12 <- list.files(path = paste0(dir, "/filesToStack10099/stackedFiles"), pattern="bbc", full.names = TRUE)
  map(file.list12, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_periodicRoots/data-files/", verbose = FALSE)
}
### LITERFALL - 7 tables; this one is large (flux data), give it time
{
  googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_litterfall/data-files/")
  googledrive::drive_mkdir("~/LTER-SOM/Data_downloads/NEON_litterfall/data-files/")
  # Litterfall fluxes = DP1.10033
  zipsByProduct(dpID="DP1.10033.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10033"), folder=T, saveUnzippedFiles = F)
  file.list13 <- list.files(path = paste0(dir, "/filesToStack10033/stackedFiles"), pattern="ltr", full.names = TRUE)
  map(file.list13, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_litterfall/data-files/", verbose = FALSE)
  # Litter chemical properties = DP1.10031
  zipsByProduct(dpID="DP1.10031.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10031"), folder=T, saveUnzippedFiles = F)
  file.list14 <- list.files(path = paste0(dir, "/filesToStack10031/stackedFiles"), pattern="ltr", full.names = TRUE)
  map(file.list14, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_litterfall/data-files/", verbose = FALSE)
  # Litter stable isotopes = DP1.10101
  zipsByProduct(dpID="DP1.10101.001", site="all", package="basic", check.size=F) +
    stackByTable(paste0(dir, "/filesToStack10101"), folder=T, saveUnzippedFiles = F)
  file.list15 <- list.files(path = paste0(dir, "/filesToStack10101/stackedFiles"), pattern="ltr", full.names = TRUE)
  map(file.list15, googledrive::drive_upload, path = "~/LTER-SOM/Data_downloads/NEON_litterfall/data-files/", verbose = FALSE)
}

### Get physiographic data about the plots (slope, aspect, etc) using geoNEON package
# Code can take ~ 20 minutes
{
# read in one of the soil megapit files
mgp_permegapit <- read.csv(paste
                          (dir, "filesToStack00096/stackedFiles/mgp_permegapit.csv", 
                          sep = "/"), header = T)
# one of the soil initial characterization files
spc_perplot <- read.csv(paste
                        (dir, "filesToStack10047/stackedFiles/spc_perplot.csv", 
                        sep = "/"), header = T)
# one of the soil periodic plot files
sls_soilCoreCollection <- read.csv(paste
                        (dir, "filesToStack10086/stackedFiles/sls_soilCoreCollection.csv", 
                        sep = "/"), header = T)
# one of the root periodic plot files
bbc_percore <- read.csv(paste
                        (dir, "filesToStack10067/stackedFiles/bbc_percore.csv", 
                        sep = "/"), header = T)
# one of the root litterfall files
ltr_fielddata <- read.csv(paste
                        (dir, "filesToStack10033/stackedFiles/ltr_fielddata.csv", 
                        sep = "/"), header = T)
# use def.extr.geo.os function in geoNEON to get plot-level metadata
mgp <- def.extr.geo.os(mgp_permegapit, 'pitNamedLocation')
spc <- def.extr.geo.os(spc_perplot, 'namedLocation')
# for distributed plots (sls), make a smaller df with just locations first
slsLocations <- as.data.frame(unique(sls_soilCoreCollection$namedLocation))
colnames(slsLocations) <- "namedLocation"
sls <- def.extr.geo.os(slsLocations, 'namedLocation')
# same for distributed plot roots (bbc)
bbcLocations <- as.data.frame(unique(bbc_percore$namedLocation))
colnames(bbcLocations) <- "namedLocation"
bbc <- def.extr.geo.os(bbcLocations, 'namedLocation')
# same for distributed litterfall (ltr)
ltrLocations <- as.data.frame(unique(ltr_fielddata$namedLocation))
colnames(ltrLocations) <- "namedLocation"
ltr <- def.extr.geo.os(ltrLocations, 'namedLocation')
# save files locally
write.csv(mgp, paste(dir, "spatial_mgp.csv", sep = "/"), row.names = F)
write.csv(spc, paste(dir, "spatial_spc.csv", sep = "/"), row.names = F)
write.csv(sls, paste(dir, "spatial_sls.csv", sep = "/"), row.names = F)
write.csv(bbc, paste(dir, "spatial_bbc.csv", sep = "/"), row.names = F)
write.csv(ltr, paste(dir, "spatial_ltr.csv", sep = "/"), row.names = F)
# Upload these to Google Drive
googledrive::drive_upload(paste(dir, "spatial_mgp.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_megapitSoil/data-files/", verbose = FALSE)
googledrive::drive_upload(paste(dir, "spatial_spc.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/data-files/", verbose = FALSE)
googledrive::drive_upload(paste(dir, "spatial_sls.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_periodicSoil/data-files/", verbose = FALSE)
googledrive::drive_upload(paste(dir, "spatial_bbc.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_periodicRoots/data-files/", verbose = FALSE)
googledrive::drive_upload(paste(dir, "spatial_ltr.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_litterfall/data-files/", verbose = FALSE)
}

### Load Climate Data, long-term averages - already stored on Google Drive, NEON_site-climate
{
clim <- "1V34g10SSWND6mCFwOS1g9Q48LZR26ooi" # google fileID for climate data - PRISM for CONUS, met stations for AK and HI
CLIM <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", clim))
landCov <- "1bw976S4lEKJYKEX7LOFRL-IwPUlmdUd5"
COV <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", landCov))
}

### Make 'master' files with phys-chem combined plus possible predictor variables
### MEGAPIT - start with Chemistry (mgc), then add on physical (mgp) + other
{
## Chem
mgc_perbiogeo <- read.csv(paste
                          (dir, "filesToStack00097/stackedFiles/mgc_perbiogeosample.csv", 
                            sep = "/"), header = T, stringsAsFactors = F)
# Clean up to remove empty rows and audit samples
mgc_perbiogeoClean <- mgc_perbiogeo %>%
  filter(!biogeoID == "", !biogeoSampleType == "Audit")
## Texture
mgp_perbiogeo <- read.csv(paste
                          (dir, "filesToStack00096/stackedFiles/mgp_perbiogeosample.csv", 
                            sep = "/"), header = T, stringsAsFactors = F)
# Clean up to remove empty rows and audit samples
mgp_perbiogeoClean <- mgp_perbiogeo %>%
  filter(!biogeoID == "", !biogeoSampleType == "Audit")
## Bulk Dens
mgp_perbulksample <- read.csv(paste
                              (dir, "filesToStack00096/stackedFiles/mgp_perbulksample.csv", 
                                sep = "/"), header = T, stringsAsFactors = F)
# Clean up to remove empty rows and audit samples
mgp_perbulksampleClean <- mgp_perbulksample %>%
  filter(!bulkDensID == "", !bulkDensSampleType == "Audit")
## Pit-level data
mgc_permegapit <- read.csv(paste
                           (dir, "filesToStack00096/stackedFiles/mgp_permegapit.csv", 
                             sep = "/"), header = T, stringsAsFactors = F)
intersect(colnames(mgc_perbiogeoClean), colnames(mgp_perbiogeoClean))
# Join chem to texture, "by" includes all common columns
megapit_biogeo <- left_join(x = mgc_perbiogeoClean, y = mgp_perbiogeoClean, 
      by = c("domainID", "siteID","pitNamedLocation","pitID", "horizonID", "biogeoID", "horizonName","biogeoHorizonProportion", 
             "biogeoSampleType", "setDate", "collectDate", "laboratoryName", 
             "labProjID", "biogeoTopDepth", "biogeoBottomDepth", "biogeoCenterDepth"))
# Make aggregated coarse frag variable
megapit_biogeo <- megapit_biogeo %>%
  mutate(coarseFrac2to20 = (coarseFrag2To5 + coarseFrag5To20)*.1)
# Add on bulk density
intersect(colnames(megapit_biogeo), colnames(mgp_perbulksampleClean))
megapit_biogeo <- left_join(x = megapit_biogeo, y = mgp_perbulksampleClean, 
      by = c("domainID", "siteID","pitNamedLocation","pitID", "horizonID", "horizonName",
      "setDate", "collectDate", "laboratoryName", "labProjID"))
# Add on pit-level metadata
intersect(colnames(megapit_biogeo), colnames(mgc_permegapit))
megapit_biogeo <- left_join(x = megapit_biogeo, y = mgc_permegapit, 
      by = c("domainID", "siteID","pitNamedLocation", "pitID", "setDate", "collectDate"))
# Add climate - from PRISM, mean of 30-yr normals for entire site
intersect(colnames(megapit_biogeo), colnames(CLIM))
megapit_biogeo <- left_join(x = megapit_biogeo, y = CLIM, 
                            by = c("siteID", "domainID")) # warning ok
# Add biome (from TIS) and dominant plants (PHEN)
intersect(colnames(megapit_biogeo), colnames(COV))
megapit_biogeo <- left_join(x = megapit_biogeo, y = COV, 
                            by = "siteID") # warning ok
# Order by pit, then by horizon and remove unneeded vars
megapit_biogeo <- arrange(megapit_biogeo, pitID, biogeoTopDepth) %>%
  select(-c(uid.x, uid.y, uid.x.x, uid.y.y, setDate, biogeoSampleType, bulkDensSampleType, 
            geodeticDatum, elevationUncertainty,recordedByC, recordedByD, 
            recordedByE, soilProfileDescriberA, soilProfileDescriberB, 
            soilProfileDescriberC, soilProfileDescriberD, soilProfileDescriberE, 
            soilProfileDescriberF))
# Write the file - local + to google drive
write.csv(megapit_biogeo, paste(dir, "megapit_soils_all.csv", sep = "/"), row.names = F)
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_megapitSoil/NEON_megapitSOIL_all/megapit_soils_all")
googledrive::drive_upload(paste(dir, "megapit_soils_all.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_megapitSoil/NEON_megapitSOIL_all/", 
             type = "spreadsheet", verbose = FALSE)
}

### INITIAL CHAR - start with chemistry (perbiogeo), then add on physical (bd, texture) + other
{
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
# Join chem to bulk density, "by" includes all common columns
intersect(colnames(spc_perbiogeo), colnames(spc_bulkdense))
spc_biogeo <- left_join(x = spc_perbiogeo, y = spc_bulkdense, 
        by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID",
        "collectDate", "horizonID", "horizonName", "laboratoryName", "dataQF")) %>%
  mutate(bulkDensComb = ifelse(is.na(bulkDensFieldMoist), bulkDensThirdBar, bulkDensFieldMoist))
# Join to particle size
intersect(colnames(spc_biogeo), colnames(spc_particlesize))
spc_biogeo <- left_join(x = spc_biogeo, y = spc_particlesize, 
        by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID",
        "collectDate", "horizonID", "horizonName",  "biogeoIDnrcs", "biogeoSampleType",
        "biogeoTopDepth", "biogeoBottomDepth", "biogeoCenterDepth", "laboratoryName", "dataQF"))
# Make aggregated coarse frag variable
spc_biogeo <- spc_biogeo %>%
  mutate(coarseFrac2to20 = (coarseFrag2To5 + coarseFrag5To20)*.1)
# Join to plot-level metadata
intersect(colnames(spc_biogeo), colnames(spc_perplot))
spc_biogeo <- left_join(x = spc_biogeo, y = spc_perplot, 
                by = c("namedLocation", "collectDate", "domainID", "siteID", 
                       "plotID", "nrcsDescriptionID", "dataQF"))
# Add spatial (slope/aspect)
spc_biogeo$plotAspect <- spc_spatial$api.slopeAspect[match(spc_biogeo$namedLocation, spc_spatial$namedLocation)]
spc_biogeo$plotSlope <- spc_spatial$api.slopeGradient[match(spc_biogeo$namedLocation, spc_spatial$namedLocation)]
# Add climate (MAT/MAP) - from PRISM, mean of 30-yr normals for entire site
spc_biogeo <- left_join(x = spc_biogeo, y = CLIM, 
                            by = c("siteID", "domainID")) # warnings ok
# Order by site, then horizon top depth; remove unneeded vars
spc_biogeo <- arrange(spc_biogeo, plotID, biogeoTopDepth) %>%
  select(-c(uid.x, uid.y, uid.x.x, uid.y.y, recordedByA, soilProfileDescriberA))
# Write the file
write.csv(spc_biogeo, paste(dir, "intitalChar_all.csv", sep = "/"), row.names = F)
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/a-NEON_initial_master-database/intitalChar_all")
googledrive::drive_upload(paste(dir, "intitalChar_all.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_initialCharacterizationSoil/a-NEON_initial_master-database/", 
                          type = "spreadsheet", verbose = FALSE)
}

### PERIODIC - start with % C and N (soilChem), then add on isotopes, moisture, pH, other
# Each dataframe has few actual data variables, with lots of metadata
# Will use mostly 'match' instead of 'join' to pick and choose vars
{
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
ntr_internalLab <- read.csv(paste
                   (dir, "filesToStack10080/stackedFiles/ntr_internalLab.csv", 
                     sep = "/"), header = T, stringsAsFactors = F)
ntr_internalLabBlanks <- read.csv(paste
                            (dir, "filesToStack10080/stackedFiles/ntr_internalLabBlanks.csv", 
                              sep = "/"), header = T, stringsAsFactors = F)
ntr_externalLab <- read.csv(paste
                            (dir, "filesToStack10080/stackedFiles/ntr_externalLab.csv", 
                            sep = "/"), header = T, stringsAsFactors = F)
sls_spatial <- read.csv(paste(dir, "spatial_sls.csv", sep = "/"), 
                        header = T, stringsAsFactors = F)

# Set chem and isotope values with flags to NA
sls_soilStableIso$d15N[sls_soilStableIso$cnIsotopeQF %in% c(1,3)] <- NA
sls_soilStableIso$d13C[sls_soilStableIso$cnIsotopeQF %in% c(2,3)] <- NA
sls_soilChem$nitrogenPercent[sls_soilChem$cnPercentQF %in% c(1,3)] <- NA
sls_soilChem$carbonPercent[sls_soilChem$cnPercentQF %in% c(2,3)] <- NA
# Need primary key for soil CN data, accounting for acidTreatment & repNumber
sls_soilChem$primaryKey <- paste(sls_soilChem$sampleID, sls_soilChem$analyticalRepNumber, sls_soilChem$acidTreatment, sep = "_")
sls_soilStableIso$primaryKey <- paste(sls_soilStableIso$sampleID, sls_soilStableIso$analyticalRepNumber, sls_soilStableIso$acidTreatment, sep = "_")
# Add isotope data to %C and N table
sls_soilChem$d15N <- sls_soilStableIso$d15N[match(sls_soilChem$primaryKey, sls_soilStableIso$primaryKey)]
sls_soilChem$organicd13C <- sls_soilStableIso$organicd13C[match(sls_soilChem$primaryKey, sls_soilStableIso$primaryKey)]
# Condense this table so each sampleID only has 1 row, e.g., take means for reps
# and put acid-treated on same row
sls_soilChem <- sls_soilChem %>%
  group_by(sampleID) %>%
  summarise_all(list( ~ if (is.numeric(.)) {
    mean(., na.rm = TRUE)
  } else {
    first(.)
  }))
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
# Calculate net N mineralization using neonNTrans
unique(ntr_internalLab$sampleCondition) # see conditions to drop (everything but 'OK')
unique(ntr_externalLab$sampleCondition) # see conditions to drop (everything but 'OK')
unique(ntr_externalLab$dataQF) # none of these need to be dropped, NEON QC already done
netNTrans <- def.calc.ntrans(kclInt = ntr_internalLab, 
                             soilMoist = sls_moisture, 
                             kclIntBlank = ntr_internalLabBlanks, 
                             kclExt = ntr_externalLab,
                             dropConditions = c("extract stored at incorrect temperature", 
                                                "soil stored at incorrect temperature",
                                                "other"), 
                             dropFlagged = T) # warnings ok, some data is missing
# Add on N-min for samples where it's available (2017 and onward)
netNTransSub <- filter(netNTrans, nTransBoutType == "tFinal")
sls_soilChem <- sls_soilChem %>%
  mutate(incubationPairID = substr(sampleID, 1, nchar(sampleID) - 9)) %>%
  left_join(select(netNTransSub, incubationPairID, netNminugPerGramPerDay), by = "incubationPairID")
# Add on field collection metadata
intersect(colnames(sls_soilChem), colnames(sls_CoreCollect))
sls_all <- left_join(x = sls_soilChem, y = sls_CoreCollect, 
          by = c("domainID", "siteID", "plotID", "plotType","sampleID", 
                 "namedLocation", "collectDate"))
# Add spatial (slope/aspect) and soil order - from NEON api
sls_all$plotAspect <- sls_spatial$api.slopeAspect[match(sls_all$namedLocation, sls_spatial$namedLocation)]
sls_all$plotSlope <- sls_spatial$api.slopeGradient[match(sls_all$namedLocation, sls_spatial$namedLocation)]
sls_all$soilOrder <- sls_spatial$api.soilTypeOrder[match(sls_all$namedLocation, sls_spatial$namedLocation)]
# Add climate (MAT/MAP) - from PRISM, mean of 30-yr normals for entire site
sls_all <- left_join(x = sls_all, y = CLIM, 
                            by = c("siteID", "domainID")) # warning ok
# Summarize numeric vars to plot level, by year
sls_reps <- sls_all %>%
  mutate(year = substr(collectDate, 1, 4),
         primaryKeyPlot = paste0(plotID,horizon,year)) %>%
  group_by(primaryKeyPlot) %>%
  summarise_at(c("standingWaterDepth", "soilTemp", 
                 "litterDepth", "sampleTopDepth", "sampleBottomDepth", 
                 "soilMoisture", "dryMassFraction", "soilInWaterpH", 
                 "soilInCaClpH", "nitrogenPercent", "organicCPercent", 
                 "d15N", "organicd13C", "netNminugPerGramPerDay"), 
               mean, na.rm = TRUE) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_at(c("standingWaterDepth", "litterDepth", "sampleTopDepth", 
              "sampleBottomDepth"), round, 1)
# Filter other vars not requiring summary to one occurence per plot-horizon-year
sls_plot <- sls_all %>% 
  mutate(year = substr(collectDate, 1, 4),
         primaryKeyPlot = paste0(plotID,horizon,year)) %>%
  group_by(primaryKeyPlot) %>%
  select("primaryKeyPlot", "namedLocation", "domainID", "siteID", "MAT_C", "MAP_mm", 
         "plotID", "plotType", "plotAspect", "plotSlope", "geodeticDatum",             
         "decimalLatitude", "decimalLongitude", "coordinateUncertainty", 
         "elevation", "elevationUncertainty","soilOrder", "nlcdClass", 
         "collectDate",  "samplingProtocolVersion",
         "sampleTiming", "horizon", "soilSamplingDevice", "acidTreatedforC") %>%
  summarise_all(first)
# Combine data
sls_all_clean <- sls_plot %>%
  left_join(sls_reps, by = "primaryKeyPlot") %>%
  mutate(sort = paste0(plotID,sampleTopDepth)) %>%
  arrange(sort) %>%
  select(-primaryKeyPlot, -sort)
# Write the file
write.csv(sls_all_clean, paste(dir, 'periodic_all.csv', sep = "/"), row.names = F)
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_periodicSoil/NEON_periodicSoil_a-master-database/periodic_all")
googledrive::drive_upload(paste(dir, "periodic_all.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_periodicSoil/NEON_periodicSoil_a-master-database/", 
                          type = "spreadsheet", verbose = FALSE)
}

### MEGAPIT ROOTS
{
# Combine biomass and chem
## Chem
rootChem <- read.csv(paste
                      (dir, "filesToStack10102/stackedFiles/bbc_rootChemistry.csv", 
                       sep = "/"), header = T, stringsAsFactors = F)
# Set chem values with flags to NA
rootChem$nitrogenPercent[rootChem$cnPercentQF %in% c(1,3)] <- NA
rootChem$carbonPercent[rootChem$cnPercentQF %in% c(2,3)] <- NA
# Condense this table so each sampleID only has 1 row, e.g., take means for reps
# and put CO2 trapped Y/N on same row, and megapit only
rootChemClean <- rootChem %>%
  group_by(cnSampleID) %>%
  summarise_all(list( ~ if (is.numeric(.)) {
    mean(., na.rm = TRUE)
  } else {
    first(.)
  })) %>%
  filter(is.na(poolSampleID))
## Biomass
mpr_biomass <- read.csv(paste
                     (dir, "filesToStack10066/stackedFiles/mpr_perrootsample.csv", 
                       sep = "/"), header = T, stringsAsFactors = F)
## Sample depths
mpr_depths <- read.csv(paste
                        (dir, "filesToStack10066/stackedFiles/mpr_perdepthincrement.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
## Pit metadata
mpr_pits <- read.csv(paste
                       (dir, "filesToStack10066/stackedFiles/mpr_perpitprofile.csv", 
                         sep = "/"), header = T, stringsAsFactors = F)
mpr_pits <- mpr_pits %>%
  group_by(pitNamedLocation) %>%
  summarise_all(first) # only need pit meta-data, one occurence per pit
## Join
mpr_all <- mpr_biomass %>%
  left_join(rootChemClean, by = c("sampleID" = "cnSampleID")) %>%
  left_join(mpr_depths, by = c("depthIncrementID", "pitNamedLocation", "endDate")) %>%
  left_join(mpr_pits, by = c("pitNamedLocation", "endDate")) %>%
  select("pitNamedLocation", "domainID" = "domainID.y", "siteID" = "siteID.y",
         "decimalLatitude", "decimalLongitude", "coordinateUncertainty",  
         "elevation", "collectDate" = "endDate", "pitID", 
         "depthIncrementID","topDepth", "bottomDepth", "rootStatus" = "rootStatus.x", 
         "sizeCategory" = "sizeCategory.x", "incrementRootBiomass", 
         "nitrogenPercent", "carbonPercent", "CNratio")
# Summarize numeric vars to pit x depth interval x size x status (e.g., average 3 pit profiles)
mpr_reps <- mpr_all %>%
  mutate(primaryKey = paste(pitID,bottomDepth,rootStatus,sizeCategory, sep = "-")) %>%
  group_by(primaryKey) %>%
  summarise_at(c("incrementRootBiomass", "nitrogenPercent", 
                 "carbonPercent", "CNratio"), mean, na.rm = TRUE)
# Filter other vars not requiring summary to one occurence per pit-depth-status-size
mpr_pit <- mpr_all %>% 
  mutate(primaryKey = paste(pitID,bottomDepth,rootStatus,sizeCategory, sep = "-")) %>%
  select("primaryKey", "pitNamedLocation", "domainID", "siteID",
         "decimalLatitude", "decimalLongitude", "coordinateUncertainty",  
         "elevation", "collectDate", "pitID", 
         "topDepth", "bottomDepth", "rootStatus", "sizeCategory") %>%
  group_by(primaryKey) %>%
  summarise_all(first)
# Re-combine data, pit-level averages
mpr_all_clean <- mpr_pit %>%
  left_join(mpr_reps, by = "primaryKey") %>%
  select(-primaryKey) %>%
  arrange(pitID, topDepth)
# Add biome (from TIS) and dominant plants (PHEN)
intersect(colnames(mpr_all_clean), colnames(COV))
mpr_all_clean <- left_join(x = mpr_all_clean, y = COV, 
                            by = c("siteID")) # warning ok
# Write the file
write.csv(mpr_all_clean, paste(dir, 'megapit_roots.csv', sep = "/"), row.names = F)
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_megapitRoots/NEON_megapitRoots_all/megapit_roots")
googledrive::drive_upload(paste(dir, "megapit_roots.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_megapitRoots/NEON_megapitRoots_all/", 
                          type = "spreadsheet", verbose = FALSE)
}

### PERIODIC ROOTS
{
# Chem already loaded above, but still need to combine biomass and chem
# Condense chem table so each sampleID only has 1 row, e.g., take means for reps
# and put CO2 trapped Y/N on same row, periodic only
rootChemClean2 <- rootChem %>%
  group_by(cnSampleID) %>%
  summarise_all(list( ~ if (is.numeric(.)) {
    mean(., na.rm = TRUE)
  } else {
    first(.)
  })) %>%
  filter(!is.na(poolSampleID))
## Biomass
bbc_biomass <- read.csv(paste
                        (dir, "filesToStack10067/stackedFiles/bbc_rootmass.csv", 
                          sep = "/"), header = T, stringsAsFactors = F)
## Sample IDs for chemistry
bbc_chemSub <- read.csv(paste
                     (dir, "filesToStack10067/stackedFiles/bbc_chemistryPooling.csv", 
                       sep = "/"), header = T, stringsAsFactors = F)
## Sample area and depth
bbc_core <- read.csv(paste
                     (dir, "filesToStack10067/stackedFiles/bbc_percore.csv", 
                       sep = "/"), header = T, stringsAsFactors = F)
## Sum root biomass, north and south core
bbc_biomass_sum <- bbc_biomass %>%
  filter(qaDryMass == "N") %>%
  mutate(clipSampleID = substr(sampleID, 1, 19)) %>%
  group_by(clipSampleID, sizeCategory, rootStatus) %>%
  summarise(rootMass = sum(dryMass), 
            sampleID = first(sampleID)) %>%
  mutate(poolSampleID = ifelse(rootStatus == "live", 
                               paste(substr(sampleID, 1, 23), sizeCategory,"POOL", sep = "."),
                               NA)) %>% # add 'poolID' to join to chemistry
  select(-sampleID)
## Summarize field data: combine north and south core
bbc_core_sum.1 <- bbc_core %>%
  filter(rootSamplingPossible == "Y") %>%
  mutate(clipSampleID = substr(sampleID, 1, 19)) %>%
  group_by(clipSampleID) %>%
  summarize_at("rootSampleArea", sum, na.rm = T) #sum area
bbc_core_sum.2 <- bbc_core %>%
  filter(rootSamplingPossible == "Y") %>%
  mutate(clipSampleID = substr(sampleID, 1, 19)) %>%
  group_by(clipSampleID) %>%
  summarize_at("rootSampleDepth", mean, na.rm = T) #take mean of depth
bbc_core_sum.3 <- bbc_core %>%
  filter(rootSamplingPossible == "Y") %>%
  mutate(clipSampleID = substr(sampleID, 1, 19)) %>%
  group_by(clipSampleID) %>%
  summarise_at(c("namedLocation", "domainID", "siteID", "plotID", "subplotID", 
                 "clipID", "decimalLatitude", "decimalLongitude", "geodeticDatum", 
                 "coordinateUncertainty",  "elevation", "collectDate", 
                 "samplingProtocolVersion", "rootSamplingMethod"), first)# first occurence for metadata
## Join all core, biomass and chemistry data
bbc_all <- bbc_core_sum.3 %>%
  left_join(bbc_core_sum.1, by = "clipSampleID") %>%
  left_join(bbc_core_sum.2, by = "clipSampleID") %>%
  mutate(rootSampleTopDepth = 0) %>%
  left_join(bbc_biomass_sum, by = "clipSampleID") %>%
  mutate(rootMassPerArea = rootMass/rootSampleArea) %>%
  rename(rootSampleBottomDepth = rootSampleDepth) %>%
  left_join(rootChemClean2, by = c("namedLocation", "domainID", "siteID", 
                                   "plotID", "poolSampleID")) %>%
  mutate(CNratio = carbonPercent/nitrogenPercent) %>%
  select(c(-collectDate.y, -uid, -cnSampleID, -plotType, 
           -cnSampleCode, -sampleType, -cnPercentQF, 
           -percentAccuracyQF, - analyticalRepNumber, -testMethod,
           -analyzedBy, -reviewedBy, -co2Trapped, 
           -remarks, -laboratoryName, -instrument, -dataQF)) %>%
  rename(collectDate = collectDate.x)
# Write the file
write.csv(bbc_all, paste(dir, 'periodic_roots.csv', sep = "/"), row.names = F)
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_periodicRoots/NEON_periodicRoots_a-master-database/periodic_roots")
googledrive::drive_upload(paste(dir, "periodic_roots.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_periodicRoots/NEON_periodicRoots_a-master-database/", 
                          type = "spreadsheet", verbose = FALSE)
}

### LITTERFALL ###
{
ltr_pertrap <- read.csv(paste
                     (dir, "filesToStack10033/stackedFiles/ltr_pertrap.csv", 
                       sep = "/"), header = T)
ltr_field <- read.csv(paste
                          (dir, "filesToStack10033/stackedFiles/ltr_fielddata.csv", 
                            sep = "/"), header = T)
ltr_mass <- read.csv(paste
                          (dir, "filesToStack10033/stackedFiles/ltr_massdata.csv", 
                            sep = "/"), header = T)
ltr_chemsub <- read.csv(paste
                          (dir, "filesToStack10033/stackedFiles/ltr_chemistrySubsampling.csv", 
                            sep = "/"), header = T)
ltr_CN <- read.csv(paste
                        (dir, "filesToStack10031/stackedFiles/ltr_litterCarbonNitrogen.csv", 
                          sep = "/"), header = T)
ltr_lig <- read.csv(paste
                        (dir, "filesToStack10031/stackedFiles/ltr_litterLignin.csv", 
                          sep = "/"), header = T)

## MASS
# Clean-up, remove NAs and mistletoe
ltr_mass_mod <- ltr_mass %>%
  filter(!is.na(massSampleID), 
         !is.na(fieldSampleID),
         qaDryMass!="Y", 
         !(remarks %in% c("Dwarf mistletoe", 
                          "mistletoe", "Mistletoe")))
# Summarize to find dupes
dupes <- ltr_mass_mod %>%
  group_by(massSampleID) %>%
  dplyr::summarize(n = n()) %>%
  filter(n > 1) # these massSampleIDs are duplicated, 116 as of Sept 2019
# Subset to duplicated massSampleIDs
ltr_mass_dupes <- ltr_mass_mod %>%
  filter(massSampleID %in% dupes$massSampleID)
View(ltr_mass_dupes) # Often masses are the same or very close (mistaken qaDryMass most likely)
# Only retain the first instance of each record in duplicate pairs - short term, NEON will ultimately fix
ltr_mass_mod <- ltr_mass_mod[!duplicated(ltr_mass_mod$massSampleID, fromLast=TRUE),] 

## PER TRAP
dupes <- ltr_pertrap %>%
  group_by(trapID) %>%
  dplyr::summarize(n = n()) %>%
  filter(n > 1) # these trapIDs are duplicated, 1 as of Sept 2019
# Only retain the first instance of each record in duplicate pairs - this also gets rid of NAs in trapID
ltr_pertrap_mod <- ltr_pertrap[!duplicated(ltr_pertrap$trapID, fromLast = TRUE),]

## FIELD
unique(ltr_field$trapCondition)
# Create a new condition field, to help filter out 'complete' traps vs ones with problems and thus incomplete data
ltr_field_mod <- ltr_field %>%
  mutate(trapCondition2 = ifelse(trapCondition %in% c("OK - Litter collected - Trap in good shape, no issues", 
                                                      "TE - Litter not collected - trap empty", 
                                                      "PF - Litter collected; Trap previously flooded", 
                                                      "TS - Litter not collected, not discarded - trap skipped"), 
                                 "Ok", "Problem"))
# fieldSampleID can be missing even if trap condition is OK for TE (trap empty) or PF (previously flooded) 
# methodological trap conditions causing missing fieldSampleID: HO (hole in trap),  RE (broken trap needing replacement), 
# TB (trap blocked), TT (trap tipped), TS (trap skipped)

## Combine data frames
ltr_fieldAndTrap <- ltr_field_mod %>%
  left_join(ltr_pertrap_mod, by= c("namedLocation", 
                                   "trapID", 
                                   "domainID", 
                                   "siteID", 
                                   "plotID")) %>% 
  filter(trapType=="Elevated" ) 
names(ltr_fieldAndTrap)
names(ltr_mass_mod)
ltr_all <- ltr_mass_mod %>%
  left_join(ltr_fieldAndTrap, by=c("domainID",
                                   "siteID", 
                                   "plotID",
                                   "trapID",
                                   "setDate",
                                   "collectDate",
                                   "fieldSampleID")) %>% 
  filter(trapType=="Elevated" ) 

## SUBSET TO TRAPS WITH ONLY 'COMPLETE' DATA:
# ~ 365 days of good trapping (+/- allowed buffer) in a given year + 90 % of trapping days no issue
# Summarize trapping days per trapID + year
ltr_fieldAndTrap_annual <- ltr_fieldAndTrap %>%
  mutate(year = ifelse(!is.na(eventID), substr(eventID, 5, 8), substr(setDate, 1,4))) %>%
  group_by(trapID, year, trapCondition2) %>%
  summarize(trappingDaysSum = sum(trappingDays)) 
# Wide format + add flags
ltr_fieldAndTrap_annual_wide <- ltr_fieldAndTrap_annual %>%
  spread(trapCondition2, trappingDaysSum) %>%
  rename(trappingDaysOk = Ok, 
         trappingDaysProb = Problem) %>%
  rowwise() %>%
  mutate(trappingDaysTotal = sum(trappingDaysOk, trappingDaysProb, na.rm = T), 
         trappingDaysTot_pass = ifelse(trappingDaysTotal <= 430 & trappingDaysTotal >=299, "Y", "N"), # total number of days flag
         trappingDays_pctGood = 100 * trappingDaysOk / trappingDaysTotal) # percentage of days good flag
# Filter to only 'good' traps
ltr_fieldAndTrap_annual_keep <- ltr_fieldAndTrap_annual_wide %>%
  filter(trappingDays_pctGood >= 90 & trappingDaysTot_pass == "Y") %>%
  mutate(trap_year = paste0(trapID, '-', year))

## Summarize litterfall, annual fluxes - per trap
ltrFlux_perTrap_perGroup <- ltr_all %>%
  mutate(year = ifelse(!is.na(eventID), substr(eventID, 5, 8), substr(setDate, 1,4)),
         trap_year = paste0(trapID, '-', year)) %>%
  filter(trap_year %in% ltr_fieldAndTrap_annual_keep$trap_year) %>%
  rowwise() %>%
  mutate(massPerArea = dryMass / trapSize) %>%
  ungroup() %>%
  group_by(siteID, plotID, trapID, year, functionalGroup) %>%
  summarize(gramsDMPerMeterSquared = sum(massPerArea)) %>%
  left_join(select(ltr_fieldAndTrap_annual_keep, trapID, year, trappingDaysOk),
            by = c("trapID", "year")) %>%
  mutate(gramsDMPerMeterSquaredPerYear = round(365 * gramsDMPerMeterSquared / trappingDaysOk, 3))

# ltrFlux_perTrap_perGroup.2 <- ltr_all %>%
#   mutate(year = ifelse(!is.na(eventID), substr(eventID, 5, 8), substr(setDate, 1,4)),
#          trap_year = paste0(trapID, '-', year)) %>%
#   filter(trap_year %in% ltr_fieldAndTrap_annual_keep$trap_year) %>%
#   group_by(siteID, plotID, trapID, year, functionalGroup) %>%
#   summarize(dryMass_total = sum(dryMass),
#             trapArea = first(trapSize))

## Add Chemistry
# Need this to add collect year to chemistry samples
ltr_chemsub_mod <- ltr_chemsub %>%
separate_rows(massSampleIDList) %>%
  mutate(fieldSampleID = substr(massSampleIDList, 
                                1,
                                nchar(massSampleIDList) - 4)) %>%
  left_join(select(ltr_field, fieldSampleID, eventID), by = "fieldSampleID") %>%
  mutate(year = ifelse(!is.na(eventID), substr(eventID, 5, 8), substr(setDate, 1,4))) %>%
  group_by(massSampleMixtureID) %>%
  summarize(year = first(year))
# CN - Take means for analytical reps & put CO2 trapped yes/no on the same line
ltr_CN_mod <- ltr_CN %>%
  group_by(cnSampleID) %>%
  summarise_all(list( ~ if (is.numeric(.)) {
    mean(., na.rm = TRUE)
  } else {
    first(.)
  })) %>%
  filter(!is.na(cnSampleID))
# Functional Groups + collect year
ltr_CN_mod <- ltr_CN_mod %>%
  mutate(funGroup = substr(massSampleMixtureID, 
                           nchar(massSampleMixtureID) - 2,
                           nchar(massSampleMixtureID)),
         functionalGroup = ifelse(funGroup == "LVS", "Leaves", "Needles")) %>%
  left_join(ltr_chemsub_mod, by = "massSampleMixtureID")
# Lignin - Take means for analytical reps
ltr_lig_mod <- ltr_lig %>%
  group_by(ligninSampleID) %>%
  summarise_all(list( ~ if (is.numeric(.)) {
    mean(., na.rm = TRUE)
  } else {
    first(.)
  })) %>%
  filter(!is.na(ligninSampleID))
# Functional Groups
ltr_lig_mod <- ltr_lig_mod %>%
  mutate(funGroup = substr(massSampleMixtureID, 
                           nchar(massSampleMixtureID) - 2,
                           nchar(massSampleMixtureID)),
         functionalGroup = ifelse(funGroup == "LVS", "Leaves", "Needles")) %>%
  left_join(ltr_chemsub_mod, by = "massSampleMixtureID")
## FINAL JOIN
ltrFlux_perTrap_perGroup_chem <- ltrFlux_perTrap_perGroup %>%
  left_join(select(ltr_CN_mod, plotID, functionalGroup, year,
                   carbonPercent, nitrogenPercent), 
            by = c("plotID", "functionalGroup", "year")) %>%
  left_join(select(ltr_lig_mod, plotID, functionalGroup, year,
                   ligninPercent, cellulosePercent), 
            by = c("plotID", "functionalGroup", "year")) %>%
  select(-gramsDMPerMeterSquared, -trappingDaysOk)
# Write the file
write.csv(ltrFlux_perTrap_perGroup_chem, paste(dir, 'litterfall_annual_with_chem.csv', sep = "/"), row.names = F)
googledrive::drive_trash("~/LTER-SOM/Data_downloads/NEON_litterfall/NEON_litterfall_all/litterfall_annual_with_chem")
googledrive::drive_upload(paste(dir, "litterfall_annual_with_chem.csv", sep = "/"), path = "~/LTER-SOM/Data_downloads/NEON_litterfall/NEON_litterfall_all/", 
                          type = "spreadsheet", verbose = FALSE)
}


