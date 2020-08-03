##########################################################################
### CODE FOR DOWNLOADING AND WRANGLING DATA FROM NEON DATA PORTAL
### Created Feb 28th, 2018
### Updated June 15, 2018
### Edits, programmatic upload to Google Drive, Jan 7 2019
### Edits, interact with Google Drive + add other vars, July 2019
### Edits, provide biome and cover type for megapit data, Oct 2019
### Edits, provide size categories for root data, Jan 2020
### S. Weintraub
##########################################################################

### Reset workspace
rm(list = ls())

### Load packages 
library(tidyverse) # joining and wrangling functions
library(neonUtilities) # to download NEON data

### LITTERFALL ###

ltr <- loadByProduct(site = "all", dpID = "DP1.10033.001", 
                     package = "basic", check.size = FALSE)
list2env(spc,envir=.GlobalEnv)

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
  filter(massSampleID %in% dupes$massSampleID) # Often masses are the same or very close (mistaken qaDryMass most likely)
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
# Create a new condition field, to help decide which traps can be used vs which had problems and should be ignored
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

# Workflow: 
# 1) filter out probelm traps (as above)
# 2) sum all masses for the bout [do this by functional group?]
# 3) then divide by area for the bout
# 4) uncertainy - SD of masses? additive uncertainy. need to research this
# 5) add up masses per unit time from all bouts in a given year - this is annual production 
# (doesn't matter if # of days trapped is different)



### OLD WAY (FOR OPTIMIZATION)
## SUBSET TO TRAPS WITH ONLY 'COMPLETE' DATA:
# ~ 365 days of good trapping (+/- allowed buffer) in a given year + 90 % of trapping days no issue
# This is an approach specific to Optimization, not necessary if you only want to know productivity in the site
# Summarize trapping days per trapID + year
ltr_fieldAndTrap_annual <- ltr_fieldAndTrap %>%
  mutate(year = ifelse(!is.na(eventID), substr(eventID, 5, 8), substr(setDate, 1,4))) %>%
  group_by(trapID, year, trapCondition2) %>%
  dplyr::summarize(trappingDaysSum = sum(trappingDays)) 
# Wide format + add flags
ltr_fieldAndTrap_annual_wide <- ltr_fieldAndTrap_annual %>%
  spread(trapCondition2, trappingDaysSum) %>%
  dplyr::rename(trappingDaysOk = Ok, 
         trappingDaysProb = Problem) %>%
  mutate(trappingDaysOk = ifelse(is.na(trappingDaysOk), 0, trappingDaysOk), 
         trappingDaysProb = ifelse(is.na(trappingDaysProb), 0, trappingDaysProb),
         trappingDaysTotal = trappingDaysOk + trappingDaysProb, 
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
  mutate(massPerArea = dryMass / trapSize) %>%
  group_by(siteID, plotID, trapID, year, functionalGroup) %>%
  dplyr::summarize(gramsDMPerMeterSquared = sum(massPerArea)) %>%
  left_join(select(ltr_fieldAndTrap_annual_keep, trapID, year, trappingDaysOk),
            by = c("trapID", "year")) %>%
  mutate(gramsDMPerMeterSquaredPerYear = round(365 * gramsDMPerMeterSquared / trappingDaysOk, 3))

## If we want to save the data
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
  }
