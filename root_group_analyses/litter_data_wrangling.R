##########################################################################
### CODE FOR DOWNLOADING AND WRANGLING NEON LITTER DATA
### ANNUAL FLUX CALCS
### S. Weintraub & C. Meier
### August 2020
##########################################################################

### Reset workspace
rm(list = ls())

### Load packages 
library(tidyverse) # joining and wrangling functions
library(neonUtilities) # to download NEON data

### DOWNLOAD DATA ###
ltr <- loadByProduct(site = "all", dpID = "DP1.10033.001", 
                     package = "basic", check.size = FALSE)
list2env(ltr,envir=.GlobalEnv) # make objects in the list into dataframes

## MASS
# Clean-up, remove QA samples and possible mistletoe contamination
ltr_mass_mod <- ltr_massdata %>%
  filter(qaDryMass!="Y", 
         !(remarks %in% c("Dwarf mistletoe", 
                          "mistletoe", "Mistletoe")))
# Summarize to find dupes
dupes <- ltr_mass_mod %>%
  group_by(massSampleID) %>%
  summarize(n = n()) %>%
  filter(n > 1) # these massSampleIDs are duplicated, 218 (0.1%) as of Aug 2020
# Subset to duplicated massSampleIDs
ltr_mass_dupes <- ltr_mass_mod %>%
  filter(massSampleID %in% dupes$massSampleID) %>%
  arrange(massSampleID)# Often masses are the same or very close (mistaken qaDryMass most likely), 
# although sometimes they vary a lot (minority of cases)

# For this analysis, will retain the first instance of each record in duplicate pairs
ltr_mass_mod <- ltr_mass_mod[!duplicated(ltr_mass_mod$massSampleID, fromLast=TRUE),] 

## PER TRAP
dupes2 <- ltr_pertrap %>%
  group_by(trapID) %>%
  summarize(n = n()) %>%
  filter(n > 1) # these trapIDs are duplicated, 13 as of Aug 2020, all OSBS
# For this analysis, will retain the first instance of each record in duplicate pairs - this also gets rid of NAs in trapID
ltr_pertrap_mod <- ltr_pertrap[!duplicated(ltr_pertrap$trapID, fromLast = TRUE),]

## Combine data frames
ltr_fieldAndTrap <- ltr_field_mod %>%
  left_join(ltr_pertrap_mod, by= c("namedLocation", 
                                   "trapID", 
                                   "domainID", 
                                   "siteID", 
                                   "plotID")) %>% 
  filter(trapType=="Elevated" ) 
names(ltr_fieldAndTrap)
names(ltr_mass_mod) # do not include eventID in the join
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
# 1) sum all masses for the bout [do this by functional group?], then divide by area for the bout
# uncertainty - SD of masses? additive uncertainty. need to research this
# 2) add up masses per unit time from all bouts in a given year - this is annual production 
# (doesn't matter if # of days trapped is different - but make sure most/all of the season is captured)
# 3) Average across years, 1 number per site +/- variance

# Step 1
sum(is.na(ltr_all$eventID.y)) # use this eventID, from the field data (missing in mass table)
ltr_all_perEvent <- ltr_all %>%
  group_by(eventID.y) %>%
  summarize(massSum = sum(dryMass, na.rm = T), # dryMass is in g
            trapCount = length(unique(trapID)), 
            trapAreaSum = trapCount*0.5, # elevated trap size is 0.5 m2
            massPerUnitArea = massSum/trapAreaSum,
            setDate = first(setDate),
            collectDate = last(collectDate))

# Step 2
ltr_all_perYear <- ltr_all_perEvent %>%
  mutate(siteYear = substr(eventID.y, 5, 13)) %>%
  group_by(siteYear) %>%
  summarize(massPerUnitAreaAnnual = sum(massPerUnitArea, na.rm = T),
            firstSetDate = first(setDate),
            lastCollectDate = last(collectDate)) %>%
  mutate(trappingDays = difftime(lastCollectDate, firstSetDate, units='days'))

# Step 3, average with uncertainty (SD)
# select D19 Alaska sites only
ltr_all_AK <- ltr_all_perYear %>%
  mutate(siteID = substr(siteYear, 6,9)) %>%
  filter(siteID %in% c("BONA", "DEJU", "HEAL")) # no filtering by length of time trapping, 
# these sites have short seasons. sometimes traps left out over the winter, other times not

# other sites, not AK
ltr_all_nonAK <- ltr_all_perYear %>%
  mutate(siteID = substr(siteYear, 6,9)) %>%
  filter(!grepl("2020|2028", siteYear), 
         trappingDays > 215 & !siteID %in% c("BONA", "DEJU", "HEAL"))

# join filtered DFs back together - FINAL TABLE
ltr_all_perSite <- bind_rows(ltr_all_AK, ltr_all_nonAK) %>%
  group_by(siteID) %>%
  summarize(massPerUnitAreaAnnualMean = mean(massPerUnitAreaAnnual, na.rm = T),
            massPerUnitAreaAnnualSD = sd(massPerUnitAreaAnnual, na.rm = T),
            numYears = n())

# Export - change path below to your own to re-run and export code
write.csv(ltr_all_perSite, '/Users/sweintraub/Documents/GitHub/lterwg-som/root_group_analyses/litterFlux_perSite.csv', row.names = F)

