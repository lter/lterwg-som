## Load packages
library(tidyverse)
## Set path to file (if others want to use this code, add your path as another if statement)
if (file.exists(
  '/Users/sweintraub/')){
  path<-'/Users/sweintraub/'
}
## Load data
data.all <- readRDS(paste0(path, "Downloads/somCompositeData_2018-11-09T17_34_23.rds", sep = "")) 
names(data.all)
## Fix location names
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
levels(as.factor(data.all$full_name)) #147 site names
data.all <- data.all %>% 
  mutate(full_name = gsub("NA", "", full_name)) %>%
  mutate(full_name = trimws(full_name)) %>%
  mutate(full_name = ifelse(network == "NEON", L1, full_name))
levels(as.factor(data.all$full_name)) #183 with the NEON data
## Split data by network
levels(as.factor(data.all$network))
Net <- split(data.all, data.all$network)

## Focus on DIRT
dirt <- Net$DIRT
dirtTreatments <- dirt %>%
  group_by(full_name, tx_L1) %>%
  summarize(meanSOC = mean(soc, na.rm = T), 
            sdSOC = sd(soc, na.rm = T), n = n())
## Clean up treatment levels
levels(as.factor(dirt$tx_L1))
dirt <- dirt %>%
  mutate(tx_L1 = gsub("CO", "C", tx_L1)) %>%
  mutate(tx_L1 = gsub("CTL", "C", tx_L1)) %>%
  mutate(tx_L1 = gsub("Control", "C", tx_L1)) %>%
  mutate(tx_L1 = gsub("Double Litter", "DL", tx_L1)) %>%
  mutate(tx_L1 = gsub("DLF", "DL", tx_L1)) %>%
  mutate(tx_L1 = gsub("No Inputs", "NI", tx_L1)) %>%
  mutate(tx_L1 = gsub("No Litter", "NL", tx_L1)) %>%
  mutate(tx_L1 = gsub("No Roots", "NR", tx_L1))
levels(as.factor(dirt$tx_L1))
## Are there plots?
levels(as.factor(dirt$L1)) # maybe?
## DIRT Plot
ggplot(dirt, aes(site_code, soc, fill = tx_L1)) + 
  geom_boxplot()
## Response ratio - (treatment - control/contol)*100
# CODE TBD

## Focus on NutNet
nn <- Net$NutNet
## Exploring levels
levels(as.factor(nn$L1)) #site
levels(as.factor(nn$L2)) #plot?
levels(as.factor(nn$L3)) #plot?
levels(as.factor(nn$L4)) #year
levels(as.factor(nn$tx_L1)) #fertilization type
levels(as.factor(nn$tx_L2)) #? 0/1
levels(as.factor(nn$tx_L3)) #? 0/1
levels(as.factor(nn$tx_L4)) #? 0/1
## Summary table by plot and treatment
nnTreatments <- nn %>%
  group_by(full_name, tx_L1, L2) %>%
  summarize(meanSOC = mean(soc, na.rm = T), 
            sdSOC = sd(soc, na.rm = T), n = n())
## NutNet Plot
ggplot(subset(nn, !is.na(nn$tx_L1)), aes(tx_L1, soc)) + 
  geom_boxplot()
## Response ratio
# TBD
