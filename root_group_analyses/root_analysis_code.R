##################################
### Data Exploration: LTER-SOM ###
###    Sept 2019, SWeintraub   ###
##################################
#Avni edits
### Reset workspace
rm(list = ls())

### Set paths
if (file.exists('/Users/sweintraub/')){
  dir1 <- ("/Users/sweintraub/Documents/GitHub/lterwg-som/") 
}

if (file.exists('/Users/JM_1/')){
  dir1 <- ("~/Documents/GitHub/lterwg-som/") 
}

if (file.exists('C:/Users/vishr_000')){
  dir1 <- ("C:/Users/vishr_000/Documents/GitHub/lterwg-som/") 
}
dir1

### Load data - if downloaded
som <- readRDS(paste(dir1, "root_group_analyses/somCompositeData_2019-10-15.rds", sep = "/"))
#landCov <- read.csv(paste(dir1, "root_group_analyses/NEONtowerSiteMetadata.csv", sep = "/"))#because the updated file already has landcover

### Load data - straight from google drive
library(googledrive)
library(tidyverse)
source(paste0(dir1,'data-processing/get_latest_som.R'))
som <- get_latest_som()

# filter to only NEON
somNEON <- filter(som, network == "NEON")

# useful function for making the DF smaller, only keep variables with not all NA
not_all_na <- function(x) any(!is.na(x))

#subsetting neon sites into groups for easier plotting
neonSiteList1 <- c("BART", 
                   "HARV", 
                   "SCBI",
                   "SERC",
                   "BLAN",
                   "DSNY",
                   "OSBS",
                   "JERC",
                   "LAJA",
                   "GUAN",
                   "STEI",
                   "UNDE",
                   "TREE",
                   "UKFS",
                   "KONZ",
                   "KONA",
                   "GRSM",
                   "MLBS",
                   "ORNL",
                   "DELA",
                   "TALL",
                   "WOOD",
                   "DCFS")

# Megapits - creating dataframes for making plots, carbon with depth, roots and soil
# have to include 4 diam because otherwise some sites would be lost
somNEONMegaRoots <- somNEON %>%
  filter(data_file%in%c("megapit_roots"),
         bgb_upperdiam%in%c("2","4"), 
         bgb_type == "live") %>%
  mutate(bgb_c = ifelse(is.na(bgb_c), 52, bgb_c),
         bgb_c_stock = bgb*(bgb_c*.01))

somNEONMegaSoil <- somNEON %>%
  filter(data_file%in%c("megapit_soils_all"))

# Putting roots and soil together into one column
somNEONMega <- bind_rows(somNEONMegaRoots, somNEONMegaSoil) %>%
  mutate(carbon_stock = ifelse(data_file=="megapit_roots", 
                               bgb_c_stock, 
                               lyr_soc_stock_calc/100)) %>% #divide by 100 was for graphing, don't do analyses on this
  select_if(not_all_na) %>%
  arrange(site_code)
sum(is.na(somNEONMega$carbon_stock)) # 6 rows have no C

#subset dfs, half of neon sites
somNEONMega1 <- somNEONMega %>%
  filter(site_code%in%neonSiteList1)
somNEONMega2 <- somNEONMega %>%
  filter(!site_code%in%neonSiteList1)

# create first plot of roots and soil with depth
ggplot(somNEONMega2, 
       aes(x = carbon_stock, 
           y = layer_bot, 
           color = data_file)) +
  geom_point(pch = 21) + 
  scale_y_reverse() + # puts 0 at the top
  #scale_x_log10() +
  facet_wrap(~ site_code, ncol = 8, scales = "free") +
  theme_bw() # save 6 x 12

# Align root and soil data, need to assign horizon to roots
somNEONMegaRootsSel <- somNEONMegaRoots %>%
  select_if(not_all_na)

somNEONMegaSoilSel <- somNEON %>%
  filter(data_file%in%c("megapit_soils_all")) %>%
  select_if(not_all_na)

## JL loop to assign horizons - takes a min or two to run
results.list = list()
for (site in somNEONMegaRoots$site_code) {
  df.roots.oneSite = somNEONMegaRoots %>% 
    filter(site_code == site)
  df.soil.oneSite = somNEONMegaSoil %>% 
    filter(site_code == site)
  site.horizons = c()
  
  # loop through each midpoint for a given site
  for (midpoint in df.roots.oneSite$layer_mid) {
    midpoint.horizon = NA
    
    # get the horizon for a given midpoint
    for (horizon in df.soil.oneSite$hzn) {
      df.soil.oneSite.oneHorizon = df.soil.oneSite %>% 
        filter(hzn == horizon)
      if ((midpoint > df.soil.oneSite.oneHorizon$layer_top[1]) & 
          (midpoint <= df.soil.oneSite.oneHorizon$layer_bot[1])) {
          midpoint.horizon = horizon
        }
      }
    # All the horizons for a given site stored in a vector
    site.horizons = c(site.horizons, midpoint.horizon)
   }
  results.list[[site]] = tibble(layer_mid = df.roots.oneSite$layer_mid,
                                hzn = site.horizons,
                                site_code = site)
}
horizon.dat = bind_rows(results.list)

# Add horizon to megapit roots, select
somNEONMegaRootsSel <- somNEONMegaRootsSel %>%
  left_join(horizon.dat, by = c("site_code", "layer_mid"))

# Sum roots by horizon
somNEONMegaRootsSel.byHor <- somNEONMegaRootsSel %>%
  group_by(site_code, hzn) %>%
  summarize(bgb_c_stock = sum(bgb_c_stock, na.rm = T))

# Join to soil data - **********USE THIS TO COMPARE OTHER EDAPHIC VARS TO ROOTS*************
somNEONMegaSoil.withRoot <- somNEONMegaSoilSel %>%
  left_join(somNEONMegaRootsSel.byHor, by = c("site_code", "hzn")) %>%
  mutate(hzn_type = ifelse(grepl("^O", hzn), "organic", "mineral")) #%>%
  #left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) #### HELP this line did not run, why is it here?

# Whole profile summed for each horizon at each site
somNEONMegaSoil.withRoot.Profile <- somNEONMegaSoil.withRoot %>%
  group_by(site_code, hzn_type) %>% 
  summarize(bgb_c_stock_sum = sum(bgb_c_stock, na.rm = T), #Jessica changed columns to _sum
            lyr_soc_stock_calc_sum = sum(lyr_soc_stock_calc, na.rm = T)) 
            #land_cover = first(Ecosystem.type)) We left this out because Ecosystem.type is not working with the 10/15/19 tarball

### Plots ###
# By horizon
ggplot(somNEONMegaSoil.withRoot, aes(y = lyr_soc_stock_calc,
                                     x = bgb_c_stock)) +
                                     #fill = Ecosystem.type)) + #Need to fix landCov and Ecosystem.type
  geom_point(pch = 21, size = 2) + 
  theme_bw() # save 4 x 6
# By profile
ggplot(somNEONMegaSoil.withRoot.Profile, aes(x = lyr_soc_stock_calc,
                                     y = bgb_c_stock, 
                                     fill = land_cover)) + 
  geom_point(pch = 21, size = 2) + 
  theme_bw() # save 4 x 6

##### NEXT TO DO ######
# Think about where it might be important, start there (types of sites/systems)
# Facet by ecosystem type/biome
# Look at covariates - nutrients, texture, etc
# root nitrogen vs soil nitrogen

# write files, if desired
# write.csv(somNEONRoots, paste(dir1, "Filtered_SOM_NEONroots_only.csv", sep = "/"))


############## Jessica's exploration
#Objective 1: whole profile sum SOC ~ whole profile root SOC; site and network as random effects; roots, landcover, nutrients, pH, texture as fixed effects in full model

#Objective 2: Does SOC~roots vary with depth? First, run with depth as covariate in full model. Next, slice by depth intervals and re-run model.

#Objective 3: Does B(SOC~depth)~B(roots~depth)? With landcover, nutrients, soil texture as fixed effects.
#Calc cumsum using root data by depth interval, then calc SOC cumsum using the horizon data

#cum sum for root C stocks
somNEONMegaRootsSelSumDepth <- somNEONMegaRootsSel %>% 
  #left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) %>% LANDCOVER, ARGGHH
  left_join(select(somNEONMegaSoil.withRoot.Profile, site_code, bgb_c_stock_sum),by="site_code") %>%
  mutate(rootfrac = round((bgb_c_stock/bgb_c_stock_sum),2)) %>%
  group_by(site_code) %>%
  mutate(rootfrac_cumsum = round(cumsum(rootfrac),2))
View(somNEONMegaRootsSelSumDepth)

#cum sum for SOC stocks
somNEONMegaSoilSelSumDepth <- somNEONMegaSoilSel %>% 
  #left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) %>% LANDCOVER
  left_join(select(somNEONMegaSoil.withRoot.Profile, site_code, lyr_soc_stock_calc_sum),by="site_code") %>%
  mutate(socfrac = round((lyr_soc_stock_calc/lyr_soc_stock_calc_sum),2)) %>%
  group_by(site_code) %>%
  mutate(socfrac_cumsum = round(cumsum(socfrac),2))
View(somNEONMegaSoilSelSumDepth)

#combine the cumsum dataframes, now root cumsum and soc cumsum are in the same dataframe with exact layer_bot for the measures
somNEONMegaSoilRootSelSumDepth<- somNEONMegaSoilSelSumDepth %>% 
  rbind(somNEONMegaRootsSelSumDepth)

#calculate clay stocks
somNEONMegaSoilRootSelSumDepthClayStock <- somNEONMegaSoilRootSelSumDepth %>%
  mutate(lyr_claystock = clay*bd_samp*(layer_bot-layer_top))

#plots for root beta curves
somNEONMega1 <-  somNEONMegaSoilRootSelSumDepth %>%
  filter(site_code%in%neonSiteList1)
somNEONMega2 <- somNEONMegaSoilRootSelSumDepth %>%
  filter(!site_code%in%neonSiteList1)
ggplot(somNEONMega2, 
       aes(x = socfrac_cumsum, 
           y = layer_bot )) +
  geom_point(pch = 21, color="black") + 
  geom_point(aes(x=rootfrac_cumsum), color="blue")+
  scale_y_reverse() + # puts 0 at the top
  #scale_x_log10() +
  facet_wrap(~ site_code, scales = "free") +
  theme_bw() # save 6 x 12

#Jessica's stats section
library(lmerTest)
library(sjstats)
library(car)
#Objective 1: whole profile sum SOC ~ whole profile root SOC; site and network as 
#random effects; roots, landcover, nutrients, pH, texture as fixed effects 
#in full model
somNEONMegaSoilRootCovariates <- somNEONMegaSoilRootSelSumDepth %>% #grabbing covariates
  group_by(site_code, hzn_type) %>%
  summarize(mat = mean(mat, na.rm = T), 
            map = mean(map, na.rm = T), 
            clay = mean(clay, na.rm=T),
            layer_bot_max = max(layer_bot, na.rm=T),
            sand = mean(sand, na.rm=T))

clay<-somNEONMegaSoilRootSelSumDepth %>% #grabbing covariates
  group_by(site_code) %>%
  summarize(minclay = min(clay, na.rm=T),
            maxclay = max(clay, na.rm=T),
            stdevclay = sd(clay, na.rm=T))

somNEONMegaSoilRoot_wholeprofilestats <- somNEONMegaSoil.withRoot.Profile %>% #joining covariates to the bgb and soc summed dataframe
  left_join(somNEONMegaSoilRootCovariates, by="site_code")

#### AVNI'S AGU TALK: Results for objective 1
full.mod<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
             bgb_c_stock_sum + hzn_type + mat + map + clay +  (1|layer_bot_max)) # removed land cover as a fixed effect
summary(full.mod)
Anova(full.mod)
AIC(full.mod)
r2(full.mod)
vif(full.mod)
plot(full.mod) #plotting residuals from the full mixed model, should have no relationship
qqnorm(residuals(full.mod)) #checking normality of residuals, should be close to linear

### Results for Objective 2

full.mod<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum +  mat + map + clay +  (1|layer_bot_max)) # removed land cover as a fixed effect
summary(full.mod)
Anova(full.mod)
AIC(full.mod)
r2(full.mod)
vif(full.mod)
plot(full.mod) #plotting residuals from the full mixed model, should have no relationship
qqnorm(residuals(full.mod))

mod1<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum  + mat + map  + clay + (1|layer_bot_max))
summary(mod1)
Anova(mod1)
AIC(mod1)
r2(mod1)

mod2<- lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
               bgb_c_stock_sum  + mat + map  + (1|layer_bot_max))
summary(mod2)
Anova(mod2)
AIC(mod2)
r2(mod2)

#Final model NOTE: try clay stocks instead of average clay
mod3<- lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
              bgb_c_stock_sum  + mat + clay  + (1|layer_bot_max))
summary(mod3)
Anova(mod3)
AIC(mod3)
r2(mod3)

####### Figure 2, differences by horizon
# Filter out horizon

#cum sum for root C stocks
somNEONMegaRootsSelSumDepth <- somNEONMegaRootsSel %>% 
  #left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) %>% LANDCOVER, ARGGHH
  left_join(select(somNEONMegaSoil.withRoot.Profile, site_code, bgb_c_stock_sum),by="site_code") %>%
  mutate(rootfrac = round((bgb_c_stock/bgb_c_stock_sum),2)) %>%
  group_by(site_code) %>%
  mutate(rootfrac_cumsum = round(cumsum(rootfrac),2))
View(somNEONMegaRootsSelSumDepth)

#cum sum for SOC stocks
somNEONMegaSoilSelSumDepth <- somNEONMegaSoilSel %>% 
  #left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) %>% LANDCOVER
  left_join(select(somNEONMegaSoil.withRoot.Profile, site_code, lyr_soc_stock_calc_sum),by="site_code") %>%
  mutate(socfrac = round((lyr_soc_stock_calc/lyr_soc_stock_calc_sum),2)) %>%
  group_by(site_code) %>%
  mutate(socfrac_cumsum = round(cumsum(socfrac),2))
View(somNEONMegaSoilSelSumDepth)

#combine the cumsum dataframes, now root cumsum and soc cumsum are in the same dataframe with exact layer_bot for the measures
somNEONMegaSoilRootSelSumDepth<- somNEONMegaSoilSelSumDepth %>% 
  rbind(somNEONMegaRootsSelSumDepth)

#Avni's beta curve
tgc <- (somNEONMegaSoilRootSelSumDepth[somNEONMegaSoilRootSelSumDepth$layer_bot!=0&!is.na(somNEONMegaSoilRootSelSumDepth$layer_bot),])
## I do "is.na and Depth!=0" just because I had a depth that was zero and the program didn't like it. Also I had NAs
tgc_site<-filter(tgc, site_code=="BART")

###Y(cumulative percent) = 1- Beta d(depth)
library(minqa)

#beta <- initialize as no of sites
#n <- no sites

n <- 1
beta <- 1

for (i in 1:n) {
  beta[i] <- bobyqa(0.9,min.rss,0.6,1)$par
}


min.rss <- function(beta){
  x = tgc_site$rootfrac_cumsum
  y = tgc_site$layer_bot
  sum((x-y)^2, na.rm=T)
}
beta <- bobyqa(0.9,min.rss,0.6,1)$par
tgc$pred <- 100*(1-beta^tgc$layer_bot)


