##################################
### Data Exploration: LTER-SOM ###
###    Sept 2019, SWeintraub   ###
##################################
#Avni edits
### Reset workspace
rm(list = ls())

### Set paths
if (file.exists('/Users/sweintraub/')){
  dir1 <- ("/Users/sweintraub/Box/Conferences_Meetings/2018_LTER-SOM") 
}

if (file.exists('/Users/JM_1/')){
  dir1 <- ("~/Documents/GitHub/lterwg-som/root_group_analyses") 
}

### Load data
som <- readRDS(paste(dir1, "somCompositeData_2019-10-13.rds", sep = "/"))
landCov <- read.csv(paste(dir1, "NEONtowerSiteMetadata.csv", sep = "/"))

# filter to only NEON
somNEON <- filter(som, network == "NEON")

# useful function for making the DF smaller, only keep variables with not all NA
not_all_na <- function(x) any(!is.na(x))

# Megapits - make plots, carbon with depth, roots and soil
somNEONMegaRoots <- somNEON %>%
  filter(data_file%in%c("megapit_roots"),
         bgb_upperdiam%in%c("2","4"), 
         bgb_type == "live") %>%
  mutate(bgb_c = ifelse(is.na(bgb_c), 52, bgb_c),
         bgb_c_stock = bgb*(bgb_c*.01))
somNEONMegaSoil <- somNEON %>%
  filter(data_file%in%c("megapit_soils_all"))
somNEONMega <- bind_rows(somNEONMegaRoots, somNEONMegaSoil) %>%
  mutate(carbon_stock = ifelse(data_file=="megapit_roots", 
                               bgb_c_stock, 
                               lyr_soc_stock_calc/100)) %>% #divide by 100 was for graphing, don't do analyses on this
  select_if(not_all_na) %>%
  arrange(site_code)
sum(is.na(somNEONMega$carbon_stock)) # 6 rows have no C
somNEONMega1 <- somNEONMega %>%
  filter(site_code%in%c("BART", 
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
                        "DCFS"))
somNEONMega2 <- somNEONMega %>%
  filter(!site_code%in%c("BART", 
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
                        "DCFS"))
# data_file == "periodic_roots"
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
somNEONMegaRootsSel <- somNEON %>%
  filter(data_file%in%c("megapit_roots"),
         bgb_upperdiam%in%c("2","4"), # only keeping fine roots
         bgb_type == "live") %>% # might want to sum live and dead instead
  mutate(bgb_c = ifelse(is.na(bgb_c), 52, bgb_c),
         bgb_c_stock = bgb*(bgb_c*.01)) %>%
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
# Join to soil data
somNEONMegaSoil.withRoot <- somNEONMegaSoilSel %>%
  left_join(somNEONMegaRootsSel.byHor, by = c("site_code", "hzn")) %>%
  mutate(hzn_type = ifelse(grepl("^O", hzn), "organic", "mineral")) %>%
  left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID"))
# Whole profile summed
somNEONMegaSoil.withRoot.Profile <- somNEONMegaSoil.withRoot %>%
  group_by(site_code) %>%
  summarize(bgb_c_stock_sum = sum(bgb_c_stock, na.rm = T), #Jessica changed columns to _sum
            lyr_soc_stock_calc_sum = sum(lyr_soc_stock_calc, na.rm = T), 
            land_cover = first(Ecosystem.type))

### Plots ###
# By horizon
ggplot(somNEONMegaSoil.withRoot, aes(x = lyr_soc_stock_calc,
                                     y = bgb_c_stock,
                                     fill = Ecosystem.type)) + 
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

# Example for cumsum
totalStemCount <- dplyr::summarize(group_by(tableA, siteID), stems_total = sum(num_stems))
tableB <- left_join(tableA, totalStemCount, by = "siteID") %>%
  mutate(relativeAbundance = round((num_stems/stems_total)*100,2), 
         cumulativeAbundance = round(cumsum(relativeAbundance),2))

# write files
write.csv(somNEONRoots, paste(dir1, "Filtered_SOM_NEONroots_only.csv", sep = "/"))


# litter only
somNEONLitter <- somNEON %>%
  filter(!is.na(anpp)) %>% 
  select_if(not_all_na)
# roots only
somNEONRoots <- somNEON %>%
  filter(!is.na(bgb)) %>% 
  select_if(not_all_na)   




############## Jessica's exploration
#Objective 1: whole profile sum SOC ~ whole profile root SOC; site and network as random effects; roots, landcover, nutrients, pH, texture as fixed effects in full model

#Objective 2: Does SOC~roots vary with depth? First, run with depth as covariate in full model. Next, slice by depth intervals and re-run model.

#Objective 3: Does B(SOC~depth)~B(roots~depth)? With landcover, nutrients, soil texture as fixed effects.
#Calc cumsum using root data by depth interval, then calc SOC cumsum using the horizon data

#cum sum for root C stocks
somNEONMegaRootsSelSumDepth <- somNEONMegaRootsSel %>% 
  left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) %>%
  left_join(select(somNEONMegaSoil.withRoot.Profile, site_code, bgb_c_stock_sum),by="site_code") %>%
  mutate(rootfrac = round((bgb_c_stock/bgb_c_stock_sum),2)) %>%
  group_by(site_code) %>%
  mutate(rootfrac_cumsum = round(cumsum(rootfrac),2))
View(somNEONMegaRootsSelSumDepth)

#cum sum for SOC stocks
somNEONMegaSoilSelSumDepth <- somNEONMegaSoilSel %>% 
  left_join(select(landCov, ID, Ecosystem.type), by = c("site_code" = "ID")) %>%
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
  filter(site_code%in%c("BART", 
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
                        "DCFS"))
somNEONMega2 <- somNEONMegaSoilRootSelSumDepth %>%
  filter(!site_code%in%c("BART", 
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
                         "DCFS"))
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
  group_by(site_code) %>%
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

full.mod<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
             bgb_c_stock_sum + land_cover + mat + map + clay + sand + (1|layer_bot_max))
summary(full.mod)
Anova(full.mod)
AIC(full.mod)
r2(full.mod)

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

#Avni's beta curve
tgc <- (somNEONMegaSoilRootSelSumDepth[somNEONMegaSoilRootSelSumDepth$layer_bot!=0&!is.na(somNEONMegaSoilRootSelSumDepth$layer_bot),])
## I do "is.na and Depth!=0" just because I had a depth that was zero and the program didn't like it. Also I had NAs
tgc_site<-filter(tgc, site_code=="ABBY")

###Y(cumulative percent) = 1- Beta ^ d(depth)
library(minqa)

# a function to calculate beta for each site
min.rss <- function(beta){
  x = tgc_site$rootfrac_cumsum
  y = 1-beta^tgc_site$layer_bot
  sum((x-y)^2,na.rm=T)
}
beta <- bobyqa(0.9,min.rss,0.01,1)$par
tgc$pred <- 100*(1-beta^tgc$layer_bot)

tgc_betas<- tgc %>% group_by(site_code) %>%
  summarize(beta = bobyqa(0.9,min.rss,0.01,1)$par) #this is calculating the same beta for all sites. why?
