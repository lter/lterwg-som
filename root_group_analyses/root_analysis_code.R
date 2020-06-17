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

if (file.exists('/Users/5a7/')){
  dir1 <- ("/Users/5a7/Documents/GitHub/lterwg-som/") 
}

if (file.exists('C:/Users/vishr_000')){
  dir1 <- ("C:/Users/vishr_000/Documents/GitHub/lterwg-som/") 
}
dir1

### Load data - if downloaded
#som <- readRDS(paste(dir1, "root_group_analyses/somCompositeData_2019-10-15.rds", sep = "/"))
#landCov <- read.csv(paste(dir1, "root_group_analyses/NEONtowerSiteMetadata.csv", sep = "/"))#because the updated file already has landcover

### Load data - straight from google drive
library(googledrive)
library(tidyverse)
library(dplyr)
library(ggplot2)
source(paste0(dir1,'data-processing/get_latest_som.R'))
som <- get_latest_som() #HINT: Select 0 and re-authorize the google api each time. Not sure why the pre-auth isn't working.

# filter to only NEON
somNEON <- filter(som, network == "NEON")
landCov <- select(somNEON, land_cover, site_code)

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
         bgb_c_stock = bgb*(bgb_c*.01)) # c_stock is (g root C)/m2

somNEONMegaSoil <- somNEON %>%
  filter(data_file%in%c("megapit_soils_all"))

# Putting roots and soil together into one column
somNEONMega <- bind_rows(somNEONMegaRoots, somNEONMegaSoil) %>%
  mutate(carbon_stock = ifelse(data_file=="megapit_roots", 
                               bgb_c_stock, 
                               lyr_soc_stock_calc)) %>% #divide by 100 for graphing, but do analyses without dividing by 100; lyr_soc_stock_calc is (g C)/m2
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

# Join to soil data - **********USE THIS DF TO COMPARE OTHER EDAPHIC VARS TO ROOTS*************
somNEONMegaSoil.withRoot <- somNEONMegaSoilSel %>%
  left_join(somNEONMegaRootsSel.byHor, by = c("site_code", "hzn")) %>%
  mutate(hzn_type = ifelse(grepl("^O", hzn), "organic", "mineral"))

# Whole profile summed (summed across combined organic and mineral horizons)
# Covariates are added to the df at a later step.
somNEONMegaSoil.withRoot.Profile <- somNEONMegaSoil.withRoot %>%
  group_by(site_code) %>% 
  summarize(bgb_c_stock_sum = sum(bgb_c_stock, na.rm = T), 
            lyr_soc_stock_calc_sum = sum(lyr_soc_stock_calc, na.rm = T), 
            land_cover = first(land_cover),
            eco_region = first(eco_region)) %>%
  filter(bgb_c_stock_sum > 1)

# Whole profile summed (only for mineral horizons)
# Covariates are added to the df at a later step.
somNEONMegaSoil.withRoot.Profile.min <- somNEONMegaSoil.withRoot %>%
  filter(hzn_type=="mineral") %>%
  group_by(site_code) %>% 
  summarize(bgb_c_stock_sum = sum(bgb_c_stock, na.rm = T), 
            lyr_soc_stock_calc_sum = sum(lyr_soc_stock_calc, na.rm = T), 
            land_cover = first(land_cover),
            eco_region = first(eco_region)) %>%
  filter(bgb_c_stock_sum > 1)

### Quick Plots ###
# One point for each layer at each site
ggplot(somNEONMegaSoil.withRoot, aes(y = lyr_soc_stock_calc,
                                     x = bgb_c_stock)) +
                                     #fill = Ecosystem.type)) + #Need to fix landCov and Ecosystem.type
  geom_point(pch = 21, size = 2) + 
  theme_bw() # save 4 x 6

# One point for each whole profile summed for each site, profile includes both organic and mineral horizons
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

#cum sum for root C stocks, contains cumulative fractions for soc & bgb and also whole profile sums
somNEONMegaRootsSelSumDepth <- somNEONMegaRootsSel %>% 
  left_join(dplyr::select(somNEONMegaSoil.withRoot.Profile, site_code, bgb_c_stock_sum),by="site_code") %>%
  mutate(rootfrac = round((bgb_c_stock/bgb_c_stock_sum),2)) %>%
  group_by(site_code) %>%
  mutate(rootfrac_cumsum = round(cumsum(rootfrac),2))
View(somNEONMegaRootsSelSumDepth)

#cum sum for SOC stocks
somNEONMegaSoilSelSumDepth <- somNEONMegaSoilSel %>% 
  left_join(dplyr::select(somNEONMegaSoil.withRoot.Profile, site_code, lyr_soc_stock_calc_sum),by="site_code") %>%
  mutate(socfrac = round((lyr_soc_stock_calc/lyr_soc_stock_calc_sum),2)) %>%
  group_by(site_code) %>%
  filter(!is.na(socfrac)) %>%
  mutate(socfrac_cumsum = round(cumsum(socfrac),2))
View(somNEONMegaSoilSelSumDepth)

#combine the cumsum dataframes, now root cumsum and soc cumsum are in the same dataframe with exact layer_bot for the measures
somNEONMegaSoilRootSelSumDepth<- somNEONMegaSoilSelSumDepth %>% 
  rbind(somNEONMegaRootsSelSumDepth) #%>%
  filter(!is.na(bgb_c_stock))

#pre-lim quick plots for root beta curves
somNEONMega1 <-  somNEONMegaSoilRootSelSumDepth %>%
  filter(site_code%in%neonSiteList1) %>%
  arrange(land_cover)

mod_root<-lm(data=somNEONMegaSoilRootSelSumDepth, rootfrac_cumsum~layer_bot)

somNEONMega2 <- somNEONMegaSoilRootSelSumDepth %>%
  filter(!site_code%in%neonSiteList1)
ggplot(somNEONMega1, 
       aes(x = socfrac_cumsum, 
           y = layer_bot )) +
  geom_point(pch = 21, color="black") + 
  geom_point(aes(x=rootfrac_cumsum), color="blue")+
  #geom_spline(aes(x=rootfrac_cumsum), color="blue")+ #formula = y ~ splines::bs(x, 2)
  geom_smooth(method=lm,color="black")+
  scale_y_reverse() + # puts 0 at the top
  #scale_x_log10() +
  facet_wrap(~ land_cover, scales = "free") +
  theme_bw() # save 6 x 12

#Jessica's stats section
library(lmerTest) # provides sig test for lmer models
library(sjstats) #for psuedo-R2 in lmer models
library(car) #for Anova


#Objective 1a: whole profile sum SOC ~ whole profile root SOC; layer_bot as random effect
# This is for O+M horizons.
# roots, landcover, MAT, MAP as fixed effects
# We want to include nutrients, pH, and texture as fixed effects but unsure how to represent them across a whole profile. Summing doesn't seem appropriate.

#Calculating covariates
somNEONMegaSoilRootCovariates <- somNEONMegaSoil.withRoot %>% # somNEONMegaSoilRootSelSumDepth %>% 
  group_by(site_code) %>%
  summarize(mat = mean(mat, na.rm = T), 
            map = mean(map, na.rm = T), 
            clay = mean(clay, na.rm=T),
            layer_bot_max = max(layer_bot, na.rm=T),
            veg_note_profile = first(veg_note_profile))
            #eco_region = )) # FIX THIS! I want eco type or region or biome in here

# Join covariates 
somNEONMegaSoilRoot_wholeprofilestats <- somNEONMegaSoil.withRoot.Profile %>% 
  left_join(somNEONMegaSoilRootCovariates, by="site_code") %>%
  filter(!is.na(veg_note_profile)) %>%
  mutate(bgb_c_stock_norm = (bgb_c_stock_sum/layer_bot_max)*200,
         soc_c_stock_norm = (lyr_soc_stock_calc_sum/layer_bot_max)*200)

#Add mycorrhizal type from Myco Database, cite Chaudhary, V., Rúa, M., Antoninka, A. et al. MycoDB, a global database of plant response to mycorrhizal fungi. Sci Data 3, 160028 (2016). https://doi.org/10.1038/sdata.2016.28, Chaudhary, V. Bala et al. (2017), Data from: MycoDB, a global database of plant response to mycorrhizal fungi, v4, Dryad, Dataset, https://doi.org/10.5061/dryad.723m1
mycodb<-read.csv("MycoDB_version4.csv")
mycodb_sum<- mycodb %>% filter(!is.na(MYCORRHIZAETYPE)) %>%
  filter(!is.na(PlantSpecies2018)) %>%
  group_by(PlantSpecies2018) %>%
  summarize(myc = first(MYCORRHIZAETYPE))

#re-formatting our df to match the mycodb_sum df
somNEONMegaSoilRoot_wholeprofilestats_myc<-separate(somNEONMegaSoilRoot_wholeprofilestats, col="veg_note_profile", remove=F, sep=", ", into=c("veg1","veg2","veg3"), extra="warn", fill="warn")
somNEONMegaSoilRoot_wholeprofilestats_myc$veg1<-gsub(" ", "_", somNEONMegaSoilRoot_wholeprofilestats_myc$veg1) #placing underscores between genus and species names
somNEONMegaSoilRoot_wholeprofilestats_myc$veg2<-gsub(" ", "_", somNEONMegaSoilRoot_wholeprofilestats_myc$veg2)
somNEONMegaSoilRoot_wholeprofilestats_myc$veg3<-gsub(" ", "_", somNEONMegaSoilRoot_wholeprofilestats_myc$veg3)
somNEONMegaSoilRoot_wholeprofilestats_myc$veg1<-tolower(somNEONMegaSoilRoot_wholeprofilestats_myc$veg1)
somNEONMegaSoilRoot_wholeprofilestats_myc$veg2<-tolower(somNEONMegaSoilRoot_wholeprofilestats_myc$veg2)
somNEONMegaSoilRoot_wholeprofilestats_myc$veg3<-tolower(somNEONMegaSoilRoot_wholeprofilestats_myc$veg3)
write.csv(somNEONMegaSoilRoot_wholeprofilestats_myc, "somNEONMegaSoilRoot_wholeprofilestats_myc.csv")
somNEONMegaSoilRoot_wholeprofilestats_myc<- read.csv("somNEONMegaSoilRoot_wholeprofilestats_myc.csv")

somNEONMegaSoilRoot_wholeprofilestats_mycjoin<-somNEONMegaSoilRoot_wholeprofilestats_myc %>%
  left_join(dplyr::select(mycodb_sum, PlantSpecies2018, myc), by=c("veg1"="PlantSpecies2018"))
write.csv(somNEONMegaSoilRoot_wholeprofilestats_mycjoin, "somNEONMegaSoilRoot_wholeprofilestats_mycjoin.csv")
somNEONMegaSoilRoot_wholeprofilestats_mycjoin<-read.csv("somNEONMegaSoilRoot_wholeprofilestats_mycjoin.csv")

#remove shrublands
somNEONMegaSoilRoot_wholeprofilestats_noshrub <- somNEONMegaSoilRoot_wholeprofilestats %>% 
  filter(land_cover!="shrubland")
#remove the three high root outliers
somNEONMegaSoilRoot_wholeprofilestats_NoOut <-somNEONMegaSoilRoot_wholeprofilestats %>% 
  filter(!site_code %in% c("HEAL","BARR","WREF")) 




#### AVNI'S AGU TALK
### Results for Objective 1: Whole profile summed SOC correlated with Roots (whole profile summed) and other covariates
null.mod <- lmer(data=somNEONMegaSoilRoot_wholeprofilestats_NoOut, lyr_soc_stock_calc_sum ~ (1|layer_bot_max))

full.mod<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats_NoOut, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum + mat  + clay + land_cover + myc + (1|layer_bot_max)) #too many eco_regions to analyze
summary(full.mod)
Anova(full.mod)
anova(full.mod, null.mod)
AIC(full.mod)
performance::r2(full.mod)
vif(full.mod)
plot(full.mod) #plotting residuals from the full mixed model, should have no relationship
qqnorm(residuals(full.mod)) #checking normality of residuals, should be close to linear

#Reduced model, dropping MAP because it is not significant, making land_cover a random effect
reduced.mod2<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats_NoOut, lyr_soc_stock_calc_sum ~ 
                     mat  + (1|layer_bot_max)) 
summary(reduced.mod2)
Anova(reduced.mod2)
anova(null.mod, reduced.mod2)
AIC(reduced.mod)
performance::r2(reduced.mod2)

##Can we get a partial correlation coefficient in the mixed model?
vif(reduced.mod)
plot(reduced.mod) #plotting residuals from the full mixed model, should have no relationship
qqnorm(residuals(reduced.mod)) #checking normality of residuals, should be close to linear

anova(null.mod, reduced.mod2)

#Obj 1b: using a multiple regression instead of mixed model to directly test layer_bot_max effect
mod<-lm(data=somNEONMegaSoilRoot_wholeprofilestats_NoOut, lyr_soc_stock_calc_sum ~ bgb_c_stock_sum)
summary(mod)
Anova(mod)
#calculating partial correlation coefficients, Greene, W.H. (2005). Econometric Analysis. 5th ed. Pearson Education.
t.values <- mod$coeff / sqrt(diag(vcov(mod)))
partcorr <- sqrt((t.values^2) / ((t.values^2) + mod$df.residual))
partcorr

#Alternative analysis: regualar linear regression, using soc and bgb normalized for depth. Whole profile measures of bgb and soc were summed, divided by layer_bot_max, and multiplied by 200 cm to get back to a stock
mod<-lm(data=somNEONMegaSoilRoot_wholeprofilestats_noshrubtundra, soc_c_stock_norm ~ bgb_c_stock_norm)
summary(mod)
Anova(mod)

#filter out tundra bc it's an outlier
somNEONMegaSoilRoot_wholeprofilestats_noshrubtundra <- filter(somNEONMegaSoilRoot_wholeprofilestats_noshrub, !eco_region=="tundra")

#Figure 1
write.csv(somNEONMegaSoilRoot_wholeprofilestats, "somNEONMegaSoilRoot_wholeprofilestats.csv")
fig1_ecoreg <- ggplot(data=somNEONMegaSoilRoot_wholeprofilestats, aes(x=bgb_c_stock_norm/1000, y=soc_c_stock_norm/1000))+
  geom_smooth(method=lm, color="black")+
  geom_point(pch=21, size=3, aes(fill = eco_region))+ # alternatively, add aes(fill=land_cover)
  scale_fill_manual(values=c("darkgreen","tan4","springgreen1","sienna3","tan","dodgerblue","gray"))+
  xlab(bquote(Whole-profile~root~biomass~(kg~C~m^-2)))+
  ylab(bquote(Whole-profile~soil~organic~C~(kg~C~m^-2)))+
  guides(fill=guide_legend(title="Land Cover", ncol=1, title.theme = element_text(size=14, angle=0), label.theme = element_text(size=14, angle=0)))+
  theme(legend.position="right", panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=16),axis.text=element_text(size=14))
fig1_ecoreg
ggsave(plot=fig1_ecoreg, filename="Fig1_wholeprof_ecoregion_norm.jpeg", dpi=300)

#Alternate Figure 1
fig1_clay <- ggplot(data=somNEONMegaSoilRoot_wholeprofilestats, aes(x=bgb_c_stock_sum/1000, y=lyr_soc_stock_calc_sum/1000))+
  geom_point(aes(size=clay), pch=19)+
  geom_smooth(method=lm, color="black")+
  xlab(bquote(Whole-profile~root~biomass~(kg~C~m^-2)))+
  ylab(bquote(Whole-profile~soil~organic~C~(kg~C~m^-2)))+
  guides(size=guide_legend(title="Percent Clay", title.theme = element_text(size=14, angle=0), label.theme = element_text(size=12, angle=0)))+
  theme(legend.position=c(0.9,0.15), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=16),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14))
fig1_clay
ggsave(plot=fig1_clay, filename="wholeprof_SOC_bgb_clay.jpeg", dpi=300)


### Results for Objective 2: Horizon-specific whole profile sums

#Dataframe with site_code, hzn_type, bgb_c_stock_sum, and lyr_soc_stock_calc_sum
somNEONMegaSoil.withRoot.Profile.hzn <- somNEONMegaSoil.withRoot %>%
  group_by(site_code, hzn_type) %>% 
  summarize(bgb_c_stock_sum = sum(bgb_c_stock, na.rm = T), 
            lyr_soc_stock_calc_sum = sum(lyr_soc_stock_calc, na.rm = T),
            land_cover = first(land_cover)) 

#Grab covariates
somNEONMegaSoilRootCovariates.hzn <- somNEONMegaSoil.withRoot %>% 
  group_by(site_code, hzn_type) %>%
  summarize(mat = mean(mat, na.rm = T), 
            map = mean(map, na.rm = T), 
            clay = mean(clay, na.rm=T), #clay is averaged across the whole profile 
            layer_bot_max = max(layer_bot, na.rm=T)) #max depth is a random effect

#Join the horizon-specific profile sums to the covariate table
somNEONMegaSoil.withRoot.Profile.hzn.stats <- somNEONMegaSoil.withRoot.Profile.hzn %>%
  left_join(somNEONMegaSoilRootCovariates.hzn, by=c("site_code", "hzn_type"))

#split the dataframes
somNEON_organic_wholeprofile <- filter(somNEONMegaSoil.withRoot.Profile.hzn.stats, hzn_type=="organic")
somNEON_mineral_wholeprofile <- filter(somNEONMegaSoil.withRoot.Profile.hzn.stats, hzn_type=="mineral")

#Organic horizon models
null.mod <- lmer(data=somNEON_organic_wholeprofile, lyr_soc_stock_calc_sum ~ (1|layer_bot_max))

full.mod<-lmer(data=somNEON_organic_wholeprofile, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum + land_cover + mat + map +(1|layer_bot_max)) #map has been omitted because the model kept failing to converge
reduced.mod<-lmer(data=somNEON_organic_wholeprofile, lyr_soc_stock_calc_sum ~ bgb_c_stock_sum + mat + land_cover +
                    (1|layer_bot_max))

summary(reduced.mod)
anova(reduced.mod, null.mod)
AIC(reduced.mod)
performance::performance_aicc(full.mod)
performance::r2(reduced.mod)
car::vif(full.mod)

#Mineral horizon models
null.mod <- lmer(data=somNEON_mineral_wholeprofile, lyr_soc_stock_calc_sum ~ (1|layer_bot_max))

full.mod<-lmer(data=somNEON_mineral_wholeprofile, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum + map + mat + clay + land_cover + (1|layer_bot_max))

reduced.mod<-lmer(data=somNEON_mineral_wholeprofile, lyr_soc_stock_calc_sum ~ 
                    bgb_c_stock_sum + mat + clay + land_cover  + (1|layer_bot_max)) 
summary(full.mod)
Anova(full.mod)
anova(full.mod, null.mod)
AIC(full.mod)
performance::r2(full.mod)
vif(full.mod)

#Figure 2 NOT USED ANYMORE 5-13-20
fig2_landcov <- ggplot(data=somNEON_organic_wholeprofile, aes(x=bgb_c_stock_sum/1000, y=lyr_soc_stock_calc_sum/1000))+
  geom_smooth(method=lm, color="black")+
  geom_point(aes(fill=land_cover), pch=21, size=3)+
  xlab(bquote(Organic-profile~root~biomass~(kg~C~m^-2)))+
  ylab(bquote(Organic-profile~soil~organic~C~(kg~C~m^-2)))+
  guides(fill=guide_legend(title="Land Cover", ncol=2, title.theme = element_text(size=12, angle=0), label.theme = element_text(size=12, angle=0)))+
  theme(legend.position=c(0.3,0.85), legend.box.background = element_rect(color = "black"), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=14),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
fig2_landcov
ggsave(plot=fig2_landcov, filename="orgprof_SOC_bgb_landcov.jpeg", dpi=300)

#Alternate Figure 2 NOT USED ANYMORE 5-13-20
fig2_clay <- ggplot(data=somNEON_organic_wholeprofile, aes(x=bgb_c_stock_sum/1000, y=lyr_soc_stock_calc_sum/1000))+
  geom_point(aes(size=clay), pch=19)+
  geom_smooth(method=lm, color="black")+
  xlab(bquote(Organic-profile~root~biomass~(kg~C~m^-2)))+
  ylab(bquote(Organic-profile~soil~organic~C~(kg~C~m^-2)))+
  guides(size=guide_legend(title="Percent Clay", title.theme = element_text(size=12, angle=0), label.theme = element_text(size=12, angle=0)))+
  theme(legend.position=c(0.1,0.85), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=14),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
fig2_clay
ggsave(plot=fig2_clay, filename="orgprof_SOC_bgb_clay.jpeg", dpi=300)

#Are O and M horizons different?
mod<-lmer(data=somNEONMegaSoil.withRoot.Profile.hzn.stats, lyr_soc_stock_calc_sum ~ 
            bgb_c_stock_sum + hzn_type +(1|layer_bot_max))
emmeans(mod, pairwise~hzn_type, adjust="tukey")

#Figure 2
fig3_hzn <- ggplot(data=somNEONMegaSoil.withRoot.Profile.hzn.stats, aes(x=bgb_c_stock_sum/1000, y=lyr_soc_stock_calc_sum/1000))+
  geom_smooth(method=lm, aes(color=hzn_type), alpha=0.3)+
  geom_point(aes(fill = hzn_type), pch=21, size=4)+
  scale_fill_manual(values=c("black","gray"))+
  scale_color_manual(values=c("black","gray"), guide="none")+
  xlab(bquote(Root~biomass~(kg~C~m^-2)))+
  ylab(bquote(Soil~organic~C~(kg~C~m^-2)))+
  guides(fill=guide_legend(title="Horizon", title.theme = element_text(size=14, angle=0), label.theme = element_text(size=14, angle=0)))+
  theme(legend.position=c(0.85,0.85), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=16),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14))
fig3_hzn
ggsave(plot=fig3_hzn, filename="Fig2_hzn_SOC_bgb.jpeg", dpi=300)

### Objective 3: beta curves for roots and SOC
####### Cum sum for betas
# Idea: Filter out horizon first, do only on mineral?

#Calculating betas for each site
###Y(cumulative percent) = 1- Beta d(depth)
library(minqa)

# a function to calculate root beta for each site
  min.rss.roots <- function(beta){
    x = somNEONMegaSoilRootSelSumDepth_site$rootfrac_cumsum #I replaced tgc_site with somNEONMegaSoilRootSelSumDepth_site
    y = 1-beta^somNEONMegaSoilRootSelSumDepth_site$layer_bot
    sum((x-y)^2,na.rm=T)
  }
 
# a function to calculate soil beta for each site
  min.rss.soc <- function(beta){
    x = somNEONMegaSoilRootSelSumDepth_site$socfrac_cumsum
    y = 1-beta^somNEONMegaSoilRootSelSumDepth_site$layer_bot
    sum((x-y)^2,na.rm=T)
  }
  

#a loop for calculating betas for each site
  results.list = list()
  for (site in somNEONMegaSoilRootSelSumDepth$site_code) {
    
    somNEONMegaSoilRootSelSumDepth_site <- filter(somNEONMegaSoilRootSelSumDepth, site_code == site)
    beta_site_roots <- bobyqa(0.1,min.rss.roots,0.01,1)$par
    beta_site_soc <- bobyqa(0.1,min.rss.soc,0.01,1)$par
    lhs <- somNEONMegaSoilRootSelSumDepth_site$rootfrac_cumsum
    rhs <- 1-beta_site_roots^somNEONMegaSoilRootSelSumDepth_site$layer_bot
    r2_site_roots <- summary(lm(lhs ~ rhs))$r.squared
    lhs <- somNEONMegaSoilRootSelSumDepth_site$socfrac_cumsum
    rhs <- 1-beta_site_soc^somNEONMegaSoilRootSelSumDepth_site$layer_bot
    r2_site_soc <- summary(lm(lhs ~ rhs))$r.squared
    results.list[[site]] = tibble(beta_roots = beta_site_roots,
                                  beta_soc = beta_site_soc,
                                  r2_roots = r2_site_roots,
                                  r2_soc = r2_site_soc,
                                  site_code = site)
  }
  
  beta.all <- bind_rows(results.list)
  View(beta.all)
  plot(beta.all$beta_roots,beta.all$beta_soc)
  
  #a loop for calculating betas for each site ****FORCING (0,0)****
write.csv(somNEONMegaSoilRootSelSumDepth, "somNEONMegaSoilRootSelSumDepth.csv") #I couldn't figure out add_row with a grouped df, so I opened Excel and did it
somNEONMegaSoilRootSelSumDepthZeros <- read.csv("somNEONMegaSoilRootSelSumDepth.csv")
# a function to calculate root beta for each site with Zeros
min.rss.roots.zeros <- function(beta){
  x = somNEONMegaSoilRootSelSumDepthZeros_site$rootfrac_cumsum #I replaced tgc_site with somNEONMegaSoilRootSelSumDepth_site
  y = 1-beta^somNEONMegaSoilRootSelSumDepthZeros_site$layer_bot
  sum((x-y)^2,na.rm=T)
}

# a function to calculate soil beta for each site
min.rss.soc.zeros <- function(beta){
  x = somNEONMegaSoilRootSelSumDepthZeros_site$socfrac_cumsum
  y = 1-beta^somNEONMegaSoilRootSelSumDepthZeros_site$layer_bot
  sum((x-y)^2,na.rm=T)
}
  
  results.list = list()
  for (site in somNEONMegaSoilRootSelSumDepthZeros$site_code) {
    
    somNEONMegaSoilRootSelSumDepthZeros_site <- filter(somNEONMegaSoilRootSelSumDepthZeros, site_code == site)
    beta_site_roots <- bobyqa(0.1,min.rss.roots.zeros,0.01,1)$par
    beta_site_soc <- bobyqa(0.1,min.rss.soc.zeros,0.01,1)$par
    lhs <- somNEONMegaSoilRootSelSumDepthZeros_site$rootfrac_cumsum
    rhs <- 1-beta_site_roots^somNEONMegaSoilRootSelSumDepthZeros_site$layer_bot
    r2_site_roots <- summary(lm(lhs ~ rhs))$r.squared
    lhs <- somNEONMegaSoilRootSelSumDepthZeros_site$socfrac_cumsum
    rhs <- 1-beta_site_soc^somNEONMegaSoilRootSelSumDepthZeros_site$layer_bot
    r2_site_soc <- summary(lm(lhs ~ rhs))$r.squared
    results.list[[site]] = tibble(beta_roots = beta_site_roots,
                                  beta_soc = beta_site_soc,
                                  r2_roots = r2_site_roots,
                                  r2_soc = r2_site_soc,
                                  site_code = site)
  }
  
  beta.all.zeros <- bind_rows(results.list)
  View(beta.all.zeros)
  plot(beta.all.zeros$beta_roots,beta.all.zeros$beta_soc)
  
# Calculating betas for M horizons separately
somNEONMegaSoilRootSelSumDepth_hzn <- somNEONMegaSoilRootSelSumDepth %>%
  mutate(hzn_type = ifelse(grepl("^O", hzn), "organic", "mineral"))
somNEONMegaSoilRootSelSumDepth_M <- filter(somNEONMegaSoilRootSelSumDepth_hzn, hzn_type=="mineral")
  
# a function to calculate root beta for each site **M horizon**
min.rss.roots <- function(beta){
  x = somNEONMegaSoilRootSelSumDepth_M_site$rootfrac_cumsum #I replaced tgc_site with somNEONMegaSoilRootSelSumDepth_site
  y = 1-beta^somNEONMegaSoilRootSelSumDepth_M_site$layer_bot
  sum((x-y)^2,na.rm=T)
}

# a function to calculate soil beta for each site **M horizon**
min.rss.soc <- function(beta){
  x = somNEONMegaSoilRootSelSumDepth_M_site$socfrac_cumsum
  y = 1-beta^somNEONMegaSoilRootSelSumDepth_M_site$layer_bot
  sum((x-y)^2,na.rm=T)
}


#a loop for calculating betas for each site **O horizon**
results.list = list()
for (site in somNEONMegaSoilRootSelSumDepth_M$site_code) {
  
  somNEONMegaSoilRootSelSumDepth_M_site <- filter(somNEONMegaSoilRootSelSumDepth_M, site_code == site)
  beta_site_roots <- bobyqa(0.1,min.rss.roots,0.01,1)$par
  beta_site_soc <- bobyqa(0.1,min.rss.soc,0.01,1)$par
  lhs <- somNEONMegaSoilRootSelSumDepth_M_site$rootfrac_cumsum
  rhs <- 1-beta_site_roots^somNEONMegaSoilRootSelSumDepth_M_site$layer_bot
  r2_site_roots <- summary(lm(lhs ~ rhs))$r.squared
  lhs <- somNEONMegaSoilRootSelSumDepth_M_site$socfrac_cumsum
  rhs <- 1-beta_site_soc^somNEONMegaSoilRootSelSumDepth_M_site$layer_bot
  r2_site_soc <- summary(lm(lhs ~ rhs))$r.squared
  results.list[[site]] = tibble(beta_roots = beta_site_roots,
                                beta_soc = beta_site_soc,
                                r2_roots = r2_site_roots,
                                r2_soc = r2_site_soc,
                                site_code = site)
}

beta.all.M <- bind_rows(results.list)
View(beta.all.M)
plot(beta.all.M$beta_roots,beta.all.M$beta_soc)


#Adding covariates and making a csv file to store the beta values 
somNEONMegaSoilRootCovariates.ldcv <- somNEONMegaSoilRootCovariates %>%
  left_join(select(somNEONMega, land_cover, site_code), by="site_code") %>%
  group_by(site_code) %>%
  summarize(land_cover = first(land_cover),
            mat = mean(mat),
            map = mean(map),
            clay = mean(clay),
            layer_bot_max = mean(layer_bot_max))

beta.all<-beta.all %>%
  left_join(somNEONMegaSoilRootCovariates.ldcv, by= "site_code")%>%
  filter(!site_code%in%c("CLBJ","JORN","GUAN","LAJA","GRSM","TEAK","BARR")) #these sites have betaSOC = 0.1
write.csv(beta.all, "beta.all_022720.csv")
beta.all<-read.csv("beta.all_022720.csv") 

beta.all.M<-beta.all.M %>%
  left_join(somNEONMegaSoilRootCovariates.ldcv, by= "site_code")%>%
  filter(!site_code%in%c("SOAP", "BONA", "DEJU", "HEAL", "MLBS","ABBY","WREF","CLBJ","JORN","GUAN","LAJA","GRSM","TEAK","BARR")) #these sites have betaSOC = 0.1
write.csv(beta.all.M, "beta.all.M_022820.csv")
beta.all.M<-read.csv("beta.all.M_022820.csv") 

beta.all.zeros<-beta.all.zeros %>%
  left_join(somNEONMegaSoilRootCovariates.ldcv, by= "site_code")%>%
  filter(!site_code%in%c("CLBJ","JORN","GUAN","LAJA","GRSM","TEAK","BARR")) #these sites have betaSOC = 0.1
write.csv(beta.all.zeros, "beta.all.zeros_052820.csv")

#quick summary stats
beta.stats<-beta.all %>% filter(land_cover %in% c("forest","rangeland/grassland")) %>%
  group_by(land_cover) %>%
  summarize(mean.soc = mean(beta_soc, na.rm=T),
            stdev.soc = sd(beta_soc,na.rm=T),
            mean.roots = mean(beta_roots,na.rm=T),
            stdev.roots = sd(beta_roots,na.rm=T)) %>%
  mutate(mean.soc = round(mean.soc, 3),
         stdev.soc = round(stdev.soc,3),
         mean.roots = round(mean.roots,3),
         stdev.roots = round(stdev.roots,3))
beta.stats$x<-c(0.45,0.45)
beta.stats$y.roots<-c(175,175)
beta.stats$y.soc<-c(200,200)
  
#Fig 3. the beta-by-beta plot
beta.landcov<-ggplot(data=beta.all, aes(x=beta_roots, y=beta_soc))+
  geom_abline(slope=1,intercept=0, lty="dashed")+  
  geom_point(aes(fill=land_cover), pch=21, size=3)+ 
  scale_fill_manual(values=c("orchid", "springgreen3","coral","dodgerblue"))+
  xlab(bquote(beta~Fine~root~biomass))+
  ylab(bquote(beta~SOC~stock))+
  guides(fill=guide_legend(title="Land cover", title.theme = element_text(size=14, angle=0), label.theme = element_text(size=14, angle=0)))+
  theme_bw()+
  theme(legend.position=c(0.15,0.15), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=16),axis.text=element_text(size=14))
beta.landcov
ggsave(plot=beta.landcov, file="beta.landcov.jpeg", dpi=300)

beta.landcov.M<-ggplot(data=beta.all.M, aes(x=beta_roots, y=beta_soc))+
  geom_abline(slope=1,intercept=0, lty="dashed")+  
  geom_point(aes(fill=land_cover), pch=21, size=3)+ 
  scale_fill_manual(values=c("orchid", "springgreen3","coral","dodgerblue"))+
  xlab(bquote(beta~Fine~root~biomass~(mineral~horizons)))+
  ylab(bquote(beta~SOC~stock~(mineral~horizons)))+
  guides(fill=guide_legend(title="Land cover", title.theme = element_text(size=14, angle=0), label.theme = element_text(size=14, angle=0)))+
  theme_bw()+
  theme(legend.position=c(0.15,0.15), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=16),axis.text=element_text(size=14))
beta.landcov.M
ggsave(plot=beta.landcov.M, file="beta.landcov.M.jpeg", dpi=300)

#Site-specific plots with root and soc beta curves
  #lhs <- tgc_site$socfrac_cumsum
  #rhs <- 1-beta_site_soc^tgc_site$layer_bot
  #site_soc <- lm(lhs ~ rhs)
  #pred_soc<-predict(site_soc)
  
  #lhs <- tgc_site$rootfrac_cumsum
  #rhs <- 1-beta_site_roots^tgc_site$layer_bot
  #site_roots <- lm(lhs ~ rhs)

beta.site1<-ggplot(somNEONMega1, 
         aes(x = socfrac_cumsum, 
             y = layer_bot )) +
    geom_point(pch = 21, aes(color=land_cover)) + 
    geom_point(aes(x=rootfrac_cumsum, color=land_cover), pch=19)+
    geom_line(aes(x=rootfrac_cumsum, color=land_cover), lty="solid")+
    geom_line(aes(x=socfrac_cumsum, color=land_cover), lty="dashed")+
    scale_color_manual(values=c("darkgreen","royalblue2","darkorchid"))+ 
    scale_y_reverse() + # puts 0 at the top
    xlab("Proportion accumulated")+
    ylab("Soil depth (cm)")+
    guides(color=guide_legend(title="Land cover", title.theme = element_text(size=12, angle=0), label.theme = element_text(size=12, angle=0)))+
    facet_wrap(~ site_code) +
    theme_bw()+
    theme(legend.position=c(0.9,0.07), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=14),axis.text=element_text(size=12),legend.title = element_text(size=12))
beta.site1
ggsave(plot=beta.site1, file="beta.site1.jpeg",dpi=300)

beta.site2<-ggplot(somNEONMega2, 
                   aes(x = socfrac_cumsum, 
                       y = layer_bot )) +
  geom_point(pch = 21, aes(color=land_cover)) + 
  geom_point(aes(x=rootfrac_cumsum, color=land_cover), pch=19)+
  geom_line(aes(x=rootfrac_cumsum, color=land_cover), lty="solid")+
  geom_line(aes(x=socfrac_cumsum, color=land_cover), lty="dashed")+
  scale_color_manual(values=c("darkorange3", "darkgreen","royalblue2","darkorchid"))+
  scale_y_reverse() + # puts 0 at the top
  xlab("Proportion accumulated")+
  ylab("Soil depth (cm)")+
  guides(color=guide_legend(title="Land cover", ncol=2,title.theme = element_text(size=12, angle=0), label.theme = element_text(size=12, angle=0)))+
  facet_wrap(~ site_code) +
  theme_bw()+
  theme(legend.position=c(0.8,0.07), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=14),axis.text=element_text(size=12),legend.title = element_text(size=12))
beta.site2
df<-as.data.frame(ggplot_build(beta.site2)$data) #to check ggplot's color scheme
ggsave(plot=beta.site2, file="beta.site2.jpeg",dpi=300)


somNEONMegaSoilRootSelSumDepth_noshcult<-somNEONMegaSoilRootSelSumDepth %>% filter(land_cover %in% c("forest","rangeland/grassland"))
segments<-data.frame(x=c(0.1,0.1,0,0,0),xend=c(0.2,0.2,0,0,0),y=c(175,200,0,0,0),yend=c(175,200,0,0,0),land_cover=factor("forest",levels=c("forest","rangeland/grassland")))
beta.summ<-ggplot(somNEONMegaSoilRootSelSumDepth_noshcult, 
                  aes(x = socfrac_cumsum, 
                      y = layer_bot )) +
  geom_point(color="black", pch = 21, size=1) + 
  geom_point(aes(x=rootfrac_cumsum),color="black", pch=19, size=1)+
  #geom_smooth(aes(x=socfrac_cumsum, group=site_code), color="gray",lty="dashed", span=1.5)+
  geom_smooth(data=subset(somNEONMegaSoilRootSelSumDepth_noshcult, land_cover=="rangeland/grassland"), aes(x=rootfrac_cumsum), lty="solid", span=1.5)+
  geom_smooth(data=subset(somNEONMegaSoilRootSelSumDepth_noshcult, land_cover=="rangeland/grassland"), aes(x=socfrac_cumsum), lty="dashed", span=1.5)+
  #scale_color_manual(values=c("darkgreen","royalblue2"))+
  scale_y_reverse(limits=c(200,0)) + # puts 0 at the top
  xlab("Proportion accumulated")+
  ylab("Soil depth (cm)")+
  annotate("segment",lty="solid", x=0.1,xend=0.2,y=175,yend=175)+
  annotate("segment",lty="dashed", x=0.1,xend=0.2,y=200,yend=200)+
  annotate("text", label="SOC", x=0.25,y=200,hjust=0)+
  annotate("text",label="Roots", x=0.25, y=175,hjust=0)+
  annotate("point", x=0.03,y=200, pch=21)+
  annotate("point", x=0.03,y=175, pch=19)+
  geom_text(data=beta.stats, aes(x=x,y=y.soc,label=mean.soc), hjust=0)+
  geom_text(data=beta.stats, aes(x=x,y=y.roots,label=mean.roots), hjust=0)+
  guides(color=guide_legend(title="Land cover", ncol=1,title.theme = element_text(size=12, angle=0), label.theme = element_text(size=12, angle=0)))+
  facet_wrap(~ land_cover) +
  theme_bw()+
  theme(legend.position="none", panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_rect(fill=NA, color="black"),panel.background=element_rect(fill="white"),axis.title=element_text(size=14),axis.text=element_text(size=12),legend.title = element_text(size=12),strip.text = element_text(size=16))
beta.summ
ggsave(plot=beta.summ, file="beta_landcov_summary_v4.jpeg",dpi=300)


#Table S3a: mixed models for beta, organic+mineral, forest and grasslands only
beta.all.nocult<-filter(beta.all, !land_cover %in% c("cultivated","shrubland"))
beta.all.for<-filter(beta.all, land_cover=="forest")
beta.all.gr<-filter(beta.all, land_cover=="rangeland/grassland")

mod <- lmer(data=beta.all.nocult, beta_soc ~ beta_roots*land_cover  + (1|layer_bot_max))
null.mod <- lmer(data=beta.all.nocult, beta_soc~(1|layer_bot_max))
full.mod <- lmer(data=beta.all.for, beta_soc ~ beta_roots + mat+ map + clay  + (1|layer_bot_max))
reduced.mod <- lmer(data=beta.m.gr, beta_soc ~ map + mat + clay + (1|layer_bot_max))
summary(full.mod)  

em<-emmeans::emmeans(mod, pairwise~land_cover, method="Tukey")
em
summary(mod)  
Anova(full.mod)
AIC(full.mod)
performance::performance_aicc(mod)
performance::r2(mod)
car::vif(full.mod)
anova(null.mod, mod)

#Table S3b: mixed models for Betas in mineral horizons only,forest and grasslands only
beta.m.nocult<-filter(beta.all.M, !land_cover %in% c("cultivated","shrubland"))
beta.m.for<-filter(beta.all.M, land_cover=="forest")
beta.m.gr<-filter(beta.all.M, land_cover=="rangeland/grassland")

null.mod <- lmer(data=beta.all.M, beta_soc~(1|layer_bot_max))
full.mod <- lmer(data=beta.all.M, beta_soc ~ beta_roots + mat+ map + (1|layer_bot_max))
reduced.mod <- lmer(data=beta.all.M, beta_soc ~ mat + (1|layer_bot_max))
summary(full.mod)  
Anova(full.mod)
emmeans::emmeans(reduced.mod, pairwise~land_cover, method="Tukey")
performance::performance_aicc(reduced.mod)
performance::r2(reduced.mod)
car::vif(full.mod)
anova(null.mod, reduced.mod)

reduced.mod <- lm(data=beta.m.nocult, beta_soc[beta.m.nocult$land_cover=="rangeland/grassland"] ~ beta_roots[beta.m.nocult$land_cover=="rangeland/grassland"])
summary(reduced.mod)
car::Anova(reduced.mod)
TukeyHSD(aov(reduced.mod), which="land_cover")
corr<-lm(data=beta.all.nocult,beta_roots~land_cover)
Anova(corr)
t.test(beta.all.nocult$beta_roots[beta.all.nocult$land_cover=="forest"],beta.all.nocult$beta_roots[beta.all.nocult$land_cover=="rangeland/grassland"])
