# This code accompanies Malhotra et al. 2021 DOI: XXX.
# This file was written by JAM Moore 12-18-2020.
# Data are publicly accessible at Environmental Data Initiative https://portal-s.edirepository.org/nis/codeGeneration?packageId=edi.521.1&statisticalFileType=r
# Once downloaded, the data file object is called "dt1" with 293592 obs. and 157 variables.
# Alternatively: Proceed directly to line #159/Analysis Section and read in the CSV that is available in the Supplementary files.

library(lmerTest) 
library(sjstats) 
library(car) 
library(dplyr)
library(ggplot2)
library(minqa)

#### Obtain and clean the data file ####
som<- dt1
not_all_na <- function(x) any(!is.na(x))

# Filter to only NEON
somNEON <- filter(som, network == "NEON")

# Calculating root and SOC stocks in NEON megapit samples
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
                               lyr_soc_stock_calc)) %>% #lyr_soc_stock_calc is (g C)/m2
  select_if(not_all_na) %>%
  arrange(site_code)

# Align root and soil data, need to assign horizon to roots
somNEONMegaRootsSel <- somNEONMegaRoots %>%
  select_if(not_all_na)

somNEONMegaSoilSel <- somNEON %>%
  filter(data_file%in%c("megapit_soils_all")) %>%
  select_if(not_all_na)

## Loop to assign horizons
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

#Cumulative sum for root C stocks. Contains cumulative fractions for SOC & BGB and also the whole profile sums.
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
  rbind(somNEONMegaRootsSelSumDepth) #%>% ###*****JESSICA, LOOK AT THIS ****###
filter(!is.na(bgb_c_stock))

#Calculating covariates
somNEONMegaSoilRootCovariates <- somNEONMegaSoil.withRoot %>% 
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


#### Analysis Section ####
somNEONMegaSoilRoot_wholeprofilestats <-read.csv("somNEONMegaSoilRoot_wholeprofilestats.csv")
#Objective 1: Are roots and SOC correlated when summed across the whole profile?
#Results associated with Table S1. Use somNEONMegaSoilRoot_wholeprofilestats
null.mod <- lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ (1|layer_bot_max))

full.mod<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum + mat + map  + clay + land_cover + (1|layer_bot_max)) 
summary(full.mod)
Anova(full.mod)
anova(full.mod, null.mod)
AIC(full.mod)
performance::r2(full.mod) #calculate Marginal R2
vif(full.mod) #check variance inflation factors
plot(full.mod) #plotting residuals from the full mixed model, should have no relationship
qqnorm(residuals(full.mod)) #checking normality of residuals, should be close to linear

#Reduced model. Add and remove paramters here to obtain the AIC and R2 values reported in Table S1.
reduced.mod2<-lmer(data=somNEONMegaSoilRoot_wholeprofilestats, lyr_soc_stock_calc_sum ~ 
                     bgb_c_stock_sum +  map  + (1|layer_bot_max)) 
summary(reduced.mod2)
Anova(reduced.mod2)
anova(null.mod, reduced.mod2)
AIC(reduced.mod2)
performance::r2(reduced.mod2)

#Objective 2: Horizon-specific whole profile sums.
somNEON_organic_wholeprofile<-read.csv("somNEON_organic_wholeprofile")
somNEON_mineral_wholeprofile<-read.csv("somNEON_mineral_wholeprofile.csv")

#Mineral horizon models. Results are reported in Table S2a. 
null.mod <- lmer(data=somNEON_mineral_wholeprofile, lyr_soc_stock_calc_sum ~ (1|layer_bot_max))

full.mod<-lmer(data=somNEON_mineral_wholeprofile, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum + mat + map + clay + land_cover + (1|layer_bot_max))

reduced.mod<-lmer(data=somNEON_mineral_wholeprofile, lyr_soc_stock_calc_sum ~ 
                    bgb_c_stock_sum +  map  + (1|layer_bot_max)) 
summary(full.mod)
Anova(full.mod)
anova(full.mod, null.mod)
AIC(full.mod)
performance::r2(reduced.mod)
vif(full.mod)


#Organic horizon models. Results are in Table S2b.
null.mod <- lmer(data=somNEON_organic_wholeprofile, lyr_soc_stock_calc_sum ~ (1|layer_bot_max))

full.mod<-lmer(data=somNEON_organic_wholeprofile, lyr_soc_stock_calc_sum ~ 
                 bgb_c_stock_sum + mat + land_cover + map + (1|layer_bot_max))
reduced.mod<-lmer(data=somNEON_organic_wholeprofile, lyr_soc_stock_calc_sum ~ land_cover + 
                    (1|layer_bot_max))

summary(reduced.mod)
anova(reduced.mod, null.mod)
Anova(reduced.mod)
performance::performance_aicc(reduced.mod)
performance::r2(reduced.mod)
car::vif(full.mod)

#Objective 3. Is Beta_SOC related to Beta_rootC?
#Beta is calculated as: Y(cumulative percent) = 1- Beta d(depth)

min.rss.roots <- function(beta){
  x = somNEONMegaSoilRootSelSumDepth_site$rootfrac_cumsum 
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

#Adding covariates and making a csv file to store the beta values 
somNEONMegaSoilRootCovariates.ldcv <- somNEONMegaSoilRootCovariates %>%
  left_join(select(somNEONMega, land_cover, site_code), by="site_code") %>%
  group_by(site_code) %>%
  summarize(land_cover = first(land_cover),
            mat = mean(mat),
            map = mean(map),
            clay = mean(clay),
            layer_bot_max = mean(layer_bot_max))

#The dataframe beta.all is summarized in Table S4. 
beta.all<-beta.all %>%
  left_join(somNEONMegaSoilRootCovariates.ldcv, by= "site_code")%>%
  filter(!site_code%in%c("CLBJ","JORN","GUAN","LAJA","GRSM","TEAK","BARR")) #these sites have betaSOC = 0.1

#separate the forest and grassland/rangeland sites to run models for beta correlations
beta.all.for<-filter(beta.all, land_cover=="forest")
beta.all.gr<-filter(beta.all, land_cover=="rangeland/grassland")

#Whole profile forest beta test. Results are shown in Table S3a. 
null.mod <- lmer(data=beta.all.for, beta_soc ~ (1|layer_bot_max))
full.mod <- lmer(data=beta.all.for, beta_soc ~ beta_roots + mat + map + clay + (1|layer_bot_max))
reduced.mod <- lmer(data=beta.all.for, beta_soc ~ map + mat  + (1|layer_bot_max))

summary(full.mod)
anova(reduced.mod, null.mod)
Anova(reduced.mod)
performance::performance_aicc(reduced.mod)
performance::r2(reduced.mod)
car::vif(full.mod)

#Whole profile grassland/rangeland beta test. Results are shown in Table S3a. 
null.mod <- lmer(data=beta.all.gr, beta_soc ~ (1|layer_bot_max))
full.mod <- lmer(data=beta.all.gr, beta_soc ~  beta_roots + mat + map + clay + (1|layer_bot_max))
reduced.mod <- lmer(data=beta.all.gr, beta_soc ~ beta_roots  + (1|layer_bot_max))

summary(full.mod)
anova(reduced.mod, null.mod)
Anova(reduced.mod)
performance::performance_aicc(full.mod)
performance::r2(full.mod)
car::vif(full.mod)

#Calculating beta coefficients just for the mineral horizons.
#Filter out the organic horizon rows in the dataframe.
somNEONMegaSoilRootSelSumDepth_hzn <- somNEONMegaSoilRootSelSumDepth %>%
  mutate(hzn_type = ifelse(grepl("^O", hzn), "organic", "mineral"))
somNEONMegaSoilRootSelSumDepth_M <- filter(somNEONMegaSoilRootSelSumDepth_hzn, hzn_type=="mineral")

# a function to calculate root beta for each site **Mineral horizon**
min.rss.roots <- function(beta){
  x = somNEONMegaSoilRootSelSumDepth_M_site$rootfrac_cumsum #I replaced tgc_site with somNEONMegaSoilRootSelSumDepth_site
  y = 1-beta^somNEONMegaSoilRootSelSumDepth_M_site$layer_bot
  sum((x-y)^2,na.rm=T)
}

# a function to calculate soil beta for each site **Mineral horizon**
min.rss.soc <- function(beta){
  x = somNEONMegaSoilRootSelSumDepth_M_site$socfrac_cumsum
  y = 1-beta^somNEONMegaSoilRootSelSumDepth_M_site$layer_bot
  sum((x-y)^2,na.rm=T)
}
#a loop for calculating betas for each site **M horizon**
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

beta.all.M<-beta.all.M %>%
  left_join(somNEONMegaSoilRootCovariates.ldcv, by= "site_code")%>%
  filter(!site_code%in%c("SOAP", "BONA", "DEJU", "HEAL", "MLBS","ABBY","WREF","CLBJ","JORN","GUAN","LAJA","GRSM","TEAK","BARR")) #these sites have betaSOC = 0.1
beta.m.for<-filter(beta.all.M, land_cover=="forest")
beta.m.gr<-filter(beta.all.M, land_cover=="rangeland/grassland")

#Mineral horizon forest beta test. Results are shown in Table S3b. 
null.mod <- lmer(data=beta.m.for, beta_soc ~ (1|layer_bot_max))
full.mod <- lmer(data=beta.m.for, beta_soc ~ beta_roots + mat + map + clay + (1|layer_bot_max))
reduced.mod <- lmer(data=beta.m.for, beta_soc ~ beta_roots + mat   + (1|layer_bot_max))

summary(full.mod)
anova(reduced.mod, null.mod)
Anova(reduced.mod)
performance::performance_aicc(reduced.mod)
performance::r2(reduced.mod)
car::vif(full.mod)

#Mineral horizon grassland/rangeland beta test. Results are shown in Table S3b. 
null.mod <- lmer(data=beta.m.gr, beta_soc ~ (1|layer_bot_max))
full.mod <- lmer(data=beta.m.gr, beta_soc ~  beta_roots + mat + map + clay + (1|layer_bot_max))
reduced.mod <- lmer(data=beta.m.gr, beta_soc ~ beta_roots  + (1|layer_bot_max))

summary(full.mod)
anova(reduced.mod, null.mod)
Anova(reduced.mod)
performance::performance_aicc(full.mod)
performance::r2(full.mod)
car::vif(full.mod)

