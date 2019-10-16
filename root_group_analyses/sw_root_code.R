##################################
### Data Exploration: LTER-SOM ###
###    Sept 2019, SWeintraub   ###
##################################

### Reset workspace
rm(list = ls())

### Set paths
if (file.exists('/Users/sweintraub/')){
  dir1 <- ("/Users/sweintraub/Box/Conferences_Meetings/2018_LTER-SOM") 
}

### Load data
som <- readRDS(paste(dir1, "somCompositeData_2019-10-13.rds", sep = "/"))
landCov <- readxl::read_excel(paste(dir1, "NEONtowerSiteMetadata.xlsx", sep = "/"))

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
                               lyr_soc_stock_calc/100)) %>%
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
    
    # get the horizon for a given midpoing
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
  summarize(bgb_c_stock = sum(bgb_c_stock, na.rm = T), 
            lyr_soc_stock_calc = sum(lyr_soc_stock_calc, na.rm = T), 
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