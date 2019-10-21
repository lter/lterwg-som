# exploratory data analysis

rm(list = ls())

### Set paths
if (file.exists('/Users/sweintraub/')){
  dir1 <- ("/Users/sweintraub/Documents/GitHub/lterwg-som/") 
}

source(paste0(dir1,'data-processing/get_latest_som.R'))
som <- get_latest_som()

names(som)

som1 <- som %>%
  mutate(bd_pft1 = ifelse(lyr_soc < 6, 
                          (-0.1229*log(lyr_soc)+1.2901),
                          (1.3774*exp(-0.0413*lyr_soc))))

somNEON1 <- somNEON %>%
  mutate(bd_pft1 = -0.1229*log(lyr_soc)+1.2901)

ggplot(som1, aes(x = bd_samp, 
                 y = bd_pft1, 
                 fill = land_cover)) + 
  geom_point(pch = 21) +
  geom_abline(slope =1, intercept = 0)

# PFT equations, Wu et al. (2003)
BD = −0.1229 × Ln (OC%) + 1.2901 (OC < 6%) 
BD = 1.3774 × exp(−0.0413 × OC%) (OC > 6%)


