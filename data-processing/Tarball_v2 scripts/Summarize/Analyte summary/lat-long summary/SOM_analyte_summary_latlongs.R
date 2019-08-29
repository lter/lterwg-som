#Script to produce summary of analyte data in SOM tarball
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/27/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv", as.is=T)
tarball <- tarball %>% filter(google_dir != "NA")


### I set this up as three versions, each with increasing depth of data summarization
  #Having trouble converting these to functions...

#Version 1: Simple var count summary
df1 <- tarball %>% group_by(google_dir) %>%
    summarize(var_n = n_distinct(lat, na.rm = TRUE),
              min = min(lat),
              max = max(lat)) 

colnames(df1) <- c("google_dir", "lat_n", "lat_min", "lat_max")

df2 <- tarball %>% group_by(google_dir) %>%
  summarize(var_n = n_distinct(long, na.rm = TRUE),
            min = min(long),
            max = max(long)) 

colnames(df2) <- c("google_dir", "long_n", "long_min", "long_max")


df.summary <- merge(df1,df2,by="google_dir")

#Export summary tables
write.csv(df.summary, "lat-long_summary.csv", row.names = F)

