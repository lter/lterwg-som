#Script to produce summary of analyte data in SOM tarball
#Derek Pierson, piersond@oregonstate.edu
#Created: 8/27/2019

library(dplyr)

#get tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-04-29.csv")
tarball <- tarball %>% filter(google_dir != "NA")


### I set this up as three versions, each with increasing depth of data summarization
  #Having trouble converting these to functions...

#Version 1: Simple var count summary
df1 <- tarball %>% group_by(google_dir) %>%
    summarize(time_series = unique(time_series),
              layers = ifelse(length(unique(layer_top)) > 1, "YES", "NO"),
              var_n = n_distinct(lyr_soc, na.rm = TRUE),
              avg = mean(lyr_soc),
              min = min(lyr_soc),
              max = max(lyr_soc)) %>% 
    mutate_if(is.numeric, round, digits = 1)
  

#Version 2: Var summary with layers
df2 <- tarball %>% group_by(google_dir) %>%
        summarize(time_series = unique(time_series),
                  layers = ifelse(length(unique(layer_top)) > 1, "YES", "NO"),
                  var_n = n_distinct(lyr_soc, na.rm = TRUE),
                  top_layers_n = n_distinct(layer_top, na.rm = TRUE),
                  mid_layers_n = n_distinct(layer_mid, na.rm = TRUE),
                  bot_layers_n = n_distinct(layer_bot, na.rm = TRUE),
                  avg = mean(lyr_soc),
                  min = min(lyr_soc),
                  max = max(lyr_soc)) %>% 
        mutate_if(is.numeric, round, digits = 1)


#Version 3: Diving deeper, grouped by site and layers
df3 <- tarball %>% group_by(google_dir, layer_top, layer_bot, layer_mid) %>%
        summarize(time_series = unique(time_series),
                  layers = ifelse(length(unique(layer_top)) > 1, "YES", "NO"),
                  var_n = n_distinct(lyr_soc, na.rm = TRUE),
                  avg = mean(lyr_soc),
                  min = min(lyr_soc),
                  max = max(lyr_soc)) %>% 
        mutate_if(is.numeric, round, digits = 1) 


#Export summary tables
write.csv(df1, "var_summary1.csv", row.names = F)
write.csv(df2, "var_summary2.csv", row.names = F)
write.csv(df3, "var_summary3.csv", row.names = F)