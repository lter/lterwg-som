# QuickSOM_dataCoverage"
# Wil Wieder < wwieder@ucar.edu >
# Built on work from Derek Pierson & Jessica Moore
# April 3, 2020, May 12, 2020


rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

#data.all <- readRDS("/Users/wwieder/Will/git_repos_local/lterwg-som/somCompositeData_2020-05-12.rds")  
data.all <- read.csv("/Users/wwieder/Will/git_repos_local/lterwg-som/521_soils_data_harmonization_415ab64c9ef9f80c3bd65fd45441e8e8.csv")  

data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

#get first four characters from observation_date
#first, make sure all are characters 
unique(data.all$site_code)
unique(data.all$observation_date)
data.all$site_code <- str_replace(data.all$site_code, "Eel", "ER-CZO")
data.all$site_code <- str_replace(data.all$site_code, "SSHCZO", "SSH-CZO")
data.all$site_code <- str_replace(data.all$site_code, "SSCZO", "SS-CZO")
data.all$site_code <- str_replace(data.all$site_code, "BcCZO", "BC-CZO")
data.all$observation_date <- str_replace(data.all$observation_date, "Aug-Sep 2011", "2011")
data.all$year<-substr(data.all$observation_date, start=1, stop=4)
data.all$year_all<-as.numeric(data.all$year)

#check for unique 4 digit values in year
print(unique(data.all$year_all))
print(list(unique(data.all$curator_PersonName)))
print(list(unique(data.all$author_PersonName)))

# --- print some basic database characteristics ---
print(dim(data.all))
# total database
print(dim(data.all)[1]*dim(data.all)[2]*1e-6)
# not missing observations
print(sum(!is.na(data.all)))
# % possible observations
print(100* sum(!is.na(data.all)) / (dim(data.all)[1]*dim(data.all)[2]) )

print(unique(data.all$location_name))
print(unique(data.all$site_code))
L1 = unique(data.all$tx_L1_level, na.rm=T)
L2 = unique(data.all$tx_L2_level)
L3 = unique(data.all$tx_L3_level)
print(L1)
print(L2)
print(L3)

# experimental treatments in database
for (i in 1:length(L1)) {
  Mtype = L1[i]
  print(Mtype)
  data.exp = data.all %>% filter(experiments == 'YES') %>%
      #filter(network == 'NutNet') %>%
      filter(tx_L1_level == Mtype | 
               tx_L2_level == Mtype | 
               tx_L3_level == Mtype | 
               tx_L4_level == Mtype)
  print(length(unique(data.exp$site_code)))
  print(unique(data.exp$site_code))
  remove(data.exp)
  print("------------")
}

data.other = data.all %>% filter(experiments == 'YES') %>%
  filter(network == 'LTER') #%>%
#  filter(tx_L2_level== 'other (add notes)')
print(unique(data.other$site_code))


data.grad = data.all %>% filter(gradient == 'YES')  %>%
  filter(network == 'CZO') 
print(unique(data.grad$site_code))


data.time = data.all %>% filter(time_series == 'YES') #%>%
#   filter(network == 'NutNet') 
print(unique(data.time$site_code))


print(unique(data.all$tx_L1_level))

# subset for each unique location to get basic climate data
data.loc = data.all %>% distinct(location_name, .keep_all = T)
sum_stats = function(din) {
  print(summary(din))
  print(paste('SD = ',sd(din, na.rm=T)))
}
dim(data.loc)
sum_stats(data.loc$mat)
sum_stats(data.loc$map)
sum_stats(data.loc$anpp)

print(unique(data.loc$land_cover))
print(sum(!is.na(data.loc$land_cover)))

print(unique(data.loc$eco_region))
print(sum(!is.na(data.loc$eco_region)))
print(sum(!is.na(data.loc$eco_type)))

# What is the best way to aggregate data across sites?
sum_stats(data.loc$ph_h2o)


#-------------------------------------------
## SOC Percents
### Clean up SOC percent values
#filter out negative soc%
data.soc.percent<-data.all %>% 
  filter(!is.na(lyr_soc)) #%>% filter(lyr_soc>0) %>% filter(lyr_soc<100)

data.plot = data.all %>% 
  filter(network!="none")  %>% 
  filter(layer_bot>0)

timePlot <-ggplot(data.plot, 
                aes(x=site_code, y=as.numeric(year)), na.omit=F)+
  geom_point(aes(color=network), show.legend = FALSE) +
  xlab("Site") + ylab("Year") +
  scale_y_continuous(breaks=seq(1975,2019,5),na.value=2020)+
  scale_x_discrete()+#labels = abbreviate) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank() ) +
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6)) +
  facet_grid(.~network, scales = "free_x", space='free_x', drop=FALSE)

timePlot

# Not using %SOC here, should we be consistent?
depthPlot<-ggplot(data.plot, 
                aes(x=site_code, y=layer_bot), na.omit=F)+
  geom_point(aes(color=network), show.legend = FALSE, size=1) +
  xlab("Site") + ylab("Depth") +
  scale_y_reverse() +
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6)) +
  facet_grid(.~network, scales = "free_x", space='free_x', drop=FALSE) +
  # to srtip off network names / boxes
  theme(strip.background = element_blank(),strip.text.x = element_blank()  )

depthPlot

#from ggpubr package
combined_site <- ggarrange(timePlot, depthPlot, ncol = 1, nrow = 2)
combined_site

ggsave(plot=combined_site, filename = "fig5.jpg", width = 15, height = 6 , dpi = 300)
ggsave(plot=combined_site, filename = "fig5.pdf", width = 15, height = 6 , dpi = 300)

