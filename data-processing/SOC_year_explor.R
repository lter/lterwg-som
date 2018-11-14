library(dplyr)
library(ggplot2)
data.all <- readRDS("/Users/JM 1/Downloads/somCompositeData_2018-11-09T17_34_23.rds") 
data.all$full_name <- paste0(data.all$site_code," ",data.all$location_name)  #Concatenate strings
data.all <- data.all %>% mutate(full_name = gsub("NA", "", full_name))  #Remove NA from strings

#get first four characters from observation_date
data.all$year<-substr(data.all$observation_date, start=1, stop=4)
data.all$year<-as.numeric(data.all$year)

#get first four characters from observation_date_1
data.all$year_1<-substr(data.all$observation_date_1, start=1, stop=4)
data.all$year_1<-as.numeric(data.all$year_1)

#merge year and year_1 using ifelse
data.all$year_all<-ifelse(data.all$year=="NA",data.all$year_1,data.all$year)

#getting obs year out of level 1
data.all$yr_L1 <- as.numeric(data.all$L1)
data.all <- data.all %>% mutate(yr_L1 = "is.na<-"(yr_L1, yr_L1 < 1900)) %>% mutate(yr_L1 = "is.na<-"(yr_L1, yr_L1 > 2020)) 
data.all$year_all<-ifelse(data.all$year_all=="NA",data.all$yr_L1,data.all$year_all)

#getting obs year out of level 2
data.all$yr_L2 <- as.numeric(data.all$L2)
data.all <- data.all %>% mutate(yr_L2 = "is.na<-"(yr_L2, yr_L2 < 1900)) %>% mutate(yr_L2 = "is.na<-"(yr_L2, yr_L2 > 2020)) 
data.all$year_all<-ifelse(data.all$year_all=="NA",data.all$yr_L2,data.all$year_all)

#getting obs year out of level 3
data.all$yr_L3 <- as.numeric(data.all$L3)
data.all <- data.all %>% mutate(yr_L3 = "is.na<-"(yr_L3, yr_L3 < 1900)) %>% mutate(yr_L3 = "is.na<-"(yr_L3, yr_L3 > 2020)) 
data.all$year_all<-ifelse(data.all$year_all=="NA",data.all$yr_L3,data.all$year_all)

#getting obs year out of level 4
data.all$yr_L4 <- as.numeric(data.all$L4)
data.all <- data.all %>% mutate(yr_L4 = "is.na<-"(yr_L4, yr_L4 < 1900)) %>% mutate(yr_L4 = "is.na<-"(yr_L4, yr_L4 > 2020)) 
data.all$year_all<-ifelse(data.all$year_all=="NA",data.all$yr_L4,data.all$year_all)

#remove 3 digit values in year_all
data.all <- data.all %>% mutate(year_all = "is.na<-"(year_all, year_all < 1900))

#check for unique 4 digit values in year
unique(data.all$year_all)

#histogram of years
ggplot(data.all, aes(x=year_all))+
  geom_histogram(aes(fill=site_code))+
  labs(x="Observation Year", y="Count of All Data")+
  facet_grid(.~network)


########### Data exploration

#turn negative soc stocks into NA, new dataframe called data.soc
data.soc<- data.all %>% filter(!is.na(soc_stock)) %>% filter(soc_stock>0)
str(data.soc)

#soc stock data exploration
ggplot(data.soc, aes(x=year_all, y=soc_stock))+
  geom_point(aes(color=site_code, shape=network))+
  scale_y_continuous(limits=c(0,30000))+
  labs(x="Observation Year", y="SOC stock")


#soc percent exploration
#filter out negative soc%
data.soc.percent<-data.all %>% filter(!is.na(soc)) %>% filter(soc>0) %>% filter(soc<1)

soc.prop<-ggplot(data.soc.percent, aes(x=year_all, y=soc))+
  geom_point(aes(color=site_code, shape=network))+
  #scale_y_continuous(limits=c(0,30000))+
  labs(x="Observation Year", y="SOC proportion")
soc.prop

soc.per<-ggplot(data.soc.percent, aes(x=year_all, y=soc))+
  geom_point(aes(color=site_code, shape=network))+
  #scale_y_continuous(limits=c(0,30000))+
  labs(x="Observation Year", y="SOC %")

soc.per<-ggplot(data.soc.percent.years, aes(x=year, y=soc))+
  geom_point(aes(color=site_code))+
  facet_grid(.~network)
soc.per

### some site-specific time series analyses
data.soc.percent.umbs.c<-data.soc.percent %>% filter(site_code=="UMBS") %>% filter(tx_L1=="C")
soc.per<-ggplot(data.soc.percent.umbs.c, aes(x=year_all, y=soc))+
  geom_point()+
  #scale_y_continuous(limits=c(0,30000))+t
  labs(x="Observation Year", y="SOC in Control Plots")
soc.per

### UMBS temp, not year
soc.per<-ggplot(data.soc.percent.umbs.c, aes(x=, y=soc))+
  geom_point()+
  #scale_y_continuous(limits=c(0,30000))+t
  labs(x="Observation Year", y="SOC in Control Plots")
soc.per