library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(httr)
library(readr)


dat<-read_csv(here('data','RS_biomass_sampling_AH.csv'))

#select most relevant columns 
names(dat)

#to properly drop NAs with hardcoded 'na" values
dat$Dry_BM_kG_ha<-if_else(dat$Dry_BM_kG_ha=='na', 'NA', dat$Dry_BM_kG_ha) 

dat<-dat %>%
     mutate(biomass_sample_date=mdy(Field_sampling_date),
             loc_ID=paste0(Latitude,'_', Longitude),
             biomass_kg_ha=as.numeric(Dry_BM_kG_ha)
             ) %>%
     drop_na(biomass_kg_ha) %>% #drop NAs
     select(biomass_sample_date,loc_ID, biomass_kg_ha,Percent_N,
             Latitude,Longitude, Sentinel2_Date,NDVI_8A,
             Sentinel2_Date2,NDVI_8A2
             ) %>%
     mutate(sample_year=(year(biomass_sample_date)),
            sample_month=(month(biomass_sample_date, label=T)),
            lat_short=round(dat$Latitude,1),
            long_short=round(dat$Longitude,1)
             )
dat  
dat$sample_month

# biomass data exploration ------------------------------------------------

unique(dat$loc_ID) #there are 390 unique biomass points with precise 
                  #GPS coordinates 
biomass_dups<-dat %>% 
  group_by(loc_ID) %>% 
  tally() %>%
  filter(n>1) 
biomass_dups #82 points have 2 biomass points

#plot biomass 
dat %>% 
  filter(loc_ID %in% biomass_dups$loc_ID) %>% 
  ggplot(aes(loc_ID, biomass_kg_ha, col=as.factor(sample_year))) +
  geom_point()+
  ggtitle('Biomass samples exact same place across 2 years')+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#look at biomass sampling dates
dat2 %>%
  group_by(biomass_sample_month=month(biomass_sample_date, label=T)) %>%
  ggplot(aes(x=biomass_sample_month)) +
  geom_bar() +
  ggtitle('Distribution of biomass observations')

# NDVI data exploration ---------------------------------------------------

#########################
#explore the second NDVI observations to see if we would ever want to use them

#check whether any of the Sentinel2_Date2 come before Sentinel2_Date
dat %>%
  filter(mdy(Sentinel2_Date2)<mdy(Sentinel2_Date))
#no--they all come after

#are there any cases with only a second NDVI observation but not first?
dat %>%
  filter(is.na(NDVI_8A) & !is.na(NDVI_8A2))
#no

dat %>%
  group_by(NDVI_date2=month(mdy(Sentinel2_Date2), label=T)) %>%
  ggplot(aes(x=NDVI_date2)) +
  geom_bar() +
  ggtitle('Distribution of later NDVI observations')

#the observations are either in Jan or May, and most rows are missing
############################

#explore the earlier NDVI observations since we don't need to use any of the 
#second observations

#drop rows without NDVI
dat2<-dat %>%
  drop_na(NDVI_8A) 

unique(dat2$loc_ID) #seems like there are 195 paired biomass/NDVI points with precise 
#GPS coordinates 

NDVI_dups<-dat2 %>% 
  group_by(loc_ID) %>% 
  tally() %>%
  filter(n>1) 
NDVI_dups #25 points have 2 biomass points

dat2

#look at NDVI dates
dat2 %>%
  group_by(NDVI_date=month(mdy(Sentinel2_Date), label=T)) %>%
ggplot(aes(x=NDVI_date)) +
  geom_bar() +
  ggtitle('Distribution of NDVI early observations')



# climate data ------------------------------------------------------------

max(dat$biomass_sample_date)
lat=unname(dat$Latitude[1])
lat



## 2. Importing data from weather API
weather <- GET(
  url = 'https://api.precisionsustainableag.org/weather/daily?', # if you want hourly, replace 'daily' with 'hourly' in the url
  query = list(
    lat = 39.02434,
    lon = -76.93934,
    start = '2015-01-01',
    end = '2022-05-01',
    output='csv',
    options = ''
  )
)
weather_parsed <- readr::read_csv(
  content(weather, as = "text"))

weather_parsed

# modeling ----------------------------------------------------------------



#distributions of NDVI and biomass_kg_ha
hist(dat2$NDVI_8A)
hist(dat$biomass_kg_ha)

