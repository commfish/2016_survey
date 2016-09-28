####### 2016 Scallop Statewide Survey
####### Ben Williams
####### ben.williams@alaska.gov

## Code   
#-----------------------------------------------------------
## All code, data, and associated documents are held in a single R project
## titles 2016_survey.Rproj  

## Try to keep to the tidy data principles http://vita.had.co.nz/papers/tidy-data.pdf  

## Naming 
   # lower case names are numeric
   # Capitolized names are factors 

## The operational plan lays out the foundation for the analyses herein

# load ----
library(tidyverse)
library(lubridate)

# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016.csv')
events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
   select(year=year, date=date, event = EVENT_ID, District = DISTRICT, 
          Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
          slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,performance=GEAR_PERFORMANCE_CODE_SW) -> event

ggplot(event, aes(date, length))+geom_point()
