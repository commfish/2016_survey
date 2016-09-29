####### 2016 Scallop Statewide Survey
####### Ben Williams
####### ben.williams@alaska.gov

## Code   
#-----------------------------------------------------------
## All code, data, and associated documents are held in a single R project
## titles 2016_survey.Rproj  

# Data were forwarded from Josh Mumm joshua.mumm@alaska.gov. I've changed the names of the original files sent
## Try to keep to the tidy data principles http://vita.had.co.nz/papers/tidy-data.pdf  

## Naming 
   # lower case names are numeric
   # Capitolized names are factors 

## The operational plan lays out the foundation for the analyses herein

# load ----
library(tidyverse)

# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016.csv')
events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
   select(year=year, date=date, Event = EVENT_ID, District = DISTRICT, 
          Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
          slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,performance=GEAR_PERFORMANCE_CODE_SW) -> event


catch <- read.csv('./data/catchComp_2016.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, sample_type = SAMPLE_TYPE) -> catch


awl <- read.csv('./data/awl_2016.csv')
awl %>% select(Event = EVENT_ID, weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, height=SHELL_HEIGHT_MM, sex=SEX_SW, gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, meat_cond=MEAT_CONDITION_SW, clapper = CLAPPER, sample_type = SAMPLE_TYPE) -> awl

# calculations----
# a_i
#Dredge width in nmi = 0.00131663
# Q = 0.83
event %>% mutate(ai=length*0.00131663*0.83) -> event

#filter out scallops from the catch data and calculate density
catch %>% filter(species==74120, cond==1) %>% 
   group_by(Event, size_class) %>% summarise(catch=sum(count, na.rm=T)) -> tab

#combine with event data - change NA catches to 0
event %>% filter(performance==1) %>% left_join(tab) %>% mutate(catch=replace(catch, which(is.na(tab1$catch)), 0), di = catch/ai) -> tab1

# calculate dbar
tab1 %>% group_by(District, Bed) %>% summarise(s = length(unique(Event)), dbar=1/s*sum(di))

