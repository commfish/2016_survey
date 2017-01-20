####### 2016 Scallop Statewide Survey
####### Ben Williams / Katie Palof
####### ben.williams@alaska.gov

### Scallop survey data summarized by WEIGHTS

## Code   
#-----------------------------------------------------------
## All code, data, and associated documents are held in a single R project
## titles 2016_survey.Rproj  

# Data were forwarded from Josh Mumm joshua.mumm@alaska.gov. I've changed the names of the original files sent
## Try to keep to the tidy data principles http://vita.had.co.nz/papers/tidy-data.pdf  

## Naming 
# lower case names are numeric
# Capitalized names are factors 

## The operational plan lays out the foundation for the analyses herein it is available in the literature folder

#NOTE - this could easily be udated for multiple years by changing the group_by(...) throughout.

# load ----
library(tidyverse)
library(reshape2)
library(scales)
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016_161027.csv')
events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
  select(year, date, Event = EVENT_ID, District = DISTRICT, 
         Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
         slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, 
         stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, 
         elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, 
         calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,
         performance=GEAR_PERFORMANCE_CODE_SW) %>% filter(performance==1)-> event
write_csv(event,'data/event.csv')

#check for replicates - if dataframe has values there are duplicates
event %>% group_by(Bed, ID) %>% filter(n()>1)

# a_i ----
#Dredge width in nmi = 0.00131663 x length of dredging in each station
# Q = 0.83
Q <- 0.83

# areas ----
# Total area of each bed
area <- read.csv('./data/area.csv')

#add ai column to events dataframe
#Dredge width in nmi = 0.00131663 x length of dredging in each station
# number of stations sampled by bed
event %>%  #ai is in nmi^2
  group_by(Bed) %>% 
  summarise(n=n()) %>% 
  left_join(area) %>% select(-grids)-> samples

event %>% mutate(ai=length * 0.00131663 * Q) %>% left_join(samples) -> event


catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE) -> catch
write_csv(catch, 'data/catch.csv')

#scallop
# weight ----
# create weight data.frame
catch %>% filter(species==74120, cond==1) %>% 
  group_by(Event, size_class) %>% select(-count) %>% 
  summarise(weight=sum(sample_wt, na.rm=T)) %>% 
  dcast(Event~size_class,sum, drop=TRUE) -> s.weight
names(s.weight) <- c('Event', 'large', 'small')

scal.weight <- merge(s.weight,event, all = TRUE) # merge with events - keep NA
scal.weight[is.na(scal.weight)] <- 0 # change NA to 0
scal.weight %>% dplyr::select(Event, large, small,year,District,Bed,n,ai,area_nm2) %>% 
  mutate(all = large+small) %>% 
  melt(., id.vars=c('Event','year','District','Bed','n','ai','area_nm2')) %>% 
  mutate(di= value/ai) %>% 
  group_by(District,Bed,year, variable) %>% 
  do(dat=(.)) %>% 
  select(dat) %>% 
  map(identity) -> scal.weight

# weight ----
## Bootstrap ------------
# apply the function to each component of the list
weight <- lapply(scal.weight$dat,f.it)

# bind the results together and convert to a dataframe
weight <- as.data.frame(do.call(rbind,weight))

#figures----------
weight %>% filter(variable=='large') %>% 
  ggplot(aes(dbar, fill=Bed))+geom_density()+ facet_wrap(~Bed)


weight %>% filter(variable=='large') %>% 
  ggplot(aes(dbar, fill=Bed))+geom_density()

weight %>% filter(variable=='small') %>% 
  ggplot(aes(N, fill=Bed))+geom_density() + facet_wrap(~Bed)

weight %>% group_by(District,Bed,year,variable) %>% 
  summarise(llN=quantile(N,0.025),ulN=quantile(N,0.975),N=mean(N), 
            lldbar=quantile(dbar,0.025),uldbar=quantile(dbar,0.975),dbar=mean(dbar)) -> weights
weights%>% 
  group_by(Bed,variable) %>% 
  ggplot(aes(Bed,N))+geom_point()+geom_errorbar(aes(ymin=llN,ymax=ulN), width=0.2)+facet_wrap(~variable)+
  scale_x_discrete(limits=c('EK1','WK1','KSH1','KSH2','KSH3'))+ scale_y_continuous(labels = comma) -> fig_bed_weight





### save tables and figures if needed ------------------------------------
write_csv(weights, 'output/bed_weights_table_Ndbar.csv')

#save figure for write up
png(filename = 'figs/bed_weight_wCI.png')
fig_bed_weight
dev.off()








