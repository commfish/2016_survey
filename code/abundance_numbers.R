####### 2016 Scallop Statewide Survey
####### Ben Williams / Katie Palof
####### ben.williams@alaska.gov

# intro----
### Scallop survey data summarized by NUMBERS

# Data were forwarded from Josh Mumm joshua.mumm@alaska.gov. 
## Try to keep to the tidy data principles http://vita.had.co.nz/papers/tidy-data.pdf  

## Naming 
# lower case names are numeric
# Capitalized names are factors 

## The operational plan lays out the foundation for the analyses herein

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


# catch by numbers ----

catch %>% filter(species==74120, cond==1) %>% 
  group_by(Event, size_class) %>% 
  summarise(catch=sum(count, na.rm=T)) %>% 
  dcast(Event~size_class,sum, drop=TRUE) -> s.catch
names(s.catch) <- c('Event', 'large', 'small')

scal.catch <- merge(s.catch,event, all = TRUE) # merge with events - keep NA
scal.catch[is.na(scal.catch)] <- 0 # change NA to 0
scal.catch %>% dplyr::select(Event, large, small,year,District,Bed,n,ai,area_nm2) %>% 
  mutate(all = large+small) %>% 
  melt(., id.vars=c('Event','year','District','Bed','n','ai','area_nm2')) %>% 
  mutate(di= value/ai) %>% 
  group_by(District,Bed,year, variable) %>% 
  do(dat=(.)) %>% 
  select(dat) %>% 
  map(identity) -> scal.catch

#summarize data before bootstraping to compare, call function file here
source('./code/functions.R')
numbers_original <- lapply(scal.catch$dat,f.sum)
numbers_original <- as.data.frame(do.call(rbind,numbers_original)) 

# bootstrap ----
numbers <- lapply(scal.catch$dat,f.it)
numbers <- as.data.frame(do.call(rbind,numbers))

# figs ----
numbers %>% filter(variable=='large') %>% 
  ggplot(aes(dbar, fill=Bed))+geom_density()+ facet_wrap(~Bed)

numbers %>% filter(variable=='large') %>% 
  ggplot(aes(dbar, fill=Bed))+geom_density()

numbers %>% filter(variable=='small') %>% 
  ggplot(aes(N, fill=Bed))+geom_density() 

numbers %>% group_by(District,Bed,year,variable) %>% 
  summarise(llN=quantile(N,0.025),ulN=quantile(N,0.975),N=mean(N), 
            lldbar=quantile(dbar,0.025),uldbar=quantile(dbar,0.975),dbar=mean(dbar)) %>% 
  group_by(Bed,variable) %>% 
  ggplot(aes(Bed,N/1000000))+geom_point()+
   geom_errorbar(aes(ymin=llN/1000000,ymax=ulN/1000000), width=0.2)+
  facet_wrap(~variable)+
  scale_x_discrete(limits=c('EK1','WK1','KSH1','KSH2','KSH3'))+ 
   scale_y_continuous(labels = comma) +ylab("Abundance (millions)")

ggsave("../figs/N_large.png", dpi=300, height=4.5, width=6.5, units="in")

# tables ----
numbers %>% group_by(Bed,year,variable) %>% 
  summarise(llN=quantile(N,0.025),ulN=quantile(N,0.975),N_b=mean(N), 
            lldbar=quantile(dbar,0.025),uldbar=quantile(dbar,0.975),dbar_b=mean(dbar), 
            var_dbar = 1/((n())-1)*sum((dbar-dbar_b)^2) ,
            cv=sqrt(var_dbar)/dbar_b*100 , 
            varN= 1/((n())-1)*sum((N-N_b)^2),
            cvN=sqrt(varN)/N_b*100) -> N_summary

N_summary %>% filter(variable=='large') %>% dplyr::select(Bed, year, cv)

write_csv(N_summary, 'output/N_summary.csv')
