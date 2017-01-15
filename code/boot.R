# load ----
library(tidyverse)
library(reshape2)
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

# scallops ----
# catch ----
# create catch data.frame
catch %>% filter(species==74120, cond==1) %>% 
   group_by(Event, size_class) %>% 
   summarise(catch=sum(count, na.rm=T)) %>% 
   dcast(Event~size_class,sum, drop=TRUE) -> s.catch
names(s.catch) <- c('Event', 'catch1', 'catch2')

scal.catch <- merge(s.catch,event, all = TRUE) # merge with events - keep NA
   scal.catch[is.na(scal.catch)] <- 0 # change NA to 0
   scal.catch %>% dplyr::select(Event, catch1, catch2,year,District,Bed,n,ai,area_nm2) %>% 
      mutate(catch3 = catch1+catch2) %>% 
      melt(., id.vars=c('Event','year','District','Bed','n','ai','area_nm2')) %>% 
      mutate(di= value/ai) %>% 
      group_by(District,Bed,year, variable) %>% 
      do(data=(.)) %>% 
      select(data) %>% 
      map(identity) -> scal.catch

# weight ----
# create weight data.frame
   catch %>% filter(species==74120, cond==1) %>% 
      group_by(Event, size_class) %>% select(-count) %>% 
      summarise(weight=sum(sample_wt, na.rm=T)) %>% 
      dcast(Event~size_class,sum, drop=TRUE) -> s.weight
   names(s.weight) <- c('Event', 'weight1', 'weight2')

scal.weight <- merge(s.weight,event, all = TRUE) # merge with events - keep NA
scal.weight[is.na(scal.weight)] <- 0 # change NA to 0
scal.weight %>% dplyr::select(Event, weight1, weight2,year,District,Bed,n,ai,area_nm2) %>% 
   mutate(weight3 = weight1+weight2) %>% 
   melt(., id.vars=c('Event','year','District','Bed','n','ai','area_nm2')) %>% 
   mutate(di= value/ai) %>% 
   group_by(District,Bed,year, variable) %>% 
   do(dat=(.)) %>% 
   select(dat) %>% 
   map(identity) -> scal.weight

# bootstrap ----
aa = scal.weight$dat[1]  
f.it <- function(x){
   # first turn the list to a dataframe
   # extract the identifiers to append to the results
   # function to be run each time for calculating dbar & N
   # function to sample by rows
   # replicate the data 1000 times
   
   x = as.data.frame(x)
   y = x[1,c(2:4,8)]
   boot.it <- function(x){
      d_bar = sum(x$di)/mean(x$n)
      N=mean(x$area_nm2)*d_bar
      c(d_bar, N)
   }

   f.do <- function(x){
   x %>% sample_n(nrow(.), replace=TRUE) -> x 
   boot.it(x)
   }

   as.data.frame(t(replicate(1000,f.do(x)))) -> out
   names(out) <- c('dbar','N')
   cbind(out,y)
}

# apply the function to each component of the list
newd <- lapply(scal.weight$dat,f.it)

# bind the results together and convert to a dataframe
newd <- as.data.frame(do.call(rbind,newd))

# plots ----
newd %>% filter(variable=='weight2') %>% 
   ggplot(aes(dbar, fill=Bed))+geom_density()+ facet_wrap(~Bed)
newd %>% filter(variable=='weight1') %>% 
   ggplot(aes(dbar, fill=Bed))+geom_density()

newd %>% filter(variable=='weight2') %>% 
   ggplot(aes(N, fill=Bed))+geom_density() + facet_wrap(~Bed)

