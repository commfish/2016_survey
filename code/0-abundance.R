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
# Capitalized names are factors 

## The operational plan lays out the foundation for the analyses herein it is available in the literature folder

#NOTE - this could easily be udated for multiple years by changing the group_by(...) throughout.

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

catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE) -> catch

#check structure of catch data
catch %>% filter(species==74120) %>% left_join(event) %>% 
   group_by(Bed, ID, size_class) %>% summarise(catch=sum(count, na.rm=T)) %>% data.frame()


# a_i ----
#Dredge width in nmi = 0.00131663 x length of dredging in each station
# Q = 0.83
Q <- 0.83

#add ai column to events dataframe
event %>% mutate(ai=length * 0.00131663 * Q) -> event #ai is in nmi^2

# areas ----
# number of stations sampled by bed
event %>% group_by(Bed) %>% summarise(n=n(), ai_bar = mean(ai)) -> samples
   

# Total area of each bed
area <- read.csv('./data/area.csv')

samples %>% left_join(area) %>% select(-grids) -> samples

#add size classes for every event

# catch ----
#filter out scallops from the catch data and calculate density
#weight is in kg
catch %>% filter(species==74120, cond==1) %>% 
   group_by(Event, size_class) %>% 
   summarise(catch=sum(count), 
             weight = sum(sample_wt)) -> catch.a
# here is where each "Event" should have a size class 1 and 2
catch.a %>% select(-weight) ->step1 #only use catch
step2 <- dcast(step1, Event ~ size_class, sum, drop=TRUE) # this puts in the 0 catchs
# try merging with event here -----
event %>% filter(performance==1) %>% left_join(samples) %>%
  merge(step2, all=T) -> mer1
# need to get back to size class 1 and 2 and catch before grouping by size class.
mer1 %>% 
  gather(size_class, catch, 28:29) %>%
  mutate(size_class = as.numeric(size_class)) -> mer2
mer2 %>%
  left_join(catch.a) -> catch.c
#----
#step2 %>%
#  gather(key = size_class, catch, -Event) %>%
#  mutate(size_class = as.numeric(size_class))-> step3
#step3 %>% 
#  left_join(catch.a) ->step4# need to convert this back to old format and add in weights if catch >0
#catch.b <- step4

# d_i ----
#combine with event data - change NA catches to 0
# do check the end result to be sure that no hauls are being double counted
catch.c %>% 
   group_by(size_class) %>% 
   mutate(catch=replace(catch, which(is.na(catch)), 0), 
          di = catch/ai, 
          weight=replace(weight, which(is.na(weight)), 0), 
          di_wt = weight/ai) -> catch.area3
#temporary work around:
#catch.area2 %>%
  #mutate(size_class2=replace(size_class, which(is.na(size_class)), 1)) -> catch.area2
# Need to make sure that every event has a row for size class 1 and size class 2...even if catch is 0.
# still have NA for 0 tows....how to deal with these.
catch.area3 %>%
  group_by(Bed, size_class) %>% summarise(n=n()) # this puts the 0 tows in the size 1 category


# lists ---- 
# create lists to hold data - 3 lists per bed - large, small, and all
c.a.bedlist <- split(catch.area3, catch.area3$Bed)# splits into a list of each Bed

#need to split into large and small - 
c.a.bedlist2 <- split(catch.area3, list(catch.area3$Bed, catch.area3$size_class))


# dbar using list ----
# using lapply , create function to call for summary data
c.a.bedlist2 %>% 
  summarise(n=mean(n),
            area=mean(area_nm2),
            ai_bar=mean(ai_bar),
            dbar=(1/n*sum(di)),
            var_dbar=1/((n)-1)*sum((di-dbar)^2), 
            cv=sqrt(var_dbar)/dbar*100,
            
            error=qt(0.975,df=(n)-1)*sqrt(var_dbar)/sqrt((n)),
            ll=dbar-error,
            ul=dbar+error,
            ss=sum((di-dbar)^2),
            N=area*dbar,
            varN=(area^2)*1/n*1/(n-1)*ss,
            cvN=sqrt(varN)/N*100,
            errorN=qt(0.975,df=(n)-1)*sqrt(varN)/sqrt((n)),
            llN=N-errorN,
            ulN=N+errorN,
            # By weight
            dbar_wt=(1/n*sum(di_wt)),
            sd_wt=sd(di_wt),
            cv_wt=sd_wt/dbar_wt*100,
            var_dbar_wt=1/((n)-1)*sum((di_wt-dbar_wt)^2), 
            error_wt=qt(0.975,df=(n)-1)*sqrt(var_dbar_wt)/sqrt((n)),
            ll_wt=dbar_wt-error_wt,
            ul_wt=dbar_wt+error_wt,
            ss_wt=sum((di_wt-dbar_wt)^2),
            N_wt=area*dbar_wt,
            varN_wt=(area^2)*1/n*1/(n-1)*ss_wt,
            cvN_wt=sqrt(varN_wt)/N_wt*100,
            errorN_wt=qt(0.975,df=(n)-1)*sqrt(varN_wt)/sqrt((n)),
            llN_wt=N_wt-errorN_wt,
            ulN_wt=N_wt+errorN_wt)
# dbar ----
# density variance for both count and weight and error bars
# average biomass per unit area

###CHECK THIS USING A LOG DISTIBUTION

#calculate for >100 mm shells
catch.area %>% filter(size_class==1|di==0) %>% 
   group_by(Bed) %>%    
   summarise(n=mean(n),
             area=mean(area_nm2),
             ai_bar=mean(ai_bar),
             dbar=(1/n*sum(di)),
             var_dbar=1/((n)-1)*sum((di-dbar)^2), 
             cv=sqrt(var_dbar)/dbar*100,
             
             error=qt(0.975,df=(n)-1)*sqrt(var_dbar)/sqrt((n)),
             ll=dbar-error,
             ul=dbar+error,
             ss=sum((di-dbar)^2),
             N=area*dbar,
             varN=(area^2)*1/n*1/(n-1)*ss,
             cvN=sqrt(varN)/N*100,
             errorN=qt(0.975,df=(n)-1)*sqrt(varN)/sqrt((n)),
             llN=N-errorN,
             ulN=N+errorN,
             # By weight
             dbar_wt=(1/n*sum(di_wt)),
             sd_wt=sd(di_wt),
             cv_wt=sd_wt/dbar_wt*100,
             var_dbar_wt=1/((n)-1)*sum((di_wt-dbar_wt)^2), 
             error_wt=qt(0.975,df=(n)-1)*sqrt(var_dbar_wt)/sqrt((n)),
             ll_wt=dbar_wt-error_wt,
             ul_wt=dbar_wt+error_wt,
             ss_wt=sum((di_wt-dbar_wt)^2),
             N_wt=area*dbar_wt,
             varN_wt=(area^2)*1/n*1/(n-1)*ss_wt,
             cvN_wt=sqrt(varN_wt)/N_wt*100,
             errorN_wt=qt(0.975,df=(n)-1)*sqrt(varN_wt)/sqrt((n)),
             llN_wt=N_wt-errorN_wt,
             ulN_wt=N_wt+errorN_wt) %>% mutate(size='large')-> large 

# What is the CV
large %>% group_by(Bed) %>% summarise(cv, cvN, cv_wt, cvN_wt)

#calculate for <100 mm shells
catch.area %>% filter(size_class==2|di==0) %>% 
   group_by(Bed) %>%    
   summarise(n=mean(n),
             area=mean(area_nm2),
             ai_bar=mean(ai_bar),
             dbar=(1/n*sum(di)),
             var_dbar=1/((n)-1)*sum((di-dbar)^2), 
             cv=sqrt(var_dbar)/dbar*100,
             error=qt(0.975,df=(n)-1)*sqrt(var_dbar)/sqrt((n)),
             ll=dbar-error,
             ul=dbar+error,
             ss=sum((di-dbar)^2),
             N=area*dbar,
             varN=(area^2)*1/n*1/(n-1)*ss,
             cvN=sqrt(varN)/N*100,
             errorN=qt(0.975,df=(n)-1)*sqrt(varN)/sqrt((n)),
             llN=N-errorN,
             ulN=N+errorN,
             # By weight
             dbar_wt=(1/n*sum(di_wt)),
             sd_wt=sd(di_wt),
             cv_wt=sd_wt/dbar_wt*100,
             var_dbar_wt=1/((n)-1)*sum((di_wt-dbar_wt)^2), 
             error_wt=qt(0.975,df=(n)-1)*sqrt(var_dbar_wt)/sqrt((n)),
             ll_wt=dbar_wt-error_wt,
             ul_wt=dbar_wt+error_wt,
             ss_wt=sum((di_wt-dbar_wt)^2),
             N_wt=area*dbar_wt,
             varN_wt=(area^2)*1/n*1/(n-1)*ss_wt,
             cvN_wt=sqrt(varN_wt)/N_wt*100,
             errorN_wt=qt(0.975,df=(n)-1)*sqrt(varN_wt)/sqrt((n)),
             llN_wt=N_wt-errorN_wt,
             ulN_wt=N_wt+errorN_wt) %>% mutate(size='small')-> small

# What is the CV
small %>% group_by(Bed) %>% summarise(cv, cvN, cv_wt, cvN_wt)

#calculate for all shells caught
catch.area %>%
   group_by(Bed) %>%    
   summarise(n=mean(n),
             area=mean(area_nm2),
             ai_bar=mean(ai_bar),
             dbar=(1/n*sum(di)),
             var_dbar=1/((n)-1)*sum((di-dbar)^2), 
             cv=sqrt(var_dbar)/dbar*100,
             error=qt(0.975,df=(n)-1)*sqrt(var_dbar)/sqrt((n)),
             ll=dbar-error,
             ul=dbar+error,
             ss=sum((di-dbar)^2),
             N=area*dbar,
             varN=(area^2)*1/n*1/(n-1)*ss,
             cvN=sqrt(varN)/N*100,
             errorN=qt(0.975,df=(n)-1)*sqrt(varN)/sqrt((n)),
             llN=N-errorN,
             ulN=N+errorN,
             # By weight
             dbar_wt=(1/n*sum(di_wt)),
             sd_wt=sd(di_wt),
             cv_wt=sd_wt/dbar_wt*100,
             var_dbar_wt=1/((n)-1)*sum((di_wt-dbar_wt)^2), 
             error_wt=qt(0.975,df=(n)-1)*sqrt(var_dbar_wt)/sqrt((n)),
             ll_wt=dbar_wt-error_wt,
             ul_wt=dbar_wt+error_wt,
             ss_wt=sum((di_wt-dbar_wt)^2),
             N_wt=area*dbar_wt,
             varN_wt=(area^2)*1/n*1/(n-1)*ss_wt,
             cvN_wt=sqrt(varN_wt)/N_wt*100,
             errorN_wt=qt(0.975,df=(n)-1)*sqrt(varN_wt)/sqrt((n)),
             llN_wt=N_wt-errorN_wt,
             ulN_wt=N_wt+errorN_wt) %>% mutate(size='all')-> all 

# What is the CV
all %>% group_by(Bed) %>% summarise(cv, cvN, cv_wt, cvN_wt)

ggplot(large, aes(Bed, N))+geom_point()+geom_errorbar(aes(ymin=llN, ymax=ulN), width=.2)+
   geom_point(data=small, aes(Bed, N), color=2)+geom_errorbar(data=small, aes(ymin=llN, ymax=ulN), width=.2, color=2)

ggplot(large, aes(Bed, N_wt))+geom_point()+geom_errorbar(aes(ymin=llN_wt, ymax=ulN_wt), width=.2)+
   geom_point(data=small, aes(Bed, N_wt), color=2)+geom_errorbar(data=small, aes(ymin=llN_wt, ymax=ulN_wt), width=.2, color=2)

#bootstrap ----
#resample by bed