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
theme_set(theme_bw())
# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016.csv')
events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
   select(year=year, date=date, Event = EVENT_ID, District = DISTRICT, 
          Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
          slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, 
          stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, 
          elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, 
          calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,
          performance=GEAR_PERFORMANCE_CODE_SW) -> event

catch <- read.csv('./data/catchComp_2016.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE) -> catch
# a_i ----
#Dredge width in nmi = 0.00131663 x length of dredging in each station

#add ai column to events dataframe
event %>% mutate(ai=length*0.00131663) -> event #ai is in nmi^2


# areas ----
# number of stations sampled by bed
event %>% group_by(Bed) %>% summarise(n = length(unique(Event))) -> samples

# Total area of each bed
# need to get areas from Josh or Ryan
# for the moment I'll just use the WKI total area of 48.65717

areas <- data.frame(Bed=unique(event$Bed), area =c(50,107,48.65717))

samples %>% left_join(areas) -> samples


# catch ----
#filter out scallops from the catch data and calculate density
#weight is in kg
catch %>% filter(species==74120, cond==1) %>% 
   group_by(Event, size_class) %>% 
   summarise(catch=sum(count, na.rm=T), 
             weight = sum(sample_wt, na.rm=T)) -> catch.a

# d_i ----
#combine with event data - change NA catches to 0
# do check the end result to be sure that no hauls are being double counted
event %>% filter(performance==1) %>% left_join(catch.a) %>% 
   group_by(size_class) %>% 
   mutate(catch=replace(catch, which(is.na(catch)), 0), 
          di = catch/ai, 
          weight=replace(weight, which(is.na(weight)), 0), 
          di_wt = weight/ai) -> catch.area


# dbar ----
# density variance for both count and weight and error bars
# average biomass per unit area

#calculate for >100 mm shells
catch.area %>% left_join(samples) %>% filter(size_class==1) %>% group_by(Bed) %>% 
   summarise(dbar=(1/mean(n)*sum(di)),
             var_dbar=1/(mean(n)-1)*sum((di-dbar)^2), 
             error = qt(0.975,df=mean(n)-1)*sqrt(var_dbar)/sqrt(mean(n)),
             ll = dbar-error,
             ul=dbar+error,
             ss = sum((di-dbar)^2),
             
             dbar_wt=(1/mean(n)*sum(di_wt)), 
             var_dbar_wt=1/(mean(n)-1)*sum((di_wt-dbar_wt)^2), 
             error_wt = qt(0.975,df=mean(n)-1)*sqrt(var_dbar_wt)/sqrt(mean(n)),
             ll_wt = dbar_wt-error_wt,
             ul_wt=dbar_wt+error_wt,
             ss_wt = sum((di_wt-dbar_wt)^2),
             n=mean(n)) %>% mutate(size='large')-> large 

#calculate for <100 mm shells
catch.area %>% left_join(samples) %>% filter(size_class==2) %>% group_by(Bed) %>% 
   summarise(dbar=1/mean(n)*sum(di),
             var_dbar=1/(mean(n)-1)*sum((di-dbar)^2), 
             error = qt(0.975,df=mean(n)-1)*sqrt(var_dbar)/sqrt(mean(n)),
             ll = dbar-error,
             ul=dbar+error,
             ss = sum((di-dbar)^2),
             
             dbar_wt=1/mean(n)*sum(di_wt), 
             var_dbar_wt=1/(mean(n)-1)*sum((di_wt-dbar_wt)^2), 
             error_wt = qt(0.975,df=mean(n)-1)*sqrt(var_dbar_wt)/sqrt(mean(n)),
             ll_wt = dbar_wt-error_wt,
             ul_wt=dbar_wt+error_wt,
             ss_wt = sum((di_wt-dbar_wt)^2),
             n=mean(n)) %>% mutate(size='small')-> small

#calculate for all shells caught
catch.area %>% left_join(samples) %>% group_by(Bed) %>% 
   summarise(dbar=(1/mean(n)*sum(di)),
             var_dbar=1/(mean(n)-1)*sum((di-dbar)^2), 
             error = qt(0.975,df=mean(n)-1)*sqrt(var_dbar)/sqrt(mean(n)),
             ll = dbar-error,
             ul=dbar+error,
             ss = sum((di-dbar)^2),
             
             dbar_wt=(1/mean(n)*sum(di_wt)), 
             var_dbar_wt=1/(mean(n)-1)*sum((di_wt-dbar_wt)^2), 
             error_wt = qt(0.975,df=mean(n)-1)*sqrt(var_dbar_wt)/sqrt(mean(n)),
             ll_wt = dbar_wt-error_wt,
             ul_wt=dbar_wt+error_wt,
             ss_wt = sum((di_wt-dbar_wt)^2),
             n=mean(n)) %>% mutate(size='all')-> all 


combi <- rbind(large,small)
combi <- rbind(combi, all)

# N ----
# Q = 0.83
Q=0.83
combi %>% left_join(samples) %>% group_by(size,Bed) %>% summarise(N = dbar*area/Q, N_wt = dbar_wt*area/Q) -> N

# var N ----
combi %>% left_join(N) -> abundance



abundance %>% left_join(areas) %>% mutate(var = (area/Q)^2*(1/n)*(1/(n-1))*ss, wt_var = (area/0.83)^2*1/n*1/(n-1)*ss_wt,
                                          UL=N+2*sqrt(var)/sqrt(n), 
                                          LL=N-2*sqrt(var)/sqrt(n), 
                                          UL_wt=N_wt+2*sqrt(wt_var)/sqrt(n),
                                          LL_wt=N_wt-2*sqrt(wt_var)/sqrt(n)) %>% 
   select(Bed, dbar, ll, ul,dbar_wt, ll_wt, ul_wt, size, N, UL, LL, N_wt, UL_wt, LL_wt)-> variances

ggplot(variances, aes(Bed, N, color=size))+geom_point()+geom_errorbar(aes(ymin=LL, ymax=UL), width=.2)+facet_grid(.~size)
ggplot(variances, aes(Bed, N_wt, color=size))+geom_point()+geom_errorbar(aes(ymin=LL_wt, ymax=UL_wt), width=.2)+facet_grid(.~size)

variances
