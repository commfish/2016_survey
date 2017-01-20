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

# scallops ----
# catch ----
# create catch data.frame
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
   
# function to summarize
f.sum <- function(x){
  # first turn the list to a dataframe
  # use dplyr to summarise each list
  # output is one row all stats.
  
  x = as.data.frame(x)
  x %>%
    group_by(year, District, Bed, variable)%>%
    summarise(n=mean(n),
              area = mean(area_nm2) ,
              dbar = (1/n*sum(di)),
              var_dbar=1/((n)-1)*sum((di-dbar)^2) ,
              cv=sqrt(var_dbar)/dbar*100,
              ss=sum((di-dbar)^2),
              N=area*dbar,
              varN=(area^2)*1/n*1/(n-1)*ss,
              cvN=sqrt(varN)/N*100) -> out
   out
  }
  
numbers_original <- lapply(scal.catch$dat,f.sum)
numbers_original <- as.data.frame(do.call(rbind,numbers_original)) 
 
#table of results

write_csv(numbers_original, 'output/original_numbers_NO_BOOT.csv')

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

# bootstrap ----
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


# catch ----
numbers <- lapply(scal.catch$dat,f.it)
numbers <- as.data.frame(do.call(rbind,numbers))

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
   ggplot(aes(Bed,N))+geom_point()+geom_errorbar(aes(ymin=llN,ymax=ulN), width=0.2)+facet_wrap(~variable)+
   scale_x_discrete(limits=c('EK1','WK1','KSH1','KSH2','KSH3'))+ scale_y_continuous(labels = comma) -> fig_bedN

#save plot for write up
png(filename = 'figs/bed_abundance_wCI.png')
fig_bedN
dev.off()

#table of results
numbers %>% group_by(Bed,year,variable) %>% 
  summarise(llN=quantile(N,0.025),ulN=quantile(N,0.975),N_b=mean(N), 
            lldbar=quantile(dbar,0.025),uldbar=quantile(dbar,0.975),dbar_b=mean(dbar), 
            var_dbar = 1/((n())-1)*sum((dbar-dbar_b)^2) ,
            cv=sqrt(var_dbar)/dbar_b*100 , 
            varN= 1/((n())-1)*sum((N-N_b)^2),
            cvN=sqrt(varN)/N_b*100) -> bed_summary
write_csv(bed_summary, 'output/bed_sum_table_Ndbar.csv')

# weight ----
# apply the function to each component of the list
weight <- lapply(scal.weight$dat,f.it)

# bind the results together and convert to a dataframe
weight <- as.data.frame(do.call(rbind,weight))


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

#save plot for write up
png(filename = 'figs/bed_weight_wCI.png')
fig_bed_weight
dev.off()

# meat weight ----
awl <- read.csv('data/awl_2016_161027.csv')

awl %>% select(Event = EVENT_ID,  species=RACE_CODE,
               j = SCALLOP_NUMBER, size_class = SCAL_SIZE_CLASS,
               weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, 
               height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, meat_weight = MEAT_WEIGHT_GRAMS,
               clapper = CLAPPER, sample_type = SAMPLE_TYPE) %>% 
   mutate(ratio = meat_weight/weight ) %>% 
   filter(species == 74120, size_class == 1, is.na(clapper), !is.na(ratio), Event %in% event$Event) %>% 
   select(Event,j,ratio)-> samp

as.data.frame(do.call(rbind,scal.catch$dat)) %>% filter(variable=='large') %>% left_join(samp) %>% filter(ratio>0)-> meat.wts

# meat.wts %>% group_by(Event,Bed,year) %>% summarise(jn=max(j)) %>% filter(jn>1) -> inter
# meat.wts %>% group_by(Event,Bed,year) %>% summarise(jn=max(j)) %>% filter(jn==1) -> inter1
# meat.wts %>% left_join(inter) %>% filter(Event %in% inter$Event) -> meat.wt2
# meat.wts %>% left_join(inter1) %>% filter(Event %in% inter1$Event) %>% select(ratio_bar=ratio,Event,year,District,Bed)-> meat.wt3

# bootstrap II----
f.wt <- function(x){
   # function bootstraps meat weight ratio by bed, not by individual event
   # first turn the list to a dataframe
   # small function to group and calculate mean for each bootstrap sample
   # replicate each sample 1000 x by year bed, district etc
   # calculate ratio with function
   
   x = as.data.frame(x)
   
   f.do <- function(y){
      y %>% 
         group_by(year,District,Bed) %>% 
         summarise(ratio = mean(ratio))
   }
   
  replicate(1000,sample_n(x, nrow(x), replace=T), simplify=FALSE) %>% 
      lapply(., f.do) %>% 
      bind_rows %>% 
      mutate(replicate=1:n())
}

#turn meat weights into list for analysis
meat.wts %>% 
   group_by(Bed) %>% 
   do(dat=(.)) %>% 
   select(dat) %>% 
   map(identity) -> meat.wt

# combine all the weight data
wts <- do.call(rbind,lapply(meat.wt$dat,f.wt))

wts %>% group_by(year, District, Bed) %>% 
   summarise(ratio_bar = mean(ratio), ll = quantile(ratio, .025), ul = quantile(ratio, .975)) -> wts_summary

bed_summary %>% 
   filter(variable=='large') %>% 
   dplyr::select(Bed,year,llN,ulN,N_b) %>% 
   left_join(wts_summary) %>% 
   mutate(min_meat_wt=llN*ll, meat_wt = N_b*ratio_bar,max_meat_wt=ulN*ul)
