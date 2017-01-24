####### 2016 Scallop Statewide Survey
####### Ben Williams / Katie Palof
####### ben.williams@alaska.gov

### Scallop survey data summarized by MEAT WEIGHTS

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

#### survey/ event data -------------------
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



# meat weight ----
# load event data above first (or have it loaded from either number or weight code files)
awl <- read.csv('data/awl_2016_161027.csv')


awl %>% select(Event = EVENT_ID,  species=RACE_CODE,
               j = SCALLOP_NUMBER, size_class = SCAL_SIZE_CLASS,
               weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, 
               height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, meat_weight = MEAT_WEIGHT_GRAMS,
               clapper = CLAPPER, sample_type = SAMPLE_TYPE) %>% 
  mutate(ratio = meat_weight/weight ) %>% 
  filter(species == 74120, size_class == 1, is.na(clapper), !is.na(ratio), 
         Event %in% event$Event) %>% 
  select(Event,j,ratio)-> samp

as.data.frame(do.call(rbind,scal.catch$dat)) %>% filter(variable=='large') %>% 
  left_join(samp) %>% filter(ratio>0)-> meat.wts

#awl 10 represenative of 40? ------------
awl %>% select(Event = EVENT_ID,  species=RACE_CODE,
                   j = SCALLOP_NUMBER, size_class = SCAL_SIZE_CLASS,
                   weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, 
                   height=SHELL_HEIGHT_MM, meat_weight = MEAT_WEIGHT_GRAMS,
                   clapper = CLAPPER, sample_type = SAMPLE_TYPE) %>% 
  filter(species == 74120, size_class == 1, is.na(clapper), !is.na(height), 
         Event %in% event$Event ) %>% mutate(m_weight = ifelse(!is.na(weight), "mw", 'ht'))%>%
  select(Event, j, height, weight, meat_weight, m_weight) ->samp2


#sample size for each event - n
samp2 %>% group_by(Event) %>% mutate(maxj = max(j), n = n()) %>% select(Event, maxj,n)-> samp2_ssize
# only include Event is n is > 11
samp2_ssize %>% 
  group_by(Event) %>% summarise(n =mean(n)) %>% 
  filter(n >11) -> samp2_ssize2 # Events with large enough samples sizes for kstest

# for each event does m_weight group 1 represent heights in m_weight group 2 ?
samp2 %>% filter(Event %in% samp2_ssize2$Event) -> samp3

samp3 %>% 
  group_by(Event) %>% do(dat=(.)) %>% select(dat) %>% map(identity) -> samp3.list # list for each event
#need to filter for those events that don't have both...

#K-S test
ks_height <- do.call(rbind, lapply(samp3.list$dat, ks_func))

samp2 %>% filter(Event == '2016D02003') -> test1
test1 %>% spread(m_weight, height, fill =NA) -> test1a
hat <- ks.test(test1a$ht, test1a$mw)
out = c(test1a$Event[1], hat$p.value)
ks_func(test1)

ks_func <- function(x){
  #first turn the list into a dataframe
  #use dplyr to seperate into two groups y and z to compare
  # output is event id and p-value
  x = as.data.frame(x)
  x %>% spread(m_weight, height, fill=NA) ->inter
  ks <-ks.test(inter$ht, inter$mw)
  out <- cbind(inter$Event[1], ks$p.value)
  out
}

#turn meat weights into list for analysis
meat.wts %>% 
  group_by(Bed) %>% 
  do(dat=(.)) %>% 
  select(dat) %>% 
  map(identity) -> meat.wt

# run bootstrap on meat weights
source('./code/functions.R')
wts <- do.call(rbind,lapply(meat.wt$dat,f.wt))

wts %>% group_by(year, District, Bed) %>% 
  summarise(ratio_bar = mean(ratio), ll = quantile(ratio, .025), 
            ul = quantile(ratio, .975)) -> wts_summary

# wts is meat weight and weights is round weight

# summarize meat weight with event - run 'abundance_numbers.R' first!!!!!!!
# use conversion to convert from grams to lbs, 1 lb = 453.592 grams
awl %>% select(Event = EVENT_ID,  species=RACE_CODE,
               j = SCALLOP_NUMBER, size_class = SCAL_SIZE_CLASS,
               weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, 
               height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, meat_weight = MEAT_WEIGHT_GRAMS,
               clapper = CLAPPER, sample_type = SAMPLE_TYPE) %>% 
  mutate(ratio = meat_weight/weight ) %>% 
  filter(species == 74120, size_class == 1, is.na(clapper), !is.na(ratio), 
         Event %in% event$Event) %>% 
  left_join(event) %>% group_by(year, District, Bed, Event) %>% 
  summarise(weight = mean(weight)) %>% 
  group_by(year, Bed) %>% summarise(weight = mean(weight)) %>% left_join(bed_num_summary) %>% 
  filter(variable=='large') %>% 
  dplyr::select(Bed,year,llN,ulN,N_b,weight) %>% 
  left_join(wts_summary) %>% 
  mutate(min_meat_wt=llN*ll*weight/453.592, meat_wt = N_b*ratio_bar*weight/453.592,meat_0.05 =meat_wt*0.05,
         max_meat_wt=ulN*ul*weight/453.592)  -> meat_weight_summary


meat_weight_summary %>% data.frame() #using weight from invididual scallops.


### save tables and figures if needed ------------------------------------
write_csv(meat.weight_summary, 'output/bed_meatwt_table_Ndbar.csv')




