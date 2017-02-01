# load ----
library(tidyverse)
library(reshape2)
library(scales)
theme_set(theme_bw()+ 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()))
source('./code/functions.R')

# data ----
events <- read.csv('./data/events_2016_161027.csv')
catch <- read.csv('./data/catchComp_2016_161027.csv')
area <- read.csv('./data/area.csv')
awl <- read.csv('data/awl_2016_161027.csv')

# event data ----
#clean up event data- change names and data types etc.

events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
   select(year, date, Event = EVENT_ID, District = DISTRICT, 
          Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
          slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, 
          stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, 
          elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, 
          calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,
          performance=GEAR_PERFORMANCE_CODE_SW, Vessel=VESSEL_NAME) %>% filter(performance==1)-> event

#check for replicates - if dataframe has values there are duplicates
event %>% group_by(Bed, ID) %>% filter(n()>1)

# join area and event dataframes
# n = number of stations sampled by bed
event %>%  #ai is in nmi^2
   group_by(Bed) %>% 
   summarise(n=n()) %>% 
   left_join(area) %>% select(-grids)-> samples

# Q = 0.83
Q <- 0.83

# add ai column to event dataframe
# Dredge width in nmi = 0.00131663 x length of dredging in each station x efficiency
event %>% mutate(ai = length * 0.00131663 * Q) %>% left_join(samples) -> event

# Catch data ----
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE) -> catch


# meat weight data ----
awl %>% select(Event = EVENT_ID,  species=RACE_CODE,
               j = SCALLOP_NUMBER, size_class = SCAL_SIZE_CLASS,
               weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, 
               height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, meat_weight = MEAT_WEIGHT_GRAMS,
               clapper = CLAPPER, sample_type = SAMPLE_TYPE) -> awl
awl %>% 
   mutate(ratio = meat_weight/weight ) %>% 
   filter(species == 74120, size_class == 1, is.na(clapper), !is.na(ratio), 
          Event %in% event$Event) -> meat_weight


# catch ----
# create number and weight dataframes
catch %>% filter(species==74120, cond==1) %>% 
   group_by(Event, size_class) %>% 
   summarise(catch=sum(count, na.rm=T)) %>% 
   dcast(Event~size_class,sum, drop=TRUE) -> s.catch 

catch %>% filter(species==74120, cond==1) %>% 
   group_by(Event, size_class) %>% 
   summarise(weight=sum(sample_wt*1000, na.rm=T)) %>% # change to grams 
   dcast(Event~size_class,sum, drop=TRUE) -> s.weight 

names(s.catch) <- c('Event', 'large', 'small')
names(s.weight) <- c('Event', 'large', 'small')

# Abundance ----
# numbers ----
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

numbers_original <- lapply(scal.catch$dat,f.sum)
numbers_original <- as.data.frame(do.call(rbind,numbers_original)) 

# bootstrap N----
numbers <- lapply(scal.catch$dat,f.it)
numbers <- as.data.frame(do.call(rbind,numbers))

numbers %>% group_by(Bed,year,variable) %>% 
   summarise(N_b=mean(N),
             llN=quantile(N,0.025),
             ulN=quantile(N,0.975),
             varN = 1/((n())-1)*sum((N-N_b)^2),
             cvN=sqrt(varN)/N_b*100,
             dbar_b=mean(dbar), 
             lldbar=quantile(dbar,0.025),
             uldbar=quantile(dbar,0.975),
             var_dbar = 1/((n())-1)*sum((dbar-dbar_b)^2) ,
             cv=sqrt(var_dbar)/dbar_b*100) -> N_summary

# weights ----
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

weights_original <- lapply(scal.weight$dat,f.sum)
weights_original <- as.data.frame(do.call(rbind,weights_original)) 

# bootstrap weight----
weights <- lapply(scal.weight$dat,f.it)
weights <- as.data.frame(do.call(rbind,weights))
weights %>% mutate(W=N*0.00220462) -> weights # change to pounds

weights %>% group_by(District,Bed,year,variable) %>% 
   summarise(llW=quantile(W,0.025),ulW=quantile(W,0.975),Weight=mean(W), 
             lldbar=quantile(dbar,0.025),uldbar=quantile(dbar,0.975),dbar_lb=mean(dbar),
             varW = 1/((n())-1)*sum((W-Weight)^2),
             cvW=sqrt(varW)/Weight*100) -> weights_summary

# meat weight ----
as.data.frame(do.call(rbind,scal.catch$dat)) %>% filter(variable=='large') %>% 
   left_join(meat_weight) %>% filter(ratio>0)-> meat.wts

# #K-S test ----
# Comparing distribution between meat weight sample and height sample
# #10 meat weight samples represenative of 40 shell heights?
# awl %>% 
# 	filter(species == 74120, size_class == 1, is.na(clapper), !is.na(height), 
# 			 Event %in% event$Event ) %>% mutate(m_weight = ifelse(!is.na(weight), "mw", 'ht'))%>%
# 	select(Event, j, height, weight, meat_weight, m_weight) -> meat_weight2
# 
# #sample size for each event - n
# meat_weight2 %>% group_by(Event) %>% 
# 	mutate(maxj = max(j), n = n()) %>% 
# 	select(Event, maxj,n) %>% 
# 	group_by(Event) %>% 
# 	summarise(n =mean(n)) %>% # only include Event if n is > 11
# 	filter(n >11) -> ssize # Events with large enough samples sizes for kstest
# 
# # for each event does m_weight group 1 represent heights in m_weight group 2 ?
# meat_weight2 %>% filter(Event %in% ssize$Event) %>%  
# 	group_by(Event) %>% do(dat=(.)) %>% select(dat) %>% map(identity) -> meat_weight3 # list for each event
# #need to filter for those events that don't have both...

# ks_height <- do.call(rbind, lapply(meat_weight3$dat[1:39], ks_func))
# ks_height2 <- do.call(rbind, lapply(meat_weight3$dat[41:61], ks_func))
# # Issue is with duplicate j values.  need individual value to compute.  so far only an issue with 
# # list 40.  test other lists.  if this is the case just remove 40.
# #problematic lists: 40
# ks_height %>% 
#    bind_rows(ks_height2) -> ks.height_all
# 
# mean(ks.height_all$p.value)
# ks.height_all %>% 
#    filter(p.value <= 0.05)

# meat weight bootstrap ----
#turn meat weights into list for analysis
meat.wts %>% 
   group_by(Bed) %>% 
   do(dat=(.)) %>% 
   select(dat) %>% 
   map(identity) -> meat.wt

meat.wts <- do.call(rbind,lapply(meat.wt$dat,f.wt))

meat.wts %>% group_by(year, District, Bed) %>% 
   summarise(ratio_bar = mean(ratio), ll = quantile(ratio, .025), 
             ul = quantile(ratio, .975)) -> meat.wts

# Numbers based GHL
awl %>% filter(species == 74120, size_class == 1, is.na(clapper), 
               Event %in% event$Event) %>% group_by(Event) %>% 
   summarise(mean_wt = mean(weight, na.rm=T)) %>% left_join(event) %>% 
   group_by(year,Bed) %>% summarise(mean_wt = mean(mean_wt)) %>% 
   left_join(N_summary) %>%  filter(variable=='large') %>% left_join(meat.wts) %>% 
   group_by(year, Bed) %>% 
   summarise(ll = ratio_bar*llN*mean_wt/453.592,
             meat = ratio_bar*N_b*mean_wt/453.592,
             ul = ratio_bar*ulN*mean_wt/453.592,
             GHL.05 = meat * .05,
             lowGHL.05 = ll * .05,
             highGHL.05 = ul * .05,
             GHL.10 = meat * .10,
             lowGHL.10 = ll * .10,
             highGHL.10 = ul * .10) -> number_GHL

# Weight based GHL 
meat.wts %>% left_join(weights_summary) %>% 
   filter(variable=='large') %>% group_by(Bed) %>% 
   summarise(ll = ratio_bar*llW,
             meat = ratio_bar*Weight,
             ul = ratio_bar*ulW,
             GHL.05 = meat * .05,
             lowGHL.05 = ll * .05,
             highGHL.05 = ul * .05,
             GHL.10 = meat * .10,
             lowGHL.10 = ll * .10,
             highGHL.10 = ul * .10) -> weight_GHL





# Clappers ----
catch %>% filter(species==74120, cond==99) %>% 
   group_by(Event) %>% summarise(weight=sum(sample_wt, na.rm=T)) -> clap.weight

clap.weight <- merge(clap.weight,event, all = TRUE) # merge with events - keep NA
clap.weight[is.na(clap.weight)] <- 0 # change NA to 0
clap.weight %>% dplyr::select(Event, weight, year,District,Bed,n,ai,area_nm2) %>% 
   mutate(di= weight/ai, clap_wt = weight) %>% select(-weight) %>% 
   group_by(District,Bed,year) %>% 
   do(dat=(.)) %>% 
   select(dat) %>% 
   map(identity) -> clap.weight

clappers_bed <- lapply(clap.weight$dat,f.clap)
clappers_bed <- as.data.frame(do.call(rbind,clappers_bed)) 
clappers_bed %>% mutate(dbar_c_lb = dbar_c*2.2046, W_c_lb = W_c*2.2046) -> clappers_bed
# convert kilograms to lbs.  

#Percentage of clappers per bed.
weights_summary %>% filter(variable == 'all') %>%  
   right_join(clappers_bed) %>% select(District, Bed, year, n, variable, Weight,W_c_lb) %>% 
   mutate(percent_clap = (W_c_lb/ (Weight + W_c_lb)*100))

# figures ----
# Numbers
numbers %>% filter(variable=='large') %>% 
   ggplot(aes(dbar, fill=Bed))+geom_density()+ facet_wrap(~Bed)

numbers %>% filter(variable=='large') %>% 
   ggplot(aes(dbar, fill=Bed))+geom_density()

numbers %>% filter(variable=='small') %>% 
   ggplot(aes(N, fill=Bed))+geom_density() 

N_summary %>% group_by(Bed,variable) %>%    
   ggplot(aes(Bed,N_b/1000000))+geom_point()+
   geom_errorbar(aes(ymin=llN/1000000,ymax=ulN/1000000), width=0.2)+
   facet_wrap(~variable)+
   scale_x_discrete(limits=c('EK1','WK1','KSH1','KSH2','KSH3'))+ 
   ylab("Abundance (millions)")

ggsave("./figs/Abundance.png", dpi=300, height=4.5, width=6.5, units="in")


# Weights
meat.wts %>%  
   ggplot(aes(ratio_bar, fill=Bed))+geom_density()+ facet_wrap(~Bed)

weight %>% filter(variable=='large') %>% 
   ggplot(aes(dbar_lb, fill=Bed))+geom_density()

weight %>% filter(variable=='small') %>% 
   ggplot(aes(N_lb, fill=Bed))+geom_density() + facet_wrap(~Bed)

weights_summary%>% 
   group_by(Bed,variable) %>% 
   ggplot(aes(Bed,Weight/1000000))+geom_point()+geom_errorbar(aes(ymin=llW/1000000,ymax=ulW/1000000), width=0.2)+facet_wrap(~variable)+
   scale_x_discrete(limits=c('EK1','WK1','KSH1','KSH2','KSH3'))+ scale_y_continuous(labels = comma) +
   ylab("Round weight (million lb)") 

ggsave("./figs/Weight.png", dpi=300, height=4.5, width=6.5, units="in")

# Meat weight

meat.wts %>% 
   ggplot(aes(Bed,ratio_bar))+geom_point()+geom_errorbar(aes(ymin=ll,ymax=ul), width=0.2)+
   scale_x_discrete(limits=c('EK1','WK1','KSH1','KSH2','KSH3'))+ 
   ylab("Meat weight / Round weight") 

ggsave("./figs/Ratio.png", dpi=300, height=4.5, width=6.5, units="in")




# Tables ----
write_csv(numbers_original, 'output/numbers_original.csv')
write_csv(N_summary, 'output/N_summary.csv')
write_csv(weights_summary, 'output/weights_summary.csv')
write_csv(meat.wts, 'output/meat.wts.csv')
write_csv(weight_GHL, 'output/weight_GHL.csv')
write_csv(number_GHL, 'output/number_GHL.csv')

