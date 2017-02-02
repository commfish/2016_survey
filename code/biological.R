# load ----
library(tidyverse)
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
library (xtable)
library (knitr)
options (scipen = 999)

# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016_161027.csv')
events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
  select(year=year, date=date, Event = EVENT_ID, District = DISTRICT, 
         Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
         slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, 
         stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, 
         elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, 
         calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,
         performance=GEAR_PERFORMANCE_CODE_SW, Vessel=VESSEL_NAME) %>% 
  filter (performance == 1, Type != "Ancillary") -> event

catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE)  %>%
  filter (Event %in% event$Event) -> catch

awl <- read.csv('./data/awl_2016_161027.csv')
awl %>% select(Event = EVENT_ID, species=RACE_CODE, weight=WHOLE_WT_GRAMS, 
               worm=SHELL_WORM_SW, height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, clapper = CLAPPER, 
               sample_type = SAMPLE_TYPE, meat_wt = MEAT_WEIGHT_GRAMS) %>%
  filter (Event %in% event$Event) -> awl


# height ----
#height/weight relationship
awl %>% left_join(event) %>% filter(Bed=='KSH1') %>% 
   ggplot(aes(height, fill=Vessel))+geom_density(alpha=.2)
   


#ggsave("./figs/Fig1.tiff", dpi=300, height=6, width=6, units="in")

#height densities - this way includes <100mm shells
awl %>% left_join(event) %>% 
  ggplot(aes(height))+geom_density(alpha=.2,fill=4)+facet_wrap(~Bed)

#height densities - 
awl %>% left_join(event) %>% filter(Bed=='KSH1', sex!='NA') %>% 
  ggplot(aes(height, fill=factor(sex),color=factor(sex)))+geom_density(alpha=.2)+facet_wrap(~Bed)
# getting some issues between using 0 and using NA - need to examine this.

# OTHER SCAL BIOLOGICAL DATA TABLES ----

awl %>% filter(species == 74120, is.na(clapper)) %>% left_join (event) -> bio # not adding events with no scals

# freq table for each variable by bed. 
worm <- table(bio$worm, bio$Bed) 
gonad <- table(bio$gonad_cond, bio$Bed) 
blist <- table(bio$blister, bio$Bed) 
weak <- table(bio$meat_cond, bio$Bed) 
clap <- table(bio$clapper, bio$Bed) 

# prop tables 
worm.p <- round(prop.table(worm, 2) * 100, 2) 
gonad.p <- round(prop.table(gonad, 2) * 100, 2)
blist.p <- round(prop.table(blist, 2) * 100, 2) 
weak.p <- round(prop.table(weak, 2) * 100, 2) 

# bind n row to prop table 
worm.pn <-  rbind(worm.p, margin.table(worm, 2))
gonad.pn <- rbind(gonad.p, margin.table(gonad, 2))
blist.pn <- rbind(blist.p, margin.table(blist, 2))
weak.pn <-  rbind(weak.p, margin.table(weak, 2))

# change row names.  Should use LUTs or some other type of substitution, in case classes observed in future changes. 
worm.pn <- as.data.frame(worm.pn) %>% mutate(Percent=c("0%", "1 - 24%", "25 - 49%", "50 - 74%", "75 - 100%", "n"))
gonad.pn <- as.data.frame(gonad.pn) %>% mutate(Stage=c("Immature", "Empty", "Initial Recovery", "Filling", "Full", "Cannot Determine", "n"))
blist.pn <- as.data.frame(blist.pn) %>% mutate(Percent = c("0%", "1 - 24%", "25 - 49%", "50 - 74%", "n"))   # no 4's (75-100%) this year
weak.pn <- as.data.frame(weak.pn) %>% mutate(Meats = c("Good", "Weak", "n"))

# reorder kayak beds together
worm.pn <- worm.pn[,c("Percent","EK1","WK1","KSH1", "KSH2", "KSH3")]
gonad.pn <- gonad.pn[,c("Stage","EK1","WK1","KSH1", "KSH2", "KSH3")]
blist.pn <- blist.pn[,c("Percent","EK1","WK1","KSH1", "KSH2", "KSH3")]
weak.pn <- weak.pn[,c('Meats',"EK1","WK1","KSH1", "KSH2", "KSH3")]


write_csv(worm.pn,'./output/worm.csv')
write_csv(gonad.pn,'./output/gonad.csv')
write_csv(blist.pn,'./output/blist.csv')
write_csv(weak.pn,'./output/weak.csv')



awl %>% filter(species == 74120, !is.na(clapper)) %>% 
   left_join(event) %>% 
   ggplot(aes(height))+geom_histogram(fill=4, alpha=.2, color=1,bins=50)+facet_wrap(~Bed, ncol=1,scale='free_y')+
   xlab('Shell height (mm)')+ylab('Number') + theme(strip.background = element_blank())

ggsave("./figs/Clappers.png", dpi=300, height=8.5, width=6.5, units="in")

# # cumbersome code to limit number of digits row-wise. Bottom row is n, others are %. 
# worm.dig <- matrix  (c(rep(2,nrow(worm.pn) - 1),0),  nrow=nrow(worm.pn),  ncol=ncol(worm.pn) + 1) 
# gonad.dig <- matrix (c(rep(2,nrow(gonad.pn) - 1),0), nrow=nrow(gonad.pn), ncol=ncol(gonad.pn) + 1) 
# blist.dig <- matrix (c(rep(2,nrow(blist.pn) - 1),0), nrow=nrow(blist.pn), ncol=ncol(blist.pn) + 1) 
# weak.dig <- matrix  (c(rep(2,nrow(weak.pn) - 1),0),  nrow=nrow(weak.pn),  ncol=ncol(weak.pn) + 1) 
# 
# # html tables. Insert this to rmd. Latex tables look better but not supported by word.
# print(xtable(worm.pn,digits=worm.dig), type = 'html')
# print(xtable(gonad.pn,digits=gonad.dig), type = 'html')
# print(xtable(blist.pn,digits=blist.dig), type = 'html')
# print(xtable(weak.pn,digits=weak.dig), type = 'html')  
# 
# # kable tables, as preview. but kable doesn't preserve digit formating.
# kable(xtable(worm.pn,digits=worm.dig))  
# kable(xtable(gonad.pn,digits=gonad.dig)) 
# kable(xtable(blist.pn,digits=blist.dig)) 
# kable(xtable(weak.pn,digits=weak.dig))   
# 
# options(scipen = 0)
