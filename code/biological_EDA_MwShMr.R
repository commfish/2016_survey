# load ----
library(tidyverse)
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
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
         performance=GEAR_PERFORMANCE_CODE_SW) %>% 
  filter (performance == 1, Type != "Ancillary") -> event

catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE)  %>%
  filter (Event %in% event$Event) -> catch

awl <- read.csv('./data/awl_2016_161027.csv')
awl %>% select(Event = EVENT_ID, species=RACE_CODE, weight=WHOLE_WT_GRAMS, SC = SCAL_SIZE_CLASS, meat = MEAT_WEIGHT_GRAMS,
               worm=SHELL_WORM_SW, height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, clapper = CLAPPER, 
               sample_type = SAMPLE_TYPE) %>%
  filter (Event %in% event$Event, species == 74120, is.na(clapper), !is.na(weight)) -> awl

##plot KHS1 WW/SH and MRR by month ----
#awl %>% filter(SC == 1) -> awl  # larges only for plots emailed. 
awl %>% left_join(event) -> awl 
awl$Month <- factor(months(awl$date), levels = c("January","February","March", "April","May","June","July","August","September", "October","November","December"))
#filter(awl, Month != "April") -> awl
awl$WH <- awl$weight/awl$height
filter(awl, Month == "May", Bed == "KSH1") -> may
filter(awl, Month == "July", Bed == "KSH1") -> july
par(mar=c(5.1,4.1,1.1,1.1))
plot(log(may$weight) ~ log(may$height), col = 'gray80', xlim = c(4.5, 5.4), 
     main = "", xlab = "log(Height (mm))", ylab = "log(Round Weight (g))", type = "n")
points(log(july$weight) ~ log(july$height), col = 'black', add = T, cex = .8 )
points(log(may$weight) ~ log(may$height), col = 'gray80', add = T, cex = .8, pch = 2)
legend(locator(1), c("May","July"), col= c('black','gray80'), cex = c(.8,.8), pch = c(1,2), bty = 'n')
mod.may <- lm(log(may$weight) ~ log(may$height))
abline(mod.may, add = T, col = "gray80")
mod.july <- lm(log(july$weight) ~ log(july$height))
abline(mod.july, add = T, col = 'black')


#densitiy plot weight/height KSH1 
 qplot(WH, data=awl, geom="density", fill=Month, alpha=I(.5),
       xlab="Weight/Height",
      ylab="Density") -> m
 m + scale_fill_manual(values=c("lightblue1", "plum2", "lightsalmon1"))
      
#Density plot of MRR (MW/WW) by month 
awl$MR <- awl$meat/awl$weight 
qplot(MR,  data=awl, geom="density", fill=Month, alpha=I(.5),
      xlab="Meat Weight/ Round Weight",
      ylab="Density") -> mr
mr + scale_fill_manual(values=c( "#f0f0f0",  "#bdbdbd", "#636363")) + theme(legend.position=c(.75, .75))


