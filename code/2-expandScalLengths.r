###############################
## EXPAND SCAL LENGTHS       ##
###############################
# josh mumm 
# accounts for unequal sampleing density of small vs large
# by expanding sample size distribution of each to the total number caught by event  


## PREP ----
set.seed(15343437) 
library (reshape2)
library (FSA)
library(tidyverse)

awl <- read.csv('./data/awl_2016_161027.csv')
event <- read.csv('./data/events_2016_161027.csv')
cc <- read.csv('./data/catchComp_2016_161027.csv')

awl %>% filter (RACE_CODE==74120 & is.na(CLAPPER)) %>% select(Event = EVENT_ID, sc=SCAL_SIZE_CLASS, sh=SHELL_HEIGHT_MM) -> awl  
event %>% filter (GEAR_PERFORMANCE_CODE_SW == 1 & STATION_TYPE != 'Ancillary') %>% select(Event = EVENT_ID, Bed = BED_SW) -> event
cc %>% filter (RACE_CODE==74120 & CONDITION_CODE_RII == 1) %>% select(Event = EVENT_ID, sc = SCAL_SIZE_CLASS, cnt = COUNT) -> cc

#join bed from events to awl
awl <- merge(awl, event, by = "Event", all.x = T) 
cc  <- merge(cc, event, by = "Event", all.x = T)

# totals by size class for each event 
cc %>% group_by(Event,Bed,sc) %>% summarise (tot = sum(cnt)) -> tot 
tot_w <- dcast(tot, Event + Bed ~ sc, value.var = "tot")     # cast tot from long to wide
names(tot_w)[3:4] <- c('L', 'S')
#replace NA cnts with 0  
tot_w$L[is.na(tot_w$L)] <- 0    
tot_w$S[is.na(tot_w$S)] <- 0  

#create empty df to hold final expanded awl 
dat <-data.frame(Event=factor(), Bed = factor(), sh = double())
###############################################################################################

## EXPAND SIZE DISTRIBUTIONS ---- 
# for each event there will 2 seperate dists to expand -smlls + lrgs

events <- unique(tot_w$Event)  
length(events)      # note: apparently one event had no CC recs

#i <- '2016D02009' # start w one event - here 242 lrgs and 168 smls

for(i in events){    
  Sys.sleep(0.1)
  print ( c("####",paste (i, "   START") , "####"))
  
  
  
  #select one event from awl 
  awl.i <- awl[awl$Event == i,]
  
  #break that one event of AWL by sizeClass
  awl.i_1 <- subset (awl.i, sc == '1')
  awl.i_2 <- subset (awl.i, sc == '2')
  
  # num measured
  mL <-  nrow(awl.i_1)
  mL
  mS <-  nrow(awl.i_2)
  mS    
  
  # tot caught 
  tot.i <- tot_w[tot_w$Event == i,]
  tot.i
  totL <- tot.i$L
  totL
  totS <- tot.i$S
  totS
  
  #create empty vectors for expanded size dists so that object exists even if no scals in it,
  #prevents latter errors when combining
  eawl_1 <- numeric(0) 
  eawl_2 <- numeric(0)  
  
  #expand LARGES 
  if(totL > 0 & mL > 1 & totL- mL > 1)   # conditional to prevent terminal errors 
  {eawl_1 <- expandLenFreq(awl.i_1$sh, w=.1, total = totL, decimals = 1) }
  eawl_1 # note this is only the additiaonl ones, not measured.
  totL - mL # check , this number should equal number of measurements in above row
  
  #expand SMALLS
  if(totS > 0 & mS > 1 & totS - mS > 1)   
  {eawl_2 <- expandLenFreq(awl.i_2$sh, w=.1, total = totS, decimals = 1) }
  eawl_2 # note this is only the additiaonl ones, not measured.
  totS - mS # check , this value should equal number of measurements in above row
  
  #compare hists.  change for L and S
  par(mfrow= c(2,1))
  hist (awl.i_2$sh, breaks = seq(0,250,1), main  = c("measured", i), freq = T, col = 'red')
  hist (eawl_2, breaks = seq(0,250,1), main  = c("additional", i), freq = T, col = 'blue')
  
  
  #combine measured with additional lengths
  L <- c(awl.i_1$sh, eawl_1)
  S <- c(awl.i_2$sh, eawl_2)
  
  #compare totals in expanded awls to tots from CC
  length(L)
  length(S)
  totL
  totS
  
  #great totals match.   
  #now assign event and bed to vector of lenghts for each sc. 
  r <- length(L)
  L.df <- data.frame(Event = as.factor(rep(i, r)),
                     Bed = as.factor(rep(tot_w[tot_w$Event == i, "Bed"], r)),
                     sc = as.factor(rep("1", r)),
                     sh = L)
  str(L.df)
  
  r <- length(S)
  S.df <- data.frame(Event = as.factor(rep(i, r)),
                     Bed = as.factor(rep(tot_w[tot_w$Event == i, "Bed"], r)),
                     sc = as.factor(rep("2", r)),
                     sh = S)
  str(S.df)
  
  
  #and bind these dfs together as total expanded lenghts (measured + additional) event i 
  all.df <- rbind(L.df,S.df)
  str(all.df)
  sum(tot.i[,c(3,4)]) # compare to total form CC
  
  dat <- rbind(dat,all.df) # append to main dat for all events 
  tail(dat)
  
  Sys.sleep(0.5)
  print ( c("####",paste (i, "   COMPLETE") , "####"))
}
##################################################################################################

## WRITE ----
str(dat)
#write to disk.   This is the expanded sizes for all beds and events.   
#write.csv(dat, "2016_expandedHeights.csv")


## ERROR CHECKING ----                                                                                                   

#compare totals
nrow(dat)
sum(tot_w[,c("L","S")])
# hmmm 23 more in expanded than on total. Presumably due to rounding issue or similar(?). 

#### compare histograms of awl to dat (combined measured + expanded)

library (lattice) 
par(mfcol = c(2,1))

hist (~ sh , data = awl[awl$Event==i,], breaks = seq(0,200,1), main  = "measured", freq = TRUE ) 
hist (~ sh , data = dat[dat$Event==i,], breaks = seq(0,200,1), main  = "expanded", freq = TRUE)

beds <- (sort(unique(dat$Bed)))
for (i in beds) {
  hist (~ sh , data = subset(awl, Bed == i), breaks = seq(0,200,1), main  = c("measured", i), freq = T, col = 'red' )   
  hist (~ sh , data = subset(dat, Bed == i), breaks = seq(0,200,1), main  = c("expanded", i), freq = T, col = 'blue')
}

# seems reasonable.  Appear to be a few more expanded smalls in west than measured 
# this is what i'd expect if the sampled ratio was less for smalls than larges. 
