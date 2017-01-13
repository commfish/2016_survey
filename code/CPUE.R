## ###################
#    CPUE           ##
######################
#Josh Mumm  170113
#Calc CPUE (cnt/nmi). Tanners only for now, could expand to other species. 

library (tidyverse)

## LOAD DATA ##########################################################################

events <- read.csv('./data/events_2016_161027.csv')
events %>% filter (GEAR_PERFORMANCE_CODE_SW == 1, STATION_TYPE != "Ancillary") %>%
            select( Event = EVENT_ID,
                    District = DISTRICT, 
                    Bed = BED_SW,
                    length=TOW_LENGTH_DESIGNATED) -> event

catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE) %>% filter(Event %in% event$Event) -> catch

## ESTIMATE CATCH BY TOW, then CPUE, and aggregate by Bed ##############################

s <- 68560 # Tanners for now

# aggregate unsorted catch and species interest 
catch %>% group_by(Event) %>% 
    summarise(t1 = sum(sample_wt[species == 99997 & sample_type == 1]),
              t2 = sum(sample_wt[species == 99997 & sample_type == 2]),
              s1 = sum(count[species == s & sample_type == 1]), 
              s2 = sum(count[species == s & sample_type == 2])) %>% 
  # join to event for Bed and set any events with no catch to 0 (none in 2016). 
      right_join (event) %>% replace_na (list(t1 = 0, t2 = 0, s1 = 0, s2 = 0)) %>%
  # expand any T2's, sum components of catch, and calc CPUE   
      mutate ( sTot = s1 + s2 + if_else(t2>0, (t1*s2/t2), 0), #expanded catch of species s by event.  Con to prevent div by 0.
               sCPM = sTot/length)  %>%                    # CPUE cnt/nmi 
  # aggregate by Bed     
      group_by(Bed) %>% summarise ( cpue_mean = mean(sCPM), 
                                    cpue_sd = sqrt(var(sCPM)), 
                                    cpue_cv = 100 * cpue_sd/cpue_mean,
                                    totCatch = sum(sTot)) -> cpm
cpm # CPUE (cnt/nmi) with sd, cv and total expanded catch (cnt) by bed, all for sp. s.     



             
             
                                
  

