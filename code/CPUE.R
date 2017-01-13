#  ###################
#    CPUE           ##
######################
#Josh Mumm  170112
#Calc CPUE (cnt/nmi). Tanners only for now.  Could expand to other species. 
#May combine steps. 

library (tidyverse)

## LOAD DATA #############################################################################
#clean up event data- change names and data types etc.
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

## ESTIMATE CATCH BY TOW, then CPUE, and aggregate by Bed #################################

# sum weights of all species by sample type and tow
catch %>% group_by(Event) %>% summarise(
    t1 = sum(sample_wt[sample_type == 1]),
    t2 = sum(sample_wt[sample_type == 2])) %>%
    right_join (event) %>% mutate (t1 = replace(t1, which(is.na(t1)), 0),  # join events with no catch and set NA to 0. 
                                   t2 = replace(t2, which(is.na(t2)), 0)) -> totWt

# sum counts of species of interest by sample type tow
s <- 68560 # Tanners for now
catch %>% filter(species == s) %>% group_by(Event) %>% 
  summarise (s1 = sum(count[sample_type == 1]), 
             s2 = sum(count[sample_type == 2])) ->  spCnt

#join total wts to species count and set NAs to 0
left_join(totWt, spCnt) %>% mutate (s1 = replace(s1, which(is.na(s1)), 0),
                                    s2 = replace(s2, which(is.na(s2)), 0)) -> comb
  
# expand any T2's, sum components of catch, and calc CPUE   
comb %>% mutate ( sTot = s1 + s2 + if_else(t2>0, (t1*s2/t2), 0), # catch of species s by event.  Con to prevent div by 0.
                  sCPM = sTot/length                    # CPUE cnt/nmi 
                  ) -> c 

# aggregate CPUE by bed
c %>% group_by(Bed) %>% summarise ( mean = mean(sCPM), 
                                    sd = sqrt(var(sCPM)), 
                                    cv = 100 * sd/mean) -> cpm
cpm   

                  



             
             
                                
  

