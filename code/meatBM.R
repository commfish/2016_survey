## ##########################
# ESTIMATE BIOMASS of MEATS #     
############################# 
#Josh Mumm 170110
#Estimates biomass of meats by bed, as product of estimated meat recovery ratio and estimated whole weight biomass.
#Two stage random sample design with selection of tows the first level and selection of scals as the second level.
#Ratio estimator based on size of primary units. Variable definition and formulae available in MRR_formulae.docx.
#Run abundance.r first. Eventually may move inside abundance.r. And change to use boot var when complete.    

library(tidyverse)

## Load Data ###################################################

#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016_161027.csv')
events %>% select(Event = EVENT_ID,
         District = DISTRICT, 
         Bed = BED_SW,
         Type = STATION_TYPE,
         performance=GEAR_PERFORMANCE_CODE_SW
         ) %>% filter(performance == 1, Type != "Ancillary")   -> event   # exclude bad and ancillary tows. 

catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID,
         species=RACE_CODE, 
         sc=SCAL_SIZE_CLASS,
         count=COUNT,
         cond = CONDITION_CODE_RII, 
         sample_type = SAMPLE_TYPE
         ) %>% filter(species == 74120, sc == 1, cond == 1, Event %in% event$Event)  -> catch  # limit to only large scals, and tows from above.  

awl <- read.csv('./data/awl_2016_161027.csv')
awl %>% transmute(Event = EVENT_ID,
         species=RACE_CODE, 
         sc = SCAL_SIZE_CLASS,
         j = SCALLOP_NUMBER,
         clapper = CLAPPER,
         y_ij = MEAT_WEIGHT_GRAMS/WHOLE_WT_GRAMS,                        # calc mrr (meat recovery ratio)
         meat_cond = MEAT_CONDITION_SW
         ) %>% filter(species == 74120, sc == 1, is.na(clapper), !is.na(y_ij), Event %in% event$Event) -> samp   
              # exclude clappers, shells, smalls, and any other non weighed scals. Only tows in above. Some redundant filtering here for robustness.

area <- read.csv ('./data/area.csv')
     
## TOW-level stats and params #################################

# join Bed to samp, add events with no sampled scals.  These tows (10) have y_ij and j = NA.   
left_join(event,samp) %>%  select(Bed,Event,j,y_ij) -> samp2  

# aggregate catch by tow and join to samp
catch %>% group_by(Event) %>% summarise(M_i = sum(count)) %>%
    right_join(samp2) %>%
    mutate(M_i = replace(M_i, which(is.na(M_i)), 0)) -> samp.catch   # (change M_i from NA to 0 for those events (10) with no scals caught 

#calc remaining tow-level stats and params.  For those tows with < 2 scals, left all NAs as is except M_i and m_i = 0.   
samp.catch %>% 
  group_by(Event) %>%     
  summarise(Bed = first(Bed),    
            M_i = first(M_i),
            m_i = length(y_ij[!is.na(y_ij)]),  # number of scals sampled in tow.  Don't count NA y_ij from those events with no scals measured, so m_i = 0 if only NA y_ij's. 
            y_i = sum(y_ij),                   # NA for tows with no scals measured.  Sum of the MRRs of the sampled scals.  
            yBar_i = y_i/m_i,                  # NaN for tows with no scals measured 
            yHat_i = M_i * yBar_i,             # NAN for tows with no scals measured. Est total y_ij for tow_i.
            ss = sum((y_ij - yBar_i)^2),       # NA for tows with no scals measured
            s2_i = ss/(m_i - 1)                # SampVar w/in tow. NA for tows with no scals measured, NaN for those 8 more tows where only 1 scal measured, can't calc var from 1 obs.
            ) -> tow 

tow[is.na(tow$s2_i),]   # NAs, Beware these events (10) with null y_i, yBar_i, yHat_i and ss because no scals caught.  8 more with NaN s2_i (sampVar), because only 1 scal measured. 

## BED-level stats & params ##################################

# calc n as number of tows completed per bed
tow %>% group_by(Bed) %>% summarise (n = length(Event)) -> bed

# calc N (number of possible tows in bed) and join to bed
a <- .9 * 8/6076           # nominal area of 1 standard tow (nmi^2).  Not applying Q here.  
area$N <- area$area_nm2/a  # total number of potential standard sized tows in each bed
left_join(bed,area[,c("Bed","N")]) -> bed 

# join M (estimated total number of 2ndary units in the pop) from abundance est. Note N(from Abun) becomes M. ** NEED TO RUN ABUNDANCE.R FIRST **  
left_join (bed,select(large, Bed, M = N)) -> bed 

## Ratio estimator ###########################################

left_join(tow,bed) -> towBed  # join Bed-level values to Tow-level

# add sums needed for rHat and var(muHat_r) estimators, and those estimates
towBed %>% 
  group_by(Bed) %>%
  summarise (N = first(N),
            n = first (n),
            M = first (M), 
            rHat = sum(yHat_i, na.rm = T)/sum(M_i),            # estimated mean MRR over tows weighted by num scals caught.   
            sum1 = sum ((yHat_i - M_i*rHat)^2, na.rm = T),     # LH sum from varTauHat_r estimator. ~ var AMONG tows. Omit NAs caused from NaN yHat_i.
            sum2 = sum(M_i*(M_i - m_i)*s2_i/m_i, na.rm = T),   # RH sum from varTauHat_r estimator. ~ var WITHIN tows. Omit NAs caused from NAN s2_i.  
            tauHat_r = rHat * M,
            muHat_r = tauHat_r/M,                              # obviously this simplifies to rHat. 
            varHat_tauHat_r = (((N*(N-n))/(n*(n-1)))*sum1 + (N/n)*sum2),
            varHat_muHat_r = varHat_tauHat_r/(M^2),
            cv = 100 * sqrt(varHat_muHat_r)/muHat_r
            ) -> rat 
rat

## Biomass of meat estimates ################################    

#join MRR estimates to large whole biomass estimate 
rat %>% select (Bed,mrr = muHat_r, varMrr = varHat_muHat_r, cvMrr = cv) %>%            # names changed for conciseness.      
  left_join (large %>% select (Bed, wbm = N_wt, varWbm = varN_wt, cvWbm = cvN_wt)) %>% # names changed to avoid confusing with N used here as pop total number of tows.    
                                                                                       # ** EVENTUALLY CHANGE TO BOOT VAR **
  # calc products of pointEsts and vars
    mutate(mbm = wbm * mrr,        # mbm = meat biomass       
          varMbm = ((wbm)^2)*varMrr + (mrr^2)*varWbm - varWbm*varMrr,  # Goodman  
          cvMbm = 100 * sqrt(varMbm)/mbm
          ) -> bm
bm   # meat recovery ratio, whole and meat biomass with var and cv by bed. 
