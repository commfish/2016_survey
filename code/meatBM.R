## ##########################
# ESTIMATE BIOMASS of MEATS #     
############################# 
#Josh Mumm 170106
#Estimates biomass of meats by bed, as product of estimated meat recovery rate and estimated whole weight biomass
#two stage random sample design with selection of tows the first level and selection of scals as the second level,
#tows are primary units, scals are secondary units
#variable defintion and formulae available in MRR_formulae.docx
#work in progress, needs checking and cleaning. Eventualy may move inside abundance.r 

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
         y_ij = MEAT_WEIGHT_GRAMS/WHOLE_WT_GRAMS,                        # meat recovery ratio (MRR)
         meat_cond = MEAT_CONDITION_SW
         ) %>% filter(species == 74120, sc == 1, is.na(clapper), !is.na(y_ij), Event %in% event$Event) -> samp   
              # exclude clappers, shells, smalls, and any other non weighed-scals. Only tows in above. Some redundant filtering here for robustness.

area <- read.csv ('./data/area.csv')
     
## TOW-level stats and params #################################

# join Bed to samp, add events with no sampled scals (NAs for now, 10 tows)   
left_join(event,samp) %>%  select(Bed,Event,j,y_ij) -> samp2  

# Aggregate catch by tow and join to samp
catch %>% group_by(Event) %>% summarise(M_i = sum(count)) %>%
  right_join(samp2) %>%
    mutate(M_i = replace(M_i, which(is.na(M_i)), 0)) -> samp.catch   # (change M_i from NA to 0 for those events (10) with no scals caught 

## TOW-level stats & params ##################################
samp.catch %>% 
  group_by(Event) %>%     # Think if want to remove NAs here. i.e. how to treat tows w no measured scal or caught scals. Currently leaving as NA.  Removed for Bed-level calcs. 
  summarise(Bed = first(Bed),    
            M_i = first(M_i),
            m_i = length(y_ij[!is.na(y_ij)]),  # number of scals sampled in tow.  Don't count NAs y_ij from those events with no scals measured.  m_i = 0, if only NA y_ij's. 
            y_i = sum(y_ij, na.rm = T),        # sum of the MRRs of the sampled scals. y_i = 0 if no scals measured.  
            yBar_i = y_i/m_i,                  # NaN for tows with no scals measured 
            yHat_i = M_i * yBar_i,             # NAN for tows with no scals measured.  Est total y_ij for tow_i
            ss = sum((y_ij - yBar_i)^2),       # NA for tows with no scals measured
            s2_i = ss/(m_i - 1)                # Na for tows with no scals measured. NaN for those 8 more tows where only 1 scal measured, can't calc var from 1 scal.
            ) -> tow 

tow[is.na(tow$s2_i),]   # NAs, Be aware 10 events with null y_i, yBar_i, yHat_i,ss because no scals caught.  8 more with NaN s2_i, beacause only 1 scal measured 

## BED-level stats & params ################################

tow %>% group_by(Bed) %>% summarise (n = length(Event)) -> bed

# calc N (number of possible tows in bed) and join to bed
a <- .9 * 8/6076           # nominal area of 1 standard tow (nmi^2).  Not applying Q here.  
area$N <- area$area_nm2/a  # total number of potential standard sized tows in each bed
left_join(bed,area[,c("Bed","N")]) -> bed 

# join M (estimated total number of 2ndary units in the pop) from abundance est. Note N becomes M. NEED TO RUN ABUNDANCE.R first.  
left_join (bed,select(large, Bed, M = N)) -> bed 

## Ratio estimator ####################################### 

left_join(tow,bed) -> towBed

# add sums needed for rHat and var(tauHat_r) estimators, and those estimators 
towBed %>% 
  group_by(Bed) %>%
  summarise (N = first(N),
            n = first (n),
            M = first (M), 
            rHat = sum(yHat_i, na.rm = T)/sum(M_i),            # estimated mean MRR over all scallops weighed 
            sum1 = sum ((yHat_i - M_i*rHat)^2, na.rm = T),     # LH sum from varTauHat_r estimator.  Masked NAs caused from NaN yHat_i. Not sure best way to handle Na's.
            sum2 = sum(M_i*(M_i - m_i)*s2_i/m_i, na.rm = T),   # RH sum from varTauHat_r estimator.  Masked NAs caused from NAN s2_i. Not sure best way to handle Na's.  
            tauHat_r = rHat * M,
            muHat_r = tauHat_r/M,
            varHat_tauHat_r = (((N*(N-n))/(n*(N-1)))*sum1 + (N/n)*sum2),
            varHat_muHat_r = varHat_tauHat_r/(M^2),
            cv = sqrt(varHat_muHat_r)/muHat_r
            ) -> rat 

## Unbiased estimator ##################################     
#doesn't require M (pop total scals by bed), which we can only estimate 

towBed %>% 
  group_by(Bed) %>%
  summarise (N = first(N),
            n = first (n),
            M = first (M),
            tauHat = (N/n) * sum(yHat_i, na.rm = T),
            muHat_1 = tauHat/n, # pop mean per tow
            muHat = tauHat/M,
            ss_u = sum ((yHat_i - muHat_1)^2, na.rm = T),     # omit NAs caused from NA yHat_i   
            s2_u = ss_u/(n - 1), 
            sum2 = sum(M_i*(M_i-m_i)*(s2_i/m_i), na.rm = T),  # omit NAs caused from NA S2_i
            varHat_tauHat = (N*(N-n)) * (s2_u/n) + (N/n)*sum2,   
            varHat_muHat = varHat_tauHat/(M^2),               #check ubiased var, looks large
            cv = sqrt(varHat_muHat)/muHat   
            ) -> unbias 

## Biomass of meat estimates ###########################    
#join unbias and rat MRR estimators to large abund estimate 
left_join(unbias %>% select (Bed,muHat,varHat_muHat), 
          rat    %>% select (Bed,muHat_r,varHat_muHat_r)
          ) %>%    
          left_join (large %>% select (Bed, ww = N_wt, varWw = varN_wt)   # changed names to avoid confusing with N used here as pop total number of tows. Eventually chng to boot var.  
                    ) %>%
# calc products of pointEsts and vars
    mutate(bm =   ww * muHat, 
      bm_r = ww * muHat_r,            
      varBm =  ((ww)^2)*varHat_muHat + (muHat^2)*varWw +  varWw*varHat_muHat,           #Check variance of product of variances from Goodman
      varBm_r = ((ww)^2)*varHat_muHat_r + (muHat_r^2)*varWw +  varWw*varHat_muHat_r,    #Check variance of product of variances from Goodman 
      cvBm = sqrt(varBm)/bm,
      cvBm_r = sqrt(varBm_r)/bm_r) -> bm

bm

# variance of MRR from unbiased est looks large.   Need to error check.  
    