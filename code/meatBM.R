#################################
## ESTIMATE BIOMASS of MEATS    #
#################################
#Josh Mumm 170105
#Estimates biomass of meats by bed, as product of estimated meat recovery rate and estimated whole weight biomass
#two stage random sample design with selection of tows the first level and selection of scals as the second level,
#tows are primary units, scals are secondary units
#variable defintion and formulae available in MRR_formulae.docx

#work in progress, needs checking and cleaning. Eventualy may move inside abundance.r 

load ----
library(tidyverse)

# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2016_161027.csv')
events %>% mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
  select(Event = EVENT_ID,
         District = DISTRICT, 
         Bed = BED_SW,
         Type = STATION_TYPE,
         ID = ID, 
         performance=GEAR_PERFORMANCE_CODE_SW
         ) %>% filter(performance == 1, Type != "Ancillary")   -> event   # exclude bad and ancillary tows. 

catch <- read.csv('./data/catchComp_2016_161027.csv')
catch %>% select(Event = EVENT_ID,
         species=RACE_CODE, 
         sc=SCAL_SIZE_CLASS,
         count=COUNT,
         sample_wt=SAMPLE_WT_KG,
         cond = CONDITION_CODE_RII, 
         sample_type = SAMPLE_TYPE
         ) %>% filter(species == 74120, sc == 1, Event %in% event$Event)  -> catch  # limit to only large scals, and only good stnd tows.  Need to be careful with names since, abund.r uses same names. 

awl <- read.csv('./data/awl_2016_161027.csv')
awl %>% transmute(Event = EVENT_ID,
         sc = SCAL_SIZE_CLASS,
         j = SCALLOP_NUMBER,      
         y_ij = MEAT_WEIGHT_GRAMS/WHOLE_WT_GRAMS ,                                        # meat recovery ratio
         meat_cond = MEAT_CONDITION_SW
         ) %>% filter(!is.na(y_ij), sc == 1, Event %in% event$Event) -> samp        # exclude clappers, shells, smalls, and any other non weighed-scals, and only good stnd tows. 

area <- read.csv ('./data/area.csv')
             
##################################
# TOW-level stats and params ----
#################################

# join Bed to samp, add events with no samp (NAs for now, 10 tows)   
left_join(event,samp) %>%  select(Bed,Event,j,y_ij) -> samp2  

# Aggregate catch by tow and join to samp
catch %>% group_by(Event) %>% summarise(M_i = sum(count)) %>% right_join(samp2) %>%
    mutate(M_i = replace(M_i, which(is.na(M_i)), 0)) -> samp.catch   # (change 10 events with no scals caught (NA M_i to 0 )

#Tow level stats and params
samp.catch %>% 
  group_by(Event) %>%     # Think if want to remove NAs here. i.e. how to treat tows w no measured scal or caught scals. Currently leaving as NA.  Removed for Bed-level calcs. 
  summarise(Bed = first(Bed),    
            M_i = first(M_i),
            m_i = length(y_ij[!is.na(y_ij)]),  # number of scals sampled in tow.  10 Events with 0 scals caught (m_i = NA) become m_i = 0. 
            y_i = sum(y_ij),     # sum of the MRRs of the sampled scals. 
            yBar_i = y_i/m_i,
            yHat_i = M_i * yBar_i, 
            ss = sum((y_ij - yBar_i)^2),
            s2_i = ss/(m_i - 1)  # creates some NaNs where m_1 = 1, can't calc var from 1 scal
            ) -> tow 

tow[is.na(tow$s2_i),]   # NAs, Be aware 10 evenvts with null y_i, yBar_i, yHat_i, because no scals caught.  8 more with NaN s2_i, beacause only 1 scal measured 
###############################################
# BED-level stats and params ----
##############################################
tow %>% 
    group_by(Bed) %>%
    summarise (n = length(Event)) -> bed

# calc N (number of possible tows in bed) and join to bed
a <- .9 * 8/6076           # nominal area of 1 standard tow (nmi^2).  Not applying Q here.  
area$N <- area$area_nm2/a  # number of possible standard sized tows in each bed
left_join(bed,area[,c("Bed","N")]) -> bed 

# join M (estimated total number of 2ndary units in the pop) from abundance est. Note N becomes M. NEED TO RUN ABUNDANCE.R first.  
left_join (bed,select(large, Bed, M = N)) -> bed 
###########################
# Ratio estimator ----
###########################
left_join(tow,bed) -> towBed

# add sums needed for rHat and var(tauHat_r) estimators, and those estimators 
towBed %>% 
      group_by(Bed) %>%
      summarise (
      N = first(N),
      n = first (n),
      M = first (M), 
      rHat = sum(yHat_i, na.rm = T)/sum(M_i, na.rm = T), # estimated MRR over all scallops in pop. Think more about na.rm, and if here is good, OR earlier.
      ss_u = sum ((yHat_i - M_i*rHat)^2, na.rm = T),  # LH sum from varTauHat_r estimator 
      sum2 = sum(M_i*(M_i - m_i)*s2_i/m_i, na.rm = T), # RH sum from varTauHat_r estimator 
      tauHat_r = rHat * M,
      mewHat_r = tauHat_r/M,
      varHat_tauHat_r = (((N*(N-n))/(n*(N-1)))*ss_u + (N/n)*sum2),
      varHat_mewHat_r = varHat_tauHat_r/(M^2),
      cv = sqrt(varHat_mewHat_r)/mewHat_r
      ) -> rat 

############################
# Unbiased estimator -----       #has advantage of not requiring M (total scals in pop by bed), which we can only estimate 
###########################
towBed %>% 
  group_by(Bed) %>%
  summarise (
    N = first(N),
    n = first (n),
    M = first (M),
    tauHat = (N/n) * sum(yHat_i, na.rm = T),
    mewHat = tauHat/M,
    mewHat_1 = tauHat/n, # pop mean per tow
    ss_u = sum ((yHat_i - mewHat_1)^2, na.rm = T),  
    s2_u = ss_u/(n - 1), 
    sum2 = sum(M_i*(M_i-m_i)*(s2_i/m_i), na.rm = T),
    varHat_tauHat = (N*(N-n)) * (s2_u/n) + (N/n)*sum2,   #Check ubiased var, looks large
    varHat_mewHat = varHat_tauHat/(M^2),
    cv = sqrt(varHat_mewHat)/mewHat   
    ) -> unbias 

##########################
# Biomass of meat estimates ----      
##########################
left_join(
    unbias %>% select (Bed,mewHat,varHat_mewHat), 
    rat    %>% select (Bed,mewHat_r,varHat_mewHat_r)) %>%    
    left_join (
    large %>% select (Bed, ww = N_wt, varWw = varN_wt)   # changed names to avoid confusing with N used here as pop total number of tows. Eventually chng to bootstraped var.  
    ) %>% mutate(
    bm =   ww * mewHat, 
    bm_r = ww * mewHat_r,            
    varBm =  ((ww)^2)*varHat_mewHat + (mewHat^2)*varWw +  varWw*varHat_mewHat,          #Check variance of product of variances from Goodman
    varBm_r = ((ww)^2)*varHat_mewHat_r + (mewHat_r^2)*varWw +  varWw*varHat_mewHat_r,    #Check variance of product of variances from Goodman 
    cvBm = sqrt(varBm)/bm,
    cvBm_r = sqrt(varBm_r)/bm_r) -> bm
bm

# variance of MRR from unbiased est looks large.   Need to error check.  
    