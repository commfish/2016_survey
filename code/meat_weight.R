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

# meat weight ----
awl <- read.csv('data/awl_2016_161027.csv')

awl %>% select(Event = EVENT_ID,  species=RACE_CODE,
               j = SCALLOP_NUMBER, size_class = SCAL_SIZE_CLASS,
               weight=WHOLE_WT_GRAMS, worm=SHELL_WORM_SW, 
               height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, meat_weight = MEAT_WEIGHT_GRAMS,
               clapper = CLAPPER, sample_type = SAMPLE_TYPE) %>% 
  mutate(ratio = meat_weight/weight ) %>% 
  filter(species == 74120, size_class == 1, is.na(clapper), !is.na(ratio), Event %in% event$Event) %>% 
  select(Event,j,ratio)-> samp