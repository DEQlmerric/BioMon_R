### Generate script to determine raw bug data missing metrics ###
### L. Merrick 5/10/2021 ###

library(tidyverse)
library(readxl)
library(RODBC)

bio.sql <- odbcConnect("BioMon")
bugs_raw <- sqlFetch(bio.sql,"dbo.VW_Raw_Macro")
bio_dev.sql <- odbcConnect("BioMon_dev")
bugs_met <- sqlFetch(bio_dev.sql,"dbo.VW_Metrics_Indexes")

# should represent unique samples # 
act_raw <- bugs_raw %>% 
           select(act_id) %>%
           distinct()

act_met <- bugs_met %>% 
  filter(parameter_name == 'O/E Ratio') %>%
  select(bhidx_id) %>% 
  mutate(act_id = str_sub(bhidx_id,1,-4)) %>%
  distinct()


#### join by activity - fingers crossed ###
missing_mets <- act_raw %>%
                left_join(act_met,by= 'act_id') %>% 
                filter(is.na(bhidx_id))
