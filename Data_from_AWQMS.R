# Start of the biomon AWQMS data pull 

library(tidyverse)
library(RODBC) 
library(readxl)
library(data.table)

### connect to BioMon SQL database ### 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
bugs <- sqlFetch(bio.sql,"dbo.VW_Raw_Macro") # this is a view with all the macro data 


  # limit columns to what is necessary
bugs <- bugs %>%
   select(act_id, MLocID, SampleStart_Date, Sample_Method, Activity_Type, Result_Numeric, DEQ_Taxon, StageID, UniqueTaxon)


setnames(bugs, old=c('act_id', 'SampleStart_Date', 'Sample_Method', 'Result_Numeric', 'DEQ_Taxon'), 
         new=c('Sample', 'Date', 'Habitat', 'Count', 'DEQ_TAXON'))







??????' Whats up with all of the station level stuff in this bugs table?  e.g.: HUC, Elev, Precip, etc.????'   
   
################################


# bring in taxonomy table

taxonomy <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") ##  DEQ Taxon table with AWQMS uid 

#limit columns to what is necessary
taxonomy <- taxonomy %>%
  select(DEQ_TAXON, Taxon, OTU_RIV05_Nov05, OTU_Stress05, FFG, Voltine, MTI)
    


# join bugs and taxonomy tables
b_t <- bugs %>% 
       left_join(taxonomy, by = c('DEQ_TAXON')) 


##############################


## bring in Stations table


##############################
 


#connect to view as a general user 
sta.sql = odbcConnect('Stations')

# Connect to stations database --------------------------------------------

#pull in stations table
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)



#limit columns to those needed for these analyses


@@
@@@@ this is specific to current PREDATOR models
@@

  
stations$ELEV_m <- stations$ELEV_Ft*0.3048 #get elevation in meters

setnames(stations, old=c('station_key', 'Lat_DD', 'Long_DD', 'Predator_WorE', 'EcoRegion2', 'EcoRegion3'), 
         new=c('STATION_KEY', 'lat', 'long', 'W_E', 'Eco2', 'Eco3'))



stations<-stations %>%
  select(STATION_KEY, MLocID, StationDes, lat, long, Eco2, Eco3, EcoRegion4, ELEV_m, precip_mm, temp_Cx10, 
         W_E, HUC6_Name, HUC8_Name, HUC10_Name, HUC12_Name, Wade_Boat, COMID)

library(plyr)
revalue(stations$Eco2, c("MARINE WEST COAST FOREST" = "MWCF", "WESTERN CORDILLERA" = "WC")) -> stations$Eco2  # change factor levels to match code below for names to Level 2 Ecoregions


#####################



# join stations table to bugs + taxonomy

b_t_s <- b_t %>%
  left_join(stations, by = c('MLocID'))
  



@
@@@
@@@@@@@@@@@@@@@@@@@@@@@   what do we need from sample info??????????
@@@
@