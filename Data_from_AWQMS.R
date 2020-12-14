# Start of the biomon AWQMS data pull 

library(tidyverse)
library(RODBC) 
library(readxl)
library(data.table)

### connect to BioMon SQL database ### 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
bugs <- sqlFetch(bio.sql,"dbo.VW_Raw_Macro") # this is a view with all the macro data 
taxonomy <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") ##  DEQ Taxon table with AWQMS uid 


# join bugs and taxonomy tables
  
b_t <- bugs %>% 
       left_join(taxonomy, by = c('tax_uid' = 'AWQMS_tax_uid')) 


##############################
## bring in Stations table



#connect to view as a general user 
sta.sql = odbcConnect('Stations')

# Connect to stations database --------------------------------------------

#pull in stations table
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)

#limit columns to those needed for these analyses

stations$ELEV_m <- stations$ELEV_Ft*0.3048 #get elevation in meters
library(data.table)
setnames(stations, old=c('station_key', 'Lat_DD', 'Long_DD', 'Predator_WorE', 'EcoRegion2', 'EcoRegion3'), 
         new=c('STATION_KEY', 'lat', 'long', 'W_E', 'Eco2', 'Eco3'))

library(dplyr)

stations<-stations %>%
  select(STATION_KEY, MLocID, StationDes, lat, long, Eco2, Eco3, EcoRegion4, ELEV_m, precip_mm, temp_Cx10, 
         W_E, HUC6_Name, HUC8_Name, HUC10_Name, HUC12_Name, Wade_Boat, COMID)

library(plyr)
revalue(stations$Eco2, c("MARINE WEST COAST FOREST" = "MWCF", "WESTERN CORDILLERA" = "WC")) -> stations$Eco2  # change factor levels to match code below for names to Level 2 Ecoregions

summary(stations$Eco2)
colnames(stations)                

#####################

