# Authors: Lesley Merrick and SHannon Hubler

# separate code for USU sites, due to varying input files

# objective: 1) create a function to classify stations based on GIS human disturbance screening metrics
#            2) called and run within 'Run_Ref_Screen' code
#            3) output then used to determine which sites move forward to GE Scoring


#### Determine candidate reference sites from the GIS reference screening metrics###
ref_gis.screen.nonDEQ <- function(gis_mets){
require(RODBC)
library(tidyverse)

                              # ## pull in information from the stations db - this requires an ODBC connection 
                              # sta.sql = odbcConnect('Stations')
                              # stations = sqlFetch(sta.sql, "VWStationsFinal") 
                              # odbcClose(sta.sql)

# 2020 thresholds - variable thresholds based on metric - thresholds generated from Screening_Thresholds.R
ref_screen <-  gis_mets %>% 
                                # #SLH: removed 'MLocId' from by=c(MLocId = MLocID) in line below, in order to run Matt's USUS sites thru
                                # left_join(stations, by = c('MLocID')) %>% # we need to add org to gis_mets , 'OrgID' = 'OrgID') 
                                # #SLH: changed 'MLocId' below and changed to MLocID, in order to run Matt's USUS sites thru
                                #   select(OrgID,MLocID,StationDes,Lat_DD, Long_DD, HUC8_Name,HUC12_Name,GNIS_Name,EcoRegion2,EcoRegion3,EcoRegion4,COMID,
                                #        rdden_km_km2,xings_km2,P_AgLand,P_Urban21Land,mines,grvl_mn_km2,P_canal) %>% 
  mutate(rd_Status = case_when(rdden_km_km2 <= 1.1854942 ~ 1,   # 25th
                               rdden_km_km2 >= 4.9079551 ~ 2, # 95th
                               TRUE ~ 0)) %>%
  mutate(xing_Status = case_when(xings_km2 <= 0.10693475 ~ 1,   # 30th
                                 xings_km2 >= 1.35543470 ~ 2,    # 95th
                                 TRUE ~ 0)) %>%
  mutate(Ag_Status = case_when(P_AgLand <= 0 ~ 1,          # 25th 
                               P_AgLand > 30.045899737 ~ 2,        # 95th
                               TRUE ~ 0)) %>%
  mutate(Urb21L_Status = case_when(P_Urban21Land <= 1.5992877 ~ 1,  # 50th 
                                   P_Urban21Land > 9.6508641 ~ 2,       # 95th
                                   TRUE ~ 0)) %>%
  mutate(mines_status = case_when(mines <= 0 ~ 1,      # 25th
                                  mines > 15 ~ 2,       # 95th
                                  TRUE ~ 0)) %>% 
  mutate(gmines_status = case_when(grvl_mn_km2 <=0 ~ 1,     # 25th 
                                   grvl_mn_km2 > 0.0048846532 ~ 2, # 95th
                                   TRUE ~ 0)) %>%
  mutate(canal_status = case_when(P_canal <=0 ~ 1,         # 25th
                                  P_canal > 2.551325 ~ 2,      # 95th - first non-zero
                                  TRUE ~ 0))
ref_screen <- ref_screen %>%
  mutate(GIS.status_2020 = case_when(
    rd_Status == 1 & xing_Status == 1 & Ag_Status == 1 & Urb21L_Status == 1 & mines_status == 1 & gmines_status == 1 &
    canal_status == 1 ~ "Ref_GIS.candidate", # meets all 
    rd_Status == 2 | xing_Status == 2 |  Ag_Status == 2 | Urb21L_Status == 2 | mines_status == 2 | gmines_status == 2 |
      canal_status == 2 ~ "Trash", 
    TRUE ~ "NO"))

ref_screen$GIS.status_2020 <- as.factor(ref_screen$GIS.status_2020)   

ref_screen <- ref_screen %>%
mutate(WorE = case_when(
  EcoRegion3 == '1' | EcoRegion3 == '3' | EcoRegion3 == '4' | EcoRegion3 == '78' ~ "W",
  TRUE ~ "E")) %>%
  mutate(Eco3 = case_when(
    EcoRegion3 == "10" ~ "ColPl",
    EcoRegion3 == "1" ~ "CoRa",
    EcoRegion3 == "80" ~ "NBR",
    EcoRegion3 == "9" ~ "EC",
    EcoRegion3 == "3" ~ "WV",
    EcoRegion3 == "11" ~ "BM",
    EcoRegion3 == "78" ~ "KlMt",
    EcoRegion3 == "4" ~ "Ca",
    EcoRegion3 == "12" ~ "SRP"))
ref_screen$WorE <- as.factor(ref_screen$WorE)    

print(with(ref_screen, table(GIS.status_2020, WorE))) # summary table of Ref status by E/W
print(with(ref_screen, table(GIS.status_2020, EcoRegion3)))


# create a column for ref "GIS candidate" status = binary (Y or N) using DEQ 2020 thresholds
ref_screen <- ref_screen %>%
  mutate(GIS.status_2020.yn = case_when(
    GIS.status_2020 == "Ref_GIS.candidate" ~ "Y", # meets all 
    TRUE ~ "N"))

ref_screen$GIS.status_2020.yn <- as.factor(ref_screen$GIS.status_2020.yn)    



.GlobalEnv$ref_screen <- ref_screen 
write.csv(ref_screen, "Reference/ref_screen_USU.csv")}


