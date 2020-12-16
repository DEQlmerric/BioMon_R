
#### Determine candidate reference sites from the GIS reference screening metrics###
ref_screen <- function(gis_mets){
require(RODBC)

## pull in information from the stations db - this requires an ODBC connection 
sta.sql = odbcConnect('Stations')
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)

# 2020 thresholds - variable thresholds based on metric - thresholds generated from Screening_Thresholds.R
ref_screen <-  gis_mets %>% 
  left_join(stations, by = c('MLocId'= 'MLocID')) %>% # we need to add org to gis_mets , 'OrgID' = 'OrgID') 
  select(OrgID,MLocId,StationDes,Lat_DD, Long_DD, HUC8_Name,HUC12_Name,GNIS_Name,EcoRegion2,EcoRegion3,EcoRegion4,COMID,
         rdden_km_km2,xings_km2,P_AgLand,P_Urban21Land,mines,grvl_mn_km2,P_canal) %>% 
  mutate(rd_Status = case_when(rdden_km_km2 <= 1.1854942 ~ 1,   # 25th
                               rdden_km_km2 >= 4.0437531 ~ 2, # 90th
                               TRUE ~ 0)) %>%
  mutate(xing_Status = case_when(xings_km2 <= 0.10693475 ~ 1,   # 30th
                                 xings_km2 >= 0.91400767 ~ 2,    # 90th
                                 TRUE ~ 0)) %>%
  mutate(Ag_Status = case_when(P_AgLand <= 0 ~ 1,          # 25th 
                               P_AgLand > 9.863038507 ~ 2,        # 90th
                               TRUE ~ 0)) %>%
  mutate(Urb21L_Status = case_when(P_Urban21Land <= 1.5992877 ~ 1,  # 50th 
                                   P_Urban21Land > 6.4403890 ~ 2,       # 90th
                                   TRUE ~ 0)) %>%
  mutate(mines_status = case_when(mines <= 0 ~ 1,      # 25th
                                  mines > 4 ~ 2,       # 90th
                                  TRUE ~ 0)) %>% 
  mutate(gmines_status = case_when(grvl_mn_km2 <=0 ~ 1,     # 25th 
                                   grvl_mn_km2 > 0.0007151642 ~ 2, # 90th
                                   TRUE ~ 0)) %>%
  mutate(canal_status = case_when(P_canal <=0 ~ 1,         # 25th
                                  P_canal > 2.551325 ~ 2,      # 95th - first non-zero
                                  TRUE ~ 0))
ref_screen <- ref_screen %>%
  mutate(ref.status_2020 = case_when(
    rd_Status == 1 & xing_Status == 1 & Ag_Status == 1 & Urb21L_Status == 1 & mines_status == 1 & gmines_status == 1 ~ "Ref_GIS.candidate", # meets all 
    rd_Status == 2 | xing_Status == 2 | Ag_Status == 2 | Urb21L_Status == 2 | mines_status == 2 | gmines_status == 2 ~ "Trash", 
    TRUE ~ "Not"))

ref_screen$ref.status_2020 <- as.factor(ref_screen$ref.status_2020)   

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

with(ref_screen, table(ref.status_2020, WorE)) # summary table of Ref status by E/W
with(ref_screen, table(ref.status_2020, EcoRegion3))


# create a column for ref "GIS candidate" status = binary (Y or N) using DEQ 2020 thresholds
ref_screen <- ref_screen %>%
  mutate(ref.status_2020.yn = case_when(
    ref.status_2020 == "Ref_GIS.candidate" ~ "Y", # meets all 
    TRUE ~ "N"))

ref_screen$ref.status_2020.yn <- as.factor(ref_screen$ref.status_2020.yn)    


with(ref_screen, table(ref.status_2020.yn, WorE)) # summary table of Ref status by E/W
with(ref_screen, table(ref.status_2020.yn, EcoRegion3))

.GlobalEnv$ref_screen <- ref_screen }