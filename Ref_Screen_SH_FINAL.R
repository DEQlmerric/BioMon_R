## Script to bring process biomon stations for Reference deteremination 

require(RODBC)
require(tidyverse)
library(openxlsx)
library(readxl)
library(ggplot2)
library(data.table)
library("FactoMineR")
library("factoextra")
library("corrplot")
#disable scientific notation 
options(scipen = 999)

#connect to view as a general user 
sta.sql = odbcConnect('Stations')
bio.sql = odbcConnect('Biomon')

    # TableNames <- sqlTables(bio.sql,errors = FALSE)

# Connect to stations database --------------------------------------------

#pull in stations table
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)

#pull in bio stations table
bio_stations = sqlFetch(bio.sql, "BIO_STATIONS") 
odbcClose(bio.sql)

# bring in historic reference sites (anything previously identified as Ref)
ref.old <- read.csv('//deqlab1/Biomon/Reference Condition/2020_Lets Wrap this Sucka up/old.ref.csv')


####### bring in StreamCat metrics
cat <- read.csv('//deqlab1/gis_wa/Project_Working_Folders/StreamCat/StreamCat_all_OR.csv')
cat_def <- read_excel('//deqlab1/gis_wa/Project_Working_Folders/StreamCat/StreamCat_metrics_definitions_2.xlsx') # StreamCat metrics, definitions, and class codings

### read ref screen metrics - changed to db table? 
met_all <- read.csv("//deqlab1/gis_wa/Project_Working_Folders/Reference/Reference Selection Process/Delin_Merge2_metrics.csv")

met_w <- read.csv("//deqlab1/gis_wa/Project_Working_Folders/Reference/Reference Selection Process/Delin_Merge2_metrics.csv") %>%
       left_join(stations, by = "MLocID") %>%
       filter(Predator_WorE == "w")

met_e <- read.csv("//deqlab1/gis_wa/Project_Working_Folders/Reference/Reference Selection Process/Delin_Merge2_metrics.csv") %>%
  left_join(stations, by = "MLocID") %>%
  filter(Predator_WorE == "e")


met_sta <- read.csv("//deqlab1/gis_wa/Project_Working_Folders/Reference/Reference Selection Process/Delin_Merge2_metrics.csv") %>%
       left_join(stations, by = "MLocID") 



# bring in bug results file, for comparison with various metric thresholds

bugs <- read.csv('//deqlab1/Biomon/R Stats/Bio Tools_Upgrade with R/OE_Stress_Metrics_data quality.csv')

bugs <- bugs %>%
  select(Sample, MLocID, STATION_KEY,  Date, OoverE , OoverE_null,  BC , oe_cond , TS , BSTI , total_richness,  EPT_rich, pct_EPT, pct_Dom5 , pct_Dom3, pct_Dom1,
         Shannon_diversity, Simpson_diversity, NonInsect_richness, pct_NonInsect, MTI )






                         
### check percentiles from DEQ only LAM generate data set ####

DEQ_percentiles_Statewide <- met_all %>% 
  select("rdden_km_k","xings_km2","P_AgLand","P_Urban21L","mines","grvl_mn_km","P_canal") %>%
  sapply(quantile, probs = c(0.01,0.1,0.25,0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.85, 0.9,0.95,0.99), na.rm = TRUE) %>%
  as.data.frame()
write.csv(DEQ_percentiles_Statewide,"DEQ_percentiles_Statewide.csv")

DEQ_percentiles_West <- met_w %>% 
  select("rdden_km_k","xings_km2","P_AgLand","P_Urban21L","mines","grvl_mn_km","P_canal") %>%
  sapply(quantile, probs = c(0.01,0.1,0.25,0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.85, 0.9,0.95,0.99), na.rm = TRUE) %>%
  as.data.frame()
write.csv(DEQ_percentiles_West,"DEQ_percentiles_West.csv")

DEQ_percentiles_East <- met_e %>% 
  select("rdden_km_k","xings_km2","P_AgLand","P_Urban21L","mines","grvl_mn_km","P_canal") %>%
  sapply(quantile, probs = c(0.01,0.1,0.25,0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.85, 0.9,0.95,0.99), na.rm = TRUE) %>%
  as.data.frame()
write.csv(DEQ_percentiles_East,"DEQ_percentiles_East.csv")

#########################################################################

                                   


####
########
##          Assign Reference status to ALL SITES, STATEWIDE, with zeros included
########
####


# 2020 thresholds - variable thresholds
ref_screen <- bio_stations %>% 
   left_join(stations, by = c('MLocID'= 'MLocID', 'OrgID' = 'OrgID')) %>%
   select(OrgID,MLocID,StationDes,Lat_DD, Long_DD, HUC8_Name,HUC12_Name,GNIS_Name,EcoRegion2,EcoRegion3,EcoRegion4,COMID) %>% 
   left_join(met_all, by = 'MLocID') %>% ### probably should add orgID to met file
   mutate(rd_Status = case_when(rdden_km_k <= 1.325 ~ 1,   # 25th
                                rdden_km_k >=4.039 ~ 2,    # 90th
                                TRUE ~ 0)) %>%
  mutate(xing_Status = case_when(xings_km2 <= 0.114 ~ 1,   # 30th
                                 xings_km2 >=1.074 ~ 2,    # 90th
                                 TRUE ~ 0)) %>%
  mutate(Ag_Status = case_when(P_AgLand <= 0 ~ 1,          # 25th 
                               P_AgLand >9.354 ~ 2,        # 90th
                               TRUE ~ 0)) %>%
  mutate(Urb21L_Status = case_when(P_Urban21L <=1.68 ~ 1,  # 50th 
                               P_Urban21L >6.66 ~ 2,       # 90th
                               TRUE ~ 0)) %>%
  mutate(mines_status = case_when(mines_km2 <= 0 ~ 1,      # 25th
                                  mines_km2 > 4 ~ 2,       # 90th
                                  TRUE ~ 0)) %>% 
  mutate(gmines_status = case_when(grvl_mn_km <=0 ~ 1,     # 25th 
                              grvl_mn_km >0.0009 ~ 2,      # 90th
                              TRUE ~ 0)) %>%
  mutate(canal_status = case_when(P_canal <=0 ~ 1,         # 25th
                              P_canal >2.93 ~ 2,           # 95th - first non-zero
                              TRUE ~ 0))
                
                              

# create a column for ref "candidate" status, using AREMP 2016 methods
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





# add in old reference designations, for use below in PDPs (ref.basic)
bio_stations$STATION_KEY <- as.factor(bio_stations$STATION_KEY)


bio_stations <- bio_stations %>% 
       left_join(ref.old, by = c('STATION_KEY'))





bio_stations$Old.ref <- as.character(bio_stations$Old.ref)
bio_stations$Old.ref[is.na(bio_stations$Old.ref)] <- 'N'
       

ref_screen <- ref_screen %>% 
       left_join(bio_stations, by = c('MLocID'= 'MLocID'))

ref_screen$Old.ref <- as.factor(ref_screen$Old.ref)



################################################################################################################################################################




                                                                              ######################
                                                                              ######
                                                                              
                                                                                  Partial dependence plots
                                                                              
                                                                              ######
                                                                              ######################
                                                                              
                                                                                    
                                                                                    #ThresholdSensitivity
                                                                               @@@ run first with zeros included
                                                                               @@@
                                                                                
                                                                              #  rdden_km_k < 1.325 &
                                                                              #  xings_km2 < 0.056 &
                                                                              #  P_AgLand <= 0 &
                                                                              #  P_Urban21L <= 0 &
                                                                              #  mines <= 0 &
                                                                              #  grvl_mn_km <=0 &
                                                                              #  P_canal <= 0 
                                                                                
                                                                                
                                                                                 ref.nordden<-
                                                                                      subset(ref_screen, #rdden_km_k < 1.325 &
                                                                                              xings_km2 < 0.056 &
                                                                                              P_AgLand <= 0 &
                                                                                              P_Urban21L <= 0 &
                                                                                              mines <= 0 &
                                                                                              grvl_mn_km <=0 &
                                                                                              P_canal <= 0 )
                                                                                              
                                                                                 
                                                                                    ref.noxing<-
                                                                                      subset(ref_screen, rdden_km_k < 1.325 &
                                                                                      # xings_km2 < 0.056 &
                                                                                      P_AgLand <= 0 &
                                                                                      P_Urban21L <= 0 &
                                                                                      mines <= 0 &
                                                                                      grvl_mn_km <=0 &
                                                                                      P_canal <= 0 )
                                                                              
                                                                                   ref.noag<-
                                                                                      subset(ref_screen, rdden_km_k < 1.325 &
                                                                                              xings_km2 < 0.056 &
                                                                                              #P_AgLand <= 0 &
                                                                                              P_Urban21L <= 0 &
                                                                                              mines <= 0 &
                                                                                              grvl_mn_km <=0 &
                                                                                              P_canal <= 0 )
                                                                                                  
                                                                                    ref.nocd21<-
                                                                                      subset(ref_screen, rdden_km_k < 1.325 &
                                                                                              xings_km2 < 0.056 &
                                                                                              P_AgLand <= 0 &
                                                                                              # P_Urban21L <= 0 &
                                                                                              mines <= 0 &
                                                                                              grvl_mn_km <=0 &
                                                                                              P_canal <= 0 )
                                                                                                  
                                                                                     ref.nomine<-
                                                                                      subset(ref_screen, rdden_km_k < 1.325 &
                                                                                              xings_km2 < 0.056 &
                                                                                              P_AgLand <= 0 &
                                                                                              P_Urban21L <= 0 &
                                                                                              # mines <= 0 &
                                                                                              grvl_mn_km <=0 &
                                                                                              P_canal <= 0 )  
                                                                                     
                                                                                     ref.nogrvl<-
                                                                                      subset(ref_screen, rdden_km_k < 1.325 &
                                                                                              xings_km2 < 0.056 &
                                                                                              P_AgLand <= 0 &
                                                                                              P_Urban21L <= 0 &
                                                                                              mines <= 0 &
                                                                                              # grvl_mn_km <=0 &
                                                                                              P_canal <= 0 )
                                                                                     
                                                                                     ref.nocanal<-
                                                                                      subset(ref_screen, rdden_km_k < 1.325 &
                                                                                              xings_km2 < 0.056 &
                                                                                              P_AgLand <= 0 &
                                                                                              P_Urban21L <= 0 &
                                                                                              mines <= 0 &
                                                                                              grvl_mn_km <=0 )
                                                                                              # P_canal <= 0 )
                                                                                                 
                                                                                                  
                                                                                    
                                                                                    ref.basic<- subset(ref_screen, #W1_Hall_screening<=1.5 & (MaxOfCOND<=2000|is.na(MaxOfCOND)) &  ###Hard cap of 2000 uS/cm
                                                                                    #            (MaxOfCOND<=CondQR99c|is.na(MaxOfCOND)) &  ###Apply upper cond limit if upper limit is <1000
                                                                                    #            (MaxOfCOND>=CondQR01|is.na(MaxOfCOND)) &
                                                                                    #            Ag_2000_5K<=3 & Ag_2000_1K<=3 & Ag_2000_WS<=3 &
                                                                                    #            URBAN_2000_5K<=3 & URBAN_2000_1K<=3 & URBAN_2000_WS<=3 &
                                                                                    #            (URBAN_2000_5K+Ag_2000_5K)<=5 & (URBAN_2000_1K+Ag_2000_1K)<=5 & (URBAN_2000_WS+Ag_2000_WS)<=10 &
                                                                                               #            CODE_21_2000_5K<=7 & CODE_21_2000_1K<=7 & CODE_21_2000_WS<=10 &
                                                                                    #            RoadDens_5K<=2 & RoadDens_1K<=2 & RoadDens_WS<=2 &
                                                                                    #            PAVED_INT_1K<=5 & PAVED_INT_5K<=10 & PAVED_INT_WS<=50 &
                                                                                    #            InvDamDist<=0.1 &
                                                                                    #            PerManMade_WS<=10 &
                                                                                    #            MINES_5K<1 &
                                                                                    #            GravelMineDensL_R5K<=0.1 & 
                                                                                               Old.ref=="Y") # &
                                                                                    #            Invasives=="Not detected" &
                                                                                               #FlowStatus != "Nonperennial" &
                                                                                               #Active=="TRUE")
                                                                                    
                                                                                    Thresholds<-seq(from=0,to=20,by=.5)
                                                                                    Regions<-unique(ref_screen$Eco3)
                                                                                    
                                                                                    Sensitivity<-data.frame(Eco3=rep(Regions,each=length(Thresholds)), 
                                                                                                         Threshold=rep(Thresholds, length(Regions)))
                                                                                    
                                                                                    # for the data frame without Urban, count (length + subset) the # sites that  pass the urban metric thresholds at various levels
                                                                                    # but is that saying set the thresholds for each stressor type to values between 0-20 (increasing by 0.5)?
                                                                                    Sensitivity$rdden<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.nordden, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 #rdden_km_k <=Sensitivity$Threshold[i]  & 
                                                                                                 rdden_km_k <=Sensitivity$Threshold[i])$MLocID)})
                                                                                    
                                                                                     Sensitivity$xing<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.noxing, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 #xings_km2 <=Sensitivity$Threshold[i]  & 
                                                                                                 xings_km2 <=Sensitivity$Threshold[i])$MLocID)})
                                                                                
                                                                                     
                                                                                      Sensitivity$Ag<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.noag, Eco3==Sensitivity$Eco3[i] & 
                                                                                                # P_AgLand <=Sensitivity$Threshold[i]  & 
                                                                                                 P_AgLand <=Sensitivity$Threshold[i])$MLocID)})
                                                                                     
                                                                                     
                                                                                     Sensitivity$Code21<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.nocd21, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 #P_Urban21L <=Sensitivity$Threshold[i]  & 
                                                                                                 P_Urban21L <=Sensitivity$Threshold[i])$MLocID)})
                                                                                     
                                                                                     
                                                                                      Sensitivity$mines<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.nomine, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 #mines <=Sensitivity$Threshold[i]  & 
                                                                                                 mines <=Sensitivity$Threshold[i])$MLocID)})
                                                                                     
                                                                                     
                                                                                      Sensitivity$gravel<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.nogrvl, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 #grvl_mn_km <=Sensitivity$Threshold[i]  & 
                                                                                                 grvl_mn_km <=Sensitivity$Threshold[i])$MLocID)})
                                                                                      
                                                                                      
                                                                                      
                                                                                      Sensitivity$canal<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.nocanal, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 #P_canal <=Sensitivity$Threshold[i]  & 
                                                                                                 P_canal <=Sensitivity$Threshold[i])$MLocID)})
                                                                                      
                                                                                     
                                                                                    #
                                                                                    Sensitivity$rdden.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 rdden_km_k<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                                    Sensitivity$xing.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 xings_km2<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                               
                                                                                     Sensitivity$Ag.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 P_AgLand<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                                    
                                                                                    Sensitivity$Cd21.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 P_Urban21L<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                                    
                                                                                    Sensitivity$mines.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 mines<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                                    
                                                                                    
                                                                                 Sensitivity$grvl.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 grvl_mn_km<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                                    
                                                                                    
                                                                                    Sensitivity$canal.alone<-
                                                                                      sapply(1:length(Sensitivity$Threshold), function(i)
                                                                                      {length(
                                                                                        subset(ref.basic, Eco3==Sensitivity$Eco3[i] & 
                                                                                                 P_canal<=Sensitivity$Threshold[i])$StationCode)})
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                               
                                                                                    
                                                                                    
                                                                                    
                                                                                    sense.melt<-melt(Sensitivity, id=c("Eco3","Threshold"))
                                                                                    
                                                                                    ggplot(sense.melt, aes(x=Threshold, y=value, group=Eco3))+
                                                                                      geom_path(aes(linetype=Eco3))+
                                                                                      facet_wrap(~variable)+theme_bw()
                                                                                    
                                                                                    
                                                                                    Sensitivity.rel<-Sensitivity
                                                                                    
                                                                                    Sensitivity.rel$rdden<-Sensitivity$rdden/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$rdden))
                                                                                    
                                                                                    Sensitivity.rel$Ag<-Sensitivity$Ag/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$Ag))
                                                                                    
                                                                                    Sensitivity.rel$Code21<-Sensitivity$Code21/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$Code21))
                                                                                    
                                                                                    Sensitivity.rel$xing<-Sensitivity$xing/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$xing))
                                                                                    
                                                                                    
                                                                                    Sensitivity.rel$mines<-Sensitivity$mines/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$mines))
                                                                               
                                                                                      
                                                                                    Sensitivity.rel$gravel<-Sensitivity$gravel/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$gravel))
                                                                                  
                                                                                     Sensitivity.rel$canal<-Sensitivity$canal/
                                                                                      sapply(1:length(Sensitivity$Eco3), function(i)
                                                                                        max(subset(Sensitivity,Eco3==Sensitivity$Eco3[i])$canal))
                                                                                  
                                                                                    
                                                                                    
                                                                                    sense.rel.melt<-melt(Sensitivity.rel, id=c("Eco3","Threshold"))
                                                                                    
                                                                                    # sense.rel.melt.partial<-droplevels(subset(sense.rel.melt, variable %in%c("Urban","Ag","Code 21", "Road density")))
                                                                                    # levels(sense.rel.melt$variable)<-c("Urban","Ag","Code 21", "Road density")
                                                                                    
                                                                                    sense.rel.melt$Type<-ifelse(sense.rel.melt$variable %in% c("rdden","Ag","Code21","xings"),"Partial","Direct")
                                                                                    sense.rel.melt$Stressor<-ifelse(sense.rel.melt$variable %in% c("rdden","rdden.alone"),"rdden",
                                                                                                                    ifelse(sense.rel.melt$variable %in% c("Ag","Ag.alone"),"Ag",
                                                                                                                           ifelse(sense.rel.melt$variable %in% c("Code21","Cde21.alone"),"Code21",
                                                                                                                                  ifelse(sense.rel.melt$variable %in% c("xing","xing.alone"),"xing","Other"))))
                                                                                    
                                                                                    
                                                                                    sense.rel.melt.sub<-droplevels(subset(sense.rel.melt, (Stressor!="rdden")|(Stressor=="rdden" & Threshold<=1.325)))
                                                                                    
                                                                                    # sense.rel.melt.sub<-sense.rel.melt
                                                                                    # sense.rel.melt.sub[sense.rel.melt.sub$Stressor=="Road density" & sense.rel.melt.sub$Threshold<=5,"value"]<-NA
                                                                              @@@@@@@@@@@@@
                                                                                      @@@@@@@@@@@   Error
                                                                              @@@@@@@@@@@@@
                                                                                      
                                                                                      
                                                                                    partial.ref.plots<-
                                                                                    # ggplot(subset(sense.rel.melt.sub, Type=="Partial" & EcoRegion3 !="CV"), aes(x=Threshold, y=value, group=EcoRegion3))+
                                                                                    ggplot(subset(sense.rel.melt, Type=="Partial" & Eco3 !="CoRa" & ( (Stressor!="rdden")|(Stressor=="rdden " & Threshold<=1.325))), aes(x=Threshold, y=value, group=Eco3))+
                                                                                      geom_path(aes(linetype=Eco3, color=Eco3),size=1)+
                                                                                      scale_linetype_discrete(name="Region")+
                                                                                      scale_color_manual(values=c("gray50","gray15","gray50","gray15","gray50","gray15"))+
                                                                                      facet_wrap(~Stressor, scales="free_x")+theme_bw()+ylab("# Reference Sites")
                                                                                    
                                                                                    partial.ref.plots.psa<-
                                                                                      ggplot(subset(sense.rel.melt, Type=="Partial" & EcoRegion3 !="CV" & ( (Stressor!="Road density")|(Stressor=="Road density" & Threshold<=5))), aes(x=Threshold, y=value, group=Stressor))+
                                                                                      geom_path(aes(linetype=Stressor), size=1)+
                                                                                      scale_linetype_discrete(name="Region")+
                                                                                      facet_wrap(~EcoRegion3)+theme_bw()+ylab("# Reference Sites")
                                                                                    
                                                                                    
                                                                                    
                                                                                    ggsave(partial.ref.plots, filename="PeteFigures011013/partial.ref.plots.eps",width=8, height=8, units="in") 
                                                                                    ggsave(partial.ref.plots.psa, filename="PeteFigures011013/partial.ref.plots.byregion.eps",width=8, height=8, units="in") 
                                                                                    
                                                                                    
#######################################################################################################################################################################                                                                                    
                                                                                    
                                                                              





###
  ### bring ref_screen and Cat metrics together
###


# winnow down StreamCat (cat) to only those metrics at Ws scale--take the definitions df (cat_def), filter out only Ws metrics (based on Ws-Cat)
cat_def_Ws <- cat_def %>% 
  filter(Ws_Cat == 'Ws') 

#Get a vector of Metric.Name from cat_def_Ws that also exist as column in cat
cat_met.name <- cat_def_Ws$Metric.Name[cat_def_Ws$Metric.Name %in% names(cat)]


cat_Ws <- cat %>%
  select(COMID, cat_met.name)



 
# merge ref_screen and cat_ws
ref_screen.kitty <- ref_screen %>% 
       left_join(cat_Ws, by = c('COMID'= 'COMID')) 

ref_screen.kitty$EcoRegion3 <- as.factor(ref_screen.kitty$EcoRegion3)


# add in bugs 
@@@@@@@-------------problem: bugs == many records per station, ref_screen.kitty = 1 record per station.... use first, last, average?
@@@@@@@-------------solution: sicne we are looking at disturbance metrics calculated across a certain timeline (and it may vary by data layer), probably best to use the most recent sample only


bugs<- setDT(bugs)[,.SD[which.max(Date)],keyby=MLocID]


@@@@@@@-------------also need to limit to 1999 (1998?) and later

bugs$Date <-as.Date(bugs$Date, format = "%m/%d/%Y")

bugs <- bugs[bugs$Date >= "1998-01-01"]




# merge ref_screen and bugs

ref_screen.kitty.bugs <- ref_screen.kitty %>% 
       left_join(bugs, by = c('MLocID'= 'MLocID')) 


# split off only ref candidate sites
ref_screen.kitty.bugs_REF <- ref_screen.kitty.bugs %>% 
  filter(ref.status_2020.yn =='Y') %>% 
  as.data.frame()#




write.csv(ref_screen.kitty.bugs, '//deqlab1/Biomon/Reference Condition/2020_Lets Wrap this Sucka up/ref_screen.kitty.bugs.csv')



ref.status <- ref_screen.kitty.bugs[,c(1:12, 37:40, 42)]
write.csv(ref.status, '//deqlab1/Biomon/Reference Condition/2020_Lets Wrap this Sucka up/ref.status.csv')












                                                                            # explore sample sizes across various groupings/thresholds
                                                                            
                                                                            
                                                                            nrow(subset(ref_screen.kitty, IWI.x > 0.958)) # 484 out of 2080
                                                                            
                                                                            
                                                                            nrow(subset(ref_screen.kitty, IWI.x >= 0.958 & ref.status_2016 =='Ref_CANDIDATE')) #264 out of 380
                                                                            nrow(subset(ref_screen.kitty, IWI.x >= 0.958 & ref.status_2016 =='Not')) #213 out of 1343
                                                                            nrow(subset(ref_screen.kitty, IWI.x >= 0.958 & ref.status_2016 =='Trash')) #13 out of 357
                                                                            
                                                                            
                                                                            nrow(subset(ref_screen.kitty, IWI.x < 0.958 & ref.status_2016 =='Ref_CANDIDATE')) #105
                                                                            nrow(subset(ref_screen.kitty, IWI.x < 0.958 & ref.status_2016 =='Not')) #1089
                                                                            nrow(subset(ref_screen.kitty, IWI.x < 0.958 & ref.status_2016 =='Trash')) #305
                                                                            


##############
#############

# visual exploration of ref screening metrics by population: E, W, all

############
#############

@@@@
    @@@@@ some weird extreme values--probably Ambient...need to filter by project?  @@@@@@@@
@@@@  
  
  
# Area_km2
ggplot(ref_screen.kitty, aes(Area_km2, colour = WorE)) +
  geom_freqpoly(bins = 100) + xlim(0,1000)

# rd_km --------------------------------------------------> NOT a FINAL METRIC...but E > W....does that make sense given density is higher for W?  Means W side has smaller watersheds?
ggplot(ref_screen.kitty, aes(rd_km, colour = WorE)) +
  geom_freqpoly(bins = 500) + xlim(0,1000)

ggplot(ref_screen.kitty, aes(x=WorE, y = rd_km)) +
  geom_boxplot() + ylim(0,1000)

# rdden_km_k ----------------------------------------------> slightly higher density, on average, for W--but considerable overlap
ggplot(ref_screen.kitty, aes(rdden_km_k, colour = ref.status_2020)) +
  geom_freqpoly(bins = 500) + xlim(0,20) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=WorE, y = rdden_km_k)) +
  geom_boxplot() + ylim(0,20)

# xings_km2 ----------------------------------------------> pretty equivalent, E slightly higher
ggplot(ref_screen.kitty, aes(xings_km2, colour = WorE)) +
  geom_freqpoly(bins = 500) + xlim(0,20)+
  facet_wrap(~ref.status_2020)

ggplot(ref_screen.kitty, aes(x=WorE, y = xings_km2)) +
  geom_boxplot() + ylim(0,5) #missing some outliers at this limit

# P_AgLand ----------------------------------------------> pretty equivalent, E slightly higher---remarkable how low these values are
ggplot(ref_screen.kitty, aes(P_AgLand, colour = WorE)) +
  geom_freqpoly(bins = 100) + xlim(0,100)+
  facet_wrap(~ref.status_2020)

ggplot(ref_screen.kitty, aes(x=WorE, y = P_AgLand)) +
  geom_boxplot() + ylim(0,5) #missing some outliers at this limit


# P_Urban21L ----------------------------------------------> substantially higher on the W
ggplot(ref_screen.kitty, aes(P_Urban21L, colour = WorE)) +
  geom_freqpoly(bins = 500) + xlim(0,50)+
  facet_wrap(~ref.status_2020)

ggplot(ref_screen.kitty, aes(x=WorE, y = P_Urban21L)) +
  geom_boxplot() + ylim(0,50) #missing some outliers at this limit


# mines ----------------------------------------------> on average the same, but higher on E more frequently (E 75th ~ W 90th)
ggplot(ref_screen.kitty, aes(mines, colour = WorE)) +
  geom_freqpoly(bins = 50) + xlim(0,50)+
  facet_wrap(~ref.status_2020)

ggplot(ref_screen.kitty, aes(x=WorE, y = mines)) +
  geom_boxplot() + ylim(0,10)+
  facet_wrap(~ref.status_2020) #missing some outliers at this limit

# grvl_mn_km ----------------------------------------------> same, very infrequent
ggplot(ref_screen.kitty, aes(grvl_mn_km, colour = WorE)) +
  geom_freqpoly(bins = 50) + xlim(0,1)+
  facet_wrap(~ref.status_2020)

ggplot(ref_screen.kitty, aes(x=WorE, y = grvl_mn_km)) +
  geom_boxplot() + ylim(0,1) +
  facet_wrap(~ref.status_2020)#missing some outliers at this limit


# P_canal ----------------------------------------------> very infrequent for both, but E with more cases of higher %canal
ggplot(ref_screen.kitty, aes(P_canal, colour = WorE)) +
  geom_freqpoly(bins = 100) + xlim(0,20)+
  facet_wrap(~ref.status_2020)

ggplot(ref_screen.kitty, aes(x=WorE, y = P_canal)) +
  geom_boxplot() + ylim(0,1)+
  facet_wrap(~ref.status_2020) #missing some outliers at this limit


##############
#############

# visual exploration of ref screening metrics by population: E, W, all

############
#############

@@@@
    @@@@@ some weird extreme values--probably Ambient...need to filter by project?  @@@@@@@@
@@@@  
  
  


# IWI.x --------------------------------------------------> 

ggplot(ref_screen.kitty, aes(IWI.x, colour = ref.status_2020)) +  
  geom_freqpoly(bins = 500) + xlim(0.4,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=IWI.x, fill=WorE )) +
  geom_boxplot() + ylim(0.4,1) 


ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=IWI.x )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0.3,1) 
  


percent_IWI.ref <- ref_screen.kitty %>% 
  filter(ref.status_2020 == "Ref_CANDIDATE") %>%
  select(IWI.x) %>%
  sapply(quantile, probs = c(0.01,0.1,0.25,0.3, 0.35,0.4,0.45,0.5,0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.85,0.9,0.95,0.99), na.rm = TRUE) %>%
  as.data.frame()


percent_IWI.Not.ref <- ref_screen.kitty %>% 
  filter(ref.status_2020 == "Not") %>%
  select(IWI.x) %>%
  sapply(quantile, probs = c(0.01,0.1,0.25,0.3, 0.35,0.4,0.45,0.5,0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.85,0.9,0.95,0.99), na.rm = TRUE) %>%
  as.data.frame()



                    # IWI.y --------------------------------------------------> 
                    
                    ggplot(ref_screen.kitty, aes(IWI.x, colour = ref.status_2020)) +  
                      geom_freqpoly(bins = 500) + xlim(0,1) +
                      facet_wrap(~WorE)
                    
                    ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=IWI.x, fill=WorE )) +
                      geom_boxplot() + ylim(0,1) 
                      
                    
                    qplot( IWI.x, IWI.y, data = ref_screen.kitty) +
                      geom_abline(intercept = 0)




# WHYD.x --------------------------------------------------> very little discrimination b/w Ref and Not

ggplot(ref_screen.kitty, aes(WHYD.x, colour = ref.status_2020)) +  
  geom_freqpoly(bins = 500) + xlim(0,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WHYD.x, fill=WorE )) +
  geom_boxplot() + ylim(0,1) 


ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WHYD.x )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0.8,1) 
  


# WCHEM.x --------------------------------------------------> slight discrimination b/w Ref and Not

ggplot(ref_screen.kitty, aes(WCHEM.x, colour = ref.status_2020)) +  
  geom_freqpoly(bins = 500) + xlim(0,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WCHEM.x, fill=WorE )) +
  geom_boxplot() + ylim(0,1) 


ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WCHEM.x )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0.8,1) 
  
                    
# WSED.x --------------------------------------------------> slightly more discrimination b/w Ref and Not

ggplot(ref_screen.kitty, aes(WSED.x, colour = ref.status_2020)) +  
  geom_freqpoly(bins = 500) + xlim(0,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WSED.x, fill=WorE )) +
  geom_boxplot() + ylim(0,1) 


ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WSED.x )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0.8,1) 
 


# WCONN.x --------------------------------------------------> slight discrimination b/w Ref and Not

ggplot(ref_screen.kitty, aes(WCONN.x, colour = ref.status_2020)) +  
  geom_freqpoly(bins = 500) + xlim(0,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WCONN.x, fill=WorE )) +
  geom_boxplot() + ylim(0,1) 


ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WCONN.x )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0.75,1) 


# WHABT.x --------------------------------------------------> probably the most discrimination b/w Ref and Not

ggplot(ref_screen.kitty, aes(WHABT.x, colour = ref.status_2020)) +  
  geom_freqpoly(bins = 500) + xlim(0,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WHABT.x, fill=WorE )) +
  geom_boxplot() + ylim(0.75,1) 


ggplot(ref_screen.kitty, aes(x=ref.status_2020, y=WHABT.x )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0.75,1) 





####### disturbance factors   ----> how do 2020 ref designations compare to StreamCat equivalents to 2020 screening metrics






@@@@@@@@@@@
  @@@@@@@@@@@    Stopped here....3.12.20
@@@@@@@@@@





# RdDensWs cut = 1.325
ggplot(ref_screen.kitty, aes(RdDensWs, colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=RdDensWs, fill=WorE )) +
  geom_boxplot() + ylim(0,1) # + geom_hline(yintercept=1.325, color="black")
                             # this hline call doesn't work becuase scales of Cat and 2016 appear to be different


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=RdDensWs )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 



# RdCrsWs
ggplot(ref_screen.kitty, aes(RdCrsWs, colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=RdCrsWs, fill=WorE )) +
  geom_boxplot() + ylim(0,1) + geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=RdCrsWs )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 


# PctCrop2011Ws
ggplot(ref_screen.kitty, aes(PctCrop2011Ws, colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=PctCrop2011Ws, fill=WorE )) +
  geom_boxplot() + ylim(0,1) + geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=PctCrop2011Ws )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 

@@@@@ --- how to properly deal with Ag metric in StreamCat?
  

  
PctHay2011Ws
  
  
# PctUrbOp2011Ws 
ggplot(ref_screen.kitty, aes(PctUrbOp2011Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=PctUrbOp2011Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=PctUrbOp2011Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 
  
# MineDensWs 
ggplot(ref_screen.kitty, aes(MineDensWs , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=MineDensWs , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=MineDensWs  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 
    
  
  
# CanalDensWs 
ggplot(ref_screen.kitty, aes(CanalDensWs , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=CanalDensWs , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=CanalDensWs  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 

#other disturbance metrics   
# PctImp2011Ws 
ggplot(ref_screen.kitty, aes(PctImp2011Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=PctImp2011Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=PctImp2011Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 
       





# InorgNWetDep_2008Ws 
ggplot(ref_screen.kitty, aes(InorgNWetDep_2008Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=InorgNWetDep_2008Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=InorgNWetDep_2008Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1) 
   

# Pestic97Ws 
ggplot(ref_screen.kitty, aes(Pestic97Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Pestic97Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Pestic97Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1)






###### natural factors  ----> compare Ref 2016 vs IWI > 0.955 (statewide ref 2016 pop 25th percentile of IWI)



# WsAreaSqKm 
ggplot(ref_screen.kitty, aes(WsAreaSqKm , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 500) + xlim(0,1000) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=WsAreaSqKm , fill=WorE )) +
  geom_boxplot() + ylim(0,1000) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=WsAreaSqKm  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1000)



# ElevWs 
ggplot(ref_screen.kitty, aes(ElevWs , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 100) + xlim(0,2500) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=ElevWs , fill=WorE )) +
  geom_boxplot() + ylim(0,2500) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=ElevWs  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,2500)



# KffactWs 
ggplot(ref_screen.kitty, aes(KffactWs , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 100) + xlim(0,1) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=KffactWs , fill=WorE )) +
  geom_boxplot() + ylim(0,1) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=KffactWs  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,1)




# Precip8110Ws 
ggplot(ref_screen.kitty, aes(Precip8110Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 100) + xlim(0,5000) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Precip8110Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,5000) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Precip8110Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,5000)




# Tmean8110Ws 
ggplot(ref_screen.kitty, aes(Tmean8110Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 100) + xlim(0,20) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Tmean8110Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,20) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Tmean8110Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,20)




# Tmax8110Ws 
ggplot(ref_screen.kitty, aes(Tmax8110Ws , colour = ref.status_2016)) +  
  geom_freqpoly(bins = 100) + xlim(0,25) +
  facet_wrap(~WorE)

ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Tmax8110Ws , fill=WorE )) +
  geom_boxplot() + ylim(0,20) #+ geom_hline(yintercept=0.056, linetype="dashed", color="black")


ggplot(ref_screen.kitty, aes(x=ref.status_2016, y=Tmax8110Ws  )) +   # all (candidate) ref sites  statewide
  geom_boxplot() + ylim(0,20)



####
 __                   _              __      _    __    __        __                                          
(_   _  | . |_    _| (_    _   _    |__)  _ (_     _)  /  \  /|  /__     _   _   _|   | |  | |    _     |_  _ 
__) |_) | | |_   (_| |    (_) | )   | \  (- |     /__  \__/   |  \__)   (_| | ) (_|   | |/\| |   (_ |_| |_ _)   
    |                                                                              

####                                                                                                        

nrow(subset(ref_screen.kitty, IWI.x > 0.958)) # 484 out of 2080


nrow(subset(ref_screen.kitty, IWI.x >= 0.958 & ref.status_2016 =='Ref_CANDIDATE')) # 264  out of 380
nrow(subset(ref_screen.kitty, IWI.x >= 0.958 & ref.status_2016 =='Not')) # 213 out of 1343
nrow(subset(ref_screen.kitty, IWI.x >= 0.958 & ref.status_2016 =='Trash')) # 13 out of 357


nrow(subset(ref_screen.kitty, IWI.x < 0.958 & ref.status_2016 =='Ref_CANDIDATE')) # 105
nrow(subset(ref_screen.kitty, IWI.x < 0.958 & ref.status_2016 =='Not')) # 1089
nrow(subset(ref_screen.kitty, IWI.x < 0.958 & ref.status_2016 =='Trash')) # 305
 
  
  
table(ref_screen.kitty$ref.status_2016, ref_screen.kitty$IWI.x)  #
  
# ref = Yes, IWI = N

ref.Y_iwi.N <- ref_screen.kitty %>% 
  filter(ref.status_2016 =='Ref_CANDIDATE') %>% 
  filter(IWI.x >= 0.958) %>%
  as.data.frame()#

ggplot(ref.Y_iwi.N, aes(x=ref.status_2016, y=RdDensWs , fill=WorE )) +
  geom_boxplot() + ylim(0,5) + geom_hline(yintercept=1.347, linetype="dashed", color="black")



# ref = N, IWI = Y

ref.N_iwi.Y <- ref_screen.kitty %>% 
  filter(ref.status_2016 =='Not') %>% 
  filter(IWI.x >= 0.958) %>%
  as.data.frame()#


# ref = Trash, IWI = Y

ref.T_iwi.Y <- ref_screen.kitty %>% 
  filter(ref.status_2016 =='Trash') %>% 
  filter(IWI.x >= 0.958) %>%
  as.data.frame()#



#######################################
########################################

choosing the individual stressor metrics, choosing the percentiles for thresholds (threshold sensitivity)


##############################
########################################










nrow(subset(ref_screen.kitty, P_AgLand < 0.208664 & rdden_km_k > 1.347614)) # 551
nrow(subset(ref_screen.kitty, rdden_km_k < 1.347614 & xings_km2  < 0.24035)) # 466
nrow(subset(ref_screen.kitty, rdden_km_k < 1.347614 & xings_km2  < 0.24035 & P_AgLand < 0.208664)) # 428

nrow(subset(ref_screen.kitty, rdden_km_k < 1.347614 & xings_km2  < 0.24035 & P_AgLand < 0.208664 & P_Urban21L < 1.996)) # 382

nrow(subset(ref_screen.kitty, rdden_km_k < 1.347614 & P_Urban21L < 1.996)) # 483

###
####### CDFs with groups
###

@@@@@@@@@@@@@@  = 2016 cuts, west only.....update with 2020 statewide thresholds



# road density
ggplot(ref_screen.kitty.bugs, aes(rdden_km_k, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=1.347, linetype="dashed", color="black") + xlim(0,12) 

ggplot(ref_screen.kitty.bugs, aes(rdden_km_k, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=1.347, linetype="dashed", color="black")+ xlim(0,12)


ggplot(ref_screen.kitty.bugs, aes(rdden_km_k, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=1.347, linetype="dashed", color="black") + xlim(0,12) 




# road crossings
ggplot(ref_screen.kitty.bugs, aes(xings_km2, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=0.24035442, linetype="dashed", color="black")

ggplot(ref_screen.kitty.bugs, aes(xings_km2, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=0.24035442, linetype="dashed", color="black")


ggplot(ref_screen.kitty.bugs, aes(xings_km2, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=0.24035442, linetype="dashed", color="black")




# Ag
ggplot(ref_screen.kitty.bugs, aes(P_AgLand, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=0.208792219, linetype="dashed", color="black")+ xlim(0,5)

ggplot(ref_screen.kitty.bugs, aes(P_AgLand, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=0.208792219, linetype="dashed", color="black")+ xlim(0,5)


ggplot(ref_screen.kitty.bugs, aes(P_AgLand, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=0.208792219, linetype="dashed", color="black") + xlim(0,5)



# Code 21 - Open Space
ggplot(ref_screen.kitty.bugs, aes(P_Urban21L, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=1.99609965, linetype="dashed", color="black")+ xlim(0,5)

ggplot(ref_screen.kitty.bugs, aes(P_Urban21L, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=1.99609965, linetype="dashed", color="black")+ xlim(0,5)


ggplot(ref_screen.kitty.bugs, aes(P_Urban21L, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=1.99609965, linetype="dashed", color="black") + xlim(0,5)



# Mines
ggplot(ref_screen.kitty.bugs, aes(mines, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=1, linetype="dashed", color="black")+ xlim(0,5)

ggplot(ref_screen.kitty.bugs, aes(mines, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=1, linetype="dashed", color="black")+ xlim(0,5)


ggplot(ref_screen.kitty.bugs, aes(mines, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=1, linetype="dashed", color="black") + xlim(0,5)



# Gravel Mines
ggplot(ref_screen.kitty.bugs, aes(grvl_mn_km, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=0.0053489396, linetype="dashed", color="black")+ xlim(0,0.2)

ggplot(ref_screen.kitty.bugs, aes(grvl_mn_km, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=0.0053489396, linetype="dashed", color="black")+ xlim(0,0.2)


ggplot(ref_screen.kitty.bugs, aes(grvl_mn_km, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=0.0053489396, linetype="dashed", color="black") + xlim(0,0.2)




# canals
ggplot(ref_screen.kitty.bugs, aes(P_canal, colour = WorE)) + stat_ecdf() +
  geom_vline(xintercept=1.70554549, linetype="dashed", color="black")+ xlim(0,5)

ggplot(ref_screen.kitty.bugs, aes(P_canal, colour = EcoRegion2)) + stat_ecdf() +
  geom_vline(xintercept=1.70554549, linetype="dashed", color="black")+ xlim(0,5)


ggplot(ref_screen.kitty.bugs, aes(P_canal, colour = EcoRegion3)) + stat_ecdf() +
  geom_vline(xintercept=1.70554549, linetype="dashed", color="black") + xlim(0,5)





dim(ref_screen.kitty.bugs %>% filter(EcoRegion3 == '3' & rdden_km_k < 7 & xings_km2 < 8 & P_AgLand <8 & P_Urban21L < 8 & mines < 8 & grvl_mn_km < 8 & P_canal < 8))
  






##################

relationships between bugs and stressors

##################

@@@  1--PCA to natural gradients
@@@  2--RF of bug metrics/indexes to natural gradients
@@@  3--residuals of ref vs not vs trash (explore variabel ref thresholds???)
@@@      ----> how does this not get circular????

  
  
plot(ref_screen.kitty.bugs$OoverE ~ ref_screen.kitty.bugs$rdden_km_k)

oe.lm <- lm(ref_screen.kitty.bugs$OoverE ~ ref_screen.kitty.bugs$rdden_km_k + ref_screen.kitty.bugs$xings_km2 + ref_screen.kitty.bugs$P_canal + ref_screen.kitty.bugs$mines + 
     ref_screen.kitty.bugs$grvl_mn_km + ref_screen.kitty.bugs$P_AgLand + ref_screen.kitty.bugs$P_Urban21L)

ggplot(ref_screen.kitty.bugs, aes(x=ref.status_2016, y=pct_EPT )) +   
  geom_boxplot() 








##
####   PCA
##

# limit df to working variables: natural gradients (numerical only? no factors or site IDs?)

ref_pca <- ref_screen.kitty.bugs %>%
select(MLocID,StationDes,EcoRegion2,EcoRegion3,EcoRegion4,COMID,ref.status_2016, ref.status_2020, ref.status_2020.yn, WorE,Area_km2,WsAreaSqKm,ElevWs,PctCarbResidWs,PctNonCarbResidWs,PctAlkIntruVolWs,PctSilicicWs,PctExtruVolWs,
PctColluvSedWs,PctGlacTilClayWs,PctGlacTilLoamWs,PctGlacTilCrsWs,PctGlacLakeCrsWs,PctGlacLakeFineWs,PctHydricWs,PctEolCrsWs,PctEolFineWs,PctSalLakeWs,PctAlluvCoastWs,PctCoastCrsWs,PctWaterWs,
KffactWs,Tmean8110Ws,Tmax8110Ws,Precip8110Ws, Al2O3Ws,CaOWs,Fe2O3Ws,K2OWs,MgOWs,Na2OWs,P2O5Ws,SWs,SiO2Ws,NWs,HydrlCondWs,CompStrgthWs)

# remove columns with "zero variance"
which(apply(ref_pca, 2, var)==0)  
ref_pca <- ref_pca %>%
  select(-PctGlacTilClayWs, -PctGlacTilLoamWs, -PctHydricWs )


# missing data blows up the PCA -- remove rows with missing data
ref_pca.cc <- ref_pca[complete.cases(ref_pca), ]
      # alternative option is to impute missing values, or use median/mean



# run PCA

@@http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#pca-data-format
  


pca.allsites <- PCA(ref_pca.cc[,c(11:44)], graph = FALSE)

summary(pca.allsites)
str(pca.allsites)
print(pca.allsites)
eig.val <- get_eigenvalue(pca.allsites) # Extract the eigenvalues/variances of principal components
fviz_eig(pca.allsites, addlabels = TRUE, ylim = c(0, 50) ) # Visualize the eigenvalues

# Extract the results for individuals and variables, respectively.
ind <- get_pca_ind(pca.allsites); 
var <- get_pca_var(pca.allsites) 

# Visualize the results individuals and variables, respectively.
fviz_pca_ind(pca.allsites)
fviz_pca_var(pca.allsites, col.var = "blue") 

@@@
@@@@@@ how to limit this to only those variables with high contributions?????
@@@


fviz_pca_biplot(pca.allsites) # Make a biplot of individuals and variables.



corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca.allsites, choice = "var", axes = 1:2)


# Color by cos2 values: quality on the factor map
fviz_pca_var(pca.allsites, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )


fviz_pca_var(pca.allsites, alpha.var = "cos2") # Change the transparency by cos2 values

corrplot(var$contrib, is.corr=FALSE)  

fviz_contrib(pca.allsites, choice = "var", axes = 1, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 2, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 1:2, top = 15)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(pca.allsites, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             )

@@@@@ crashes              
              # fviz_pca_ind(pca.allsites, col.ind = "cos2", 
              #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              #              repel = TRUE # Avoid text overlapping (slow if many points)
              #              )
              



grp <- as.factor(ref_pca.cc$EcoRegion3)
# Color variables by groups
fviz_pca_var(pca.allsites, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

@@@@@@@@ doesnt work.  grouping var not the same length as # rows in pca



# Color by groups


# Eco3
fviz_pca_ind(pca.allsites,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = ref_pca.cc$EcoRegion3, # color by groups
             palette = 'lancet', #c('violet', 'blue', 'green', 'gray', 'orange', 'red', 'pink', 'black', 'forest green'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
             )

# Ref 2016
fviz_pca_ind(pca.allsites,
             geom.ind = 'point', # show points only (nbut not "text")
             col.ind = ref_pca.cc$ref.status_2016, # color by groups
             palette = c('forest green', 'blue', 'red'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = 'Ref Class 2016')

# Ref 2020
fviz_pca_ind(pca.allsites,
             geom.ind = 'point', # show points only (nbut not "text")
             col.ind = ref_pca.cc$ref.status_2020, # color by groups
             palette = c('forest green', 'blue', 'red'),
             #addEllipses = TRUE, # Concentration ellipses
             legend.title = 'Ref class 2020')



# W or E
fviz_pca_ind(pca.allsites,
             geom.ind = 'point', # show points only (nbut not "text")
             col.ind = ref_pca.cc$WorE, # color by groups
             palette = c( 'blue', 'red'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = 'W or E',
             #pointshape = ref_pca.cc$ref.status_2020,
            
             )


# scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".


# Change the size of arrows an labels
fviz_pca_var(pca.allsites, arrowsize = 1, labelsize = 5, 
             repel = TRUE)



fviz_pca_biplot(pca.allsites, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = ref_pca.cc$EcoRegion3,
                col.ind = "black",
                # Color variable by groups
                #col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Eco3", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
             )+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors     # Variable colors





fviz_pca_ind(pca.allsites, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = ref_pca.cc$EcoRegion3, 
                addEllipses = TRUE,
                #col.ind = "black",
                # Color variable by groups
                #col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Eco3"), #, color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
             )+
  ggpubr::fill_palette("lancet") #+      # Indiviual fill color
 # ggpubr::color_palette("npg")      # Variable colors     # Variable colors


fviz_pca_ind(pca.allsites,
             geom.ind = "point", # show points only (but not "text")
             pointshape = 21,
             pointsize = ref_pca.cc$ref.status_2020.yn,
             fill.ind = ref_pca.cc$EcoRegion3, # color by groups
             palette = c('purple', 'blue', 'green', 'orange', 'red', 'gray', 'pink', 'black', 'forest green'), 
             addEllipses = FALSE, # Concentration ellipses
             legend.title = "Groups"
             ) 



fviz_pca_biplot(pca.allsites,
             geom.ind = "point", # show points only (nbut not "text")
             pointshape = 21,
             pointsize = ref_pca.cc$ref.status_2020.yn,
             fill.ind = ref_pca.cc$EcoRegion3, # color by groups
             palette = c('purple', 'blue', 'green', 'orange', 'red', 'gray', 'black','yellow',  'forest green'), 
             addEllipses = FALSE, # Concentration ellipses
             select.var = list(cos2  = 0.5),  # opnly include variables with cos2 > 0.5
             legend.title = "Groups"
             ) 



    # from CA F&G code
    library(ggplot2)
    
    pca.plot<-
    ggplot(stations, aes(x=Nat.PC1, y=Nat.PC2))+
      geom_point()+
      theme_bw()
    
    ggsave(pca.plot, filename="PeteFigures011013/pca.plot.eps",width=10, height=8, units="in") 
    # ggsave(pca.plot, filename="PeteFigures011013/pca.plot.jpg",width=10, height=8, units="in") 
    
    pca.plot.refs<-
    pca.plot+
      geom_point(data=subset(stations, SiteStatus=="Reference"), shape=21, fill="gray90", size=4)
    
    ggsave(pca.plot.refs, filename="PeteFigures011013/pca.plot.refs.eps",width=10, height=8, units="in") 







