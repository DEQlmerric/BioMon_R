# Authors: Lesley Merrick, Shannon Hubler
 
# 12.14.2020

# pull bug count data from AWQMS (using a view from Biomon SQL dbase)
# pull Taxonomy table (from Biomon SQL dbase)
# pull Stations table (from Station SQL dbase)
# Format data for analyses
      #limit fields to only those necessary, 
      #rename as needed to match existing models

 






library(tidyverse)
library(RODBC) 
library(readxl)
library(data.table)
library(writexl)

### connect to BioMon SQL database ### 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
bugs <- sqlFetch(bio.sql,"dbo.VW_Raw_Macro") # this is a view with all the macro data 


# limit to necessary fields
bugs.lim <- bugs %>%
  select(act_id, MLocID, StationDes, Project1, SampleStart_Date, Sample_Method, 
         Activity_Type, DEQ_Taxon, StageID, Result_Numeric, Result_Unit, UniqueTaxon) 

setnames(bugs.lim, old=c('DEQ_Taxon'), 
         new=c('DEQ_TAXON'))

#change columns to factors so can extract levels
cols <- colnames(bugs.lim[c(1,2,3, 4,6,7,8,9,11)])
bugs.lim[cols] <- lapply(bugs.lim[cols], factor)


#split into count and density
# count = ALL samples
# density = incomplete
bugs.count <- bugs.lim[bugs.lim$Result_Unit == 'count',]
bugs.density <- bugs.lim[bugs.lim$Result_Unit == '#/ft2',]




#########

#     SAMPLE INFO Table

#########


# calculate and add total abundance
sample.info <- bugs.count %>%
      group_by(act_id, MLocID, StationDes, Project1, SampleStart_Date, Sample_Method, 
               Activity_Type,) %>%
      summarize(tot.abund= sum(Result_Numeric))



 
################################

# Bug count table

################################

# bring in taxonomy table

taxonomy <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") ##  DEQ Taxon table with AWQMS uid 

#limit columns to what is necessary
taxonomy <- taxonomy %>%
  select(DEQ_TAXON, Taxon, OTU_RIV05_Nov05, OTU_Stress05, 'OTU_BCG_WV-PL', Phylum, Class, SubClass, Order, 
         SubOrder, 'Super Family', Family, SubFamily, Tribe, Genus, SubGenus, SpeciesGroup, SpeciesComplex, 
         Species, SubSpecies, MTI, Voltine, FFG)



    
taxonomy$DEQ_TAXON <- as.factor(taxonomy$DEQ_TAXON)


###

# join bugs and taxonomy tables
b_t <- bugs.count %>% 
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

stations$ELEV_m <- stations$ELEV_Ft*0.3048 #get elevation in meters

setnames(stations, old=c('station_key', 'Lat_DD', 'Long_DD', 'Predator_WorE', 'EcoRegion2', 'EcoRegion3'), 
         new=c('STATION_KEY', 'lat', 'long', 'W_E', 'Eco2', 'Eco3'))



stations<-stations %>%
  select(MLocID, StationDes, lat, long, Eco2, Eco3, EcoRegion4, ELEV_m, precip_mm, temp_Cx10, 
         W_E, HUC6_Name, HUC8_Name, HUC10_Name, HUC12_Name, Wade_Boat, COMID)

library(plyr)
revalue(stations$Eco2, c("MARINE WEST COAST FOREST" = "MWCF", "WESTERN CORDILLERA" = "WC")) -> stations$Eco2  
# change factor levels to match code below for names to Level 2 Ecoregions


#####################



# join stations table to bugs + taxonomy

b_t_s <- b_t %>%
  left_join(stations, by = c('MLocID'))


setnames(b_t_s, old=c('Result_Numeric','act_id', 'SampleStart_Date', 'Sample_Method'), 
         new=c('Count','Sample', 'Date', 'Habitat'))

    



                                            
                                            
                                            
                                            #######################
                                            
                                            #  Write table for USU
                                            
                                            ######################
                                            
                                            
                                            ###########    SAMPLE INFO
                                            
                                            sample.info_USU <- sample.info  %>%
                                              select(act_id, MLocID, SampleStart_Date, Sample_Method, Activity_Type, tot.abund)
                                            
                                            # limit to riffle and transect data
                                            sample.info_USU <- sample.info_USU[sample.info_USU$Sample_Method == 'Benthic Kick - Targeted Riffle' | 
                                                                                 sample.info_USU$Sample_Method == 'Benthic Kick - Transect', ]
                                            # remove unwanted text
                                            sample.info_USU$Sample_Method <- as.factor(str_remove(sample.info_USU$Sample_Method, "Benthic Kick - "))
                                            
                                            
                                            
                                            
                                            # select only dates from 1999 and later
                                            sample.info_USU$SampleStart_Date <- as.Date(sample.info_USU$SampleStart_Date)
                                            
                                            sample.info_USU <- sample.info_USU[sample.info_USU$SampleStart_Date > "1999-01-01",]
                                            
                                            sample.info_USU <- sample.info_USU %>% 
                                              mutate(Lab.name = case_when(SampleStart_Date > '1999-01-01' & SampleStart_Date < '2001-12-31' ~ 'Rhithron',
                                                                          SampleStart_Date > '2002-01-01' & SampleStart_Date < '2008-12-31' ~ 'Kate.Parkin',
                                                                          SampleStart_Date > '2009-01-01' & SampleStart_Date < '2012-12-31' ~ 'Cole.Ecological',
                                                                          SampleStart_Date > '2013-01-01' & SampleStart_Date < '2017-12-31' ~ 'Rhithron',
                                                                          SampleStart_Date > '2018-01-01' & SampleStart_Date < '2019-12-31' ~ 'Cole.Ecological',
                                                                          TRUE ~ 'ERROR'))
                                            
                                            # create table, in order as called for by USU
                                            sample.info_USU <- sample.info_USU %>% 
                                              rename(Station = MLocID, SampDate = SampleStart_Date)  %>%
                                              mutate(SampleTime = 'NULL') %>%
                                              mutate(Method = "Kick net")  %>%
                                              rename(Habitat = Sample_Method) %>%
                                              mutate(FieldSplit = 0) %>%
                                              mutate(FieldNotes = 'NULL') %>%
                                              mutate(Qualitative = 'N') %>%
                                              mutate(LabNotes = 'NULL') %>%
                                              mutate(Mesh = 500) %>%
                                              mutate(LabSplit = '') %>%
                                              rename(Count = tot.abund)
                                            
                                            
                                            
                                            sample.info_USU <- sample.info_USU %>%
                                              mutate(Area = case_when(Habitat == 'Targeted Riffle'  ~ 0.74, 
                                                                      Habitat == 'Transect' ~ 0.99,
                                                                      TRUE ~ -999)) 
                                            
                                                            @@@@@@@@
                                                              # calculate Lab Split from density and count data
                                                              #  % = count / (density * area)
                                                              --but gotta do this for a single sample, when this will come in as all bugs in a sample
                                                            
                                                            
                                                            density.usu <- bugs.density %>%
                                                              group_by(act_id) %>%
                                                              arrange(desc(Taxonomic_Name)) %>%
                                                              filter(row_number()==1)
                                                            @@@@@@@
                                              
                                              
                                            # Final USU "BugSample" table -- put in correct order
                                            sample.info_USU <- sample.info_USU %>%
                                            select(act_id, Station, SampDate, SampleTime, Method, Habitat, Area, FieldSplit, FieldNotes, 
                                                   LabSplit, Qualitative, LabNotes, Mesh, Count, Lab.name)
                                            
                                            
                                            
                                            
                                            ########## SITE INFO
                                            
                                            # For USU                
                                            site.info_USU <- bugs %>%
                                              select(MLocID, StationDes, Lat_DD, Long_DD, ELEV_Ft, HUC12, COMID,  ) %>%
                                              mutate(System1 = "Lotic",
                                                     System2 = "Stream",
                                                     State = "OR",
                                                     County = "",
                                                     Country = "USA",
                                                     LandUse = "",
                                                     Reference = "")
                                            
                                            site.info_USU <- unique(site.info_USU)                                
                                            
                                            
                                            
                                            
                                            site.info_USU <- site.info_USU %>%
                                              rename(Station = MLocID, Location = StationDes, Lat = Lat_DD, Long = Long_DD) %>%
                                              mutate(Elevation = ELEV_Ft*0.3048)
                                            
                                            
                                            site.info_USU <- site.info_USU %>%
                                              select(Station, System1, System2, Location, HUC12, COMID, Lat, Long, State, County, Country, Elevation,
                                                     LandUse, Reference )
                                            
                                            
                                            
                                            ########### Bug count data
                                            
                                            # BugData table
                                            bug.counts_USU <- sample.info_USU %>%
                                              left_join(b_t, by = c('act_id', 'StationDes', 'Project1'))
                                            
                                            
                                            bug.counts_USU <- bug.counts_USU %>%
                                              select(act_id, Taxon, StageID, Result_Numeric, Station, SampDate, Habitat, Activity_Type, Phylum, Class, SubClass, Order, 
                                                     SubOrder, 'Super Family', Family, SubFamily, Tribe, Genus, SubGenus, SpeciesGroup, SpeciesComplex, Species, SubSpecies) %>%
                                              mutate(BigRareCount = 0)
                                            
                                            
                                            bug.counts_USU <- bug.counts_USU[,-c(1,2)]
                                            
                                            
                                            
                                            
                                            
                                            ########## FINAL Tables
                                            
                                            # export to Excel
                                            sheets <- list("BugSample" = sample.info_USU, "SiteInfo" = site.info_USU, "BugData" = bug.counts_USU) 
                                            
                                            writexl::write_xlsx(sheets, 'C://Git/Biomon_R/bugs analyses/data/ODEQ_bugs_from.AWQMS.xlsx')
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
