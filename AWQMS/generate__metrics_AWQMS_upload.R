### Create BioMon metrics/indexes AWQMS uploads from Shannon's R export file 
### 6/21/2018, Lesley Merrick

library(tidyverse)
library(data.table)
library(lubridate)

setwd("A:/AWQMS/BioMon")
#read file from Shannon's R output
bio_wide = read.csv("//deqlab1/BioMon/R Stats/Bio Tools_Upgrade with R/OE_Stress_Metrics_data quality.csv")
# convert date format 
bio_wide$Date = mdy(bio_wide$Date)
colnames(bio_wide)


##############################################
################## Metrics ###################
##############################################

### clean up and make long format ###
### currently done by project - this requires some familiarity with the data - I created a document to reference how projects were reconiled to AWQMS projects ####
## Get a list of projects in the dataset
unique(bio_wide$Project_name)

biomet_long <- bio_wide %>%
  #Select by project
  filter(Project_name %in% c('Tickle Creek','Umatilla Biomon_Tox','Deschutes Probabilistic','SE Oregon','Ref-ODFW')) %>%
  #filter(Project_name %in% c('Oregon Plan','OP Lower Columbia','OP Lower Columbia Reference','Oregon Plan Willamette','Oregon Plan - ODFW Macros','Oregon Plan - Reference 2007','Oregon Plan - Reference')) %>%
   #filter(Project_name %in% c('Crooked TMDL','Trout Creek TMDL','Malheur TMDL','Mid Coast Bugs','South Coast TMDL','Tillamook RBS','RBS')) %>%
    #filter(Project_name %in% c('Reference Trend','Umatilla Biomon_Tox','Deschutes Probabilistic','SE Oregon','Ref-ODFW')) %>%
  # remove unneeded columns
          select(-X,
                -STATION_KEY,
                -Site.name,
                -long,
                -lat,
                -Eco3,
                -Eco2,
                -O,
                -E,
                -OoverE,
                -Onull,
                -Enull,
                -OoverE_null,
                -oe_cond,
                -BC,
                -BC_null,
                -outlier_05,
                -outlier_01,
                -TS,
                -BSTI,
                -MTI,
                -HUC6_Name,
                -HUC8_Name,
                -HUC10_Name,
                -HUC12_Name) %>%
  # rename to match AWQMS 
  rename('Total Taxa Richness' = total_richness,
         'Coleoptera Taxa Richness' = Coleoptera_rich,
         'Diptera Taxa Richness' = Diptera_rich,
         'Ephemeroptera Taxa Richness'= Ephemeroptera_rich,
         'Plecoptera Taxa Richness'=Plecoptera_rich,
         'Trichoptera Taxa Richness'=Trichoptera_rich,
         'Baetidae Taxa Richness' = Baetidae_rich,
         'Chironomidae Taxa Richness'= Chironomidae_rich,
         'Hydropsychidae Taxa Richness'= Hydropsychidae_rich,
         'EPT Taxa Richness'= EPT_rich,
         '% Coleoptera Taxa'= pct_Coleoptera,
         '% Diptera Taxa'= pct_Diptera,
         '% Ephemeroptera Taxa'= pct_Ephemeroptera,
         '% Plecoptera Taxa'= pct_Plecoptera,
         '% Trichoptera Taxa'= pct_Trichoptera,
         '% EPT Taxa'= pct_EPT,
         'Multivoltine Taxa Richness' = multivoltine_rich,
         'Semivoltine Taxa Richness'= semivoltine_rich,
         'Univoltine Taxa Richness' = univoltine_rich,
         '% Multivoltine Taxa' = pct_multivoltine,
         '% Semivoltine Taxa' = pct_semivoltine,
         '% Univoltine Taxa' = pct_univoltine,
         'Collector Filterer Richness'= CF_rich,
         'Collector Gatherer Richness' = CG_rich,
         'Macrophyte Herbivore Richness' = MH_rich,
         'Omnivore Richness' = OM_rich,
         'Parasite Richness' = PA_rich,
         'Piercer Herbivore Richness' = PH_rich,
         'Predator Richness' = PR_rich,
         'Scraper Richness'= SC_rich,
         'Shredder Richness' = SH_rich,
         '% Collector Filterer Taxa'= pct_CF,
         '% Collector Gatherer Taxa'= pct_CG,
         '% Macrophyte Herbivore Taxa'= pct_MH,
         '% Omnivore Taxa' = pct_OM,
         '% Parasite Taxa'= pct_PA,
         '% Piercer Herbivore Taxa'= pct_PH,
         '% Predator Taxa'= pct_PR,
         '% Scraper Taxa'= pct_SC,
         '% Shredder Taxa'= pct_SH,
         '% Individuals in top 5 taxa'= pct_Dom5,
         '% Individuals in top 3 taxa' = pct_Dom3,
         '% Individuals in top 1 taxa'= pct_Dom1,
         'Shannon Diversity'= Shannon_diversity,
         'Simpson Diversity'=Simpson_diversity,
         'Non-Insect Taxa Richness'= NonInsect_richness,
         '% Non-Insect Taxa'= pct_NonInsect,
         'RIVPACS Sub-Sample Total Abundance'= tot_abund_RIV,
         'Stressor Models Total Abundance'= tot_abund_STR) %>%
  # make long format
  gather('Total Taxa Richness','Coleoptera Taxa Richness','Diptera Taxa Richness','Ephemeroptera Taxa Richness',
         'Plecoptera Taxa Richness','Trichoptera Taxa Richness','Baetidae Taxa Richness','Chironomidae Taxa Richness',
         'Hydropsychidae Taxa Richness','EPT Taxa Richness','% Coleoptera Taxa','% Diptera Taxa','% Ephemeroptera Taxa',
         '% Plecoptera Taxa','% Trichoptera Taxa','% EPT Taxa','Multivoltine Taxa Richness','Semivoltine Taxa Richness',
         'Univoltine Taxa Richness','% Multivoltine Taxa','% Semivoltine Taxa','% Univoltine Taxa','Collector Filterer Richness',
         'Collector Gatherer Richness','Macrophyte Herbivore Richness','Omnivore Richness','Parasite Richness','Piercer Herbivore Richness',
         'Predator Richness','Scraper Richness','Shredder Richness','% Collector Filterer Taxa','% Collector Gatherer Taxa',
         '% Macrophyte Herbivore Taxa','% Omnivore Taxa','% Parasite Taxa','% Piercer Herbivore Taxa','% Predator Taxa',
         '% Scraper Taxa','% Shredder Taxa','% Individuals in top 5 taxa','% Individuals in top 3 taxa','% Individuals in top 1 taxa',
         'Shannon Diversity','Simpson Diversity','Non-Insect Taxa Richness','% Non-Insect Taxa','Evenness','RIVPACS Sub-Sample Total Abundance',
         'Stressor Models Total Abundance', key = "ID", value = "Score_long") %>%
  # add additional awqms required columns
  mutate(Media = "Biological") %>%
  #######UPDATE PROJECT ######
  mutate(project = "Water Quality Response") %>%
  mutate(CollectionMethod = "Composite") %>%
  mutate(assemblage = "Benthic Macroinvertebrates") %>%
  mutate(equipment = "D-Frame Net") %>%
  # add habitat to comment field, what about other comments?
  mutate(com = ifelse(Habitat_sampled == 'R',"Riffle Habitat",
                       ifelse(Habitat_sampled == 'P',"Pool Habitat",
                              ifelse(Habitat_sampled == 'G',"Glide Habitat",
                                     ifelse(Habitat_sampled == 'T',"Transect Sample - Multiple Habitats",
                                            ifelse(Habitat_sampled == 'O',"Other - Unknown Habitat","")))))) %>%
  # convert to AWQMS sample types
  mutate(type = ifelse(Field_QC == 'S' & Lab_QC == 'S',"Sample-Routine",
                       ifelse(Field_QC == 'S' & Lab_QC == 'CP',"Sample-Routine",
                       ifelse(Field_QC == 'S' & Lab_QC == 'CD',"Quality Control Sample-Lab Duplicate",
                        ifelse(Field_QC == 'FP'& Lab_QC == 'CP',"Sample-Routine",
                        ifelse(Field_QC == 'FP'& Lab_QC == 'S',"Sample-Routine",
                        ifelse(Field_QC == 'FD'& Lab_QC == 'S',"Quality Control Sample-Field Replicate",
                        ifelse(Field_QC == 'FD2'& Lab_QC == 'S',"Quality Control Sample-Field Replicate",
                        ifelse(Field_QC == 'FD2'& Lab_QC == 'CP',"Quality Control Sample-Field Replicate",
                        ifelse(Field_QC == 'FD2'& Lab_QC == 'CD',"Quality Control Sample-Lab Duplicate",
                        ifelse(Field_QC == 'FD3'& Lab_QC == 'S',"Quality Control Sample-Field Replicate",
                        ifelse(Field_QC == 'FD'& Lab_QC == 'CP',"Quality Control Sample-Field Replicate",
                        ifelse(Field_QC == 'FD'& Lab_QC == 'CD',"Quality Control Sample-Lab Duplicate",""))))))))))))) %>%
                                 #ifelse(Field_QC == 'FD2',"Quality Control Sample-Field Replicate",
                                        #ifelse(Field_QC == 'FD3',"Quality Control Sample-Field Replicate",
                                               #ifelse(Field_QC == 'FD4',"Quality Control Sample-Field Replicate",
                                                      #ifelse(Field_QC == 'FD5',"Quality Control Sample-Field Replicate",
                                                             #ifelse(Lab_QC == 'CD',"Quality Control Sample-Lab Duplicate",""))))))))) %>%
  # add short sample type to generate activity ID
  mutate(type_short = ifelse(Field_QC == 'S'& Lab_QC == 'S',"SR",
                     ifelse(Field_QC == 'S' & Lab_QC == 'CP',"SR",
                     ifelse(Field_QC == 'S' & Lab_QC == 'CD',"QCLR",
                     ifelse(Field_QC == 'FP'& Lab_QC == 'CP',"SR",
                     ifelse(Field_QC == 'FP'& Lab_QC == 'S',"SR",
                     ifelse(Field_QC == 'FD'& Lab_QC == 'S',"QCFR",
                     ifelse(Field_QC == 'FD2'& Lab_QC == 'S',"QCFR:2",
                     ifelse(Field_QC == 'FD2'& Lab_QC == 'CP',"QCFR:2",
                     ifelse(Field_QC == 'FD2'& Lab_QC == 'CD',"QCLR",
                     ifelse(Field_QC == 'FD3'& Lab_QC == 'S',"QCFR",
                     ifelse(Field_QC == 'FD'& Lab_QC == 'CP',"QCFR",
                     ifelse(Field_QC == 'FD'& Lab_QC == 'CD',"QCLR","")))))))))))))%>%
                            #Field_QC == 'FP',"SR",
                            #ifelse(Field_QC == 'FD',"QCFR",
                                   #ifelse(Field_QC == 'FD2',"QCFR:2",
                                          #ifelse(Field_QC == 'FD3',"QCFR:3",
                                                 #ifelse(Field_QC == 'FD5',"QCFR:5",
                                                               #ifelse(Lab_QC == 'CD',"QCLR","")))))))))%>%
#Add context 
  mutate(context = ifelse(ID == 'Total Taxa Richness',"USEPA",
                   ifelse(ID == '% Ephemeroptera Taxa',"USEPA",       
                   ifelse(ID == '% EPT Taxa',"USEPA",
                   ifelse(ID == '% Individuals in top 3 taxa',"USEPA",
                   ifelse(ID == '% Individuals in top 5 taxa',"USEPA",
                   ifelse(ID == '% Non-Insect Taxa',"USEPA",
                   ifelse(ID == 'Ephemeroptera Taxa Richness',"USEPA",
                   ifelse(ID == 'EPT Taxa Richness',"USEPA",
                   ifelse(ID == 'Scraper Richness',"USEPA",
                   ifelse(ID == 'Shannon Diversity',"USEPA",
                   ifelse(ID == 'Shredder Richness', "USEPA",
                   ifelse(ID == 'Total Taxa Richness', "USEPA","OREGONDEQ"))))))))))))) %>%      
  #round scores - AWQMS doesn't like long numbers 
  mutate(score = round(Score_long,2)) %>%
  # generate activity ID
  #mutate(monloc = ifelse(!STATION_KEY %like% "dfw_", paste0(STATION_KEY, "-ORDEQ"),paste0(STATION_KEY)))%>%
  #mutate(monloc = paste0(STATION_KEY, "-ORDEQ")) %>%
  mutate(Date4Id = strftime(Date, format = '%Y%m%d', tz = 'UTC')) %>%
  mutate(activity = paste(MLocID,Date4Id,as.character(Habitat_sampled),type_short,sep =":"))%>%
  # truncate to match AWQMS upload 
  select(activity,type,Media,Date,project,MLocID,ID,context,
         score,com,CollectionMethod,assemblage,equipment)

# write to server folder to upload to AWQMS          
write.csv(biomet_long,"OregonPlan_Met_Long.csv",row.names = FALSE)

##################################################
#####INDEX######
### generate index upload 
bioindex_long <- bio_wide %>%
    # calculate % taxa loss 
  mutate(taxa_loss = (1-bio_wide$OoverE)*100) %>%
  # filter by project 
  #filter(Project_name %in% c('Reference Trend','Umatilla Biomon_Tox','Deschutes Probabilistic','SE Oregon','Ref-ODFW')) %>%
  #filter(Project_name %in% c('Crooked TMDL','Trout Creek TMDL','Malheur TMDL','Mid Coast Bugs','South Coast TMDL','Tillamook RBS','RBS')) %>%
  filter(Project_name %in% c('Oregon Plan','OP Lower Columbia','OP Lower Columbia Reference','Oregon Plan Willamette','Oregon Plan - ODFW Macros','Oregon Plan - Reference 2007','Oregon Plan - Reference')) %>%
    # remove unneeded columns
    select(-X,
           -O,
           -E,
           -BC_null,
           -outlier_05,
           -outlier_01,
           -total_richness,
           -Coleoptera_rich,
           -Diptera_rich,
           -Ephemeroptera_rich,
           -Plecoptera_rich,
           -Trichoptera_rich,
           -Baetidae_rich,
           -Chironomidae_rich,
           -Hydropsychidae_rich,
           -EPT_rich,
           -pct_Coleoptera,
           -pct_Diptera,
           -pct_Ephemeroptera,
           -pct_Plecoptera,
           -pct_Trichoptera,
           -pct_EPT,
           -multivoltine_rich,
           -semivoltine_rich,
           -univoltine_rich,
           -pct_multivoltine,
           -pct_semivoltine,
           -pct_univoltine,
           -CF_rich,
           -CG_rich,
           -MH_rich,
           -OM_rich,
           -PA_rich,
           -PH_rich,
           -PR_rich,
           -SC_rich,
           -SH_rich,
           -pct_CF,
           -pct_CG,
           -pct_MH,
           -pct_OM,
           -pct_PA,
           -pct_PH,
           -pct_PR,
           -pct_SC,
           -pct_SH,
           -pct_Dom5,
           -pct_Dom3,
           -pct_Dom1,
           -wade_boat,
           -HUC6_Name,
           -HUC8_Name,
           -HUC10_Name,
           -HUC12_Name,
           -long,
           -lat,
           -Onull,
           -Enull,
           -OoverE_null,
           -Shannon_diversity,
           -Simpson_diversity,
           -Evenness) %>%
   mutate(BC = round(BC,2)) %>%
   mutate(MTI = round(MTI,2)) %>%
   rename('O/E Ratio' = OoverE,
          'Bray Curtis Similarity Index' = BC,
          'BSTI' = BSTI,
          'Inferred Temperature' = TS,
          'MTI' = MTI,
          'O/E Condition' = oe_cond,
          '% Taxa Loss'= taxa_loss) %>%
  #gather('O/E Ratio','O/E Condition',key = "Index Type ID", value = "Index Score") %>% 
  gather('O/E Ratio','% Taxa Loss','Bray Curtis Similarity Index','Inferred Temperature','MTI',
         'BSTI','O/E Condition',key = "ID", value = "Score") %>%
  ###### ADD PROJECT#####
  mutate(project = "Oregon Plan") %>%
  #add habitat to comment field, what about other comments?
   mutate(com = ifelse(Habitat_sampled == 'R',"Riffle Habitat",
                    ifelse(Habitat_sampled == 'P',"Pool Habitat",
                           ifelse(Habitat_sampled == 'G',"Glide Habitat",
                                  ifelse(Habitat_sampled == 'T',"Transect Sample - Multiple Habitats",
                                         ifelse(Habitat_sampled == 'O',"Other - Unknown Habitat","")))))) %>% 
  # Shorten condition names to fit in AWQMS... sorry Shannon 
  mutate(Score = ifelse(Score == 'Least disturbed', "Good",
                             ifelse(Score == 'Moderately disturbed', "Fair",
                                    ifelse(Score == 'Most disturbed',"Poor",paste0(Score))))) %>%
  #building index ID 
  mutate(type_short = ifelse(Field_QC == 'S'& Lab_QC == 'S',"SR",
                      ifelse(Field_QC == 'S' & Lab_QC == 'CP',"SR",
                      ifelse(Field_QC == 'S' & Lab_QC == 'CD',"QCLR",
                      ifelse(Field_QC == 'FP'& Lab_QC == 'CP',"SR",
                      ifelse(Field_QC == 'FP'& Lab_QC == 'S',"SR",
                      ifelse(Field_QC == 'FD'& Lab_QC == 'S',"QCFR",
                      ifelse(Field_QC == 'FD'& Lab_QC == 'CP',"QCFR",
                      ifelse(Field_QC == 'FD2'& Lab_QC == 'S',"QCFR:2",
                      ifelse(Field_QC == 'FD2'& Lab_QC == 'CP',"QCFR:2",
                      ifelse(Field_QC == 'FD2'& Lab_QC == 'CD',"QCLR",
                      ifelse(Field_QC == 'FD3'& Lab_QC == 'S',"QCFR",
                      ifelse(Field_QC == 'FD'& Lab_QC == 'CD',"QCLR",""))))))))))))) %>%
  
  mutate(Qualifier = ifelse(Use_303d == 'Yes',"DQL=A","DQL=E")) %>%
  mutate(com = ifelse(Qualifier == "DQL=E", paste(com,reason_no303, sep ="- Reason E="),paste0(com)))%>%
  #generate index ID 
  mutate(id_short = ifelse(ID == 'O/E Ratio', "OE",
                           ifelse(ID == '% Taxa Loss',"TL",
                                  ifelse(ID == 'Bray Curtis Similarity Index',"BC",
                                         ifelse(ID == 'Inferred Temperature',"TS",
                                                ifelse(ID == 'BSTI', "BSTI",
                                                       ifelse(ID == 'MTI', "MTI",
                                                              ifelse(ID == 'O/E Condition', "COND",""))))))))%>%
  #mutate(monloc = ifelse(!STATION_KEY %like% "dfw_", paste0(STATION_KEY, "-ORDEQ"),paste0(STATION_KEY))) %>%
  mutate(Date4Id = strftime(Date, format = '%Y%m%d', tz = 'UTC')) %>%
  mutate(activity = paste(MLocID,Date4Id,as.character(Habitat_sampled),type_short,id_short, sep =":"))%>%
  select(activity,ID,Score,Date,MLocID,project,com,Qualifier,Eco3,Eco2)

write.csv(bioindex_long,"OregonPlan_Index_Long.csv",row.names = FALSE)
