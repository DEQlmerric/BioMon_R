# L. Merrick 8/27/2020
# script to 

library(tidyverse)
library(readxl)
library(AWQMSdata)
library(RODBC) 

#pull in hybird taxon table from SQL BioMon database 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
hybrid_taxon <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") %>% 
  mutate(AWQMS_tax_uid = as.character(AWQMS_tax_uid))
# this is the downloaded AWQMS taxon table. I don't know the best way to get this integrated without downloaded the lastest version each time. 
awqms_t <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/AWQMS_Taxon_19jan2021.xlsx", sheet = "Taxon")

# Use AWQMSdata to pull data for results in AWQMS - 
swb <- AWQMS_Data(startdate = "2015-01-15", enddate = NULL, project = 'Statewide Biomonitoring') 
swb_actid <- swb %>% 
  select(act_id,MLocID,SampleStartDate,Project1) %>%
  filter(!grepl("-FM$", act_id)) %>%
  filter(!grepl("RE1$", act_id)) %>%
  #mutate(actid2 = sub('-.*', '', swb_actid$act_id)) %>% #this will need to be updated depending on the format
  #select(-act_id) %>%
  distinct()  
write.csv(swb_actid,"swb_actid2.csv")
# ended up giving up on joins and manually assigned act_id based on SVN 
ai<- read.csv("ai_sum.csv")
# source of .xlxs Biomon_Phoenix_for SQL migration_FINAL.mdb as excel file 
Samp <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/invert_sample_info_11Jan2021.xlsx", sheet = "Sheet1") %>%
  select(SVN,Project,Comments,Methods_ok,Taxonomist,Subsample_percent_value,Area_sampled) %>%  #seems like this is all I need
  rename(act_comments = Comments)
counts <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/invert_count_11Jan2021.xlsx", sheet = "Invert_count")

#### pull in a test data set from Biomon_Phoenix_for SQL migration_FINAL.mdb as excel file  ####
Samp <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/ChemData_bug.xlsx", sheet = "Sample_Info") %>%
  select(SVN,Comments,Methods_ok,Taxonomist) %>%  #seems like this is all I need
  rename(act_comments = Comments)
counts <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/ChemData_bug.xlsx", sheet = "invert_count")
act_id <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/ChemData_bug.xlsx", sheet = "act_id") # I formatted this in excel 

#### generate AWQMS upload ####
d_count <- counts %>% 
  left_join(Samp, by = 'SVN') %>%
  filter(Project == 'Reference Trend') %>%
  left_join(hybrid_taxon,by = 'TAXON_CODE') %>%
  left_join(awqms_t, by = c('AWQMS_tax_uid'= 'UID')) %>%
  left_join(ai, by = 'SVN') %>%
  #left_join(swb_actid, by = c('MLocID' = 'MLocID','Date' = 'SampleStartDate')) %>% 
  mutate(colmeth = case_when(Habitat_sampled == 'R' ~ "Benthic Kick - Riffle",
                             Habitat_sampled == 'T' ~ "Benthic Kick - Transect",
                             TRUE ~ as.character('Benthic Kick - Mixed')),
         act_type = case_when(Field_QA == 'S'& Lab_QA == 'S' ~ 'SR', # translation in R
                              Field_QA == 'S'& Lab_QA == 'LP' ~ 'SR',
                              Field_QA == 'FP' & Lab_QA == 'S' ~ 'SR',
                              Field_QA == 'FD' & Lab_QA == 'S'~ 'QCFR',
                              Field_QA == 'FD' & Lab_QA == 'LP'~ 'QCFR',
                              Field_QA == 'S' & Lab_QA == 'LD' ~ 'QCLR',
                              Field_QA == 'FP' & Lab_QA == 'LD' ~ 'QCLR',
                              Field_QA == 'FD' & Lab_QA == 'LD' ~ 'QCLR',
                              TRUE ~ 'ERROR'), 
         media = 'Biological',
         assemblage = 'Benthic Macroinvertebrates',
         equip = 'D-Frame Net',
         char = 'Count',
         unit = 'count',
         status = if_else(Methods_ok == 'Yes','Final','Rejected'),
         value = 'Actual', #when would this be estimated? 
         intent = 'Population Census',
         method = 'fixed count 500',
         context = 'OREGONDEQ',
         method_name = 'Benthic Macroinvertebrates',
         qual = if_else(Methods_ok == 'Yes','','ALT'),
         speciesID = if_else(UniqueTaxon == 'Yes', 'UniqueTaxon','AmbiguousTaxon'),
         habit = "",
         Project1 = 'Statewide Biomonitoring') %>%
  #act_id = paste(act_id,act_type,'BENTHIC', sep = '-')) %>% # still have to figure out the multiple duplicates 
  select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,Count,unit,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON,SVN) %>%  ## missing Taxonomy_lab, FFG,Volt from taxa table
  rename(result = Count)

d_density <- counts %>% 
  left_join(Samp, by = 'SVN') %>%
  filter(Project == 'Reference Trend') %>%
  filter(!(is.na(Subsample_percent_value))) %>%
  left_join(hybrid_taxon,by = 'TAXON_CODE') %>%
  left_join(awqms_t, by = c('AWQMS_tax_uid'= 'UID')) %>%
  left_join(ai, by = 'SVN') %>% 
  mutate(colmeth = case_when(Habitat_sampled == 'R' ~ "Benthic Kick - Riffle",
                             Habitat_sampled == 'T' ~ "Benthic Kick - Transect",
                             TRUE ~ as.character('Benthic Kick - Mixed')),
         act_type = case_when(Field_QA == 'S'& Lab_QA == 'S' ~ 'SR', # translation in R
                              Field_QA == 'S'& Lab_QA == 'LP' ~ 'SR',
                              Field_QA == 'FP' & Lab_QA == 'S' ~ 'SR',
                              Field_QA == 'FD' & Lab_QA == 'S'~ 'QCFR',
                              Field_QA == 'FD' & Lab_QA == 'LP'~ 'QCFR',
                              Field_QA == 'S' & Lab_QA == 'LD' ~ 'QCLR',
                              Field_QA == 'FP' & Lab_QA == 'LD' ~ 'QCLR',
                              Field_QA == 'FD' & Lab_QA == 'LD' ~ 'QCLR',
                              TRUE ~ 'ERROR'), 
         media = 'Biological',
         assemblage = 'Benthic Macroinvertebrates',
         equip = 'D-Frame Net',
         char = 'Density',
         result = (Count/Area_sampled)*(Subsample_percent_value),
         unit = '#/ft2',
         status = if_else(Methods_ok == 'Yes','Final','Rejected'),
         value = 'Actual', #when would this be estimated? 
         intent = 'Species Density',
         method = 'fixed count 500',
         context = 'OREGONDEQ',
         method_name = 'Benthic Macroinvertebrates',
         qual = if_else(Methods_ok == 'Yes','','ALT'),
         speciesID = if_else(UniqueTaxon == 'Yes', 'UniqueTaxon','AmbiguousTaxon'),
         habit = "",
         Project1 = 'Statewide Biomonitoring') %>%
  #act_id = paste(act_id,act_type,'BENTHIC', sep = '-')) %>% # still have to figure out the multiple duplicates 
  select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,result,unit,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON,SVN)  ## missing Taxonomy_lab, FFG,Volt from taxa table
# bring both together 
d <-rbind(d_count,d_density)

# run a couple checks to verify SVN and act_id counts match
check <- counts %>% 
  left_join(Samp, by = 'SVN') %>%
  filter(Project == 'Reference Trend') 
SVN_sum <- check %>% 
  group_by(SVN) %>%
  summarise(n= n()) 
act_svn_sum <- d %>% 
  filter(unit == 'count') %>%
  group_by(act_id,SVN) %>%
  summarise(n= n())  
act_id_sum <- d %>% 
  filter(unit == 'count') %>%
  group_by(act_id) %>%
  summarise(n= n())  

write.csv(d,"//deqlab1/BioMon/Databases/RawData_to_AWQMS/Statewide Biomonitoring_4.csv") 
