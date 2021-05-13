### Generate AWQMS upload from data template ###
### L. Merrick 5/7/2021 ###

library(tidyverse)
library(readxl)
library(RODBC)

# Enter File Information 
data_path <- "//deqlab1/Assessment/Integrated_Report/Call_for_data/2022/Submitted Data/Biological Data/City of Gresham/CityOfGresham_GreshMacros_01012020-12312020_LAM.xlsx"
awqms_project <- 'Integrated Report - Call for Data'


#pull in hybrid taxon table from SQL BioMon database 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
hybrid_taxon <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") %>% 
  mutate(AWQMS_tax_uid = as.character(AWQMS_tax_uid))
# this is the downloaded AWQMS taxon table. I don't know the best way to get this integrated without downloaded the lastest version each time. 
awqms_t <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/AWQMS_Taxon_19jan2021.xlsx", sheet = "Taxon")

#bring in data 
Samp <- read_excel(data_path, sheet = "Sample") %>%
        filter(!is.na('Monitoring Location ID')) %>% 
        select("Sample ID (Locked)","Project ID","Alternate Project ID",Comments,
               Taxonomist,"Subsample Amount") %>%  
  rename(act_id= "Sample ID (Locked)",project = "Project ID",
         AltProjectID = "Alternate Project ID",
         act_comments = Comments, subsample_squares = "Subsample Amount") %>%
  mutate(Methods_ok = "Yes",
         Fixed_Count = "fixed count 500",
         Subsample_fraction_value = (subsample_squares/30),
         Area_sampled = 8)

counts <- read_excel(data_path, sheet = "Results") %>% 
  select("Sample ID (Locked)","Monitoring Location ID","Sample Date",
         "Habitat Type","Field QA","Lab QA","Taxon",
         "Taxonomic Serial Number","Stage ID","Count Value","Unique Taxon") %>%
  rename(act_id="Sample ID (Locked)",MLocID = "Monitoring Location ID",
         Date = "Sample Date",Habitat_sampled ="Habitat Type",Field_QA = "Field QA",
         Lab_QA ="Lab QA",TaxonSN = "Taxonomic Serial Number",
         StageID = "Stage ID",Count = "Count Value",UniqueTaxon = "Unique Taxon")
  
  
### build table with sample info  
d_samp <- counts %>% 
  left_join(Samp, by = 'act_id') %>%
  left_join(hybrid_taxon,by = 'Taxon') %>%
  left_join(awqms_t, by = c('AWQMS_tax_uid'= 'UID')) %>%
  mutate(colmeth = case_when(Habitat_sampled == 'R' ~ "Benthic Kick - Riffle",
                             Habitat_sampled == 'T' ~ "Benthic Kick - Transect",
                             Habitat_sampled == 'P' ~ "Benthic Kick - Pool",
                             TRUE ~ as.character('Benthic Kick - Mixed')),
         act_type = case_when(Field_QA %in% c('S','FP') & Lab_QA %in% c('S','CP') ~ 'SR', # translation in AWQMS
                              Field_QA == 'FD' & Lab_QA %in% c('S','CP') ~ 'QCFR',
                              Field_QA %in% c('S','FP','FD') & Lab_QA == 'CD' ~ 'QCLR',
                              TRUE ~ 'ERROR'), # should this default to sample routine for missing values? 
         media = 'Biological',
         assemblage = 'Benthic Macroinvertebrates',
         equip = 'D-Frame Net',
         status = if_else(Methods_ok == 'Yes','Final','Rejected'),
         value = 'Actual', #when would this be estimated? 
         method = Fixed_Count,
         context = 'OREGONDEQ',
         method_name = 'Benthic Macroinvertebrates',
         qual = if_else(Methods_ok == 'Yes','','ALT'),
         speciesID = if_else(UniqueTaxon == 'Yes', 'UniqueTaxon','AmbiguousTaxon'),
         habit = "",
         Project1 = project,
         Project2 = AltProjectID,
         Comments = '')%>%
   select(act_id,act_type,Field_QA,Lab_QA,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,method,context,DEQ_TAXON,
         Count,Area_sampled,Subsample_fraction_value,Comments)  ## missing Taxonomy_lab, FFG,Volt from taxa table


### confirm act_ids #### - should match the number in the Samp table
d_actid_check <- d_samp %>% 
  select(act_id,act_type,Field_QA,Lab_QA) %>%
  distinct()

#create a table of counts 
d_count <- d_samp %>% 
  mutate(char = 'Count',
         unit = 'count',
         value = 'Actual', #when would this be estimated? 
         intent = 'Population Census') %>%
  select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,Count,unit,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON) %>%  ## missing Taxonomy_lab, FFG,Volt from taxa table
  rename(result = Count)


#### density #####
d_density <- d_samp %>% 
  mutate(char = 'Density',
         result = (Count/(Area_sampled*Subsample_fraction_value)),
         unit = '#/ft2',
         value = 'Calculated', #when would this be estimated? 
         intent = 'Species Density') %>%
  select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,result,unit,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON)  ## missing Taxonomy_lab, FFG,Volt from taxa table

# bring both together 
d <-rbind(d_count,d_density) 

# run a couple checks to verify SVN and act_id counts match
SVN_sum <- d_samp %>% 
  group_by(SVN) %>%
  summarise(n= n()) 
act_svn_sum <- d_samp %>% 
  group_by(act_id,SVN) %>%
  summarise(n= n())
#write.csv(act_svn_sum,"act_svn_sum.csv")
act_id_sum <- d_samp %>% 
  group_by(act_id) %>%
  summarise(n= n())  
#write.csv(act_id_sum,"act_id_sum.csv")

write.csv(d,"//deqlab1/BioMon/Databases/RawData_to_AWQMS/DEQ_2020.csv",na = "",row.names = FALSE) 