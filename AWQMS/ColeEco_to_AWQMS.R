### Generate AWQMS upload from Cole Ecological ###
### L. Merrick 5/7/2021 ###

library(tidyverse)
library(readxl)
library(RODBC)

# Enter File Information 
data_path <- "//deqlab1/biomon/Bugs/2021/20-132_2020_OR_DEQ_Data_4-26-21.xlsx"
awqms_project <- 'Statewide Biomonitoring'


#pull in hybrid taxon table from SQL BioMon database 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
hybrid_taxon <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") %>% 
  mutate(AWQMS_tax_uid = as.character(AWQMS_tax_uid))
# this is the downloaded AWQMS taxon table. I don't know the best way to get this integrated without downloaded the lastest version each time. 
awqms_t <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/AWQMS_Taxon_19jan2021.xlsx", sheet = "Taxon")

#bring in data 
Samp <- read_excel(data_path, sheet = "Sample List", skip = 2) %>%
        select(SVN,Comments,Methods_ok,Fixed_Count,Taxonomist,Subsample_squares,total_squares,Area_sampled) %>%  
        rename(act_comments = Comments)

counts <- read_excel(data_path, sheet = "Column Format", skip = 2) %>% 
          select(SVN,STATION_KEY,Date,Habitat_sampled,Field_QA,Lab_QA,TAXON_CODE,TaxonSN,StageID,Count,UniqueTaxon,Comments)

### build table with sample info  
d_samp <- counts %>% 
  left_join(Samp, by = 'SVN') %>%
  left_join(hybrid_taxon,by = 'TAXON_CODE') %>%
  left_join(awqms_t, by = c('AWQMS_tax_uid'= 'UID')) %>%
  mutate(colmeth = case_when(Habitat_sampled == 'R' ~ "Benthic Kick - Riffle",
                             Habitat_sampled == 'T' ~ "Benthic Kick - Transect",
                             Habitat_sampled == 'P' ~ "Benthic Kick - Pool",
                             TRUE ~ as.character('Benthic Kick - Mixed')),
         act_type = case_when(Field_QA %in% c('S','FP') & Lab_QA %in% c('S','LP') ~ 'SR', # translation in AWQMS
                              Field_QA == 'FD' & Lab_QA %in% c('S','LP') ~ 'QCFR',
                              Field_QA %in% c('S','FP','FD') & Lab_QA == 'LD' ~ 'QCLR',
                              TRUE ~ 'ERROR'), # should this default to sample routine for missing values? 
         media = 'Biological',
         assemblage = 'Benthic Macroinvertebrates',
         equip = 'D-Frame Net',
         status = if_else(Methods_ok == 'Yes','Final','Rejected'),
         value = 'Actual', #when would this be estimated? 
         method = case_when(Fixed_Count == '500' ~ "fixed count 500",
                            Fixed_Count == '100' ~ "fixed count 100",
                            Fixed_Count == '300' ~ "fixed count 300",
                            Fixed_Count == '600' ~ "fixed count 600",
                            TRUE ~ 'ERROR'),
         context = 'OREGONDEQ',
         method_name = 'Benthic Macroinvertebrates',
         qual = if_else(Methods_ok == 'Yes','','ALT'),
         speciesID = if_else(UniqueTaxon == 'Yes', 'UniqueTaxon','AmbiguousTaxon'),
         habit = "",
         Project1 = awqms_project,
         MLocID = paste(STATION_KEY,"ORDEQ",sep ="-")) %>%
  mutate(Date4Id = strftime(Date, format = '%Y%m%d', tz = 'UTC'),
         act_id = paste(MLocID,Date4Id,as.character(Habitat_sampled),act_type,sep =":")) %>%
  select(act_id,act_type,Field_QA,Lab_QA,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,Comments,method,context,DEQ_TAXON,SVN,
         Count,Area_sampled,Subsample_squares,total_squares)  ## missing Taxonomy_lab, FFG,Volt from taxa table


### confirm act_ids ####
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
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON,SVN) %>%  ## missing Taxonomy_lab, FFG,Volt from taxa table
  rename(result = Count)


#### density #####
d_density <- d_samp %>% 
           mutate(char = 'Density',
                  Subsample_percent_value = (Subsample_squares/total_squares),
                  result = (Count/(Area_sampled*Subsample_percent_value)),
                  unit = '#/ft2',
                  value = 'Calculated', #when would this be estimated? 
                  intent = 'Species Density') %>%
      select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,result,unit,
              status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON,SVN)  ## missing Taxonomy_lab, FFG,Volt from taxa table

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

