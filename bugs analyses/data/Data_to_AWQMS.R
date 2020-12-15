# L. Merrick 8/27/2020
# script to 
 
library(tidyverse)
library(readxl)


# pull in a test data set from Biomon_Phoenix_for SQL migration_FINAL.mdb as excel file 

Samp <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/ChemData_bug.xlsx", sheet = "Sample_Info") %>%
                  select(SVN,Comments,Methods_ok,Taxonomist) %>%  #seems like this is all I need
                   rename(act_comments = Comments)
counts <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/ChemData_bug.xlsx", sheet = "invert_count")
act_id <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/ChemData_bug.xlsx", sheet = "act_id") # I formatted this in excel 


d <- counts %>% 
     left_join(hybrid_taxon,by = 'TAXON_CODE') %>%
     left_join(Samp, by = 'SVN') %>%
     left_join(act_id, by = c('MLocID' = 'MLocID','Date' = 'SampleStartDate')) %>% 
     mutate(colmeth = case_when(Habitat_sampled == 'R' ~ "Benthic Kick - Riffle",
                                Habitat_sampled == 'T' ~ "Benthic Kick - Transect",
                                TRUE ~ as.character('Benthic Kick - Mixed')),
            act_type = case_when(Field_QA == 'S' ~ 'SR',  # translation in R 
                                 Field_QA == 'FP' ~ 'SR',
                                 Field_QA == 'FD' ~ 'QCFR',
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
            act_id = paste(act_id,act_type,'BENTHIC', sep = '-')) %>% # still have to figure out the multiple duplicates 
            select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,Count,unit,
                   status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,Comments,method,context,DEQ_TAXON)  ## missing Taxonomy_lab, FFG,Volt from taxa table

write.csv(d,"//deqlab1/BioMon/Databases/RawData_to_AWQMS/Lady/bugs_awqms_3.csv") 
            