### L. Merrick 5/6/2020
### merging DEQ and AWQMS taxon tables 

library(tidyverse)
require(RODBC) #get rid of this
library(odbc)
library(readxl)
library(data.table)

## building final SQL table for SQL 
## After Shannon reviewed the taxon table and Lesley added missing

# this the AWQMS table after adding the missing taxa 
AWQMS_table <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/Taxon_AWQMS.xlsx", sheet = "Taxon")
DEQ_AWQMS <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/DEQtoAWQMStaxonomymatch2.xlsx", sheet = "Decoder") 
DEQ_Taxon <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/DEQ_Taxon.xlsx", sheet = "Sheet1") %>% 
              rename(tax_comments = Comments)

# 
hybrid_taxon <- DEQ_Taxon %>% 
                left_join(DEQ_AWQMS,by = 'TAXON_CODE') %>% 
                left_join(AWQMS_table, by = c('tax_uid'='UID'))

write.csv(hybrid_taxon,"//deqlab1/BioMon/Databases/RawData_to_AWQMS/Taxon4SQL.csv")


DEQ_t <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/ODEQ_TAXONOMY TABLE_2019_MBC_10-11-19.xlsx", sheet = "Sheet1")
AWQMS_t <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/AWQMS_Taxon.xlsx", sheet = "Sheet1")
raw_d <- read_excel("//deqlab1/BioMon/Databases/RawData_to_AWQMS/18-132_2018_OR_DEQ_Data_10-4-19.xlsx", sheet = "Column Format (2)_DEQ edits")

## explore the AWQMS table 

a_tsn_sum <- AWQMS_t %>% 
             group_by(tax_extrnl_id) %>% 
             summarise(total = n()) %>% 
             filter(total > 1)
### checked out some of these out and the multiple records with the same TSN are "retired" names 

## merge awqms to DEQ 
d_a_match <- DEQ_t %>% 
       left_join(AWQMS_t, by = c('TSN'='tax_extrnl_id','Taxon'= 'tax_name')) %>% 
       filter(!is.na(tax_uid))

d_a_NoMatch <- DEQ_t %>% 
  left_join(AWQMS_t, by = c('TSN'='tax_extrnl_id','Taxon'= 'tax_name')) %>% 
  filter(is.na(tax_uid))

write.csv(d_a_match,"//deqlab1/BioMon/Databases/RawData_to_AWQMS/Taxon_Match.csv")
write.csv(d_a_NoMatch,"//deqlab1/BioMon/Databases/RawData_to_AWQMS/Taxon_NoMatch.csv")

      

  

