# Start of the biomon AWQMS data pull 

library(tidyverse)
library(RODBC) 
library(readxl)
library(data.table)

### connect to BioMon SQL database ### 
bio.sql <- odbcConnect("BioMon") ### This requires an ODBC connection to the BioMon Database - directions here \\deqlab1\BioMon\Databases\ODBC_32_BioMon.docx
data <- sqlFetch(bio.sql,"dbo.VW_Raw_Macro") # this is a view with all the macro data 
tax <- sqlFetch(bio.sql,"dbo.Taxon_DEQ") ##  DEQ Taxon table with AWQMS uid 

d_t <- data %>% 
       left_join(tax, by = c('tax_uid' = 'AWQMS_tax_uid')) 



#This is my addition