# Author: Shannon Hubler
# Initial date: 1/14/21
#
#
# Purpose: Bring together GIS and GE screening results into a single output file

# Do this for both DEQ and USU sites.  
# My initial thought was to bring all sites together (DEQ = 2514, USU = 929) into a single data frame/file
# But the GIS outputs for both sets of sites have different column names and dimensions (DEQ ~ 30, USU ~ 60)

library(tidyverse)

# bring in GIS outputs

gis.deq <- read.csv('Reference/ref_screen.csv')

gis.usu <- read.csv('Reference/ref_screen_USU.csv')


####### bring in Google Earth screens outputs
### DEQ
ge.deq <- read.csv('Reference/GE_Site_sum.scores_ave_bpj_DEQ.csv')

# add an ownership column
ge.deq <- ge.deq %>%
  mutate(owner = 'DEQ')


#### USU
ge.usu <- read.csv('Reference/GE_Site_sum.scores_ave_bpj_USU.csv')

# add ownership
ge.usu <- ge.usu %>%
  mutate(owner = 'USU')



#### combine all Google Earth sites into one data.frame
ge.2020_all <- rbind(ge.deq, ge.usu)
ge.2020_all <- ge.2020_all[,-c(1)]

write.csv(ge.2020_all, '//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/_Final outputs/REF.2020_FINAL_ALL_deq.usu.csv', 
          row.names=FALSE)



##### merge GIS and Ref

## DEQ
ge.deq_lim <- ge.deq %>%
  select(MLocID, Disturb.score, BPJ_final, Ref2020_FINAL)

gis.ge_deq <- gis.deq %>%
  full_join(ge.deq_lim, by = c('MLocId'='MLocID'))

########### change final ref status field, based on conditional status in multiple columns
# 
gis.ge_deq$Ref2020_FINAL <- ifelse(is.na(gis.ge_deq$Ref2020_FINAL), 
                            gis.ge_deq$GIS.status_2020, gis.ge_deq$Ref2020_FINAL)


# one more time to deal with Ref_GIS.candidate showing up for weird sites wihtout Lat/Longs 
@@@ 
  @@@@@@ hopefully fix these weird unknown sites, remove permanently, then delete this section
@@@
  
gis.ge_deq$Ref2020_FINAL <- ifelse(gis.ge_deq$Ref2020_FINAL=='Ref_GIS.candidate', 
                                   'NO', gis.ge_deq$Ref2020_FINAL)



write.csv(gis.ge_deq, '//deqlab1/GIS_WA/Project_WOrking_Folders/Reference/2020/_Final outputs/FINAL.reference.2020_gis.ge_DEQ.csv',
          row.names = FALSE)

######

## USU

#####

ge.usu_lim <- ge.usu %>%
  select(MLocID, Disturb.score, BPJ_final, Ref2020_FINAL)

gis.ge_usu <- gis.usu %>%
  left_join(ge.usu_lim, by = c('MLocID'))


########### change final ref status field, based on conditional status in multiple columns
# 
gis.ge_usu$Ref2020_FINAL <- ifelse(is.na(gis.ge_usu$Ref2020_FINAL), 
                                   gis.ge_usu$GIS.status_2020, gis.ge_usu$Ref2020_FINAL)
        
            




write.csv(gis.ge_usu, '//deqlab1/GIS_WA/Project_WOrking_Folders/Reference/2020/_Final outputs/FINAL.reference.2020_gis.ge_USU.csv',
          row.names = FALSE)


