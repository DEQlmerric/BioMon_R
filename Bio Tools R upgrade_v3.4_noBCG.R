# Author: Shannon Hubler
# Version 3.0
  # original: 12/9/14
  # version 3.0: 10/2/18
  # 12.14.20 = get this code into the Biomon_R GitHub repository (SLH)



# The purpose of this script is to:
#     1) import biological and predictor data from the DEQ Lab Biomonitoring database (Biomon_Phoenix)
          # update 3.0: source Stations Info from new SQL Stations database (Lesley Merrick)
#     2) link bio and predictor data
#     3) Run RIVPACS models (currently PREDATOR: MWCF and WCCP)
#     4) bring in STRESSOR ID scores (TS and BSTI)
#     5) Link STRESSOR ID scores and O/E, metrics
#     6) Assign condition classes to 
#           a) O/E = based on Biocriteria Listing Methodology
#           b) TS and BSTI = based on separate CART model benchmarks


#############################################################################

# Step 1: Import tables directly from Stations, then Biomon_Phoenix for 1) Station Table (Predictors and model regions), 2) Bug data, 
# 3)Sample Info, and 4) Taxonomy 

#############################################################################

### Bring in Stations Table from SQL database
# Script to generate AWQMS upload for stations from a list of stations - adapted from Travis's VolMon script 

require(rgdal)
require(RODBC)
library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)

#disable scientific notation 
#options(scipen = 999)
#setwd("A:/R Stats/Bio Tools_Upgrade with R")

#connect to view as a general user 
sta.sql = odbcConnect('Stations')

# Connect to stations database --------------------------------------------

#pull in stations table
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)

#limit columns to those needed for these analyses

stations$ELEV_m <- stations$ELEV_Ft*0.3048 #get elevation in meters
library(data.table)
setnames(stations, old=c('station_key', 'Lat_DD', 'Long_DD', 'Predator_WorE', 'EcoRegion2', 'EcoRegion3'), 
         new=c('STATION_KEY', 'lat', 'long', 'W_E', 'Eco2', 'Eco3'))

library(dplyr)

stations<-stations %>%
  select(STATION_KEY, MLocID, StationDes, lat, long, Eco2, Eco3, EcoRegion4, ELEV_m, precip_mm, temp_Cx10, 
         W_E, HUC6_Name, HUC8_Name, HUC10_Name, HUC12_Name, Wade_Boat, COMID)

library(plyr)
revalue(stations$Eco2, c("MARINE WEST COAST FOREST" = "MWCF", "WESTERN CORDILLERA" = "WC")) -> stations$Eco2  # change factor levels to match code below for names to Level 2 Ecoregions

summary(stations$Eco2)
colnames(stations)                

#####################


library(plyr)
library(reshape)
library(reshape2)
library(data.table)
library(RODBC)
library(lubridate)           
library(lattice)
library(ggplot2) 
library(gridExtra)
#library(dplyr)




channel <- odbcConnectAccess("//deqlab1/Biomon/Databases/Biomon_Phoenix_for SQL migration_FINAL.mdb") # connect to database


#access.con <- odbcConnectAccess('//deqlab1/biomon/databases/Biomon_Phoenix.mdb')
TableNames<- sqlTables(channel,errors = FALSE)  # get names of tables
      # replaced this section with code below from Lesley/Travis to connect to SQL Stations table
      # stations <- sqlFetch(channel, "STATION 2015") # bring in the Station table
      # predictors<-sqlFetch(channel, "STATION 2015_calculated")
      # stations<-merge(x=stations, y=predictors, by='STATION_KEY')

bugs<-sqlFetch(channel, 'Invert_count')   # bring in the bug table
setnames(bugs, old=c('SVN'), new=c('Sample'))

bugs <- bugs[order(bugs$Sample),] 

#fix any differences in UniqueTaxon column
bugs$UniqueTaxon <- dplyr::case_when(
  # Syntax: logical test ~ value to use when test is TRUE
  bugs$UniqueTaxon == "Y" ~ "Yes",
  bugs$UniqueTaxon == "YES" ~ "Yes",
  bugs$UniqueTaxon == "Yes" ~ "Yes",
  bugs$UniqueTaxon == "N" ~ "No",
  bugs$UniqueTaxon == "NO" ~ "No",
  bugs$UniqueTaxon == "No" ~ "No",
  
)


# roll all IDs of same taxon in same sample to one record and sum the counts


# rename the SVN column to 'Sample' and 'CountValue' to match the saved PREDATOR models .RData files, 'TAXON_CODE_B' to match Taxonomy table
colnames(bugs)



sample.info <-sqlFetch(channel, 'Invert_sample_info')
setnames(sample.info, old=c('SVN'), new=c('Sample'))
colnames(sample.info)


taxa<-sqlFetch(channel, 'Taxonomy')             # bring in the taxonomy table


# bring in reference designations and merge with stations

library(RODBC)  

channel <- odbcConnectAccess("//deqlab1/BioMon/Databases/Biomon_Phoenix _for SQL migration.mdb") # connect to database


ref<-sqlFetch(channel, 'DEQ ref_old and new')  
colnames(ref)
ref<-ref[,c(1,5:9)]


stations.ref<-merge(stations, ref, by='STATION_KEY', all.x=TRUE)  
colnames(stations.ref)


#############################################################################

#Step 2: merge the data.frames + calculate metrics

#############################################################################

#'bugs' and 'taxonomy'
b.t<-merge(x=bugs, y=taxa, by='TAXON_CODE', all.x=TRUE)#
#colnames(b.t)
#write.csv(b.t, 'A:/Databases/b.t.csv')


# 'Invert Sample Info' and 'Stations'
sample.stations<-merge(y=stations.ref, x=sample.info, by='MLocID', all.x=TRUE, suffix=c('','.y'))
sample.stations<-sample.stations %>%
  select(-STATION_KEY.y)
colnames(sample.stations)

#bugs+taxonomy and sample.info and stations
b.s<-merge(b.t, sample.stations, by=c('Sample', 'MLocID', 'Date', 'Habitat_sampled', 'Field_QA', 'Lab_QA', 'Increment_Field', 'Increment_Lab'), all.x=TRUE)
setnames(b.s, old=c('STATION_KEY.x'), new=c('STATION_KEY'))     
colnames(b.s)
b.s<-b.s %>%
  select(-STATION_KEY.y)


# check for duplicates or missign data following merge
      length(b.s$Sample) - length(b.t$Sample)# 
      #b.s_dups[b.s_dups$Sample=='02022EMAPt', ]
  

## calculate total abundance from raw data
tot.abund<-aggregate(b.s$Count,  list(Sample=b.s$Sample), sum)  
colnames(tot.abund)[colnames(tot.abund)=="x"] <- "total.abundance"
              


# create a dataframe of relative abundances
rel.abund<-b.s[, c("Sample", "Taxon", "Count")]
rel.abund<-merge(rel.abund, tot.abund)
rel.abund<-ddply(.data = rel.abund, .(Sample, Taxon), plyr::summarize, rel.abund=Count/total.abundance)



          # Travis' code for how to deal with this thru the dplyr way:
          # rel.abund <- rel.abund %>%
          #   group_by(Sample, Taxon) %>%
          #   summarise(sum = sum(Count),
          #             sum_abundance = first(total.abundance),
          #             rel.abund.col = sum/sum_abundance)
          # 
          # 
          # check <- tot.abund %>%
          #   left_join(rel.abund, by = "Sample") %>%
          #   mutate(diff = total.abundance - sum_abundance) %>%
          #   filter(diff > 0)

# create a dataframe of relative abundances, but only for unique taxa -- used for Diversity/Evenness metrics
b.s.unique<- subset(b.s, UniqueTaxon=='Yes')
rel.abund.unique<-b.s.unique[, c("Sample", "Taxon", "Count")]
rel.abund.unique<-merge(rel.abund.unique, tot.abund)
rel.abund.unique<-ddply(.data = rel.abund.unique, .(Sample, Taxon), plyr::summarize, rel.abund.unique=Count/total.abundance)



###
####
##  ## METALS TOLERANCE INDEX
####
###


# need to drop taxa without tolerances, then re-adjust relative abundances
colnames(b.s)

myvars <- c("Sample", "Taxon", "Count", "MTI")
rel.abund.mti <- b.s[myvars]
# drop taxa missing MTI
    #rel.abund.mti<-rel.abund.mti[complete.cases(rel.abund.mti),]
rel.abund.mti<-subset(rel.abund.mti, MTI != 666)     # remove '666' from d.f (they are taxa not used in the index)

#calculate total abundance
tot.abund.mti<-aggregate(rel.abund.mti$Count,  list(Sample=rel.abund.mti$Sample), sum)  
setnames(tot.abund.mti, old=c('x'), new=c('tot.abund.mti'))
rel.abund.mti<-merge(rel.abund.mti, tot.abund.mti, by='Sample')
rel.abund.mti$rel.abund<-(rel.abund.mti$Count/rel.abund.mti$tot.abund.mti)
rel.abund.mti$tol_X_ra<-(rel.abund.mti$MTI * rel.abund.mti$rel.abund)

#calculate MTI: sum_across.taxa(rel abund * tol)

MTI<-aggregate(rel.abund.mti$tol_X_ra,  list(Sample=rel.abund.mti$Sample), sum) 
setnames(MTI, old=c('x'), new=c('MTI'))



###
####
##  ## Total Richness
####
###
 

b.s$Unique.num<-ifelse(b.s$UniqueTaxon=='Yes', 1,0)
total.richness<-aggregate(b.s$Unique.num,  list(Sample=b.s$Sample), sum)  
colnames(total.richness)[colnames(total.richness)=="x"] <- "total.richness"


###
####
##  ## Order_Family
####
###

# richness of all Orders --separatetly
order.rich<-count(b.s, vars=c('Sample','UniqueTaxon', 'Order'))
order.rich<- subset(order.rich, UniqueTaxon=='Yes')
order.rich<-cast(order.rich, Sample ~ Order)
colnames(order.rich) <- paste(colnames(order.rich), "rich",  sep = ".") #add '.rich' to note the type of metric
order.rich[is.na(order.rich)] <- 0
colnames(order.rich)[colnames(order.rich)=="Sample.rich"] <- "Sample"

order5.rich<-subset(order.rich, select=c(Sample, Coleoptera.rich, Diptera.rich, Ephemeroptera.rich, Plecoptera.rich, Trichoptera.rich)) # limit families to 3




# richness of all Families --separately  
family.rich<-count(b.s, vars=c('Sample','UniqueTaxon', 'Family'))
family.rich<- subset(family.rich, UniqueTaxon=='Yes')
family.rich<-cast(family.rich, Sample ~ Family)
colnames(family.rich) <- paste(colnames(family.rich), "rich",  sep = ".") #add '.rich' to note the type of metric
family.rich[is.na(family.rich)]<- 0
colnames(family.rich)[colnames(family.rich)=="Sample.rich"] <- "Sample"    

family3.rich<-subset(family.rich, select=c(Sample, Baetidae.rich, Chironomidae.rich, Hydropsychidae.rich)) # limit families to 3


           
# percent Order
# first calculate Order abundance
a<-ddply(.data=b.s, .(Sample, Order), summarize, Count=sum(Count))  # sum counts, across the  orders separately
a<-merge(tot.abund, a, all.x=TRUE)                              # bring in total abundance
pct_Order<-ddply(.data=a, .(Sample, Order), summarize, pct_ = (Count/total.abundance)*100 )  # calc % Abund: across station & order
pct_Order<-cast(pct_Order, Sample ~ Order)                    # matrify pct
colnames(pct_Order) <- paste("pct", colnames(pct_Order),   sep = "_")  # add prefix to denote metric type
colnames(pct_Order)[colnames(pct_Order)=="pct_Sample"] <- "Sample"
pct_Order[is.na(pct_Order)]<- 0
 
pct_order5<-subset(pct_Order, select=c(Sample, pct_Coleoptera, pct_Diptera, pct_Ephemeroptera, pct_Plecoptera, pct_Trichoptera)) # limit orders


          
###
####
##  ## EPT 
####
###

# richness

EPT.richness<-count(b.s, vars=c('Sample','UniqueTaxon', 'Order'))
EPT.richness<- subset(EPT.richness, UniqueTaxon=='Yes')
EPT.richness<-subset(EPT.richness,  Order=='Ephemeroptera' | Order=='Plecoptera' |Order=='Trichoptera')
EPT.richness<-aggregate(EPT.richness$freq,  list(Sample=EPT.richness$Sample), sum)  
colnames(EPT.richness)[colnames(EPT.richness)=="x"] <- "EPT.rich"

# % EPT
pct_EPT<-ddply(.data=pct_Order, .(Sample), summarize, 
               pct_EPT = sum(pct_Ephemeroptera, pct_Plecoptera, pct_Trichoptera)
               )
 


##
###
####
#     Voltinism = # of broods per year
####
###
##

# richness

voltine.rich<-count(b.s, vars=c('Sample','UniqueTaxon', 'Voltine'))
voltine.rich<- subset(voltine.rich, UniqueTaxon=='Yes')
voltine.rich<-cast(voltine.rich, Sample ~ Voltine) 
voltine.rich[is.na(voltine.rich)]<- 0
setnames(voltine.rich, old = c('MV', 'SV', 'UV'), new = c('multivoltine.rich','semivoltine.rich', 'univoltine.rich'))
colnames(voltine.rich)
voltine.rich<-voltine.rich %>%
  select(Sample, multivoltine.rich, semivoltine.rich, univoltine.rich)


# pct_Voltinism

a<-ddply(.data=b.s, .(Sample, Voltine), plyr::summarize, Count=sum(Count)) #sum counts, across the voltinism categories separately
a<-merge(tot.abund, a, all.x=TRUE)
pct_Voltine<-ddply(.data=a, .(Sample, Voltine), plyr::summarize, pct = Count/total.abundance*100)
pct_Voltine<-cast(pct_Voltine, Sample ~ Voltine) 
pct_Voltine<-pct_Voltine[,c(1:3,5)]
setnames(pct_Voltine, old = c('MV', 'SV', 'UV'), new = c('pct_multivoltine','pct_semivoltine', 'pct_univoltine'))




##
###
####
#     Functional Feeding Groups
####
###
##

# richness
ffg.rich<-count(b.s, vars=c('Sample','UniqueTaxon', 'FFG'))
ffg.rich<- subset(ffg.rich, UniqueTaxon=='Yes')
ffg.rich<-cast(ffg.rich, Sample ~ FFG) 
colnames(ffg.rich) <- paste(colnames(ffg.rich), "rich",  sep = ".")
colnames(ffg.rich)[colnames(ffg.rich)=="Sample.rich"] <- "Sample"


# pct_FFG

a<-ddply(.data=b.s, .(Sample, FFG), plyr::summarize, Count=sum(Count)) #sum counts, across the FFG categories separately
a<-merge(tot.abund, a, all.x=TRUE)

pct_FFG<-ddply(.data=a, .(Sample, FFG), plyr::summarize, pct = Count/total.abundance*100)
pct_FFG<-cast(pct_FFG, Sample ~ FFG) 
pct_FFG<-pct_FFG[,c(1:10)]
colnames(pct_FFG) <- paste("pct", colnames(pct_FFG),   sep = "_")  # add prefix to denote metric type
colnames(pct_FFG)[colnames(pct_FFG)=="pct_Sample"] <- "Sample"
ffg.rich[is.na(ffg.rich)]<- 0




##
###
####
#     Dominance
####
###
##

# % Dominant - Top 5 taxa
z<-ddply(.data=b.s, .(Sample),  plyr::summarize, Count=tail(sort(Count),5))
z<-merge(z, tot.abund)
z.1<-ddply(.data = z, .(Sample), plyr::summarize, pct.abund=Count/total.abundance*100)
pct_Dom.5<-ddply(.data=z.1, .(Sample), plyr::summarize, pct_Dom.5=sum(pct.abund))


# % Dominant - Top 3 taxa
z<-ddply(.data=b.s, .(Sample),  plyr::summarize, Count=tail(sort(Count),3))
z<-merge(z, tot.abund)
z.1<-ddply(.data = z, .(Sample), plyr::summarize, pct.abund=Count/total.abundance*100)
pct_Dom.3<-ddply(.data=z.1, .(Sample), plyr::summarize, pct_Dom.3=sum(pct.abund))

# % Dominant - Top 1 taxon
z<-ddply(.data=b.s, .(Sample),  plyr::summarize, Count=max(Count))
z<-merge(z, tot.abund)
pct_Dom.1<-ddply(.data = z, .(Sample), plyr::summarize, pct_Dom.1=Count/total.abundance*100)


# combine all dominance together
pct_Dom<- merge(pct_Dom.5, pct_Dom.3)
pct_Dom<- merge(pct_Dom, pct_Dom.1)



##
###
####
#     Diversity/Evenness
####
###
##

SH.div<-ddply(.data=rel.abund.unique, .(Sample), plyr::summarize, Shannon_diversity= -(sum(rel.abund.unique*log(rel.abund.unique))))
Simp.div<-ddply(.data=rel.abund.unique, .(Sample), plyr::summarize, Simpson_diversity= 1-sum(rel.abund.unique^2))
summary(Simp.div)


# Evenness
even<-merge(SH.div, total.richness)
even<-ddply(.data=even, .(Sample), plyr::summarize, Evenness= Shannon_diversity/(log(total.richness)))
summary(even)  


##
###
####
#     Non-Insect
####
###
##


# richness of all Non-Insects  
noninsect.rich<-count(b.s, vars=c('Sample','UniqueTaxon', 'Class'))
noninsect.rich<- subset(noninsect.rich, UniqueTaxon=='Yes' & Class != 'Insecta')
noninsect.rich<-ddply(.data=noninsect.rich, .(Sample), plyr::summarize, Non.Insect_richness=sum(freq))
summary(noninsect.rich)


# percent Non-Insects
# first calculate Order abundance
a<-ddply(.data=b.s, .(Sample, Class), plyr::summarize, Count=sum(Count))  # sum counts, across the  orders separately
a<- subset(a, Class != 'Insecta')
pct_Non.Insect<-ddply(.data=a, .(Sample), plyr::summarize, Count=sum(Count))
pct_Non.Insect<-merge(tot.abund, pct_Non.Insect, all.x=TRUE)                              # bring in total abundance
pct_Non.Insect<-ddply(.data = pct_Non.Insect, .(Sample), plyr::summarize, pct_Non.Insect=Count/total.abundance*100)
pct_Non.Insect[is.na(pct_Non.Insect)] <- 0

 
####
### ##
####  ##
###     ####        Combine all metrics together        #### ###
####  ##
### ##
####


list.of.data.frames = list(total.richness, order5.rich, family3.rich, 
                           pct_order5, EPT.richness, pct_EPT, 
                           voltine.rich, pct_Voltine,
                           ffg.rich, pct_FFG, pct_Dom, SH.div, 
                           Simp.div, even, noninsect.rich, pct_Non.Insect,
                           MTI)



metrics<-merge(list.of.data.frames[1], list.of.data.frames[2], by='Sample', all.x = TRUE)

for(i in 3:length(list.of.data.frames)){
  metrics <- merge(metrics, list.of.data.frames[i], by='Sample', all.x = TRUE)
  
}




#############################################################################
#############################################################################
#############################################################################
#############################################################################

# Step 3: format data files for STRESSOR ID


?????????????????? ??????????????? ????????? ???????????? ??? ???????????? ??? ??? ?????????????????? ??????????????? ???????????? ????????? ????????? ????????? ???????????? ???????????? ??? ????????? ??????????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ???????????? ??? ??? ?????????????????? ??????????????? ???????????? ????????? ????????? ????????? ???????????? ???????????? ??? ????????? ??????????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ???????????? ??? ??? ?????????????????? ??????????????? ???????????? ????????? ????????? ????????? ???????????? ???????????? ??? ????????? ???????????????


#############################################################################
#############################################################################
#############################################################################
#############################################################################

# 
# 
#  library(plyr)  # for aggregate function
# 
# ##
# ####
# #   Subset for STRESSOR
# ###
# ##
# 
# # 3.1 = first get counts from taxa to OTUs (no ambigous taxa)
stress.bugs<-aggregate(b.s$Count,  list(Sample=b.s$Sample, OTU_Stress05=b.s$OTU_Stress05), sum) # For each OTU in a Sample, sum the Counts
names(stress.bugs)[c(1:3)] <- c("Sample","OTU_Stress05","Count")            
stress.bugs<-arrange(stress.bugs, Sample)  # sort the d.f by Sample
stress.bugs<-subset(stress.bugs, OTU_Stress05 != 666) # remove '666' from d.f (they are ambiguous taxa not used in models)
 


#  
# # 3.2 = export file for use in C2 to run Stressor ID models (TS and BSTI)
write.csv(stress.bugs, '//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/stress.bugs.csv', row.names = FALSE )
# 
#       # Final format for export to 'C2' is Sample, OTU, Count 
#       # there is no need to subsample for Stressor ID models
#       # C2 can be used to tabulate the data
# 
# 
# # 3.3 = calculate total abundance from Stressor input files
tot.abund.STR<-aggregate(stress.bugs$Count,  list(Sample=stress.bugs$Sample), sum)  
colnames(tot.abund.STR)[colnames(tot.abund.STR)=="x"] <- "total.abundance.STR"
                   head(tot.abund.STR)
                   hist(tot.abund.STR$total.abundance.STR, breaks=100)
# 
# 
# 
# ####
#   ####
# #       3.4 = Bring in Stressor scores in from C2.
#   ####
# ####

#change file name to most recent run from C2
stress.scores<-read.csv('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/Stressor Scores_all samples_11.26.19.csv')


stress.scores<-rename(stress.scores, c("Code"="Sample")) #rename first column to 'Sample'

stress.scores$TS<-round(stress.scores$TS, 1) # Format TS to appropriate sig figs
stress.scores$BSTI<- (sin((3.14159265358979*((10^stress.scores$BSTI_WA_Inv)-1)/2)))^2*100 #Untransform BSTI
stress.scores$BSTI<- round(stress.scores$BSTI, 0) # Format BSTI to appropriate sig figs
stress.scores <- subset(stress.scores, select = -c(BSTI_WA_Inv) ) # drop untransformed--call out by name, in case column order changes

stress.scores[stress.scores$Sample =='12001MCB',] # 2387 12001MCB 20.3   12

                  @@@@@@@@@@@@@@@@
                    @@@@@@@@@@@@@@@@
                        @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FUTURE WORK: transition Stressor ID models to R
                    @@@@@@@@@@@@@@@@
                  @@@@@@@@@@@@@@@@



#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 4: Make PREDATOR input files

?????????????????? ??????????????? ????????? ???????????? ??? ??????????????? ??? ??? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ?????????????????? ??????????????? ??? ????????? ???????????? ???????????? ???????????? ??????????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ??????????????? ??? ??? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ?????????????????? ??????????????? ??? ????????? ???????????? ???????????? ???????????? ??????????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ??????????????? ??? ??? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ?????????????????? ??????????????? ??? ????????? ???????????? ???????????? ???????????? ??????????????? 

????????? ????????? ????????? ????????? ????????? 
????????? ????????? ????????? ????????? ????????? 
????????? ????????? ????????? ????????? ?????????
#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


#####
#####
#       4.1  Subset for PREDATOR
######
######

# first, get counts from Taxon/Taxon_Code ----> OTUs (no ambuiguous taxa)
colnames(b.s)
b.oe<-aggregate(b.s$Count,  list(Sample=b.s$Sample, OTU=b.s$OTU_RIV05_Nov05, MLocID=b.s$MLocID,  STATION_KEY=b.s$STATION_KEY, 
                                 Eco2=b.s$Eco2, Eco3=b.s$Eco3), sum )# For each OTU in a Sample, sum the Counts

colnames(b.oe)
b.oe<-rename(b.oe, c("x"="Count")) 
head(b.oe)
b.oe<-arrange(b.oe, Sample)               # sort the d.f by Sample
b.oe<-subset(b.oe, OTU != 666)      # remove '666' from d.f (they are ambiguous taxa not used in models)
head(b.oe)


                  #############          
                  #############
                  #############
                  #############
                  
                  @@@@@ IF new data is subsampled to 300 count 'AND' in MAtrix format, execute the following  (IGNORE subsampling below)
                  # bug.test<-read.table("MWCF_bugs_mat.txt",row.names="sample",header=T,sep="\t");
                  # dim(bug.test)
                  
                  #############          
                  #############
                  #############
                  #############

#####
#####
#       4.2 rarify to 300 count
######
######

# rarify to 300 count per sample --> this standardizes 'effort' across all samples 
# since O/E is basically 'reference taxa richness', it is highly related to total count


# load the 'reshape2' package and source in the 'rarify' script for subsampling to 300

source('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/rarify_w_seed.r')
        #source('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/rarify.r')

# Input bug files from Access tables (Steps 1-3) are '3-column' format and NOT-SUBSAMPLED to 300 count 
# To subsample and turn into a site*taxa matrix, execute the following:


# all bug data - rarify (subsample to 300), and matrify (turn into Sample x Taxa matrix)
        # rarify does not affect samples with OTU counts less than 300


b.rare.seed <- rarify.seed(na.omit(b.oe), 'Sample', 'Count', 300) 
        #b.rare <- rarify(na.omit(b.oe), 'Sample', 'Count', 300) #original rarify code from JVS, without a seed set.  Use to compare rarify.seed results

    
                  
    
#####
#####
#       4.3 get total abundance of rarified samples for PREDATOR
######
######


tot.abund.RIV<-aggregate(b.rare.seed$Count, list(Sample=b.rare.seed$Sample, STATION_KEY=b.rare.seed$STATION_KEY), sum)
names(tot.abund.RIV)[3]<-'tot.abund.RIV'
    

#####
#####
#       4.4 Matrify: convert from long format (many rows per sample), to site X species matrix (one row per sample)
######
######


dm.rare<- melt(b.rare.seed, measure.var = 'Count') 
bugs.all <- dcast(dm.rare, Sample+STATION_KEY+Eco2+Eco3 ~ OTU, fun.aggregate = sum) # Sample x OTU doesn't seem to be unique so you need the fun.aggregate.  
head(bugs.all)
 
 

     
#####
#####
#       4.5 subset the data for each PREDATOR model
######
######

bugs.MWCF<-subset(bugs.all, Eco2=='MWCF')
dim(bugs.MWCF)
summary(bugs.MWCF$Eco2)   ### verify only MWCF samples

bugs.WCCP<-subset(bugs.all, Eco3== 4 | Eco3 ==10 | Eco3== 78 | Eco3== 9| Eco3== 11)
dim(bugs.WCCP)
summary(bugs.WCCP$Eco2)  # Mostly you will see 'WC', but LEVEL3 Eco 10 (Col. Plateau) is in L2 'WIBR'

            # confirm that in the WCCP file, all records with Eco2 = 'WIBR', are located in Eco3 = '10' (Columbia Plateau)
            check.wccp.wibr<-ifelse(bugs.WCCP$Eco2=='WC', 'OK', 
                                  ifelse(bugs.WCCP$Eco2 == 'COLD DESERTS'& bugs.WCCP$Eco3 == '10', 'WIBR.10', 'Doh!'))
            
            table(check.wccp.wibr) # if 'Doh!', there's a problem
            
            
 

#####
#####
#       4.6  Create data.frame for NBR Null Model 
######
######

# samples from Eco3 = '80' and Eco3 = '12' 
bugs.NBR<-subset(bugs.all, Eco3== 80 | Eco3 ==12 )
dim(bugs.NBR)
table(bugs.NBR$Eco2, bugs.NBR$Eco3) # All samples must be from WIBR -- no Eco3 should be from '10'






###
#     4.7 create predictor files to match the bugs.model files
###


# get single records of Samples (SVN) and Stations (STATION_KEY)
b.samps<-unique(b.s[,c('Sample', 'MLocID', 'STATION_KEY', 'Date', 'Habitat_sampled', 'Field_QA', 'Lab_QA', 'Increment_Field', 'Increment_Lab')])    # use unique records so that they can be matched with predictors from Station table


b.samps.sta<-merge(b.samps, stations.ref, by="MLocID", all.x=TRUE, suffix=c("", ".y"))   
#setnames(b.samps.sta, old = c('LAT_field.x', 'LONG_field.x'), new = c('LAT_field', 'LONG_field'))
b.samps.sta<-b.samps.sta[c('Sample', 'STATION_KEY','MLocID', 'Date', 'long', 'lat','temp_Cx10', 'precip_mm', 
                           'Eco2', 'Eco3', 'ELEV_m', 'W_E')]                       
colnames(b.samps.sta)


               

# transform predictors: date --> daynum, Elev_m --> elev_sqrt; change name of LONG to 'long' (match PREDATOR model input)


b.samps.sta$daynum<-yday(b.samps.sta$Date)      #  date to daynum
b.samps.sta$elev_sqrt<-sqrt(b.samps.sta$ELEV_m) # transform elevation to sqrt of meters    
b.samps.sta$east<-as.numeric(ifelse(b.samps.sta$W_E =='w', 0,
                                  (ifelse(b.samps.sta$W_E =='e', 1,"" ))))   #change W_E to 0 or 1 (match PREDATOR input)

setnames(b.samps.sta, old=c('precip_mm','temp_Cx10'), new=c('precip', 'temp'))
colnames(b.samps.sta)

# make sure you have the following predictors, at a minimum: 
                  # daynum, 
                  # long, 
                  # east, 
                  # elev_sqrt, 
                  # precip, 
                  # temp  
        # other variables are fine, they will be ignored in running PREDATOR



#subset predictors to match model regions
preds.mwcf<-as.data.frame(subset(b.samps.sta, Eco2=='MWCF'))
preds.wccp<-subset(b.samps.sta, Eco2=='WC' | Eco3==10)
    
    #check for duplicate Samples in each file
    which(duplicated(bugs.MWCF$Sample))
    which(duplicated(bugs.WCCP$Sample))
    which(duplicated(preds.mwcf$Sample))
    which(duplicated(preds.wccp$Sample))  
    
    bugs.MWCF[duplicated(bugs.MWCF), ]
    bugs.WCCP[duplicated(bugs.WCCP), ]
    preds.mwcf[duplicated(preds.mwcf), ]
    preds.wccp[duplicated(preds.wccp), ]
    
                                       

# total # of observations in predictors may not match bugs.....merge tables so only matched remain
dim(bugs.MWCF); dim(preds.mwcf)
dim(bugs.WCCP); dim(preds.wccp)
mwcf.b.p<-merge(bugs.MWCF, preds.mwcf, by=c('Sample', 'STATION_KEY', 'Eco2', 'Eco3'), suffix=c("", ".y")) 

wccp.b.p<-merge(bugs.WCCP, preds.wccp, by=c('Sample', 'STATION_KEY', 'Eco2', 'Eco3'), suffix=c("", ".y")) 


#carve out bug and predictor data into FINAL input data.frames
      @@@@ 
        @@@@@@ would be best to use a list of taxa and call out other columns by name specifically @@@@@
        @@@@@@ but could be messy as new taxa are added into the OTU list...but I guess that requires changing column numbs too...
      @@@@
        
colnames(mwcf.b.p)
bugs.MWCF.F<-as.data.frame(mwcf.b.p[,c(1:296)])
preds.mwcf.F<-as.data.frame(mwcf.b.p[,c(1,297:307)])

colnames(wccp.b.p)
bugs.WCCP.F<-as.data.frame(wccp.b.p[,c(1:296)])
preds.wccp.F<-as.data.frame(wccp.b.p[,c(1,297:307)])
    

#verify dimensions are same for each input file
dim(bugs.MWCF.F); dim(preds.mwcf.F) 
dim(bugs.WCCP.F); dim(preds.wccp.F)


# need this to get Sample as row.names for 'model.predict.v4.1'.  If don't then O/E not assigned to Sample IDs.
row.names(bugs.MWCF.F)<-bugs.MWCF.F$Sample  
row.names(preds.mwcf.F)<-preds.mwcf.F$Sample
row.names(bugs.WCCP.F)<-bugs.WCCP.F$Sample
row.names(preds.wccp.F)<-preds.wccp.F$Sample

              
##  Align bug and predictor data, by site/sample;

row.names(bugs.MWCF.F)==row.names(preds.mwcf.F);#check sample(row) alignment of bug and predictor data;
bugs.MWCF.F<-bugs.MWCF.F[row.names(preds.mwcf.F),];#samples are not aligned. Fix by aligning bugs data to predictor data, since latter is sorted by sample type;
row.names(bugs.MWCF.F)==row.names(preds.mwcf.F);#check alignment again -- alignment OK;

row.names(bugs.WCCP.F)==row.names(preds.wccp.F);#check sample(row) alignment of bug and predictor data;
bugs.WCCP.F<-bugs.WCCP.F[row.names(preds.wccp.F),];#samples are not aligned. Fix by aligning bugs data to predictor data, since latter is sorted by sample type;
row.names(bugs.WCCP.F)==row.names(preds.wccp.F);#check alignment again -- alignment OK;




#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 5: Run PREDATOR = Making predictions for new (all) data.

?????????????????? ??????????????? ????????? ???????????? ??? ????????? ??? ??? ??????????????? ??????????????? ?????????????????? ??? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ?????????????????? ??????????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ????????? ??? ??? ??????????????? ??????????????? ?????????????????? ??? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ?????????????????? ??????????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ????????? ??? ??? ??????????????? ??????????????? ?????????????????? ??? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ??????????????? ?????????????????? ???????????????
#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################



############################

# From JVS original code:
# The 2 files (bugs and predictors) should have similar formats as the original taxa and predictor data sets used to build the model (see step 1 above);
# Notes on format --
#   A) The sample ID column in both files should be read into R as a row name (see Step 1 examples).
#   B) Predictor data set -- Must include columns with the same names, units, etc.,
#        as the model's predictor variables. All other columns will be ignored;
#        Column order does not matter;
#        Predictions and calculations of O/E will be made only for those samples that have;
#        complete data for all model predictors.;
#   C)  Sample-by-taxa matrix.  Sample ID's (row names) must match those of predictor data.
#        Model predictions will include only those taxa (columns) in this file that are also
#        in the calibration data (bugcal.pa);
#        To see a list of the calibration taxa, do the following:
         #                                                         names(bugcal.pa)[colSums(bugcal.pa)>0];
##########;

#source in prediction script and load models
source("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/model.predict.v4.1.r") # bring in model predict function (JVS script)

# load and run each model separately


#################

# 5.1 MWCF model

################

load('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/Nov05model_MWCF_16jan13.Rdata');    # bring in MWCF model


#Drop all samples/sites from bug and predictor data that do not not have complete data for the model predictors;
      # preds.mwcf.F<-rename(preds.mwcf.F, c("Long"="long"))
      

preds.mwcf.F<-preds.mwcf.F[complete.cases(preds.mwcf.F[,preds.final]),]
bugs.MWCF.F<-bugs.MWCF.F[row.names(preds.mwcf.F),]
dim(preds.mwcf.F); dim(bugs.MWCF.F)   

                  

#make predictions for test data;
OE.assess.test<-model.predict.v4.1(bugcal.pa,grps.final,preds.final, grpmns,covpinv,
                                   prednew=preds.mwcf.F,bugnew=bugs.MWCF.F,Pc=0.5);

                  

oe.mwcf<-OE.assess.test$OE.scores; #create a d.f out of OE.scores
        head(OE.assess.test$OE.scores)# look at O/E scores, for all samples;

        head(OE.assess.test$Group.Occurrence.Probs) # Look at the predicted group membership probabilities;
        head(OE.assess.test$Capture.Probs)  # Look at the predicted probabilties for each taxon at each site;

# Assign PREDATOR condition classes == MWCF benchmarks
oe.mwcf$OoverE<-round(oe.mwcf$OoverE, 2)
oe.mwcf$oe.cond<-(ifelse(oe.mwcf$OoverE <= 0.85, 'Most disturbed', 
       ifelse(oe.mwcf$OoverE > 0.85 & oe.mwcf$OoverE < 0.92, 'Moderately disturbed', 
              ifelse(oe.mwcf$OoverE >= 0.92 & oe.mwcf$OoverE < 1.25, 'Least disturbed', 
                     ifelse(oe.mwcf$OoverE >= 1.25, 'Enriched', -999)))))
     
              # calculate min - max for each condition class
              ddply(oe.mwcf, .(oe.cond), plyr::summarize, min = min(OoverE), max = max(OoverE))
                      # verify that results are consistent with PREDATOR documentation benchmarks: <=0.85, 0.86 - 0.91, 0.92 - 1.24, > 1.24
                      # verify no '-999' values

              
              
# assess all samples: 

source('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/assess.all.samples.v4.1_2.r')              
                            
assess.all_MWCF<- assess.all.samples.1.0(result.prd=OE.assess.test, bugnew=bugs.MWCF.F, Pc=0.5)


    # bring in BCG attributes for each taxon, merge with assess df
library(BCGcalc)
bcg.taxa<-TaxaMaster_Ben_BCG_PacNW

bcg.taxa<-bcg.taxa %>%
  select(-c(Phylum, SubPhylum, Class, SubClass, Order, Family, Tribe, Genus, SubGenus, Species))

assess.all_MWCF <- merge(assess.all_MWCF, bcg.taxa, by.x=c('Taxon'), by.y=c('TaxaID'), all.x=TRUE )            
              
assess.all_MWCF <- arrange(assess.all_MWCF, Sample)


assess.all_MWCF<-assess.all_MWCF[,c("Sample", "STATION_KEY", "Eco2", "Eco3", "Taxon", "observed", "Predicted", "Big.diff", "In.OtoE", "Class",            
        "BCG_Attr", "NonTarget", "Thermal_Indicator", "Long_Lived", "FFG", "Habit", "Life_Cycle")]

assess.all_MWCF<- arrange(assess.all_MWCF, Sample, desc(Class), BCG_Attr)


        
## Write  tables of O/E outputs
write.csv( OE.assess.test$OE.scores, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/MWFC_OEscores.csv", row.names=TRUE)
write.csv( OE.assess.test$Group.Occurrence.Probs, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/MWFC_grp.probs.csv", row.names=TRUE)
write.csv( OE.assess.test$Capture.Probs, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/MWFC_capt.probs.csv", row.names=TRUE)

write.csv(assess.all_MWCF, 'assess.all_MWCF.csv')




#############

# 5.2  WCCP model

#############

# need to remove MWCF objects, or WCCP predictions will be made on MWCF model data
rm(bugcal.pa,grps.final,preds.final, grpmns,covpinv) 

# bring in WCCP model
load('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/Nov05model_WCCP_16jan13.Rdata');    


#Drop all samples/sites from bug and predictor data that do not not have complete data for the model predictors;
        # preds.wccp.F<-rename(preds.wccp.F, c("Long"="long"))

preds.wccp.F<-preds.wccp.F[complete.cases(preds.wccp.F[,preds.final]),]
bugs.WCCP.F<-bugs.WCCP.F[row.names(preds.wccp.F),]
dim(preds.wccp.F); dim(bugs.WCCP.F)   


#make predictions for test data;
OE.assess.test<-model.predict.v4.1(bugcal.pa,grps.final,preds.final, grpmns,covpinv,
                                   prednew=preds.wccp.F,bugnew=bugs.WCCP.F,Pc=0.5);

oe.wccp<-OE.assess.test$OE.scores # create a d.f of OE.scores
                  head(oe.wccp)# look at O/E scores, for all samples;

                  head(OE.assess.test$Group.Occurrence.Probs)#Look at the predicted group membership probabilities;
                  head(OE.assess.test$Capture.Probs)#Look at the predicted probabilties for each taxon at each site;

# Assign PREDATOR condition classes == WCCP benchmarks
oe.wccp$OoverE<-round(oe.wccp$OoverE, 2)
oe.wccp$oe.cond<-(ifelse(oe.wccp$OoverE <= 0.78, 'Most disturbed', 
                         ifelse(oe.wccp$OoverE > 0.78 & oe.wccp$OoverE < 0.93, 'Moderately disturbed', 
                                ifelse(oe.wccp$OoverE >= 0.93 & oe.wccp$OoverE < 1.24, 'Least disturbed', 
                                       ifelse(oe.wccp$OoverE >= 1.24, 'Enriched', -999)))))

                  # calculate min - max for each condition class
                  ddply(oe.wccp, .(oe.cond), summarize, min = min(OoverE), max = max(OoverE))
                        # verify that results are consistent with PREDATOR documentation benchmarks: <=0.78, 0.79 - 0.92, 0.93 - 1.23, > 1.23

                  
# assess all samples: 

source('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/assess.all.samples.v4.1_2.r')              
                            
assess.all_WCCP<- assess.all.samples.1.0(result.prd=OE.assess.test, bugnew=bugs.WCCP.F, Pc=0.5)


# bring in BCG attributes for each taxon, merge with assess df
library(BCGcalc)
bcg.taxa<-TaxaMaster_Ben_BCG_PacNW

bcg.taxa<-bcg.taxa %>%
  select(-c(Phylum, SubPhylum, Class, SubClass, Order, Family, Tribe, Genus, SubGenus, Species))

assess.all_WCCP <- merge(assess.all_WCCP, bcg.taxa, by.x=c('Taxon'), by.y=c('TaxaID'), all.x=TRUE )            
              
assess.all_WCCP <- arrange(assess.all_WCCP, Sample)


assess.all_WCCP<-assess.all_WCCP[,c("Sample", "STATION_KEY", "Eco2", "Eco3", "Taxon", "observed", "Predicted", "Big.diff", "In.OtoE", "Class",            
        "BCG_Attr", "NonTarget", "Thermal_Indicator", "Long_Lived", "FFG", "Habit", "Life_Cycle")]

assess.all_WCCP<- arrange(assess.all_WCCP, Sample, desc(Class), BCG_Attr)


                  
  
## Write  tables of O/E outputs
write.csv( OE.assess.test$OE.scores, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/WCCP_OEscores.csv", row.names=TRUE)
write.csv( OE.assess.test$Group.Occurrence.Probs, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/WCCP_grp.probs.csv", row.names=TRUE)
write.csv( OE.assess.test$Capture.Probs, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/WCCP_capt.probs.csv", row.names=TRUE)

write.csv(assess.all_WCCP, 'assess.all_WCCP.csv')

                  
                  


#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 6: Assess NBR samples with Null model

  
?????????????????? ??????????????? ????????? ???????????? ??? ???????????? ??? ??? ??????????????? ??????????????? ??????????????? ??? ?????????????????? ???????????? ????????? ????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ???????????? ??? ??? ??????????????? ??????????????? ??????????????? ??? ?????????????????? ???????????? ????????? ????????? 
?????????????????? ??????????????? ????????? ???????????? ??? ???????????? ??? ??? ??????????????? ??????????????? ??????????????? ??? ?????????????????? ???????????? ????????? ?????????
#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


# create a null model taxa list
null.taxa.NBR<-list("Baetis", "Chironominae", "Optioservus", "Orthocladiinae", "Rhyacophila", "Trombidiformes", "Diphetor_hageni",
                      "Epeorus", "Zaitzevia", "Brachycentrus")

bugs.NBR.melt<- melt(bugs.NBR, id.vars=c('Sample', 'STATION_KEY','Eco2', 'Eco3'), variable.name = "Taxon", value.name = "Count")
        #setnames(bugs.NBR.melt, old=c('variable', 'value'), new=c('Taxon', 'Count'))
bugs.NBR.melt<-arrange(bugs.NBR.melt, Sample)
bugs.NBR.melt.null.taxa<-bugs.NBR.melt[bugs.NBR.melt$Taxon %in% null.taxa.NBR & bugs.NBR.melt$Count > 0 , ] # cut down to only include null taxa > 0
bugs.NBR.melt.null.taxa$Taxon<-droplevels(bugs.NBR.melt.null.taxa$Taxon)
table(bugs.NBR.melt.null.taxa$Taxon) # ensure only null.taxa.NBR appear


oe.nbr<-ddply(bugs.NBR.melt.null.taxa, .(Sample), summarize, Onull=length(Taxon), Enull=7.56, OoverE.null=length(Taxon)/7.56)  



oe.nbr$OoverE.null<-round(oe.nbr$OoverE.null, 2)
oe.nbr$oe.cond<-(ifelse(oe.nbr$OoverE.null <= 0.50, 'Most disturbed', 
                                          ifelse(oe.nbr$OoverE.null > 0.50 & oe.nbr$OoverE.null < 0.75, 'Moderately disturbed', 
                                                 ifelse(oe.nbr$OoverE.null >= 0.75 & oe.nbr$OoverE.null < 1.31, 'Least disturbed', 
                                                        ifelse(oe.nbr$OoverE.null >= 1.31, 'Enriched', -999)))))






#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 7: Calculate Willamette Valley--Puget Lowlands BCG

    @@@@@@@@@
           @
          @
         @
         @
         @
         @
#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


# Installing the BCGcalc library (with the vignette) from GitHub
# library(devtools) 
# install_github("leppott/BCGcalc", force=TRUE, build_vignettes=TRUE)


library(BCGcalc)

colnames(b.s)

# limit the bug data to only that needed for BCG calcs.  Basically it is sample, taxonID, counts, and unique status.
bugs.bcg<-b.s[,c('Sample','OTU_BCG_WV-PL','Count','UniqueTaxon', 'COMID')]
setnames(bugs.bcg, old=c('Sample','OTU_BCG_WV-PL','Count','UniqueTaxon'), new=c('SAMPLEID','TaxaID','N_TAXA','EXCLUDE'))
    
# change the Unique status to what BCG wants: which taxa to exclude.  
#This means we need to reverse what comes out of Biomon dbase, as we store what is unique.
bugs.bcg$EXCLUDE<- plyr::revalue(bugs.bcg$EXCLUDE, c("Yes"="FALSE", "No" = "TRUE"))
bugs.bcg$EXCLUDE <- as.factor( bugs.bcg$EXCLUDE) # need to refactor to drop 'Yes/No' and retain 'TRUE/FALSE'
summary(bugs.bcg)




# eliminate records without proper OTus (666, NA), as well as missing Unique taxa info
bugs.bcg <- bugs.bcg[bugs.bcg$TaxaID !='666',] # remove 666 taxa
bugs.bcg <- bugs.bcg[!is.na(bugs.bcg$TaxaID),]# remove taxa without OTUs (NA)
bugs.bcg <- bugs.bcg[!is.na(bugs.bcg$EXCLUDE),]# remove sample swithout Unique taxa (Exclude) info, as they will fail all richness metrics



# merge bug data with BCG taxonomy table
colnames(TaxaMaster_Ben_BCG_PacNW)
    
bugs.bcg <- merge(bugs.bcg, TaxaMaster_Ben_BCG_PacNW, by="TaxaID", all.x=TRUE, suffix=c("", ".y"))

colnames(bugs.bcg)

 

    
# bring in slope data and watershed area with COMID and merge with bcg bugs

slope <- read.csv('//deqlab1/GIS_WA/Project_Working_Folders/StreamCat/elevslope.csv') # slope data
slope<-slope[,c('COMID', 'SLOPELENKM')]  
colnames(slope)  

# TRAVIS PRITCHARD 10.2.19: this wasn't working and I couldn't figure out why. I replaces the merge with left_join
#bugs.bcg <- merge(bugs.bcg, slope, by='COMID', all.x=TRUE, suffix=c("", ".y"))
bugs.bcg <- dplyr::left_join(bugs.bcg, slope)


streamcat<- read.csv('//deqlab1/GIS_WA/Project_Working_Folders/StreamCat/StreamCat_all_OR.csv') # NHD watershed areas from StreamCat

colnames(streamcat)
streamcat<-streamcat[, c('COMID', 'WsAreaSqKm')]

#bugs.bcg2<-merge(bugs.bcg, streamcat, by='COMID', all.x=TRUE, suffix=c("", ".y"))
bugs.bcg <-  dplyr::left_join(bugs.bcg, streamcat)

colnames(bugs.bcg)

#convert ws area to mi2
bugs.bcg$Area_mi2 <- bugs.bcg$WsAreaSqKm*0.386102


#### The following code comes from Eric Leppo's R code for calculating the PL-WV BCG
###  SUBSET the bcg data to ensure less than 600

library(knitr)


# Subsample to 600 organisms 


# subsample
library(BioMonTools)
mySize <- 600
Seed.OR <- 18590214
colnames(bugs.bcg)
bugs.mysize <- rarify(inbug=bugs.bcg, sample.ID="SAMPLEID"
                    ,abund="N_TAXA",subsiz=mySize, mySeed=Seed.OR)



            # Compare pre- and post- subsample counts
            df.compare <- merge(bugs.bcg, bugs.mysize, by=c("SAMPLEID", "TaxaID")
                              , suffixes = c("_Orig","_600"))
            df.compare <- df.compare[,c("SAMPLEID", "TaxaID", "N_TAXA_Orig", "N_TAXA_600")]
            #View(df.compare)
            
            # compare totals
            tbl.totals <- aggregate(cbind(N_TAXA_Orig, N_TAXA_600) ~ SAMPLEID, df.compare, sum)
            #View(tbl.totals)
            
            tbl.compare <- head(df.compare)
            tbl.compare.caption <- "First few rows of original and rarified data."
            kable(tbl.compare, caption=tbl.compare.caption)         


            # check to see if rarification worked
            # calculate total abundance from original data and subsampled data
            bcg.totabund <-aggregate(bugs.bcg$N_TAXA, list(SAMPLEID=bugs.bcg$SAMPLEID), sum)
            colnames(bcg.totabund)[colnames(bcg.totabund)=="x"] <- "total.abundance"
            
            bcg.subabund <-aggregate(bugs.mysize$N_TAXA, list(SAMPLEID=bugs.mysize$SAMPLEID), sum)
            colnames(bcg.subabund)[colnames(bcg.subabund)=="x"] <- "total.abundance"
            
            bcg.abund_compare<-merge(bcg.totabund, bcg.subabund, by='SAMPLEID')

### add a column indicating which set of rules to follow: 300ct or 500ct
      # SLH: I am going to choose the 500ct for all samples for two reasons
      #       1) It is way easier to just call out one model
      #       2) our standard methods since 1999 have been a 500ct approach, thus any samples 
      #           with < 500 are due to full sorts, not an artifact
      #    sites prior to 1999 have different field and lab methods, and should be flagged anyway

bugs.mysize$INDEX_NAME <- 'BCG_PacNW_v1_500ct' # BCG_PacNW_v1_500ct
  
 
# add a column for SITE_TYPE, based on NHD slope data



bugs.mysize$SITE_TYPE <- ifelse(bugs.mysize$SLOPELENKM < 1.0, "lo", "hi")
bugs.mysize$SITE_TYPE <- as.factor(bugs.mysize$SITE_TYPE)

colnames(bugs.mysize)

write.csv(bugs.mysize, "A:/Projects/Biomon Redux/BCG_Willy and Puget/R/bugs.mysize_OR.csv")

    # limit to samples Jen Stamp has already run
                # bugs.mysize_limited <- bugs.mysize[bugs.mysize$SAMPLEID=='01103CSR' | bugs.mysize$SAMPLEID=='02087REF' | bugs.mysize$SAMPLEID=='03013CSR' |
                #              bugs.mysize$SAMPLEID=='02087REF' | bugs.mysize$SAMPLEID=='02087REF' | bugs.mysize$SAMPLEID=='02087REF' |
                #              bugs.mysize$SAMPLEID=='03053CSR' | bugs.mysize$SAMPLEID=='03054CSR' | bugs.mysize$SAMPLEID=='06012CSR' |
                #              bugs.mysize$SAMPLEID=='06019CSRw' | bugs.mysize$SAMPLEID=='06021CSR' | bugs.mysize$SAMPLEID=='06022CSR' |
                #              bugs.mysize$SAMPLEID=='06024CSR' | bugs.mysize$SAMPLEID=='06025CSR' | bugs.mysize$SAMPLEID=='06027CSR' |
                #              bugs.mysize$SAMPLEID=='06027CSRp' | bugs.mysize$SAMPLEID=='06028CSR' | bugs.mysize$SAMPLEID=='06029CSR' |
                #              bugs.mysize$SAMPLEID=='06030CSR' | bugs.mysize$SAMPLEID=='06031CSR' | bugs.mysize$SAMPLEID=='06032CSR' |
                #              bugs.mysize$SAMPLEID=='06034CSR' | bugs.mysize$SAMPLEID=='06038CSR' | bugs.mysize$SAMPLEID=='06039CSR' |
                #              bugs.mysize$SAMPLEID=='06042CSR' | bugs.mysize$SAMPLEID=='06043CSR' | bugs.mysize$SAMPLEID=='06045CSR' |
                #              bugs.mysize$SAMPLEID=='06046CSR' | bugs.mysize$SAMPLEID=='06049CSR' | bugs.mysize$SAMPLEID=='06050CSR' |
                #              bugs.mysize$SAMPLEID=='06053CSR' | bugs.mysize$SAMPLEID=='06055CSR' | bugs.mysize$SAMPLEID=='06056CSR' |
                #              bugs.mysize$SAMPLEID=='06057CSR' | bugs.mysize$SAMPLEID=='07006OPR' | bugs.mysize$SAMPLEID=='07008OPR' |
                #              bugs.mysize$SAMPLEID=='07019OPR' | bugs.mysize$SAMPLEID=='07020OPR' ,]
                # write.csv(bugs.mysize_limited, "A:/Projects/Biomon Redux/BCG_Willy and Puget/R/bugs.mysize_OR_limited.csv")


# 1.A. Calculate Metrics
# Extra columns to keep in results
keep.cols <- c("Area_mi2", "SITE_TYPE") #, "SurfaceArea", "Density_m2", "Density_ft2"    # SLH 4.3.19 = added SITE_TYPE
# Run Function

#colnames(bugs.mysize)[colnames(bugs.mysize)=="INDEX_NAME"] <- "INDEX_REGION"
bugs.mysize$INDEX_REGION <- bugs.mysize$SITE_TYPE



df.metrics <- metric.values(bugs.mysize, "bugs", fun.cols2keep = keep.cols)
#2# QC
dim(df.metrics)
View(df.metrics)
colnames(df.metrics)
# Save
write.csv(df.metrics, "A:/Projects/Biomon Redux/BCG_Willy and Puget/R/Metric.Values.Test.csv") #, col.names=TRUE, row.names=FALSE, sep="\t"

# 1.B. Metric Membership
# Import Rules
            
df.rules <- read_excel(system.file("./extdata/Rules.xlsx"
                                  , package="BCGcalc"), sheet="BCG_PacNW_v1_500ct") 



# Run function
df.Metric.Membership <- BCG.Metric.Membership(df.metrics, df.rules)
# Show Results
View(df.Metric.Membership)
summary(df.Metric.Membership)
# Save Results
write.csv(df.Metric.Membership, "A:/Projects/Biomon Redux/BCG_Willy and Puget/R/Metric.Membership.Test.csv")
           

# 1.C. Level Assignment
# Run Function
df.Level.Membership <- BCG.Level.Membership(df.Metric.Membership, df.rules)
# Show results
View(df.Level.Membership)
# Save Results
write.csv(df.Level.Membership, "A:/Projects/Biomon Redux/BCG_Willy and Puget/R/Level.Membership.Test.csv")
           

# 1.D. Level Membership
# Run Function
df.Levels <- BCG.Level.Assignment(df.Level.Membership)

# 1.E. Flags
# Import QC Checks
df.checks <- read_excel(system.file("./extdata/MetricFlags.xlsx"
                                    , package="BCGcalc"), sheet="Flags") 

######################################
### TRAVIS PRITCHARD 10.2.19

### MetricFlags.xlsx bundled in BCGclac package has an error in the INDEX_REGION column
### This fix is in here now until that can get fixed
### Warning message can be ignored:
### attributes are not identical across measure variables; they will be dropped 
################################################################

#df.checks$INDEX_REGION <- "bcg_pacnw_v1"


# qc.checks() from BCGclac does not work properly. Use qc.checks_edited from "\\deqlab1\biomon\R Stats\Bio Tools_Upgrade with R\qc.checks_edited.R" until that gets fixed
# 


# Run Function
df.flags <- qc.checks(df.metrics, df.checks)
# Change terminology; PASS/FAIL to NA/flag
df.flags[,"FLAG"][df.flags[,"FLAG"]=="FAIL"] <- "flag"
df.flags[, "FLAG"][df.flags[,"FLAG"]=="PASS"] <- NA
# long to wide format
df.flags.wide <- dcast(df.flags, SAMPLEID ~ CHECKNAME, value.var="FLAG")
# Calc number of "flag"s by row.
df.flags.wide$NumFlags <- rowSums(df.flags.wide=="flag", na.rm=TRUE)
# Rearrange columns
NumCols <- ncol(df.flags.wide)
df.flags.wide <- df.flags.wide[, c(1, NumCols, 2:(NumCols-1))]
# Merge Levels and Flags
df.Levels.Flags <- merge(df.Levels, df.flags.wide, by="SAMPLEID", all.x=TRUE)
# Show Results
View(df.Levels.Flags)
# Summarize Results
table(df.flags[,"CHECKNAME"], df.flags[,"FLAG"], useNA="ifany")
# Save Results
write.csv(df.Levels.Flags, "A:/Projects/Biomon Redux/BCG_Willy and Puget/R/Levels.Flags.Test.csv")


 
###              
  ##### continuous scoring, based on Level probability    (SLH code--this is not standard application to the BCG)         
###

head(df.Level.Membership )             
df.Level.Membership$bcg.continuous <- df.Level.Membership$L1*1 + df.Level.Membership$L2*2 + df.Level.Membership$L3*3 + 
  df.Level.Membership$L4*4 + df.Level.Membership$L5*5 + df.Level.Membership$L6*6

              
              
# make a small dataframe for merging with final output
colnames(df.Level.Membership)
bcg.scores <- df.Level.Membership[ , c(1, 3, 10)   ]                                      #(df.Level.Membership$SAMPLEID, df.Level.Membership$bcg.continuous), ]
                
                
#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 8: Create SUMMARY BUGS table

          @@@@
         @@  @@
        @@    @@
         @@   @@
          @@ @@
          @@@@@
          @@ @@
         @@   @@
        @@     @@
         @@   @@
          @@@@@
            
#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# 8.1 = combine OE from both models
oe.all<-rbind(oe.wccp, oe.mwcf)
oe.all<-cbind(Sample=rownames(oe.all), oe.all) # no 'Sample' column, causes problems later
                        
                        colnames(oe.mwcf)
                        colnames(oe.nbr)

#need same columns as predictive models to append rows
oe.nbr$O<-NA
oe.nbr$E<-NA
oe.nbr$OoverE<-NA
oe.nbr$BC<-NA
oe.nbr$BC.null<-NA
oe.nbr$outlier.05<-NA
oe.nbr$outlier.01<-NA

oe.all<-rbind(oe.all, oe.nbr)  
oe.all<-arrange(oe.all, Sample)  

# 8.2 = combine abundances and station/sample info back in with O/E scores

metrics<-merge(metrics, tot.abund.RIV, by='Sample', all.x=TRUE, suffix=c('','.y')) 
metrics<-merge(metrics, tot.abund.STR, by='Sample', all.x=TRUE, suffix=c('','.y'))

# combine O/E -> Stress -> mets
oe.stress<-merge(oe.all, stress.scores, by='Sample', all.y=TRUE)
  colnames(oe.stress)
  colnames(metrics)
  #setnames(metrics, old=c('STATION_KEY.x'), new=c('STATION_KEY'))            

oe.stress.mets<-merge(oe.stress, metrics, by=c('Sample'), all.x=TRUE, suffix=c('','.y'))
  colnames(oe.stress.mets)
  colnames(stations.ref)
  colnames(sample.stations)
  colnames(b.samps)  




  

oe.stress.mets<-merge(oe.stress.mets, b.samps, by='Sample', all.x=TRUE, suffix=c('','.y'))  # 
oe.stress.mets<-oe.stress.mets %>%
  select(-STATION_KEY.y)

colnames(oe.stress.mets)


                                # # find duplicate records
                                # z<-duplicated(b.samps, by='Sample')
                                # summary(z)
                                # # find unmatched
                                # oe.abunds.stress[is.na(match(oe.abunds.stress$Sample,oe.abunds$Sample)),]
                                # oe.abunds.stress[!oe.abunds.stress$Sample%in%oe.abunds$Sample,]
                                # 
                                # b.samps[is.na(match(b.samps$Sample,oe.abunds.stress$Sample)),]
                                # b.samps[!b.samps$Sample%in%oe.abunds.stress$Sample,]
                                # 
                                # tot.abund[is.na(match(b.samps$Sample,tot.abund$Sample)),]
                                # b.samps[!b.samps$Sample%in%oe.abunds.stress$Sample,]
                                # 
                                # oe.abunds[is.na(match(oe.abunds$Sample,oe.abunds.stress$Sample)),]
                                # oe.abunds[!oe.abunds$Sample%in%oe.abunds.stress$Sample,]
                                # 

colnames(stations)  
colnames(sample.info) 
colnames(sample.stations)
colnames(sta)

sta<-sample.stations[c('Sample', 'MLocID', 'STATION_KEY', 'Site_name', 'OrganizationID', 'Project',  'long', 'lat',
                        'Eco2', 'Eco3','EcoRegion4', 'wade_boat', 'HUC6_Name', 'HUC8_Name', 'HUC10_Name',
                        'HUC12_Name', 'Methods_ok', 'RefPrF','Dist90F', 'EcoT20_FURR', 'Bug_RefMay', 'PREDATOR reference_model' )] 
 


oe.stress.mets.sta<-merge(oe.stress.mets, sta, by=c('Sample', 'MLocID', 'STATION_KEY'), all.x=TRUE, suffix=c('','.y'))#
    colnames(oe.stress.mets.sta)
    # setnames(oe.stress.mets.sta, old=c( 'Habitat Sampled'), new=c( 'Habitat_sampled'))
    #oe.stress.mets.sta<-oe.stress.mets.sta %>%
    #  select(-c(MLocID.y, STATION_KEY.y))

oe.stress.mets.sta<-unique(oe.stress.mets.sta)                                        
                    
      table(oe.stress.mets.sta$OoverE, oe.stress.mets.sta$Eco2)  


# merge with bcg scores
@@@@         setnames(bcg.scores, old=c('SAMPLEID'), new=c('Sample'))   
@@@@          oe.stress.mets.sta <- merge(oe.stress.mets.sta, bcg.scores, by='Sample', all.x=TRUE, suffix=c('','.y'))


########################
########################
########################
########################

# Step 9: Data Quality Objectives for Integrated Report

        @@@@
       @    @
      @     @
       @    @
        @@@@
           @
          @
         @



      
########################
########################
########################
########################


# 9.1 = low count
oe.stress.mets.sta$low.count<-as.factor(ifelse(oe.stress.mets.sta$tot.abund.RIV < 150, 'YES','NO'))


#9.2 = field and lab methods


          
      # this section no longer used.  Methods now recorded at sample log in and brought in via sample.info table
          # # check for Owner and Project to verify everything covered
          # levels(oe.stress.mets.sta$Project_name)
          # levels (oe.stress.mets.sta$Owner)     
          # 
          # oe.stress.mets.sta$Date<-as.Date(oe.stress.mets.sta$Date, "%m/%d/%Y")
          # 
          # oe.stress.mets.sta$methods.ok<-as.factor(ifelse(oe.stress.mets.sta$Owner == 'Hawkins'| oe.stress.mets.sta$Owner == 'Hafele' | oe.stress.mets.sta$Owner == 'PDX' | oe.stress.mets.sta$Owner == 'Rogue Basin Coordinating Council 2005', 
          #                        'Yes', ifelse(oe.stress.mets.sta$Owner == 'ODEQ' & oe.stress.mets.sta$Date > '1999-01-01', 'Yes', 
          #                                      ifelse(oe.stress.mets.sta$Owner == 'ODEQ' & oe.stress.mets.sta$Date <= '1999-01-01', 'No',
          #                                             ifelse(oe.stress.mets.sta$Owner == 'Yamhill Basin Council', 'No', 
          #                                                    ifelse(oe.stress.mets.sta$Owner == 'WA_Ecology', 'WA', "-999")
          #                                             )))))

          
        
          
          
          # 
          # #  IF there are samples as '-999', the ifelse code above should be adapted to include them 
          # 
          table(oe.stress.mets.sta$Methods_ok)
          table(oe.stress.mets.sta$Organization_ID, oe.stress.mets.sta$Methods_ok)  
          table(oe.stress.mets.sta$Project_name, oe.stress.mets.sta$Methods_ok) # use this to look by Project to see where they fall in 
          #   
          #   # check to see if certain samples, Projects, Owners are coming thru
          #   oe.stress.mets.sta[oe.stress.mets.sta$Owner=='ODEQ',]
          #   oe.stress.mets.sta[oe.stress.mets.sta$Sample=='12001MSTP',]


# 9.3 = Outlier analysis
oe.stress.mets.sta$OE.outlier<-as.factor(ifelse(oe.stress.mets.sta$outlier.01 == 1, 'Yes', 'No'))

            table(oe.stress.mets.sta$OE.outlier)
            table(oe.stress.mets.sta$Project_name, oe.stress.mets.sta$OE.outlier)  

            
# 9.4 = Index period (date range)

library(lubridate)

oe.stress.mets.sta$month<-month(oe.stress.mets.sta$Date)
oe.stress.mets.sta$month[oe.stress.mets.sta$month == 'NA'] <- -999
table(oe.stress.mets.sta$month)

oe.stress.mets.sta$index.period<-as.factor(ifelse(oe.stress.mets.sta$month == 6, 'Yes', 
                                                  ifelse(oe.stress.mets.sta$month == 7, 'Yes',
                                                    ifelse(oe.stress.mets.sta$month == 8, 'Yes',
                                                      ifelse(oe.stress.mets.sta$month == 9, 'Yes',
                                                        ifelse(oe.stress.mets.sta$month == 10, 'Yes', 'No'))))))
                                                               
                                                               
                                                               
                                                               
                                   

    table(oe.stress.mets.sta$index.period, oe.stress.mets.sta$month)
    summary(oe.stress.mets.sta$index.period)
            

# 9.5 = FINAL Biocriteria/Integrated Report filter:
oe.stress.mets.sta$Use.303d<- ifelse(oe.stress.mets.sta$low.count == 'NO' & oe.stress.mets.sta$Habitat_sampled == 'R' 
                                        & oe.stress.mets.sta$Methods_ok == 'Yes' & oe.stress.mets.sta$OE.outlier == 'No' 
                                            & oe.stress.mets.sta$wade_boat == 'wadeable'& oe.stress.mets.sta$index.period == 'Yes', 
                                                'Yes', 'No')
                                     
                                     
                                          



table(oe.stress.mets.sta$Use.303d)                              

      

library(plyr)
            # ???
            colnames(oe.stress.mets.sta)
            head(oe.stress.mets.sta[,c(2,12,15,20,34:38)])
            
            # check to see that screens are doing what they are supposed to do.  
            ddply(oe.stress.mets.sta, .(low.count, Habitat_sampled, Methods_ok, OE.outlier, wade_boat), summarize, Use303d = max(Use.303d)) 
                      # only the following combination should pass 'Yes':
                      #           low.count = 'No'
                      #           Habitat = 'R'
                      #           methods.ok = 'Yes'
                      #           OE.outlier = 'No'
                      #           wadeable = 'wadeable'         
                      #                    
            
            oe.stress.mets.sta[oe.stress.mets.sta$Project_name=='Yamhill Basin',]

      
            


# 9.6 = create a column of why failed

low.count<-as.factor(ifelse(oe.stress.mets.sta$tot.abund.RIV < 150, 'lowcount',''))

methods<-as.factor(ifelse(oe.stress.mets.sta$Methods_ok == 'Yes', '', 'methods'))
                                                                       

outlier<-as.factor(ifelse(oe.stress.mets.sta$outlier.01 == 1, 'outlier', ''))


habitat<-as.factor(ifelse(oe.stress.mets.sta$Habitat == 'R', '', 'habitat'))

wadeable<-ifelse(oe.stress.mets.sta$wade_boat == 'wadeable','', 'non.wadeable')

index<-ifelse(oe.stress.mets.sta$index.period == 'Yes', '', 'index')

oe.stress.mets.sta$reason.no.303<-as.factor(paste(low.count, methods, outlier, habitat, wadeable, index, sep=' '))

summary(oe.stress.mets.sta$reason.no.303)


#oe.stress.mets.sta<-arrange(oe.stress.mets.sta, Sample) 
#setnames(oe.stress.mets.sta, old=c('Habitat Sampled'), new=c('Habitat_sampled'))

##############
##############
##############
##############

  @    @@@@@@
 @@   @@    @@ 
@ @  @@      @@ 
  @  @@      @@ #  Step 10: Create final data files
  @  @@      @@ 
  @   @@    @@
@@@@@  @@@@@@ 
  
  
  ##############
##############
##############
##############   


# 10.1 = create data files for SUMMARY_BUGS_R: results + metadata


colnames(oe.stress.mets.sta)  
                #oe.stress.mets.sta.ref <- oe.stress.mets.sta[ , -c(95)]     # drop duplicated columns

# change order of columns
oe.stress.mets.sta<-oe.stress.mets.sta[,c("Sample", "MLocID", "STATION_KEY", "Date", "Habitat_sampled", "Field_QA", 
        "Lab_QA", "Increment_Field", "Increment_Lab", "Site_name", "OrganizationID", "Project", "lat", "long", "Eco3", "Eco2", "EcoRegion4",
        "RefPrF", "Dist90F", "EcoT20_FURR", "Bug_RefMay", "PREDATOR reference_model",
        "O", "E", "OoverE", "Onull", "Enull", "OoverE.null", "BC", "BC.null", "outlier.05", "outlier.01", "oe.cond", "TS", "BSTI", 
        "total.richness", "Coleoptera.rich", "Diptera.rich", "Ephemeroptera.rich", 
        "Plecoptera.rich", "Trichoptera.rich", "Baetidae.rich", "Chironomidae.rich", "Hydropsychidae.rich", 
        "EPT.rich", "pct_Coleoptera", "pct_Diptera", "pct_Ephemeroptera", "pct_Plecoptera",
        "pct_Trichoptera", "pct_EPT", "multivoltine.rich", "semivoltine.rich", "univoltine.rich", 
        "pct_multivoltine", "pct_semivoltine", "pct_univoltine", "CF.rich", "CG.rich", "MH.rich", "OM.rich", 
        "PA.rich", "PH.rich", "PR.rich", "SC.rich", "SH.rich", "pct_CF", "pct_CG", "pct_MH", "pct_OM", 
        "pct_PA", "pct_PH", "pct_PR", "pct_SC", "pct_SH", "pct_Dom.5", "pct_Dom.3", "pct_Dom.1", 
        "Shannon_diversity", "Simpson_diversity", "Evenness", "Non.Insect_richness", "pct_Non.Insect", 
        "MTI", "tot.abund.RIV", "total.abundance.STR",  "wade_boat", "HUC6_Name", "HUC8_Name", 
        "HUC10_Name", "HUC12_Name",  "low.count", "Methods_ok", "OE.outlier", "month", "index.period", 
        "Use.303d", "reason.no.303")]  # "bcg.continuous",


  

# change column names so that they can be imported into Access
setnames(oe.stress.mets.sta, old=c("OoverE.null", "BC.null", "outlier.05", "outlier.01", 
                         "oe.cond", "total.richness", "Coleoptera.rich","Diptera.rich", "Ephemeroptera.rich", "Plecoptera.rich", 
                         "Trichoptera.rich", "Baetidae.rich", "Chironomidae.rich", "Hydropsychidae.rich", "EPT.rich", 
                         "multivoltine.rich", "semivoltine.rich", "univoltine.rich", "CF.rich", "CG.rich", "MH.rich", "OM.rich", 
                         "PA.rich", "PH.rich", "PR.rich", "SC.rich", "SH.rich", "pct_Dom.5", "pct_Dom.3", "pct_Dom.1", 
                         "Non.Insect_richness", "pct_Non.Insect", "tot.abund.RIV", "total.abundance.STR", "low.count", 
                         "OE.outlier", "index.period", "Use.303d", "reason.no.303"), 
         new=c("OoverE_null", "BC_null", "outlier_05", "outlier_01", 
               "oe_cond", "total_richness", "Coleoptera_rich","Diptera_rich", "Ephemeroptera_rich", "Plecoptera_rich", 
               "Trichoptera_rich", "Baetidae_rich", "Chironomidae_rich", "Hydropsychidae_rich", "EPT_rich", 
               "multivoltine_rich", "semivoltine_rich", "univoltine_rich", "CF_rich", "CG_rich", "MH_rich", "OM_rich", 
               "PA_rich", "PH_rich", "PR_rich", "SC_rich", "SH_rich", "pct_Dom5", "pct_Dom3", "pct_Dom1", 
               "NonInsect_richness", "pct_NonInsect", "tot_abund_RIV", "tot_abund_STR", "low_count", 
               "OE_outlier", "index_period","Use_303d", "reason_no303"))



# create a data dictionary 
variables<-c("Sample", "MLocID", "STATION_KEY", "Date", "Habitat_sampled", "Field_QA", 
             "Lab_QA", "Increment_Field", "Increment_Lab", "Site_name", "OrganizationID", "Project", "lat", "long", "Eco3", "Eco2", "EcoRegion4",
             "RefPrF", "Dist90F", "EcoT20_FURR", "Bug_RefMay", "PREDATOR reference_model",
             "O", "E", "OoverE", "Onull", "Enull", "OoverE.null", "BC", "BC.null", "outlier.05", "outlier.01", "oe.cond", "TS", "BSTI", 
             "total.richness", "Coleoptera.rich", "Diptera.rich", "Ephemeroptera.rich", 
             "Plecoptera.rich", "Trichoptera.rich", "Baetidae.rich", "Chironomidae.rich", "Hydropsychidae.rich", 
             "EPT.rich", "pct_Coleoptera", "pct_Diptera", "pct_Ephemeroptera", "pct_Plecoptera",
             "pct_Trichoptera", "pct_EPT", "multivoltine.rich", "semivoltine.rich", "univoltine.rich", 
             "pct_multivoltine", "pct_semivoltine", "pct_univoltine", "CF.rich", "CG.rich", "MH.rich", "OM.rich", 
             "PA.rich", "PH.rich", "PR.rich", "SC.rich", "SH.rich", "pct_CF", "pct_CG", "pct_MH", "pct_OM", 
             "pct_PA", "pct_PH", "pct_PR", "pct_SC", "pct_SH", "pct_Dom.5", "pct_Dom.3", "pct_Dom.1", 
             "Shannon_diversity", "Simpson_diversity", "Evenness", "Non.Insect_richness", "pct_Non.Insect", 
             "MTI", "tot.abund.RIV", "total.abundance.STR", "wade_boat", "HUC6_Name", "HUC8_Name", 
             "HUC10_Name", "HUC12_Name",  "low.count", "Methods_ok", "OE.outlier", "month", "index.period", 
             "Use.303d", "reason.no.303")   #  "bcg.continuous", 
  
  
definitions<-c("unique sample ID", "unique site identifier","OLD unique site ID", "sample collection date", 
    "type of habitat sample collected from: R=riffle, P=pool, G=glide, T=transect, O=other", 
    "field QA type: S=single sample, FP=first of series of duplicates, FD=second (or more) of a series of duplicates", 
    "lab QA type: S=single sample, CP=first of series of lab splits, FD=second (or more) of a series of lab splits", 
    "sequential # of field samples", "sequential # of lab samples", "descriptive name of sampling location", 
    "entity in charge of sampling", "project sampling associated with", "field latitude", "field longitude", 
    "Level III Omernik ecoregion", "Level II Omernik ecoregion", "Level IV Omernik ecoregion", 
    "reference desingation from Miller et al. 2016 methods (Western Oregon only)", "Most disturbed (trashed) designation from Miller et al. 2016 methods", 
    "Reference designation from Drake 2004 methods (whole state)", "Reference sites used to construct PREDATOR models (May 2005)", "which PREDATOR model was the site used in",
    "RIVPACS Observed taxa", "RIVPACS Expected taxa", "ratio of Observed to Expected", "RIVPACS null model Observed taxa", 
    "RIVPACS null model Expected model", "O/E from null model", "Bray Curtis similarity Index (Van Sickle 2008)", 
    "null model BC similarity index", "RIVPACS/BC outlier test @ p=0.05", "RIVPACS/BC outlier test @ p=0.01", 
    "condition associated with appropriate O/E model (NBR & SRP ecoregions =null)", 
    "macroinvertebrate inferred 7-day average seasonal max temperature (oC)", 
    "Biological Sediemnt Tolerance Index: macroinvertebrate inferred % fines", "# of unique taxa", "# unique Coleoptera taxa",
    "# unique Diptera taxa", "# unique Ephemeroptera taxa", "# unique Plecoptera taxa", "# unique Trichoptera taxa",   
    "# unique Baetidae taxa", "# unique Chironomidae taxa", "# unique Hydropsychidae taxa", "# unique EPT taxa", 
    "% abundance of Coleoptera", "% abundance of Diptera", "% abundance of Ephemeroptera", "% abundance of Plecoptera",
    "% abundance of Trichoptera", "% abundance of EPT", "short-lived taxa: # of unique taxa with multiple broods per year", 
    "long-lived taxa: # of unique taxa that take more than one year to complete their life cycle ", 
    "# of unique taxa that have one brood per year", "% abundance of short-lived (multivoltine) taxa", 
    "% abundance of long-lived (semivoltine) taxa","% abundance of univoltine taxa", "# of unique Collector Filterer FFG taxa", 
    "# of unique Collector Gatherer FFG taxa", "# of unique Macrophyte Herbivore FFG taxa", "# of unique Omnivore FFG taxa", 
    "# of unique Parasite FFG taxa", "# of unique Piercer Herbivore FFG taxa", "# of unique Predator FFG taxa", 
    "# of unique Scraper FFG taxa", "# of unique Shredder FFG taxa", "% abundance of Collector Filterers", 
    "% abundance of Collector Gatherers", "% abundance of Macrophyte Herbivores", "% abundance of Omnivores", 
    "% abundance of Parasites", "% abundance of Piercer Herbivores", "% abundance of Predators", "% abundance of Scrapers",
    "% abundance of Shredders", "% abundance of 5 most abundant taxa", "% abundance of 3 most abundant taxa", 
    "% abundance of 1 most abundant taxa", "Shannon_diversity: sum(relative abundance x log(relative abundance))",   
    "Simpson_diversity: 1 - sum(relative abundance^2)", "Evenness: Shannon_diversity/(log(total.richness))", 
    "# of unique Non Insect taxa", "% abundance of Non Insect taxa", "Montana Metals Tolerance Index (McGuire)","total abundance following subsampling for RIVPACS", 
    "total abudnance used in Stressor Models (temp and fines)", "sampled by wading or boat protocols",
    "Basin (HUC 6)", "Sub-Basin (HUC 8)", "Watershed (HUC 10)", "Sub-Watershed (HUC 12)", 
    "total abundance for RIVPACS model < 150", "sampling methods and program verified to fit all DEQ requirements for models", 
    "outlier for O/E model", "sample collection month", "was the sample collected within the proper index period?",
    "is the sample appropriate for assessing biocriteria and 303d reporting (based on all QA/QC objectives)", 
    "why is the sample not appropriate for 303d assessment?") #"2016 Miller et al: interagency agreed upon reference sites",
                                                              # "Willamette Valley/Puget Lowlands BCG scores--continuous scale", 

# combine variables and definitions into a data frame for the excel file
mets.meta<-as.data.frame(cbind(variables, definitions))


write.csv(oe.stress.mets.sta, '//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/OE_Stress_Metrics_data quality.csv')
write.csv(mets.meta, '//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/Bug metrics_dictionary.csv')





#                                       library(xlsx)
#                                       # Create a new workbook for outputs
#                                       OE_Stress_Metrics_data.quality <- createWorkbook()
#                                       # Create a sheet in workbook for each table
#                                       sheet1 <- createSheet(OE_Stress_Metrics_data.quality, sheetName = "Macroinvertbrate Results")
#                                       sheet2 <- createSheet(OE_Stress_Metrics_data.quality, sheetName = "Metadata")
#                                       # Add the tables calculated above to the sheet
#                                       addDataFrame(oe.stress.mets.sta, sheet1)
#                                       addDataFrame(mets.meta, sheet2)
#                                       
#                                       saveWorkbook(OE_Stress_Metrics_data.quality, "//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/OE_Stress_Metrics_data quality.xlsx") 


#                 #bring in results from a model run outside of this code, but in the stand-alone model
#                 mwcf09_12<-read.csv('//deqlab1/Biomon/Personal_Folders/Shannon/data requests/EPA/Gretchen/2015/09-12_MWCF_OEscores_23feb15.csv')
#                   
#                 wccp09_12<-read.csv('//deqlab1/Biomon/Personal_Folders/Shannon/data requests/EPA/Gretchen/2015/09-12_WCCP_OEscores_23feb15.csv')
#                 
#                 oe.09_12<-rbind(mwcf09_12, wccp09_12, deparse.level = 1)
#                 
#                 oe.09_12<-oe.09_12[,c(1,4)]
#                 
#                 oe.09_12.comp<-merge(oe.09_12, oe.abunds.stress.sta, by=c('Sample'), all.x=TRUE)
#                 
#                 
#                 oe.09_12.low<-[oe.09_12.comp$total.abundance < 300,]
#                 oe.09_12.low <- oe.09_12.comp[ which(oe.09_12.comp$total.abundance<300 ), ]
#                 
#                 oe.09_12.low$oe.diff <-oe.09_12.low$OoverE.x - oe.09_12.low$OoverE.y
#                 # differences are due to rounding errors...all 0.00xxxx


                                
                                      
                                      
##############
##############
##############
##############

        @      @
       @@     @@
      @ @    @ @
        @      @                #  Step 10: Write final SUMMARY BUGS back into Biomon_Phoenix.mdb
        @      @
        @      @
      @@@@@  @@@@@@ 
                          

##############
##############
##############
##############

channel <- odbcConnectAccess("A:/Databases/Biomon_Phoenix_for SQL migration_FINAL.mdb") # connect to database
sqlDrop(channel, sqtable="SUMMARY_BUGS_R", errors = TRUE) # delete old version of Summary Bugs
#add Summary Bugs table to Biomon Phoenix
sqlSave(channel, dat=oe.stress.mets.sta, tablename = "SUMMARY_BUGS_R", append = FALSE, rownames = FALSE, colnames = FALSE, 
        varTypes = c(Date="Date"))





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
##############
##############
##############
##############
  
            
            ### = query out stations with BOTH riffle and transect samples  =  examine habitat effects on O/E, TS, and BSTI
@@@@@
  @@@@@ not working down from here.  Looks like wrong column order and necessary columns dropped
  @@@@@
  @@@@@
  
            tran.scores<-sum.bugs[sum.bugs$HabitatSampled == 'T', ] # pull out only T samples
            tran.scores<-tran.scores[c(1:149), ]                    # cut out NA's
            tran.stations<-tran.scores$STATION_KEY 
            tran.stations<-tran.scores[,1]                          # create a vector of stations
            t.r.scores<- subset(sum.bugs, STATION_KEY %in% tran.stations)   #query out all T and R samples, but limit to only Stations with T samples
            t.r.scores<-t.r.scores[,c(1,2,19, 20, 5, 8,13,  17, 18, 14, 15, 16, 26, 27 )]
            
            
            # t.r.scores<-t.r.scores[duplicated(t.r.scores$STATION_KEY),]   # doesn't do what I want...
            t.r.scores <- t.r.scores[order(t.r.scores$STATION_KEY, t.r.scores$HabitatSampled),]

            a<-ddply(t.r.scores, .(STATION_KEY, Date), summarize, trans = sum(HabitatSampled == 'T'), riff = sum(HabitatSampled == 'R')      ) 
            
            a<-a[a$trans>0 & a$riff>0, ]  #which Station/Dates have paired riffle and transect samples?  = 128 STATION/DATES
            
            t.r.scores_matched<-merge(t.r.scores, a, by=c('STATION_KEY', 'Date')) #256 records, from 128 Station/Dates

            hab.emap<-read.csv('//deqlab1/biomon/Projects/Biomon Redux/EMAP_W phabbest.csv')
            colnames(hab.emap)
            setnames(hab.emap, old=c("SVN", "DATE"), new=c("Sample", "Date"))
            
            str(hab.emap)
            hab.emap$Date<-as.Date(hab.emap$Date, format = "%m/%d/%Y")
            
            hist(hab.emap$PCT_FN, breaks = 50, xlim=c(0,100))
            
            
                        t.r.scores_hab<-merge(t.r.scores_matched, hab.emap, by=c('STATION_KEY', 'Date'), all.x=TRUE)
            colnames(t.r.scores_hab)
            attach(t.r.scores_hab)
            plot(OoverE~XSLOPE, pch=as.character(HabitatSampled), xlim=c(0,10))
            plot(OoverE~XSLOPE, pch=as.character(HabitatSampled), xlim=c(0,10))
            
            splom(t.r.scores_hab[c(5,8,9,21,22,24,26, 27)], 
                  main="ggfg")
            
            qplot(hp, mpg, data=mtcars, shape=am, color=am, 
                  facets=gear~cyl, size=I(3),
                  xlab="Horsepower", ylab="Miles per Gallon") 
            
            qplot(PCT_FAST, OoverE, data=t.r.scores_hab, color=HabitatSampled) 
            qplot(XSLOPE, OoverE, data=t.r.scores_hab, color=HabitatSampled) 
            qplot(Map_Slope, OoverE, data=t.r.scores_hab, color=HabitatSampled)
            qplot(ELEV_m, OoverE, data=t.r.scores_hab, color=HabitatSampled)
            qplot(Precip_mm, OoverE, data=t.r.scores_hab, color=HabitatSampled)
            qplot(Temp_CX10, OoverE, data=t.r.scores_hab, color=HabitatSampled)
            
            
            
            
            # O/E
            z<- t.r.scores_hab[,c(1,2,4,5)]
            z.cast <- dcast(z, STATION_KEY+Date ~ HabitatSampled  ) 
            z.cast$resid.oe<-z.cast$R - z.cast$T
            
            # TS
            y<- t.r.scores_hab[,c(1,2,4,8)]
            y.cast <- dcast(y, STATION_KEY+Date ~ HabitatSampled  ) 
            y.cast$resid.ts<-y.cast$R - y.cast$T
            
            # BSTI
            x<- t.r.scores_hab[,c(1,2,4,9)]
            x.cast <- dcast(x, STATION_KEY+Date ~ HabitatSampled  ) 
            x.cast$resid.BSTI<-x.cast$R - x.cast$T
            
            summary(z.cast$resid.oe)
            summary(y.cast$resid.ts)
            summary(x.cast$resid.BSTI)
            
            par(mfrow=c(1,3))
            hist(z.cast$resid.oe, breaks=20)
            hist(y.cast$resid.ts, breaks=20)
            hist(x.cast$resid.BSTI, breaks=20)
            
            boxplot(z.cast$resid.oe, ylab='resid.oe (R - T)', main='O/E')
            abline(h=0, col = 'red')
            boxplot(y.cast$resid.ts, ylab='resid.BSTI (R - T)', main='TS')
            abline(h=0, col = 'red')
            boxplot(x.cast$resid.BSTI, ylab='resid.ts (R - T)', main='BSTI')
            abline(h=0, col = 'red')
            
            
            
            
            z.cast<-merge(z.cast, y.cast, by =c('STATION_KEY', 'Date'))
            z.cast<-merge(z.cast, x.cast, by =c('STATION_KEY', 'Date'))
            z.cast_hab<-merge(z.cast, hab.emap, by='STATION_KEY', all.x=TRUE )
            colnames(z.cast_hab)
            
            
            # O/E residuals vs natural gradients
            p1<-qplot(PCT_FAST, resid.oe, data=z.cast_hab) 
            p2<-qplot(XSLOPE, resid.oe, data=z.cast_hab) 
            p3<-qplot(Map_Slope, resid.oe, data=z.cast_hab) 
            p4<-qplot(ELEV_m, resid.oe, data=z.cast_hab) 
            p5<-qplot(Precip_mm, resid.oe, data=z.cast_hab) 
            p6<-qplot(Temp_CX10, resid.oe, data=z.cast_hab) 
            grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
            
            
            # TS residuals vs natural gradients
            p1<-qplot(PCT_FAST, resid.ts, data=z.cast_hab) 
            p2<-qplot(XSLOPE, resid.ts, data=z.cast_hab) 
            p3<-qplot(Map_Slope, resid.ts, data=z.cast_hab) 
            p4<-qplot(ELEV_m, resid.ts, data=z.cast_hab) 
            p5<-qplot(Precip_mm, resid.ts, data=z.cast_hab) 
            p6<-qplot(Temp_CX10, resid.ts, data=z.cast_hab) 
            grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
            
            # BSTI residuals vs natural gradients
            p1<-qplot(PCT_FAST, resid.BSTI, data=z.cast_hab) 
            p2<-qplot(XSLOPE, resid.BSTI, data=z.cast_hab) 
            p3<-qplot(Map_Slope, resid.BSTI, data=z.cast_hab) 
            p4<-qplot(ELEV_m, resid.BSTI, data=z.cast_hab) 
            p5<-qplot(Precip_mm, resid.BSTI, data=z.cast_hab) 
            p6<-qplot(Temp_CX10, resid.BSTI, data=z.cast_hab) 
            grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
            
            
            qplot(PCT_FN, resid.BSTI, data=z.cast_hab) 
            qplot(PCT_FN, BSTI, data=t.r.scores_hab, color=HabitatSampled) 

#################################################################################################################
###################################################################################################################
###########################################################################################################################
##################################################################################################################################
########################################################################################################################################




@@@@@@@@@@@
  FUTURE WORK: 
  
  @@@@
  @@@@@@@ = how to get oe.NBR.null into this???
@@@@
  
  
  combine O/E and Stressor ID outputs from each model, into a single data.frame  ----> = 'SUMMARY_BUGS' table

  effects of rarify vs. rarify.seed

  bring in Stressor ID scores

  roll O/E, TS, BSTI into single d.f

  assign condition status 

  export back into SUMMARY_BUGS table in Biomon_Phoenix  --> actually, replace table with each new run

  calculate typical metrics (e.g., %abund, richness, %richness, tolerant, FFG, etc )

  Make individual components separate functions to source in, so front end is clean and easy 
      1) query in data from Biomon_Phoenix
      2) pull data and export for Stresor ID
          ---------------------------------------> eventually, create new models and run everything directly in R
      3) run PREDATOR
      4) calculate metrics
      5) export results out to Biomon_Phoenix



