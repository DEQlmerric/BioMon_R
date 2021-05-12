#
# Author: Shannon Hubler
# 12.15.2020

# Adapted from Step 8 in Bio Tools R upgrade_v3.4_noBCG.

# Purpose: pull togethe rall bug analyses into a single table
  # metrics
  # Stressor
  # PREDATOR
  


#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 1: Create SUMMARY BUGS table


  
  #########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# 1.1 = combine OE from both models
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

# 1.2 = combine abundances and station/sample info back in with O/E scores

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






oe.stress.mets<-merge(oe.stress.mets, b.samps.sta, by='Sample', all.x=TRUE, suffix=c('','.y'))  # 


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

