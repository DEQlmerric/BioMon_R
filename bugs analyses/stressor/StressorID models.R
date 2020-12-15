#
# Author: Shannon Hubler

# 12/15/2020

# Purpose: Current version = 2005 Weighted Averaging Models, for use with C2 software
      # 1) Prepare data for running Stressor ID models
      # 2) Export data for use in C2 software
      # 3) Import Stressor models results

# 
# 
#



# ##
# ####
# #  1.0 = Subset for STRESSOR
# ###
# ##
# 
# # 1.1 = first get counts from taxa to OTUs (no ambigous taxa)
stress.bugs<-aggregate(b_t_s$Count,  list(Sample=b_t_s$Sample, OTU_Stress05=b_t_s$OTU_Stress05), sum) # For each OTU in a Sample, sum the Counts
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
  
  