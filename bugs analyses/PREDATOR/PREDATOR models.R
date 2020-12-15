#
# Author: Shannon Hubler

# 12/14/2020

# Purpose: create stand alone script for running 2005 version RIVPACS models (PREDATOR)
      # 1) Data formating for input files
      # 2) Run PREDATOR models: MWCF, WCCP
      # 3) Run NBR null model
 

library(devtools)

#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################



# Step 1: Make PREDATOR input files



#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


#####
#####
#       1.1  Subset for PREDATOR
######
######

# first, get counts from Taxon/Taxon_Code ----> OTUs (no ambuiguous taxa)

b.oe<-aggregate(b_t_s$Count,  list(Sample=b_t_s$Sample, OTU=b_t_s$OTU_RIV05_Nov05, MLocID=b_t_s$MLocID,  STATION_KEY=b_t_s$STATION_KEY, 
                                 Eco2=b_t_s$Eco2, Eco3=b_t_s$Eco3), sum )# For each OTU in a Sample, sum the Counts

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
#       1.2 rarify to 300 count
######
######

# rarify to 300 count per sample --> this standardizes 'effort' across all samples 
# since O/E is basically 'reference taxa richness', it is highly related to total count


# load the 'reshape2' package and source in the 'rarify' script for subsampling to 300

source('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/rarify_w_seed.r')

@@@@
  @@@@@
  @@@@@
    @@@@@@@@@@  Would be better to get off of the DEQ netork and source in directly form github.....???
  @@@@
  @@@@
@@@@
  
source_url('https://github.com/DEQlmerric/BioMon_R.git/rarify_w_seed.R')


@@@@@@  doesnt work.....?????????????????????



#source('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/rarify.r')

# Input bug files from Access tables (Steps 1-3) are '3-column' format and NOT-SUBSAMPLED to 300 count 
# To subsample and turn into a site*taxa matrix, execute the following:


# all bug data - rarify (subsample to 300), and matrify (turn into Sample x Taxa matrix)
# rarify does not affect samples with OTU counts less than 300


b.rare.seed <- rarify.seed(na.omit(b.oe), 'Sample', 'Count', 300) 
#b.rare <- rarify(na.omit(b.oe), 'Sample', 'Count', 300) #original rarify code from JVS, without a seed set.  Use to compare rarify.seed results




#####
#####
#       1.3 get total abundance of rarified samples for PREDATOR
######
######


tot.abund.RIV<-aggregate(b.rare.seed$Count, list(Sample=b.rare.seed$Sample, STATION_KEY=b.rare.seed$STATION_KEY), sum)
names(tot.abund.RIV)[3]<-'tot.abund.RIV'


#####
#####
#       1.4 Matrify: convert from long format (many rows per sample), to site X species matrix (one row per sample)
######
######


dm.rare<- melt(b.rare.seed, measure.var = 'Count') 
bugs.all <- dcast(dm.rare, Sample+STATION_KEY+Eco2+Eco3 ~ OTU, fun.aggregate = sum) # Sample x OTU doesn't seem to be unique so you need the fun.aggregate.  
head(bugs.all)




#####
#####
#       1.5 subset the data for each PREDATOR model
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
#       1.6  Create data.frame for NBR Null Model 
######
######

# samples from Eco3 = '80' and Eco3 = '12' 
bugs.NBR<-subset(bugs.all, Eco3== 80 | Eco3 ==12 )
dim(bugs.NBR)
table(bugs.NBR$Eco2, bugs.NBR$Eco3) # All samples must be from WIBR -- no Eco3 should be from '10'






###
#     1.7 create predictor files to match the bugs.model files
###

@@@@@@
  @@@@@@@@@
  @@@@@@@@
  @@@@@@@@@@@@@@@@@@@@ NEED TO FIX COLUMN INFO BELOW--based on old PHX structure....needs to match AWQMS
  @@@@@@@@
  @@@@@@@@
@@@@@
# get single records of Samples (SVN) and Stations (STATION_KEY)
b.samps<-unique(b_t_s[,c('Sample', 'MLocID', 'STATION_KEY', 'Date', 'Habitat_sampled', 'Field_QA', 'Lab_QA', 'Increment_Field', 'Increment_Lab')])    # use unique records so that they can be matched with predictors from Station table


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



# Step 2: Run PREDATOR = Making predictions for new (all) data.



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

# 2.1 MWCF model

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

# 2.2  WCCP model

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



# Step 3: Assess NBR samples with Null model



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




