
  
# Google Earth Reference Screens: FINAL Scoring

# 1) bring in GE Screen results.
# 2) Re-format for calculations
# 3) Calculate final site sum scores

library(reshape2)
require(tidyverse)
library(readxl)
# bring in data
alt.usu <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/GE_USU Ref_Screen_ALT.xlsx',
                      sheet='USU_ALT_2020')
alt.usu_qc <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/GE_USU Ref_Screen_ALT.xlsx',
                      sheet='USU_ALL_2020')



mbs.usu <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/GE_USU_REF_Screen_MBS_FIXED.xlsx',
                      sheet='USU_MBS_2020')
mbs.usu_qc <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/GE_USU_REF_Screen_MBS_FIXED.xlsx',
                         sheet='USU_ALL_2020')



slh.usu <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/GE_USU_Ref_Screen_SLH.xlsx',
                      sheet='USU_SLH_2020')
slh.usu_qc <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/GE_USU_Ref_Screen_SLH.xlsx',
                         sheet='USU_ALL_2020')





# merge data frames together

screens <- rbind(mbs.usu, mbs.usu_qc, alt.usu, alt.usu_qc, slh.usu, slh.usu_qc )
colnames(screens)[colnames(screens)=="Sample date"] <- "Sample.date"

# use reshape2 to make df go from wide to long (fewer columns)


screens_long <- melt(screens,
        # ID variables - all the variables to keep but not split apart on
    id.vars=c("Agency_ID", "Sample.date", "Type", "Scorer", "Timestamp"),
        # The source columns
    measure.vars=c("ag_crops", "ag_cafo",	"ag_grazing",	"ag_stockponds",	
      "ag_orchardplantation",	"built_impermeable",	"built_permeable",
      "built_residence",	"built_sewagetreatment",	"log_under5",	"log_6toMA",
      "log_overMA",	"log_landslide",	"roads",	"railroads",	"powerlines",
      "rec_hiking",	"rec_campground",	"rec_parks",	"rec_hatchery",	"hydro_dam",
      "hydro_channelization",	"hydro_riprapdikes"),
        # Name of the destination column that will identify the original
        # column that the measurement came from
    variable.name="disturbance", 
    value.name="value"
)


### List of QC sites 

qc_sites <- as.vector(screens$Agency_ID[screens$QC=='Y'])
qc_sites <-unique(qc_sites)

# long format does a good job, but not sure how to calculate across two rows
# what if cast back to wide format, but only by putting Extent and Proximity as columns?
# then concatenate E & P text strings into a new column,
# followed by ifelse statements for text strings: AA = 0, AL=1, AM=3, etc...

screens_wide <- dcast(screens_long, Agency_ID + Sample.date + Scorer + disturbance + Timestamp ~ Type, value.var=unique("value"))
# if get error meassage 'defaulting to length' then need to fix non-unique records across grouping variables


 
screens_wide <- screens_wide %>%
  mutate(E.P = case_when(
    E == 'A' & P == 'A' ~ 0,
    E == 'A' & P == 'L' ~ 1,
    E == 'A' & P == 'M' ~ 3,
    E == 'A' & P == 'H' ~ 10,
    E == 'L' & P == 'A' ~ 1,
    E == 'L' & P == 'L' ~ 1,
    E == 'L' & P == 'M' ~ 3,
    E == 'L' & P == 'H' ~ 10,
    E == 'M' & P == 'A' ~ 3,
    E == 'M' & P == 'L' ~ 3,
    E == 'M' & P == 'M' ~ 10,
    E == 'M' & P == 'H' ~ 10,
    E == 'H' & P == 'A' ~ 10,
    E == 'H' & P == 'L' ~ 10,
    E == 'H' & P == 'M' ~ 10,
    E == 'H' & P == 'H' ~ 10,
     TRUE ~ 999)) 




# control for max of 3 for hiking stress               
screens_wide <- screens_wide %>%        
    mutate(E.P_adj.hike = case_when(disturbance == 'rec_hiking' & E.P == 10 ~ 3, 
                                TRUE ~ E.P))    
 
 
 
 
         

#summarize the data

GE_Site_sum.scores <- screens_wide %>%
  #define what makes each roup unique
  group_by(Agency_ID, Sample.date, Scorer, Timestamp ) %>%
  #summarise the stats per group
  summarize(Disturb.score = sum(E.P_adj.hike))
  
# get station and BPJ alone, with distinct values
screen.bpj <- screens %>%
  select(Agency_ID, Sample.date, Scorer, Timestamp, Scorer_BPJ)

screen.bpj <- distinct(screen.bpj) # remove duplicates


# merge BPJ into scores           
GE_Site_sum.scores <- GE_Site_sum.scores  %>% 
   left_join(screen.bpj, by = c('Agency_ID', 'Sample.date', 'Scorer', 'Timestamp' )) 




##########
##########
##
# visualize and explore the final scores
##
#########
##########

hist(GE_Site_sum.scores$Disturb.score, breaks =150, xlim=c(0,100))

ggplot(GE_Site_sum.scores, aes(Disturb.score)) +
  geom_freqpoly(bins = 50) + xlim(0,25) +
  facet_wrap(~Scorer_BPJ)

ggplot(GE_Site_sum.scores, aes(x=Scorer_BPJ, y = Disturb.score)) +
  geom_boxplot() + ylim(0,50) +
  facet_wrap(~Scorer)

ggplot(GE_Site_sum.scores, aes(x=Scorer, y = Disturb.score)) +
  geom_boxplot() + ylim(0,50) +
  facet_wrap(~Scorer_BPJ)

# make a table of scores by BPJ category
with(GE_Site_sum.scores, table(Disturb.score, Scorer_BPJ)) 



# compare QC site scores: 20 or so sites, all scored by Adam/Matt/Shannon for comparisons of consistency


qc <- GE_Site_sum.scores %>% 
  filter(Agency_ID %in% qc_sites)

ggplot(qc, aes(x=Scorer, y=Disturb.score))+
  geom_boxplot() + ylim(0,50) +
  facet_wrap(~Scorer_BPJ)

ggplot(qc, aes(y=Agency_ID, x=Disturb.score, colour = Scorer)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.0,), aes(shape=Scorer_BPJ, size = 2)) + xlim(0,30)+
  theme(axis.text.x = element_text(angle = 0))




# select individual sites for review

      # qc.site <- screens_wide[screens_wide$Agency_ID=='XN-SS-4120',]
      # view(qc.site)




# Adam's code - pull out BPJ Y and dist score >15 and BPJ N and dist score <15
bpj_y_above<-subset(GE_Site_sum.scores, Scorer_BPJ=="Y" & Disturb.score >= 15)

bpj_n_below<-subset(GE_Site_sum.scores, Scorer_BPJ=="N" & Disturb.score < 15)

bpj_question<-subset(GE_Site_sum.scores, Scorer_BPJ=="?"  | Scorer_BPJ == "N?")

          bpj.checks.usu <-rbind(bpj_n_below, bpj_question)
          write.csv(bpj.checks.usu, '//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/bpj.checks.usu.csv')

          
#########
#
#
#    Combine Scores + BPJ for FINAL REF STATUS
#
#
#########

# first, get single scores for a site on any given sample date 
GE_Site_sum.scores_ave <- GE_Site_sum.scores %>%
  #define what makes each roup unique
  group_by(Agency_ID, Sample.date) %>%
  #summarise the stats per group
  summarize(mean(Disturb.score))



 # check for duplicates
GE_Site_sum.scores_ave_repeats<- GE_Site_sum.scores_ave%>%
 group_by(Agency_ID)%>%
  filter(n()>1) 

#
  # visually look at each set of duplicates, for each station
  # if there are no significant changes in scores, thru time, then simply average across all dates
  # if there are significant changes in scores, it will require dropping certain dates from the ref pool
#

# second, no significant score changes observed, average all samples per station
GE_Site_sum.scores_ave <- GE_Site_sum.scores %>%
  #define what makes each roup unique
  group_by(Agency_ID) %>%
  #summarise the stats per group
  summarize(Disturb.score = mean(Disturb.score))



# combine single disturb scores per station, with final BPJ status
bpj.final <- read.csv('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/FINAL_BPJ_2021_USU.csv')

bpj.final <- bpj.final%>%
  select(Agency_ID, BPJ_final)

GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave  %>% 
   left_join(bpj.final, by = c('Agency_ID')) 



GE_Site_sum.scores_ave_bpj[c("BPJ_final")][is.na(GE_Site_sum.scores_ave_bpj[c("BPJ_final")])] <- "Score"

GE_Site_sum.scores_ave_bpj$BPJ_final<-as.character(GE_Site_sum.scores_ave_bpj$BPJ_final)

GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave_bpj  %>% 
   mutate(Ref2020_FINAL = case_when(BPJ_final == 'Score' & Disturb.score < 15 ~ 'YES',
                                   BPJ_final == 'Score' & Disturb.score >= 15 ~ 'NO',
                                   BPJ_final == 'Y'  ~ 'YES',
                                   BPJ_final == 'N'  ~ 'NO',
                                   TRUE ~ 'WhatchuTalkinBoutWillis'))

# change Agency_ID to 'MLocID' to match with stations table
colnames(GE_Site_sum.scores_ave_bpj)[which(names(GE_Site_sum.scores_ave_bpj) == "Agency_ID")] <- "MLocID"



###########

# bring in station level data --USU sites not in Stations dbase, so need to get from GIS screens dataframe                                   

###########

                              #' require(RODBC)
                              #' 
                              #' #connect to view as a general user 
                              #' sta.sql = odbcConnect('Stations')
                              #' #pull in stations table
                              #' stations = sqlFetch(sta.sql, "VWStationsFinal") 
                              #' odbcClose(sta.sql)
                              #' 
                              #' 
                              #' stations <- stations %>%
                              #'   select(MLocID, StationDes, Lat_DD, Long_DD, EcoRegion3)
                              #' 
                              #' 
                              #' # merge with stations
                              #' GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave_bpj %>%
                              #'   left_join(stations, by = 'MLocID')
                              #' 
                              #' @@@
                              #'   @@@ The USU sites dont match with STATIONS, leaving NAs for most records
                              #' @@@


 




# still missing site level info, merge back with GIS data file
gis.usu <- read_excel('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE_Screens/USU_all/USU_Ref_Screen.xlsx',
                      sheet='ref_screen')

gis.usu <- gis.usu %>%
  select(MLocID, lat, long, location,  EcoRegion3)

library(data.table)
setnames(gis.usu, old=c('lat', 'long', 'location'), 
         new=c( 'Lat_DD', 'Long_DD', 'StationDes'))

GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave_bpj %>%
  select(MLocID, Disturb.score, BPJ_final, Ref2020_FINAL) # this drops out DEQ station info....

GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave_bpj %>%
  left_join(gis.usu, by = c('MLocID'))


# create a table of USU reference sites only, and export to final outputs folder
ref2020.sites_FINAL <- GE_Site_sum.scores_ave_bpj[GE_Site_sum.scores_ave_bpj$Ref2020_FINAL=='YES',]

write.csv(ref2020.sites_FINAL, '//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/_Final outputs/REF.sites.only_2020_FINAL_usu.csv')


# look at ref population

with(ref2020.sites_FINAL, table(Ref2020_FINAL))
with(ref2020.sites_FINAL, table(Ref2020_FINAL, EcoRegion3))


ref2020.sites_by_eco <- with(ref2020.sites_FINAL, table(Ref2020_FINAL, EcoRegion3))
write.csv(ref2020.sites_by_eco, '//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/_Final outputs/REF.2020_FINAL_by_ecoregion_USU.csv')




# export GE_Site_sum.scores_ave_bpj  --> this needs to be linked to GIS screen results (in "final ref tables.R')
write.csv(GE_Site_sum.scores_ave_bpj, 'Reference/GE_Site_sum.scores_ave_bpj_USU.csv')

