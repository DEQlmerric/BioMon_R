

# We Updated 2020 GIS metric thresholds
# this means all of the DEQ watersheds we scored in mid 2020, + new sites in CA/ID were re-run to verify if GIS-candidates

# this resulted in an increase in GIS ref candidates--thus we need to figure out which sites haven't been verified by Google Earth screens


H:\Project_Working_Folders\Reference\2020\GE Screens\Screen results

ref_June2020 <- read.csv('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/GE Screens/Screen results/REF.2020_FINAL.csv')
ref_June2020 <- ref_June2020 %>%
  select(MLocID, Ref2020_FINAL)
ref_June2020 <- rename(ref_June2020, Ref_June = Ref2020_FINAL)


ref_screen.Dec2020 <- read.csv('C:\\git\\BioMon_R\\Reference\\ref_screen.csv')
ref_screen.Dec2020 <-ref_screen.Dec2020[,-1]


colnames(ref_June2020)
colnames(ref_screen.Dec2020)


ref_screen.Dec2020 <- ref_screen.Dec2020 %>%
  left_join(ref_June2020, by=c('MLocId'= 'MLocID'))
ref_screen.Dec2020$Ref_June <- as.factor(ref_screen.Dec2020$Ref_June )

missing.GE <-filter(ref_screen.Dec2020, ref.status_2020.yn=='Y'& is.na(Ref_June))

                    
