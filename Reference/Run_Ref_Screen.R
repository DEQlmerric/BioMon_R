# Authors: Lesley Merrick and Shannon Hubler

# objective:  1) Run the function "ref_gis.screen" from 'Fun_RefScreem.R' 
#             2) Take GIS screen metric outputs and classifies stations as GIS candidate reference sites, or trashed (highest disturbances)






library(readxl)

# update file location based on the sites you are screening - this should be an export from the GIS tool 
# this file must have a column MlocID and sites should be in the stations database 
gis_mets <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_PNW.xlsx", 
                       sheet = "Ref_Screen_Metrics_PNW")


source('Reference/Fun_RefScreen.R')
ref_gis.screen(gis_mets)







#############

#  USU sites

############



matt <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_USU_Matt.xlsx", 
                       sheet = "RefScreen_Matt")


adam <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_USU_Adam.xlsx", 
                   sheet = "Ref_Screen_Metrics_USU_Adam")


gis_mets_USU <- matt %>%
  rbind(adam) 
gis_mets_USU<- rename(gis_mets_USU, EcoRegion3 = US_L3CODE)


source('Reference/Fun_RefScreen_Non-DEQ.R')


ref_gis.screen.nonDEQ(gis_mets_USU)

