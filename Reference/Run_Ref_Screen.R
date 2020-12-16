### Run this to screen for candidate reference sites ### 

# update file location based on the sites you are screening - this should be an export from the GIS tool 
# this file must have a column MlocID and sites should be in the stations database 
gis_mets <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_PNW.xlsx", 
                       sheet = "Ref_Screen_Metrics_PNW")
