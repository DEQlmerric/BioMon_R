### Run this to screen for candidate reference sites ### 

# update file location based on the sites you are screening - this should be an export from the GIS tool 
# this file must have a column MlocID and sites should be in the stations database 
gis_mets <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_PNW.xlsx", 
                       sheet = "Ref_Screen_Metrics_PNW")



matt <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_USU_Matt.xlsx", 
                       sheet = "RefScreen_Matt")


adam <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_USU_Adam.xlsx", 
                   sheet = "Ref_Screen_Metrics_USU_Adam")

#adam <- rename(adam, EcoRegion3 = US_L3CODE)
                            # adam <- adam %>%
                            #   select( "OBJECTID", "MLocID", "Area_km2", "rd_km", "rdden_km_km2", "total_strm_km", "xings", "xings_km2", "canal_km", 
                            #           "P_canal", "mines", "mines_km2", "grvl_mn", "grvl_mn_km2", "Ag_km2", "P_AgLand", "Urban21_km2", "P_Urban21Land")

gis_mets <- matt %>%
  rbind(adam) 
gis_mets<- rename(gis_mets, EcoRegion3 = US_L3CODE)


ref_screen.nonDEQ(gis_mets)

