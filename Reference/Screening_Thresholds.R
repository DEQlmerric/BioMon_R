### determine the percentiles of reference screening metrics to get reference screening thresholds

require(tidyverse)
require(readxl)

# This is a screening metrics for all DEQ and ODFW biomon sites - this should be 2514 delins 
gis_mets <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_PNW.xlsx", 
                       sheet = "Ref_Screen_Metrics_PNW")

### this calculates the statewide percentiles for GIS screening metrics ### 
DEQ_percentiles_Statewide <- gis_mets %>% 
  select("rdden_km_km2","xings_km2","P_AgLand","P_Urban21Land","mines","grvl_mn_km2","P_canal") %>%
  sapply(quantile, probs = c(0.01,0.1,0.25,0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.85, 0.9,0.95,0.99), na.rm = TRUE) %>%
  as.data.frame()
write.csv(DEQ_percentiles_Statewide,"Reference/DEQ_percentiles_Statewide.csv")

# use the percentile output for ref/trashed thresholds within the 'Fun_RefScreen' code
