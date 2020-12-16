roads <- read.csv("C:\\Users\\lmerric\\Desktop\\GIS\\Reference\\roads_compare.csv")

ref_roads <- roads %>% 
             mutate(rdden_km_k = (rd_km/Area_km2),
                    rdden_km_k_BLM = (rd_km_BLM/Area_km2),
                    rd_Status = case_when(rdden_km_k <= 1.325 ~ 1,   # 25th
                                          rdden_km_k >=4.039 ~ 2,    # 90th
                                          TRUE ~ 0),
                     rd_Status_BLM = case_when(rdden_km_k_BLM <= 1.325 ~ 1,   # 25th
                                               rdden_km_k_BLM >=4.039 ~ 2,    # 90th
                                               TRUE ~ 0),
                     status_match = if_else(rd_Status == rd_Status_BLM,"Yes","No")) %>%
              #filter(status_match == 'No') %>%
              left_join(stations, by = c('MLocId' = 'MLocID')) %>%
              select(MLocId,rd_km,rd_km_BLM,rdden_km_k,rdden_km_k_BLM,rd_Status,rd_Status_BLM,
              status_match,ReferenceSite)

write.csv(ref_roads,"C:\\Users\\lmerric\\Desktop\\GIS\\Reference\\roadslayers_analysis.csv")
