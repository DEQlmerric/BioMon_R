# Authors: Shannon Hubler, Lesley Merrick

# Initial Code: 6.4.2021

# Purpose: Provide a front end for users to source and run functions that will:
#     1) Query AWQMS to find which uploaded raw bug data samples DO NOT have corresponding summary data
#     2) Query out the "missing summary" raw data, from AWQMS.  Link to Stations and Taxonomy tables.
#     3) Calculate summary metrics
#     4) Calculate PREDATOR O/E scores and supporting outputs
      5) Calculate Stressor ID models
              ........NO FUNCTION EXISTS.........
              # export "missing samples" as csv, 
              # import into C2 (third party software)
              # calculate both stressor models
              # import results back into R and make scale changes
#     6) Create a SUMMARY TABLE for upload to AWQMS
              
              

              
              
              
              
# 
source('bugs analyses/data/Data_from_AWQMS.R')
data.raw.AWQMS()
              
              

# run bug metrics function
source('bugs analyses/metrics/bug metrics.R')
bug.metrics(b_t_s)

# run PREDATOR function
source('bugs analyses/PREDATOR/PREDATOR models.R')
bug.PREDATOR(b_t_s)
