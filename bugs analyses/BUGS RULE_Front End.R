# Authors: Shannon Hubler, Lesley Merrick

# Initial Code: 6.4.2021

# Purpose: Provide a front end for users to source and run functions that will:
#     1) Query AWQMS to find which uploaded raw bug data samples DO NOT have corresponding summary data
#     2) Query out the "missing summary" raw data, from AWQMS.  Link to Stations and Taxonomy tables.
#     3) Calculate summary metrics
#     4) Calculate PREDATOR O/E scores and supporting outputs
#     5) Calculate Stressor ID models
#             ........NO FUNCTION EXISTS.........
              # export "missing samples" as csv, 
              # import into C2 (third party software)
              # calculate both stressor models
              # import results back into R and make scale changes
#     6) Create a SUMMARY TABLE for upload to AWQMS
              
              

              
# @@@  PLACEHOLDER: Lesleys "missing summary" query           
              
              
# run data query from AWQMS
source('bugs analyses/data/Data_from_AWQMS.R')
b_t_s <- data.raw.AWQMS()
              
              

# run bug metrics function
source('bugs analyses/metrics/bug metrics.R')
metrics <- bug.metrics(b_t_s)

# run PREDATOR function
source('bugs analyses/PREDATOR/PREDATOR models.R')
model_outputs <- bug.PREDATOR(b_t_s)

#unpack the model output list
oe.mwcf <- model_outputs[["oe.mwcf"]]
oe.wccp <- model_outputs[["oe.wccp"]]
oe.nbr <- model_outputs[["oe.nbr"]]