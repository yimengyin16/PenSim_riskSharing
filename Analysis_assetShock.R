

#*******************************************************************************
##                              Notes                                       ####
#*******************************************************************************

# This script analyze the asset-shock scenario.  




#*******************************************************************************
##                            Global settings                               ####
#*******************************************************************************

## Loading packages
source("libraries.R")

# dir_modelResults <- "Outputs/"
dir_modelResults <- "Outputs_500sims/"
dir_outputs      <- "Outputs_Analysis/"


dr    <- 0.075 # discount rate
infl  <- 0.02  # assumed inflation rate
Nyear <- 40    # Number of sim years for contribution analysis
cola_baseline <- 0.015

DC_EECrate <- 0.03
DC_ERCrate <- 0.03


#*******************************************************************************
##                           Loading results                               ####
#*******************************************************************************

source("Analysis_loadingResults.R")


#*******************************************************************************
##              Measures: Employer and employee contribution                ####
#*******************************************************************************

***********************************************************




