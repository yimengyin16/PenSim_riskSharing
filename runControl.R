#*******************************************************************************
#                           ### Initialization ####                      
#*******************************************************************************

## Cleaning up memory
rm(list = ls())
suppressMessages(gc())


## Loading packages
source("libraries.R")


## Setting folder paths
# dir_Outputs <- "Outputs_500sims/"
dir_Outputs <- "Outputs/"

#******************************************************************************* 
#                          ## Notes on contingent COLA
#*******************************************************************************

# The model assumes that continget COLA is implemented from year 2, and
# and there had been a constant cola = cola_baseline up to year 1.

# In year t, the year t-1 values of conditioning variables (FR, return) are used
# to determine the contingent cola in year t. 

# In the model, any increase or decrease in AL caused by contingent COLA 
# are treated as a result of "benefit change", and the increases/decreases are 
# amortized. Model consistency is checked.


# Questions for comparative analysis

# What to compare? Cost, contribuiton volatility, risk measure?
# What constant cola should be used in the comparison of constant cola vs. contingent cola?


# COLA contingent on Return. 
  # Example: Maryland ERPS, TRS
   # Return assumption met: capped at 2.5%
   # return assumtpion not met: cappted at 1%


# COLA contingent on funded ratio
  # Example: Arizona
   # If funded ratio >= 70%: 1%~2% COLA; otherwise, 0 cola 
  # Example: Minnesota GERF
   # 1% base cola. 
   # FR >= 90% for two consecutive years: cola increases to 2.5%
   # FR < 85% for two consecutive years or FR < 80 for 1 year, COLA decreases to 1%
  # Example: Montana
   # COLA capped at 1.5%;
   # reduced by 0.1% for every 2% less than 90%.  


#*******************************************************************************
#                         # Model Parameters  ####
#*******************************************************************************
folder_run <- "."
filename_RunControl <- dir(folder_run, pattern = "^RunControl")
path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)

# Import global parameters
runList <- read_excel(path_RunControl, sheet="paramlist", skip  = 1) %>% filter(!is.na(runname), include == TRUE)
runList

# # Import return scenarios
# returnScenarios <- read_excel(path_RunControl, sheet="returns", skip = 0) %>% filter(!is.na(scenario))

# Import global parameters
Global_paramlist <- read_excel(path_RunControl, sheet="Global_paramlist") %>% filter(!is.na(nyear)) %>% 
	as.list


#*******************************************************************************
#                        ####  Run Model ####
#*******************************************************************************

for(runName in runList$runname ){
   
   suppressMessages(gc())
	# runName <- runList$runname

	cat(runName, "\n")
	paramlist <- get_parmsList(runList, runName)
	paramlist$seed <- 1234 # For generating investment returns
	paramlist$v <- 1/(1 + paramlist$i)
	# Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
	# Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)

	
	source("Model_sim_DB(4).R")
	outputs_list <- list(paramlist        = paramlist,
											 Global_paramlist = Global_paramlist,
											 results          = penSim_DB_results)

	save(outputs_list, file = paste0(dir_Outputs, "Outputs_", runName, ".RData"))
}









