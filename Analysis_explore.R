

#*******************************************************************************
##                              Notes                                       ####
#*******************************************************************************

# Research question



#' Analysis of EEC policies 


#' Analysis of cola policies 
#'  - distribution of 40-year cola 


#' Analysis of employer cost and cost uncertainty



#' Analysis of benefit level and benefit uncertainty 



#' Lifetime analysis for employees (intergenerational equity)





#*******************************************************************************
##                            Global settings                               ####
#*******************************************************************************

## Loading packages
source("libraries.R")

dir_modelResults <- "Outputs/"
dir_outputs      <- "Outputs_Analysis/"



#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

# Labels for run names 
run_labels <- c(
	cola_baseline = "Baseline",
	
	cola_return   = "Contingent COLA: \nreturn",
	cola_FR       = "Contingent COLA: \nFunded ratio threshold",
	cola_FRramp   = "Contingent COLA: \nFunded ratio ramp",
	cola_SDRS     = "SDRS fast repayment",
	
	EEC_sharedADC = "Contingent EEC: \nShared ADC",
	EEC_return    = "Contingent EEC: \nReturn",
	EEC_FR        = "Contingent EEC: \nFunded ratio",
	
	hybrid_DB     = "hybrid_DB"
	# EEC_sharedNC  = "Contingent EEC: \nshared",
)

run_levels <- names(run_labels)
run_levels


# Loading results
results_all <- get_results(dir_modelResults) %>% 
	select(runname, sim, year, everything()) %>% 
	mutate(runname_wlabel =  factor(runname, levels = run_levels, labels = run_labels),
				 runname = factor(runname, levels = run_levels)
				 #ERC_PR = ERC / salary,
				 #ERC2   = NC.ER + SC, # For SDRS policy analysis only
				 #ERC2_PR = ERC2 / salary
				 ) 
results_all %>% head
results_all$runname %>% unique



#*******************************************************************************
##                EEC polici                                                  ####
#*******************************************************************************






