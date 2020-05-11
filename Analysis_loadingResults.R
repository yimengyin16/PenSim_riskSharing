#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

# Labels for run names 
run_labels <- c(
	baseline = "Baseline",
	
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
	filter(runname %in% names(run_labels)) %>% 
	select(runname, sim, year, everything()) %>% 
	mutate(runname_wlabel =  factor(runname, levels = run_levels, labels = run_labels),
				 runname = factor(runname, levels = run_levels),
				 #ERC_PR = ERC / salary,
				 #ERC2   = NC.ER + SC, # For SDRS policy analysis only
				 #ERC2_PR = ERC2 / salary
				 PR = EEC/EEC_PR
	) 

results_all %>% head
results_all$runname %>% unique






#*******************************************************************************
##                      Create additional variables                         ####
#*******************************************************************************

# Add DC contributions for hybrid plans

results_all %<>% 
	mutate(EEC_DB = EEC,
				 ERC_DB = ERC,
				 C_DB   = EEC_DB + ERC_DB, 
				 
				 EEC_DC = ifelse(runname == "hybrid_DB", salary * DC_EECrate, 0),
				 ERC_DC = ifelse(runname == "hybrid_DB", salary * DC_ERCrate, 0),
				 C_DC   = EEC_DC + ERC_DC, 
				 
				 EEC = EEC_DB + EEC_DC,
				 ERC = ERC_DB + ERC_DC,
				 
				 C = EEC + ERC,
				 
				 ERC_PR = ERC / salary,
				 EEC_PR = EEC / salary
	)

# results_all %>% filter(runname %in% c("baseline", "hybrid_DB"), sim == 0, year <=20) %>% 
# select(runname, year, C_DB, C_DC)

