#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

# Labels for run names 
run_labels_FR75 <- c(
	baseline = "Baseline",
	cola_return        = "Contingent COLA: \nreturn",
	cola_FR            = "Contingent COLA: \nFunded ratio threshold",
	cola_FRramp        = "Contingent COLA: \nFunded ratio ramp",
	cola_SDRS          = "SDRS fast repayment",
	EEC_sharedADCfloor = "Contingent EEC: \nShared ADC",
	EEC_return         = "Contingent EEC: \nReturn",
	EEC_FR             = "Contingent EEC: \nFunded ratio",
	hybrid_DB          = "hybrid_DB"
	# EEC_sharedNC  = "Contingent EEC: \nshared",
)

run_levels_FR75 <- names(run_labels_FR75)
run_levels_FR75


run_labels_FR100 <- c(
	baseline_FR100      = "Baseline \nYear-1 funded ratio 100%",
	cola_return_FR100   = "Contingent COLA: \nreturn\nYear-1 funded ratio 100%",
	cola_FR_FR100       = "Contingent COLA: \nFunded ratio threshold\nYear-1 funded ratio 100%",
	cola_FRramp_FR100   = "Contingent COLA: \nFunded ratio ramp\nYear-1 funded ratio 100%",
	cola_SDRS_FR100     = "SDRS fast repayment\nYear-1 funded ratio 100%",
	EEC_sharedADCfloor_FR100 = "Contingent EEC: \nShared ADC\nYear-1 funded ratio 100%",
	EEC_return_FR100    = "Contingent EEC: \nReturn\nYear-1 funded ratio 100%",
	EEC_FR_FR100        = "Contingent EEC: \nFunded ratio\nYear-1 funded ratio 100%",
	hybrid_DB_FR100     = "hybrid_DB\nYear-1 funded ratio 100%"
	# EEC_sharedNC  = "Contingent EEC: \nshared",
)

run_levels_FR100 <- names(run_labels_FR100)
run_levels_FR100

run_labels_all <- c(run_labels_FR75, run_labels_FR100)
run_levels_all <- c(run_levels_FR75, run_levels_FR100)




# Loading results
results_all <- get_results(dir_modelResults) %>% 
	filter(runname %in% names(run_labels_all)) %>% 
	select(runname, sim, year, everything()) %>% 
	mutate(runname_wlabel =  factor(runname, levels = run_levels_all, labels = run_labels_all),
				 runname = factor(runname, levels = run_levels_all),
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

