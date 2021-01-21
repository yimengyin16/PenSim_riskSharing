#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

# Labels for run names 
run_labels_FR75 <- c(
	baseline           = "Baseline",
	cola_return        = "Contingent COLA: \nreturn",
	cola_returnSmooth  = "Contingent COLA: \nreturn smoothed",
	cola_returnSmooth_calib  = "Contingent COLA: \nreturn smoothed\ncalib",
	cola_returnSmooth_calib2 = "Contingent COLA: \nreturn smoothed\ncalib2",
	cola_returnSmooth_calib2_s10 = "Contingent COLA: \nreturn smoothed\ncalib2_s10",
	cola_FR            = "Contingent COLA: \nFunded ratio threshold",
	cola_FR_calib      = "Contingent COLA: \nFunded ratio threshold\ncalib",
	cola_FRramp        = "Contingent COLA: \nFunded ratio ramp",
	cola_SDRS          = "SDRS fast repayment",
	cola_SDRS_noCAeec  = "SDRS fast repayment,\nno CA EEC",
	EEC_return         = "Contingent EEC: \nReturn",
	EEC_returnSmooth   = "Contingent EEC: \nReturn smoothed",
	EEC_FR             = "Contingent EEC: \nFunded ratio",
	EEC_FR_t100        = "Contingent EEC: \nFunded ratio \n100% threshold",
	EEC_sharedADCfloor = "Contingent EEC: \nShared ADC\no EEC cap",
	EEC_sharedADCcap   = "Contingent EEC: \nShared ADC\10% EEC cap",
	EEC_sharedADC      = "Contingent EEC: \nShared ADC 0 floor",
	hybrid_DB          = "hybrid_DB",
	hybrid_DB_noLegacy = "hybrid_DB_no legacy cost",
	mixed_WRS          = "WRS-like",
	DA                 = "conditional indexation\nno assetSmoothing",
	DA_as              = "conditional indexation",
	DA_as_none         = "conditional indexation \n none",
	
	DA_as_act          = "conditional indexation \n actives only",
	DA_as_ret          = "conditional indexation \n retirees only"
	
	# EEC_sharedNC  = "Contingent EEC: \nshared",
)

run_levels_FR75 <- names(run_labels_FR75)
run_levels_FR75


run_labels_FR100 <- c(
	baseline_FR100      = "Baseline \nYear-1 funded ratio 100%",
	cola_return_FR100   = "Contingent COLA: \nreturn\nYear-1 funded ratio 100%",
	
	cola_returnSmooth_calib_FR100 = "Contingent COLA: \nreturn smoothed\ncalib FR 100%",
	cola_returnSmooth_calib2_FR100 = "Contingent COLA: \nreturn smoothed\ncalib FR 100%",
	
	cola_FR_FR100       = "Contingent COLA: \nFunded ratio threshold\nYear-1 funded ratio 100%",
	cola_FR_calib_FR100 = "Contingent COLA: \nFunded ratio threshold\nYear-1 funded ratio 100%\ncalib",
	cola_FRramp_FR100   = "Contingent COLA: \nFunded ratio ramp\nYear-1 funded ratio 100%",
	cola_SDRS_FR100     = "SDRS fast repayment\nYear-1 funded ratio 100%",
	cola_SDRS_noCAeec_FR100 = "SDRS fast repayment,\nno CA EEC\n funded ratio 100%",
	EEC_return_FR100    = "Contingent EEC: \nReturn\nYear-1 funded ratio 100%",
	EEC_FR_FR100        = "Contingent EEC: \nFunded ratio\nYear-1 funded ratio 100%",
	EEC_sharedADCfloor_FR100 = "Contingent EEC: \nShared ADC\nYear-1 funded ratio 100%",
	hybrid_DB_FR100     = "hybrid_DB\nYear-1 funded ratio 100%",
	mixed_WRS_FR100     = "WRS-like funded ratio 100%",
	DA_as_FR100              = "conditional indexation,  funded ratio 100%"
	# DA_as_none_FR100         = "conditional indexation \n none,  funded ratio 100%",
	# DA_as_act_FR100          = "conditional indexation \n actives only,  funded ratio 100%",
	# DA_as_ret_FR100          = "conditional indexation \n retirees only,  funded ratio 100%"
	# DA_FR100                  = "conditional indexation \funded ratio 100%"
	# EEC_sharedNC  = "Contingent EEC: \nshared",
)




run_levels_FR100 <- names(run_labels_FR100)
run_levels_FR100

run_labels_all <- c(run_labels_FR75, run_labels_FR100)
run_levels_all <- c(run_levels_FR75, run_levels_FR100)


df_run <- tibble(
	runname       = run_levels_all,
	runname_label = run_labels_all
)



# Loading results
results_all <- get_results(dir_modelResults) %>% 
	filter(runname %in% names(run_labels_all)) %>% 
	select(runname, sim, year, everything()) %>% 
	mutate(runname_wlabel =  factor(runname, levels = run_levels_all, labels = run_labels_all),
				 runname = factor(runname, levels = run_levels_all),
				 #ERC_PR = ERC / salary,
				 #ERC2   = NC.ER + SC, # For SDRS policy analysis only
				 #ERC2_PR = ERC2 / salary
				 PR = ifelse(str_detect(runname, "DA"), PR, EEC/EEC_PR),
				 SC_legacy = na2zero(SC_legacy)
	) 

#results_all %>% head
# results_all$runname %>% unique
#results_all %>% select(runname, sim, year, ERC, SC_legacy)




#*******************************************************************************
##                      Create additional variables                         ####
#*******************************************************************************

# Add DC contributions for hybrid plans

results_all %<>% 
	mutate(EEC_DB = EEC,
				 ERC_DB = ERC,
				 C_DB   = EEC_DB + ERC_DB, 
				 
				 EEC_DC = ifelse(str_detect(runname, "hybrid_DB"), salary * DC_EECrate, 0),
				 ERC_DC = ifelse(str_detect(runname, "hybrid_DB"), salary * DC_ERCrate, 0),
				 C_DC   = EEC_DC + ERC_DC, 
				 
				 EEC = EEC_DB + EEC_DC,
				 ERC = ERC_DB + ERC_DC,
				 
				 # adding legacy SC, currently only applied to hybrid plans
				 ERC = ERC + SC_legacy,
				 
				 C = EEC + ERC,
				 
				 ERC_PR =  ifelse(str_detect(runname, "DA"), ERC/PR, ERC / salary),
				 EEC_PR =  ifelse(str_detect(runname, "DA"), EEC/PR, EEC / salary)
	)

# results_all %>% filter(runname %in% c("baseline", "hybrid_DB"), sim == 0, year <=20) %>% 
# select(runname, year, C_DB, C_DC)

