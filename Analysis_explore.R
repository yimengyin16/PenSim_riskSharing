

#*******************************************************************************
##                              Notes                                       ####
#*******************************************************************************

# Notes on measures
#  https://docs.google.com/document/d/1oi2UEfcrsk6359-zbJTb8DamtbpoEGUFLzcY19njwUI/edit




#*******************************************************************************
##                            Global settings                               ####
#*******************************************************************************

## Loading packages
source("libraries.R")

dir_modelResults <- "Outputs_90y/"
dir_outputs      <- "Outputs_Analysis/"

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



dr    <- 0.075
infl  <- 0.02 
Nyear <- 40 

#*******************************************************************************
##              Measures: Employer and employee contribution                ####
#*******************************************************************************

## Measures:
#  1. Percentiles of PV employer contributions at year 1
#  2. Max increase in ERC rate in 5 years
#  3. measure of "sim-to-sim" comparison


## PV ERC and EEC 

results_stch <- 
  results_all %>% 
	filter(sim >= 1, year <= Nyear) 


df_PVC <- 
  results_stch %>% 
	select(runname, runname_wlabel, sim, year, policy_type, ERC, EEC, UAAL) %>% 
	group_by(runname, sim) %>% 
	summarise(
		        runname_wlabel = unique(runname_wlabel),
						
		        # PV of ERC
		        ERC_PV         = sum(ERC / (1 + dr)^(year - 1)),
						ERCwUAAL_PV    = sum(ERC / (1 + dr)^(year - 1)) + UAAL[year == max(year)] / (1 + unique(dr))^(max(year) - 1),
						
						# PV of EEC
						EEC_PV         = sum(EEC / (1 + dr)^(year - 1))
						)

df_PVC_qtiles <-
	df_PVC %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						
						ERC_PV_q90 = quantile(ERC_PV, 0.90),
						ERC_PV_q75 = quantile(ERC_PV, 0.75),
						ERC_PV_q50 = quantile(ERC_PV, 0.50),
						ERC_PV_q25 = quantile(ERC_PV, 0.25),
						ERC_PV_q10 = quantile(ERC_PV, 0.1),
						
						ERCwUAAL_PV_q90 = quantile(ERCwUAAL_PV, 0.90),
						ERCwUAAL_PV_q75 = quantile(ERCwUAAL_PV, 0.75),
						ERCwUAAL_PV_q50 = quantile(ERCwUAAL_PV, 0.50),
						ERCwUAAL_PV_q25 = quantile(ERCwUAAL_PV, 0.25),
						ERCwUAAL_PV_q10 = quantile(ERCwUAAL_PV, 0.1),
						
						EEC_PV_q90 = quantile(EEC_PV, 0.90),
						EEC_PV_q75 = quantile(EEC_PV, 0.75),
						EEC_PV_q50 = quantile(EEC_PV, 0.50),
						EEC_PV_q25 = quantile(EEC_PV, 0.25),
						EEC_PV_q10 = quantile(EEC_PV, 0.1)
						)

df_PVC_qtiles

## Maximum increase in ERC rate WITHIN 5 years

# Note:
#  - Need to take into account the possibility that the changes in less then 5 years
#    are greater than the 5-year change. The method below takes care of this. 

get_nyearMax <- function(x) max(x[-1] - x[1])
get_nyearMin <- function(x) min(x[-1] - x[1])
# x <- filter(results_stch, sim ==1, runname == "cola_baseline") %>% pull("ERC_PR")
# zoo::rollapply(x, width = 6, get_nyearMax, fill = NA, align = "right")

df_ERCMaxChg <- 
	results_stch %>% 
	select(runname, runname_wlabel, sim, year, policy_type, ERC_PR) %>%
	group_by(runname, sim) %>% 
	mutate(   ERC_PR_chg5y    = rollapply(ERC_PR, width = 6, get_nyearMax, fill = NA, align = "right")) %>% # zoo::rollapply
	summarise(runname_wlabel = unique(runname_wlabel),
		        ERC_PR_chg5yMax = max(ERC_PR_chg5y, na.rm = TRUE))

df_ERCMaxChg


df_ERCMaxChg_qtile <- 
	df_ERCMaxChg %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						
						ERC_PR_chg5yMax_q90 = quantile(ERC_PR_chg5yMax, 0.90),
						ERC_PR_chg5yMax_q75 = quantile(ERC_PR_chg5yMax, 0.75),
						ERC_PR_chg5yMax_q50 = quantile(ERC_PR_chg5yMax, 0.50),
						ERC_PR_chg5yMax_q25 = quantile(ERC_PR_chg5yMax, 0.25),
						ERC_PR_chg5yMax_q10 = quantile(ERC_PR_chg5yMax, 0.1)
						)
df_ERCMaxChg_qtile

	
## "sim-to-sim" comparison 
df_bySim <- 
	left_join(results_stch %>% 
							select(runname, runname_wlabel, sim, year, policy_type, ERC, ERC_PR, EEC, EEC_PR, UAAL),
						results_stch %>% 
							ungroup %>% 
							filter(runname == "cola_baseline") %>% 
							select(sim, year, ERC_baseline = ERC, 
										            ERC_PR_baseline = ERC_PR,
										            EEC_baseline = EEC, 
										            EEC_PR_baseline = EEC_PR,
										            UAAL_baseline   = UAAL),
						by = c("sim", "year")
	) %>% 
	filter(runname != "cola_baseline") %>% 
	mutate(ERC_PR_diffBySim = ERC_PR - ERC_PR_baseline,
				 ERC_diffBySim    = ERC    - ERC_baseline,
				 EEC_PR_diffBySim = EEC_PR - EEC_PR_baseline,
				 EEC_diffBySim    = EEC    - EEC_baseline
				 ) 
	
df_bySim



#*******************************************************************************
##              Measures:  Benefit for a single cohort                      ####
#*******************************************************************************

# Cohorts to examine:
	# Cohort 1: Retired at age 60 in year 1 (1 - 41)
	# Cohort 2: Retired at age 60 in year 15 (15- 55)
		#  - After the 15-year amortization period for the year-1 UAAL 

# Risk measures:
#  - PV (discount only) Benefit for age 60-100: percentiles
#  - PV (discount + attribution) for benefit for age 60-100: percentiles
#  - Max decrease in real benefit in 5-years for a single cohort: percentiles 
#  - Benefit level at age 80

# Normalize the benefit at age 60 to 100. 
# Need qxm.r in decrement table 

load("Inputs/riskShaing_demographics_100y.RData")


df_benefit_y1 <- 
	results_all %>% 
	filter(sim >= 1, year %in% 1:41) %>% 
	select(runname, runname_wlabel, policy_type, sim, year, cola_actual) %>% 
	group_by(runname, sim) %>% 
	mutate(age = 60:100) %>% 
	left_join(decrement %>% ungroup() %>% filter(ea == min(ea))  %>% select(age, qxm.r) , by = "age") %>% 
	mutate(qxm.r   = ifelse(age == max(age), 1, qxm.r),
				 fct_qxm = ifelse(age == 60, 1, lag(cumprod(1-qxm.r))),
				 fct_dr  = 1/(1 + dr)^(age - 60),
				 B       = 100 * ifelse(age == 60, 1, lag(cumprod(1+cola_actual))),
				 B_real  = B / (1 + infl)^(age - 60),
				 B_real_chg5y = rollapply(B_real, width = 6, get_nyearMin, fill = NA, align = "right")
				 )
df_benefit_y1 


df_benefit_y1 %<>% 
	summarise(runname_wlabel = unique(runname_wlabel),
		        B_PV_dr    = sum(B * fct_dr),
						B_PV_tot   = sum(B* fct_dr * fct_qxm),
						B_real_chg5yMin = min(B_real_chg5y, na.rm = TRUE),
						B_real_age80   = B_real[age == 80]		
						)


df_benefit_qtile_y1 <- 
	df_benefit_y1 %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						
						B_PV_dr_q90 = quantile(B_PV_dr, 0.90),
						B_PV_dr_q75 = quantile(B_PV_dr, 0.75),
						B_PV_dr_q50 = quantile(B_PV_dr, 0.50),
						B_PV_dr_q25 = quantile(B_PV_dr, 0.25),
						B_PV_dr_q10 = quantile(B_PV_dr, 0.1),
						
						B_PV_tot_q90 = quantile(B_PV_tot, 0.90),
						B_PV_tot_q75 = quantile(B_PV_tot, 0.75),
						B_PV_tot_q50 = quantile(B_PV_tot, 0.50),
						B_PV_tot_q25 = quantile(B_PV_tot, 0.25),
						B_PV_tot_q10 = quantile(B_PV_tot, 0.1),
						
						B_real_chg5yMin_q90 = quantile(B_real_chg5yMin, 0.90),
						B_real_chg5yMin_q75 = quantile(B_real_chg5yMin, 0.75),
						B_real_chg5yMin_q50 = quantile(B_real_chg5yMin, 0.50),
						B_real_chg5yMin_q25 = quantile(B_real_chg5yMin, 0.25),
						B_real_chg5yMin_q10 = quantile(B_real_chg5yMin, 0.1),
						
						B_real_age80_q90 = quantile(B_real_age80, 0.90),
						B_real_age80_q75 = quantile(B_real_age80, 0.75),
						B_real_age80_q50 = quantile(B_real_age80, 0.50),
						B_real_age80_q25 = quantile(B_real_age80, 0.25),
						B_real_age80_q10 = quantile(B_real_age80, 0.1)
						)
	

df_benefit_qtile_y1 %>% select(runname, starts_with("B_PV"))
df_benefit_qtile_y1 %>% select(runname, starts_with("B_real"))




df_benefit_y15 <- 
	results_all %>% 
	filter(sim >= 1, year %in% 15:55) %>% 
	select(runname, runname_wlabel, policy_type, sim, year, cola_actual) %>% 
	group_by(runname, sim) %>% 
	mutate(age = 60:100) %>% 
	left_join(decrement %>% ungroup() %>% filter(ea == min(ea))  %>% select(age, qxm.r) , by = "age") %>% 
	mutate(qxm.r   = ifelse(age == max(age), 1, qxm.r),
				 fct_qxm = ifelse(age == 60, 1, lag(cumprod(1-qxm.r))),
				 fct_dr  = 1/(1 + dr)^(age - 60),
				 B       = 100 * ifelse(age == 60, 1, lag(cumprod(1+cola_actual))),
				 B_real  = B / (1 + infl)^(age - 60),
				 B_real_chg5y = rollapply(B_real, width = 6, get_nyearMin, fill = NA, align = "right")
	)
df_benefit_y15 


df_benefit_y15 %<>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						B_PV_dr    = sum(B * fct_dr),
						B_PV_tot   = sum(B* fct_dr * fct_qxm),
						B_real_chg5yMin = min(B_real_chg5y, na.rm = TRUE),
						B_real_age80   = B_real[age == 80]		
	)


df_benefit_qtile_y15 <- 
	df_benefit_y1 %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						
						B_PV_dr_q90 = quantile(B_PV_dr, 0.90),
						B_PV_dr_q75 = quantile(B_PV_dr, 0.75),
						B_PV_dr_q50 = quantile(B_PV_dr, 0.50),
						B_PV_dr_q25 = quantile(B_PV_dr, 0.25),
						B_PV_dr_q10 = quantile(B_PV_dr, 0.1),
						
						B_PV_tot_q90 = quantile(B_PV_tot, 0.90),
						B_PV_tot_q75 = quantile(B_PV_tot, 0.75),
						B_PV_tot_q50 = quantile(B_PV_tot, 0.50),
						B_PV_tot_q25 = quantile(B_PV_tot, 0.25),
						B_PV_tot_q10 = quantile(B_PV_tot, 0.1),
						
						B_real_chg5yMin_q90 = quantile(B_real_chg5yMin, 0.90),
						B_real_chg5yMin_q75 = quantile(B_real_chg5yMin, 0.75),
						B_real_chg5yMin_q50 = quantile(B_real_chg5yMin, 0.50),
						B_real_chg5yMin_q25 = quantile(B_real_chg5yMin, 0.25),
						B_real_chg5yMin_q10 = quantile(B_real_chg5yMin, 0.1),
						
						B_real_age80_q90 = quantile(B_real_age80, 0.90),
						B_real_age80_q75 = quantile(B_real_age80, 0.75),
						B_real_age80_q50 = quantile(B_real_age80, 0.50),
						B_real_age80_q25 = quantile(B_real_age80, 0.25),
						B_real_age80_q10 = quantile(B_real_age80, 0.1)
	)


df_benefit_qtile_y15 %>% select(runname, starts_with("B_PV"))
df_benefit_qtile_y15 %>% select(runname, starts_with("B_real"))




#*******************************************************************************
##              Measures:  Lifetime analysis                                ####
#*******************************************************************************

# Need to use salary in the input demographic data

load("Inputs/riskShaing_demographics_100y.RData")

# cohort: ea = 25, yos = 0 in year 1, retirement year =  36
df_actives_ea25 <- 
	df_actives %>% 
	filter(ea ==  25, year - (age-ea) == 1) %>% 
	select(ea, age, year, N = number.a, salary = sx, NCx, ALx)
df_actives_ea25

df_retirees_ea25 <- 
  df_retirees %>% filter(ea ==  25, year - (age-ea) == 1, year.retire == 36) %>% 
	select(ea, age, year, N = number.r, benefit = B.r)

df_ea25 <-  
	bind_rows(df_actives_ea25, 
						df_retirees_ea25) %>% 
	mutate(fct_dec = N /N[year == 1]) %>% 
	mutate_all(funs(na2zero(.)))
df_ea25


# Merge to simulation results and calculate contributions and benefit 
df_ea25 <- 
results_all %>% 
	filter(sim >= 1, year %in% 1:76) %>% 
	select(runname, runname_wlabel, year, sim, policy_type, EEC_PR, ERC_PR, AL, C, NC, PR, cola_actual) %>% 
	left_join(df_ea25, by = "year") %>% 
	group_by(runname, sim) %>% 
	mutate(EEC    = salary * EEC_PR, 
				 NC_ER  = NCx - EEC,
				 SC_tot = C - NC,
				 SC_ER1 = (SC_tot / PR) * salary,
				 SC_ER2 = SC_tot * (ALx / AL),
				 ERC1   = NC_ER + SC_ER1,
				 ERC2   = NC_ER + SC_ER2,
				 cola_actual = ifelse(age>=60, cola_actual, 0),
				 benefit = ifelse(age >= 60, benefit[age == 60] * lag(cumprod(1+cola_actual)),0 )
				 )
df_ea25	


# Measure 1. Distribution of the PVB / PVC ratio at entry age: for a single cohort (ea = 25 or 30)
#   - PVC: PV of sum(salary_t * (ERCrate_t + EECrate_t) for t in working age
# 	- PVB: PV of sum(B_t) for t in retirement age

# discont rate for this measure
dr_m1 <- 0.075

df_ea25_m1 <- 
	df_ea25 %>% 
	mutate(fct_dr = 1/(1+dr_m1)^(year - 1)) %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
		        ERC1_PV = sum(ERC1 * N * fct_dr),
						ERC2_PV = sum(ERC2 * N * fct_dr),
						EEC_PV = sum(EEC * N * fct_dr),
						NC_PV  = sum(NCx  * N * fct_dr),
						C1_PV  = sum( (ERC1+EEC) * N * fct_dr),
						C2_PV  = sum( (ERC2+EEC) * N * fct_dr),
						B_PV   = sum(benefit * N * fct_dr)
						) %>% 
	mutate(
		B_ERC1 = B_PV / ERC1_PV ,
		B_ERC2 = B_PV / ERC2_PV , 
		B_C1   = B_PV / C1_PV ,
		B_C2   = B_PV / C2_PV ,
		B_EEC  = B_PV / EEC_PV
	)

df_ea25_m1_qtile <- 
df_ea25_m1 %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						B_C1_q90 = quantile(B_C1, 0.90, na.rm = T),
						B_C1_q75 = quantile(B_C1, 0.75, na.rm = T),
						B_C1_q50 = quantile(B_C1, 0.50, na.rm = T),
						B_C1_q25 = quantile(B_C1, 0.25, na.rm = T),
						B_C1_q10 = quantile(B_C1, 0.1, na.rm = T),
						
						B_C2_q90 = quantile(B_C2, 0.90, na.rm = T),
						B_C2_q75 = quantile(B_C2, 0.75, na.rm = T),
						B_C2_q50 = quantile(B_C2, 0.50, na.rm = T),
						B_C2_q25 = quantile(B_C2, 0.25, na.rm = T),
						B_C2_q10 = quantile(B_C2, 0.1, na.rm = T),
						
						B_EEC_q90 = quantile(B_EEC, 0.90, na.rm = T),
						B_EEC_q75 = quantile(B_EEC, 0.75, na.rm = T),
						B_EEC_q50 = quantile(B_EEC, 0.50, na.rm = T),
						B_EEC_q25 = quantile(B_EEC, 0.25, na.rm = T),
						B_EEC_q10 = quantile(B_EEC, 0.1,na.rm = T)
						)

df_ea25_m1_qtile
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C1")) # salary based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C2")) # AL based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_EEC"))















# Need to use salary in the input demographic data

load("Inputs/riskShaing_demographics_100y.RData")

# cohort: ea = 25, yos = 0 in year 1, retirement year =  36
df_actives_ea25 <- 
	df_actives %>% 
	filter(ea ==  25, year - (age-ea) == 15) %>% 
	select(ea, age, year, N = number.a, salary = sx, NCx, ALx)
df_actives_ea25

df_retirees_ea25 <- 
	df_retirees %>% filter(ea ==  25, year - (age-ea) == 15, year.retire == 50) %>% 
	select(ea, age, year, N = number.r, benefit = B.r)

df_ea25 <-  
	bind_rows(df_actives_ea25, 
						df_retirees_ea25) %>% 
	mutate(fct_dec = N /N[year == 15]) %>% 
	mutate_all(funs(na2zero(.)))
df_ea25


# Merge to simulation results and calculate contributions and benefit 
df_ea25 <- 
	results_all %>% 
	filter(sim >= 1, year %in% 15:90) %>% 
	select(runname, runname_wlabel, year, sim, policy_type, EEC_PR, ERC_PR, AL, C, NC, PR, cola_actual) %>% 
	left_join(df_ea25, by = "year") %>% 
	group_by(runname, sim) %>% 
	mutate(EEC    = salary * EEC_PR, 
				 NC_ER  = NCx - EEC,
				 SC_tot = C - NC,
				 SC_ER1 = (SC_tot / PR) * salary,
				 SC_ER2 = SC_tot * (ALx / AL),
				 ERC1   = NC_ER + SC_ER1,
				 ERC2   = NC_ER + SC_ER2,
				 cola_actual = ifelse(age>=60, cola_actual, 0),
				 benefit = ifelse(age >= 60, benefit[age == 60] * lag(cumprod(1+cola_actual)),0 )
	)
df_ea25	


# Measure 1. Distribution of the PVB / PVC ratio at entry age: for a single cohort (ea = 25 or 30)
#   - PVC: PV of sum(salary_t * (ERCrate_t + EECrate_t) for t in working age
# 	- PVB: PV of sum(B_t) for t in retirement age

# discont rate for this measure
dr_m1 <- 0.075

df_ea25_m1 <- 
	df_ea25 %>% 
	mutate(fct_dr = 1/(1+dr_m1)^(year - 1)) %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						ERC1_PV = sum(ERC1 * N * fct_dr),
						ERC2_PV = sum(ERC2 * N * fct_dr),
						EEC_PV = sum(EEC * N * fct_dr),
						NC_PV  = sum(NCx  * N * fct_dr),
						C1_PV  = sum( (ERC1+EEC) * N * fct_dr),
						C2_PV  = sum( (ERC2+EEC) * N * fct_dr),
						B_PV   = sum(benefit * N * fct_dr)
	) %>% 
	mutate(
		B_ERC1 = B_PV / ERC1_PV ,
		B_ERC2 = B_PV / ERC2_PV , 
		B_C1   = B_PV / C1_PV ,
		B_C2   = B_PV / C2_PV ,
		B_EEC  = B_PV / EEC_PV
	)

df_ea25_m1_qtile <- 
	df_ea25_m1 %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						B_C1_q90 = quantile(B_C1, 0.90, na.rm = T),
						B_C1_q75 = quantile(B_C1, 0.75, na.rm = T),
						B_C1_q50 = quantile(B_C1, 0.50, na.rm = T),
						B_C1_q25 = quantile(B_C1, 0.25, na.rm = T),
						B_C1_q10 = quantile(B_C1, 0.1, na.rm = T),
						
						B_C2_q90 = quantile(B_C2, 0.90, na.rm = T),
						B_C2_q75 = quantile(B_C2, 0.75, na.rm = T),
						B_C2_q50 = quantile(B_C2, 0.50, na.rm = T),
						B_C2_q25 = quantile(B_C2, 0.25, na.rm = T),
						B_C2_q10 = quantile(B_C2, 0.1, na.rm = T),
						
						B_EEC_q90 = quantile(B_EEC, 0.90, na.rm = T),
						B_EEC_q75 = quantile(B_EEC, 0.75, na.rm = T),
						B_EEC_q50 = quantile(B_EEC, 0.50, na.rm = T),
						B_EEC_q25 = quantile(B_EEC, 0.25, na.rm = T),
						B_EEC_q10 = quantile(B_EEC, 0.1,na.rm = T)
	)

df_ea25_m1_qtile
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C1")) # salary based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C2")) # AL based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_EEC"))







