

#*******************************************************************************
##                              Notes                                       ####
#*******************************************************************************

# 1. Contribution risks
# 2. Benefit risks
# 3. Lifetime analysis


# Notes on measures
#  https://docs.google.com/document/d/1oi2UEfcrsk6359-zbJTb8DamtbpoEGUFLzcY19njwUI/edit



#*******************************************************************************
##                            Global settings                               ####
#*******************************************************************************

## Loading packages
source("libraries.R")

dir_modelResults <- "Outputs/"
dir_modelResults <- "Outputs_500sims/"
dir_outputs      <- "Outputs_Analysis/"


dr    <- 0.075 # discount rate
infl  <- 0.02  # assumed inflation rate
Nyear <- 40    # Number of sim years for contribution analysis
cola_baseline <- 0.015

DC_EECrate <- 0.025
DC_ERCrate <- 0.028


#*******************************************************************************
##                           Loading results                               ####
#*******************************************************************************

source("Analysis_loadingResults.R")
# Outputs: 
   # results_all
   # run_labels

#*******************************************************************************
##              Measures: Contribution                ####
#*******************************************************************************

## Measures:
#  1. Percentiles of PV employer contributions at year 1
#  2. Max increase in ERC rate in 5 years
#  3. measure of "sim-to-sim" comparison

## PV ERC and EEC 

results_stch_FR75 <-  
  results_all %>% 
	filter(!str_detect(runname, "FR100")) %>% 
	filter(sim >= 1, year <= Nyear) 


df_PVC <- 
  results_stch_FR75 %>% 
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
# Why PV ERC of SDRS is so low, even with final UAAL?


## Maximum increase in ERC rate WITHIN 5 years

# Note:
#  - Need to take into account the possibility that the changes in less then 5 years
#    are greater than the 5-year change. The method below takes care of this. 

get_nyearMax <- function(x) max(x[-1] - x[1])
get_nyearMin <- function(x) min(x[-1] - x[1])
# x <- filter(results_stch, sim ==1, runname == "cola_baseline") %>% pull("ERC_PR")
# zoo::rollapply(x, width = 6, get_nyearMax, fill = NA, align = "right")

df_ERCMaxChg <- 
	results_stch_FR75 %>% 
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


# Proability of ERC rate rising by more than 5% in 40 years




## Make tables for draft paper

# Table for PV ERC, w/o final UAAL
tab_PVERC_qtiles <- 
df_PVC_qtiles %>% 
	select(runname, contains("ERC_PV")) %>% 
	gather(qtile, value, -runname) %>% 
	mutate(qtile = factor(qtile) %>% fct_inorder()) %>% 
	mutate(value = 100 * value / value[runname == "baseline" & qtile == "ERC_PV_q50"]) %>% 
	spread(runname, value)

	
# Table of max increase in ERC rate in 5 years
tab_ERCMaxChg_qtiles <- 
df_ERCMaxChg_qtile %>% 
	select(-runname_wlabel) %>% 
	gather(qtile, value, -runname) %>% 
	mutate(qtile = factor(qtile) %>% fct_inorder()) %>% 
	# mutate(value = 100 * value / value[runname == "baseline" & qtile == "ERC_PV_q50"]) %>% 
	spread(runname, value)


tab_PVERC_qtiles %>% kable(digits=1)
tab_ERCMaxChg_qtiles %>% kable(digits = 2)

# Notes:
#  - why does cola_return policy have the lowest compound COLA but does not have the largest cost reduction effect? 



#*******************************************************************************
##              Measures: Contribution sim-to-sim             ####
#*******************************************************************************

	
## "sim-to-sim" comparison 
df_bySim_FR75 <- 
	left_join(results_stch_FR75 %>% 
							select(runname, runname_wlabel, sim, year, policy_type, ERC, ERC_PR, EEC, EEC_PR, UAAL),
						results_stch_FR75 %>% 
							ungroup %>% 
							filter(runname == "baseline") %>% 
							select(sim, year, ERC_baseline = ERC, 
										            ERC_PR_baseline = ERC_PR,
										            EEC_baseline = EEC, 
										            EEC_PR_baseline = EEC_PR,
										            UAAL_baseline   = UAAL),
						by = c("sim", "year")
	) %>% 
	filter(runname != "cola_baseline") %>% 
	mutate(ERC_PR_diffBySim = ERC_PR - ERC_PR_baseline,
				 ERC_diffBySim    = ERC - ERC_baseline,
				 EEC_PR_diffBySim = EEC_PR - EEC_PR_baseline,
				 EEC_diffBySim    = EEC - EEC_baseline
				 ) 
	
df_bySim_FR75 %>% 
	filter(runname != "baseline") 


# 1. quantile differences across all sims and years, difference in ERC rate

tab_bySim_ERCptiles <- 
df_bySim_FR75 %>% 
	filter(runname != "baseline") %>% 
	group_by(runname) %>% 
	summarise(diff_ERCrate_90 = quantile(ERC_PR_diffBySim, 0.90),
						diff_ERCrate_75 = quantile(ERC_PR_diffBySim, 0.75),
						diff_ERCrate_50 = quantile(ERC_PR_diffBySim, 0.50),
						diff_ERCrate_25 = quantile(ERC_PR_diffBySim, 0.25),
						diff_ERCrate_10 = quantile(ERC_PR_diffBySim, 0.10)
						) %>% 
	gather(ptile, values, -runname) %>% 
	mutate(ptile = factor(ptile) %>% fct_inorder()) %>% 
	spread(runname, values)
tab_bySim_ERCptiles %>% kable(digits = 5)


# 2. Dividing simulations into 5 groups based 40-year CAGR, difference in ERC rate:
#    1) 90th percentile and above []
#    2) 75th~90th percentile [)
#    3) 25th~75th percentile [)
#    4) 25th percentile and below [)
# Grouping can be done based on other variables (e.g. std of return)

df_CAGR40 <- 
results_all %>% 
	filter(runname == "baseline", sim >= 1) %>% 
	group_by(sim) %>% 
	summarise(CAGR40 = get_geoReturn(i.r)) 
  #summarise(CAGR40 = sd(i.r)) 

grpVals_CAGR40 <-
	df_CAGR40 %>% 
	summarise(CAGR40_q90 = quantile(CAGR40, 0.90),
						CAGR40_q75 = quantile(CAGR40, 0.75),
						CAGR40_q50 = quantile(CAGR40, 0.50),
						CAGR40_q25 = quantile(CAGR40, 0.25),
						CAGR40_q10 = quantile(CAGR40, 0.10)
	) 

df_CAGR40 %<>% 
	mutate(grp_CAGR40 = case_when(
		CAGR40 >= grpVals_CAGR40$CAGR40_q90 ~ 1,
		CAGR40 >= grpVals_CAGR40$CAGR40_q75 & CAGR40 < grpVals_CAGR40$CAGR40_q90 ~ 2,
		CAGR40 >= grpVals_CAGR40$CAGR40_q25 & CAGR40 < grpVals_CAGR40$CAGR40_q75 ~ 3,
		CAGR40 >= grpVals_CAGR40$CAGR40_q10 & CAGR40 < grpVals_CAGR40$CAGR40_q25 ~ 4,
		CAGR40 < grpVals_CAGR40$CAGR40_q10 ~ 5,
		TRUE ~ NA_real_) 
	)

	
	
df <- 
df_bySim_FR75 %>% 
	left_join(df_CAGR40, by = "sim") %>% 
	# mutate(grp_CAGR40 = case_when(
	# 	CAGR40 >= grpVals_CAGR40$CAGR40_q90 ~ 1,
	# 	CAGR40 >= grpVals_CAGR40$CAGR40_q75 & CAGR40 < grpVals_CAGR40$CAGR40_q90 ~ 2,
	# 	CAGR40 >= grpVals_CAGR40$CAGR40_q25 & CAGR40 < grpVals_CAGR40$CAGR40_q75 ~ 3,
	# 	CAGR40 >= grpVals_CAGR40$CAGR40_q10 & CAGR40 < grpVals_CAGR40$CAGR40_q25 ~ 4,
	# 	CAGR40 < grpVals_CAGR40$CAGR40_q10 ~ 5,
	# 	TRUE ~ NA_real_) 
	# 	) %>% 
	group_by(runname, grp_CAGR40) %>% 
	summarise(diff_ERCrate_avg     = mean(ERC_PR_diffBySim, na.rm = TRUE),
						ERCrate_baseline_avg = mean(ERC_PR_baseline, na.rm = TRUE))
df

df %>%  
	filter(runname != "baseline") %>% 
	ggplot(aes(x = grp_CAGR40, y = diff_ERCrate_avg, color = runname, shape = runname)) + theme_yy() +
	geom_line()+
	geom_point()

tab_bySim_ERCgrp <- 
df %>% 
	spread(runname, diff_ERCrate_avg)
tab_bySim_ERCgrp %>% kable(digits = 5)



# df_bySim_FR75 %>% 
# 	filter(runname != "baseline") %>% 
# 	group_by(runname, sim) %>% 
#   summarise(diff_ERCrate_avg = mean(ERC_PR_diffBySim, na.rm = TRUE)) %>% 
#   left_join(df_CAGR40, by = "sim") %>% 
# 	ggplot(aes(x = CAGR40, y = diff_ERCrate_avg, color = runname)) + theme_yy() +
# 	geom_line()
	
df_PVC2 <- 
df_PVC %>% 
	#filter(runname != "baseline") %>% 
	left_join(df_CAGR40, by = "sim") %>% 
	group_by(runname, grp_CAGR40) %>% 
	summarise(ERC_PV = mean(ERC_PV, na.rm = TRUE),
						EEC_PV = mean(EEC_PV, na.rm = TRUE)) %>% 
	ungroup() %>% 
	mutate(ERC_PV = 100 * ERC_PV / ERC_PV[runname == "baseline" & grp_CAGR40 == 3],
				 EEC_PV = 100 * EEC_PV / EEC_PV[runname == "baseline" & grp_CAGR40 == 3])

	
df_PVC2 %>% select(-EEC_PV) %>% 
	spread(runname, ERC_PV)

df_PVC2 %>% select(-ERC_PV) %>% 
	spread(runname, EEC_PV)
# Very similar to tab_PVERC_qtiles


#*******************************************************************************
##              Measures:  Benefit for a single cohort in DB                ####
#*******************************************************************************

# Cohorts to examine:
	# Cohort 1: Retired at age 60 in year 1 (1 - 41)
	# Cohort 2: Retired at age 60 in year 15 (15- 55)
		#  - After the 15-year amortization period for the year-1 UAAL 

# Research questions and Risk measures:
# 
# 1. COLA is intended to provide protection against inflation. Even for plans with 
#    fixed COLA, few of them provide full protection against inflation (usually fraction of CPI)
#    Contingent COLA policies can even further weaken the inflation protection: lower 
#    average COLA; same average COLA but weaker protection in bad scenarios (uncertainty in the strength of protection). 
#    
#    Q: To what extent do contingent COLA policies weaken the (overall) protection against inflation?
#    
#    Measures:
# 		- PV (discount only or +  + attribution ) Benefit for age 60-100: percentiles
# 		- Real benefit level at age 80
#
# 2. DB plans are intended to provide secured retirement income. But contingent
#     COLA policies create uncertainty in the benefit payments. The policies may 
#     cause deteriorated standard-of-living if retirees experience large declines 
#     in real benefit payments that may force them to change their consumption behavior. 
#    
#    Q: Would contingent COLA policies lead to declines in real benefit that may 
#       affect retirees welfare?
#    
#    Measures:
# 			- Max decrease in real benefit in 5-years for a single cohort: percentiles 
#       - Probability of real benefits fall below 90% of starting benefit
#  
# 3. Additional question: How would the initial funded ratio affect the impact of 
#    COLA policies contingent upon funded ratio? 
#    

# Normalize the benefit at age 60 to 100. 
# Need qxm.r in decrement table 

load("Inputs/riskShaing_demographics_100y.RData")

## Create a model run with cola_actual = infl and append to results_all

df_colaFull <- 
  results_all %>% 
	filter(runname == "baseline") %>% 
	mutate(cola_actual = infl,
				 runname     = "cola_full")

df_benefit_y1 <- 
	bind_rows(results_all, df_colaFull) %>% 
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


df_benefit_qtile_y1 <- 
	df_benefit_y1 %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						B_PV_dr    = sum(B * fct_dr),
						B_PV_tot   = sum(B* fct_dr * fct_qxm),
						B_real_chg5yMin = min(B_real_chg5y, na.rm = TRUE),
						B_real_age80   = B_real[age == 80]		
	) %>% 
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

## Q1 measures
  # B_PV total and B_real_age80
  # runs: baseline, cola_return, cola_FR, cola_SDRS, cola_FR100, cola_SDRS_100

runs_benefit <- c("cola_full", "baseline", "cola_return", "cola_FR", "cola_SDRS", "cola_FR_FR100", "cola_SDRS_FR100")
runs_benefit_lables <- c(
	"COLA equal to inflation",
	"Baseline \nYear-1 funded ratio 100%",
	"Contingent COLA: \nreturn",
	"Contingent COLA: \nFunded ratio threshold",
	"SDRS",
	"Contingent COLA: \nFunded ratio threshold\nYear-1 funded ratio 100%",
	"SDRS \nYear-1 funded ratio 100%"
)
	
	
tab_benefit1 <- 
df_benefit_qtile_y1 %>% 
	filter(runname %in% runs_benefit) %>% 
	select(runname, starts_with("B_PV_tot"), starts_with("B_real_age80")) %>% 
	gather(Var, values, -runname) %>%
	mutate(runname = factor(runname, levels= runs_benefit),
		     Var = factor(Var) %>% fct_inorder(),
				 values = ifelse(str_detect(Var, "B_PV_tot"),  100 * values / values[runname == "baseline" & Var == "B_PV_tot_q50"], values)
	) %>% 
	spread(runname, values) 
tab_benefit1 %>% kable(digits = 2)


## Q2 measures

# Max 5-year benefit change
tab_benefit2 <- 
df_benefit_qtile_y1 %>% 
	filter(runname %in% runs_benefit) %>% 
	select(runname, starts_with("B_real_chg5yMin")) %>% 
	gather(Var, values, -runname) %>%
	mutate(runname = factor(runname, levels= runs_benefit),
				 Var = factor(Var) %>% fct_inorder()
	) %>% 
	spread(runname, values) 
tab_benefit2 %>% kable(digits = 2)

# Probability of benefit falling below 90% of starting benefit

df_benefit_probLowBen <- 
df_benefit_y1 %>% 
	filter(runname %in% setdiff(runs_benefit,c("baseline", "cola_full"))) %>% 
	ungroup() %>% 
	mutate(runname = factor(runname, levels = runs_benefit, labels = runs_benefit_lables)) %>% 
	group_by(runname, sim) %>% 
	mutate(B_real_low = cumany(B_real <= 90)) %>% 
	group_by(runname, year) %>% 
	summarise(B_real_low = sum(B_real_low)/n()) 


fig_title <- "Probability of inflation-adjusted benefit falling below 90% of the year-1 value\n at anytime up ot the given year"
fig_benefit_probLowBen <- 
df_benefit_probLowBen %>% 
	ggplot(aes(y = B_real_low, x = year, color = runname, shape = runname)) + theme_yy() +
	geom_line() + 
	geom_point() + 
	scale_y_continuous(labels = percent) +
	labs(title = fig_title,
			 x = "Year",
			 y = "Probability",
		   color = NULL,
			 shape = NULL) +
	guides(color = guide_legend(keywidth = 1.5, keyheight = 3))
fig_benefit_probLowBen

save_figure(fig_benefit_probLowBen, dir_outputs, w = 9, h = 6 )




## Exploratory tables

tab_Bpv_FR75 <- 
df_benefit_qtile_y1 %>% 
	filter(!str_detect(runname, "FR100")) %>% 
	select(runname, starts_with("B_PV_tot")) %>% 
	gather(Var, values, -runname) %>%
	mutate(Var = factor(Var) %>% fct_inorder(),
				 values = 100 * values / values[runname == "baseline" & Var == "B_PV_tot_q50"]
				 ) %>% 
	spread(runname, values) %>% 
	select(Var, baseline, contains("cola_"))

tab_Breal_FR75 <- 
df_benefit_qtile_y1 %>% 
	filter(!str_detect(runname, "FR100")) %>% 
	select(runname, starts_with("B_real")) %>% 
	gather(Var, values, -runname) %>%
	mutate(Var = factor(Var) %>% fct_inorder()) %>% 
	spread(runname, values) %>% 
	select(Var, baseline, contains("cola_"))

tab_Bpv_FR100 <- 
df_benefit_qtile_y1 %>% 
	filter(str_detect(runname, "FR100|cola_full")) %>% 
	select(runname, starts_with("B_PV_tot")) %>% 
	gather(Var, values, -runname) %>%
	mutate(Var = factor(Var) %>% fct_inorder(),
				 values = 100 * values / values[runname == "baseline_FR100" & Var == "B_PV_tot_q50"]
	) %>% 
	spread(runname, values) %>% 
	select(Var, baseline_FR100, contains("cola_"))


tab_Breal_FR100 <- 
df_benefit_qtile_y1 %>% 
	filter(str_detect(runname, "FR100|cola_full")) %>% 
	select(runname, starts_with("B_real")) %>% 
	gather(Var, values, -runname) %>%
	mutate(Var = factor(Var) %>% fct_inorder()) %>% 
	spread(runname, values) %>% 
	select(Var, baseline_FR100, contains("cola_"))


tab_Bpv_FR75
tab_Breal_FR75

tab_Bpv_FR100
tab_Breal_FR100



# df_benefit_y15 <- 
# 	results_all %>% 
# 	filter(sim >= 1, year %in% 15:55) %>% 
# 	select(runname, runname_wlabel, policy_type, sim, year, cola_actual) %>% 
# 	group_by(runname, sim) %>% 
# 	mutate(age = 60:100) %>% 
# 	left_join(decrement %>% ungroup() %>% filter(ea == min(ea))  %>% select(age, qxm.r) , by = "age") %>% 
# 	mutate(qxm.r   = ifelse(age == max(age), 1, qxm.r),
# 				 fct_qxm = ifelse(age == 60, 1, lag(cumprod(1-qxm.r))),
# 				 fct_dr  = 1/(1 + dr)^(age - 60),
# 				 B       = 100 * ifelse(age == 60, 1, lag(cumprod(1+cola_actual))),
# 				 B_real  = B / (1 + infl)^(age - 60),
# 				 B_real_chg5y = rollapply(B_real, width = 6, get_nyearMin, fill = NA, align = "right")
# 	)
# df_benefit_y15 
# 
# 
# df_benefit_y15 %<>% 
# 	summarise(runname_wlabel = unique(runname_wlabel),
# 						B_PV_dr    = sum(B * fct_dr),
# 						B_PV_tot   = sum(B* fct_dr * fct_qxm),
# 						B_real_chg5yMin = min(B_real_chg5y, na.rm = TRUE),
# 						B_real_age80   = B_real[age == 80]		
# 	)
# 
# 
# df_benefit_qtile_y15 <- 
# 	df_benefit_y1 %>% 
# 	summarise(runname_wlabel = unique(runname_wlabel),
# 						
# 						B_PV_dr_q90 = quantile(B_PV_dr, 0.90),
# 						B_PV_dr_q75 = quantile(B_PV_dr, 0.75),
# 						B_PV_dr_q50 = quantile(B_PV_dr, 0.50),
# 						B_PV_dr_q25 = quantile(B_PV_dr, 0.25),
# 						B_PV_dr_q10 = quantile(B_PV_dr, 0.1),
# 						
# 						B_PV_tot_q90 = quantile(B_PV_tot, 0.90),
# 						B_PV_tot_q75 = quantile(B_PV_tot, 0.75),
# 						B_PV_tot_q50 = quantile(B_PV_tot, 0.50),
# 						B_PV_tot_q25 = quantile(B_PV_tot, 0.25),
# 						B_PV_tot_q10 = quantile(B_PV_tot, 0.1),
# 						
# 						B_real_chg5yMin_q90 = quantile(B_real_chg5yMin, 0.90),
# 						B_real_chg5yMin_q75 = quantile(B_real_chg5yMin, 0.75),
# 						B_real_chg5yMin_q50 = quantile(B_real_chg5yMin, 0.50),
# 						B_real_chg5yMin_q25 = quantile(B_real_chg5yMin, 0.25),
# 						B_real_chg5yMin_q10 = quantile(B_real_chg5yMin, 0.1),
# 						
# 						B_real_age80_q90 = quantile(B_real_age80, 0.90),
# 						B_real_age80_q75 = quantile(B_real_age80, 0.75),
# 						B_real_age80_q50 = quantile(B_real_age80, 0.50),
# 						B_real_age80_q25 = quantile(B_real_age80, 0.25),
# 						B_real_age80_q10 = quantile(B_real_age80, 0.1)
# 	)
# 
# 
# df_benefit_qtile_y15 %>% select(runname, starts_with("B_PV"))
# df_benefit_qtile_y15 %>% select(runname, starts_with("B_real"))
# 



#*******************************************************************************
##              Measures: Benefit risk of DC                ####
#*******************************************************************************

#!!! To be updated with vested benefits



# Need to use salary in the input demographic data
load("Inputs/riskShaing_demographics_bf.5_100y.RData")

# cohort: ea = 25, yos = 0 in year 1, retirement year =  36 (same as the 1st cohort in the lifetime analysis)

df_actives_ea25_dc <- 
	df_actives %>% 
	filter(ea ==  25, year - (age-ea) == 1) %>% 
	select(ea, age, year, N = number.a, salary_indiv = sx, PVFBx_DB_indiv = PVFBx.r, NCx_DB_indiv = NCx)
df_actives_ea25_dc

df_retirees_ea25_dc <- 
	df_retirees %>% filter(ea ==  25, year - (age-ea) == 1, year.retire == 36) %>% 
	select(ea, age, year, N = number.r, B_DB_indiv = B.r, PVFBx_DB_indiv = ALx.r, ax.r)
df_retirees_ea25_dc

df_ea25_dc <-  
	bind_rows(df_actives_ea25_dc, 
						df_retirees_ea25_dc) %>% 
	mutate(C_DC_indiv = salary_indiv * (DC_EECrate + DC_ERCrate), 
		     fct_dec    = N /N[year == 1]) %>% 
	mutate_all(list(na2zero))
df_ea25_dc



# Merge to simulation results and calculate contributions and benefit 
df_ea25_dc <- 
	results_all %>% 
	filter(sim >= 0, year %in% 1:76, runname == "hybrid_DB") %>% 
	select(runname, runname_wlabel, sim, year, policy_type, i.r) %>% 
	left_join(df_ea25_dc, by = "year") %>% 
	group_by(runname, sim)
df_ea25_dc

# Calculate DC balance for an individual
df_ea25_dc %<>%
	group_by(runname, sim) %>% 
	mutate(
		B_DC_indiv = 0, 
		DC_balance_indiv = 0,
		DC_balance_indiv = ifelse(age<=60, fnC(DC_balance_indiv, C_DC_indiv, i.r), 0),
		NC_PR = NCx_DB_indiv / salary_indiv
		)

# Calculate DC benefit
# Assumptions:
#  - balance at 60 is converted to a life annuity using the same discount rate and 
#    decrements as DB plan


df_ea25_dc %<>%
	group_by(runname, sim) %>% 
	mutate(
		B_DC_indiv = ifelse(age == 60, DC_balance_indiv / ax.r, 0),
		B_DC_indiv = ifelse(age >= 60, B_DC_indiv[age == 60] * (1+cola_baseline)^(age - 60), 0),
		DC_balance_indiv = getBalanceC(DC_balance_indiv, C_DC_indiv, B_DC_indiv, i.r)
	)
df_ea25_dc 
# NC rate for DB component is 4.56% 


# Distribution account balance at age 60
df_ea25_dc_blc_qtile <- 
df_ea25_dc %>%
	group_by(runname) %>% 
	filter(sim>0, age == 60) %>%
	summarize(PVFB_DB = mean(PVFBx_DB_indiv),
						
						DC_balance_avg = mean(DC_balance_indiv),
						DC_balance_q90 = quantile(DC_balance_indiv, 0.90),
						DC_balance_q75 = quantile(DC_balance_indiv, 0.75),
						DC_balance_q50 = quantile(DC_balance_indiv, 0.50),
						DC_balance_q25 = quantile(DC_balance_indiv, 0.25),
						DC_balance_q10 = quantile(DC_balance_indiv, 0.1),
						
						
						DB_B = mean(B_DB_indiv),
						
						B_DC_avg = mean(B_DC_indiv),
						B_DC_q90 = quantile(B_DC_indiv, 0.90),
						B_DC_q75 = quantile(B_DC_indiv, 0.75),
						B_DC_q50 = quantile(B_DC_indiv, 0.50),
						B_DC_q25 = quantile(B_DC_indiv, 0.25),
						B_DC_q10 = quantile(B_DC_indiv, 0.1)
						
						
						)
df_ea25_dc_blc_qtile 	

df_ea25_dc_blc_qtile %>% select(runname, PVFB_DB, starts_with("DC_balance"))
df_ea25_dc_blc_qtile %>% select(runname, DB_B,    starts_with("B_DC"))


df_ea25_dc %>% filter(sim>=0,  age == 60) %>% 
	ungroup %>% 
	ggplot(aes(x = DC_balance_indiv)) + 
	geom_histogram(bins = 50)

df_ea25_dc %>% filter(sim>=0,  age == 60) %>% 
	ungroup %>% 
	ggplot(aes(x = B_DC_indiv)) + 
	geom_histogram(bins = 50)

# Notes:
#  - Although the total cost rate of the the DC component (6% of payroll) is very close
#    to the aggregate NC rate of the DB plan, it may differ from the NC rates of individual
#    ea cohorts.(the higher the ea, the higher the NC rate). 
#  - For the cohort ea = 25, the NC rate is 4.56%, which is lower than the DC contribution 
#    rate of 6%. However, the PVB upon retirement age is much higher the mean and median
#    DC account balance (52k, 37k, 35k)
#  - But this may not be a fair comparison of the level of benefit:
#     - DC plan: member can claim the DC account balance upon separation before retirement.
#     - DB plan: Only service retirement is modeled, so it is assumed that the member
#                will receive no benefit upon separation before retirement (age 65). 
#                This actually explains why the individual PVB of DB upon retirement is so high
#                compared to the contribution rate (NC): the contributions made by members who have 
#                separated before age 60 are all used to support the retirement benefit for those 
#                who remain active until retirement (17% of the cohort)
#  - What is the right question to ask about the DC/hybrid plan benefit? 
#     - We may want to focus on the uncertainty in final balance and annuity benefit
#     - It may not be fair to directly compare the benefit level of DB and DC/hybrid, 
#       because DC better benefit upon separation before retirement (greater flexibility)


# Issue
#  - qxt looks quite high for younger ages and low for older ages. How is this compared
#    to qxt used in real-world AVs?
#  - If deferred retirement benefit is important for comparing overall benefit level 
#    for DB and DC(hybrid), do we need to include it? What it will take to include it? 




#*******************************************************************************
##         Measures:  Lifetime analysis Prep data           ####
#*******************************************************************************

# !!! All lifetime analysis needs to be updated with vested benefits. 

# Notes on incorporating vested benefits:
  # Measure 1: easy, just add discounted value of PVFBx.v at all working ages. 
  # Measure 3: easy, similar to measure 1
  # Measure 2: more difficult but doable, need to calculate cash flow of vested benefit payments



# Need to use salary in the input demographic data

load("Inputs/riskShaing_demographics_100y.RData")

dr_m1 <- 0.075

## cohort: ea = 25, yos = 0 in year 1, retirement year =  36
df_actives_ea25 <- 
	df_actives %>% 
	filter(ea ==  25, year - (age-ea) == 1) %>% 
	select(ea, age, year, 
				 N = number.a, 
				 salary_indiv = sx, 
				 NCx.r_indiv = NCx.r, 
				 NCx.v_indiv = NCx.v, 
				 ALx.r_indiv = ALx.r, 
				 ALx.v_indiv = ALx.v, 
				 PVFBx.r_indiv = PVFBx.r, 
				 PVFBx.v_indiv = PVFBx.v)
df_actives_ea25

df_retirees_ea25 <- 
  df_retirees %>% filter(ea ==  25, year - (age-ea) == 1, year.retire == 36) %>% 
	select(ea, age, year, 
				 N = number.r, 
				 B.r_indiv = B.r, 
				 PVFBx.r_indiv = ALx.r)

df_ea25 <-  
	bind_rows(df_actives_ea25, 
						df_retirees_ea25) %>% 
	mutate(fct_dec = N /N[year == 1]) %>% 
	mutate_all(list(na2zero))
df_ea25


# Merge to simulation results and calculate contributions and benefit 
df_ea25 <- 
results_all %>% 
	filter(sim >= 0, year %in% 1:76) %>% 
	select(runname, runname_wlabel, year, sim, policy_type, 
				 EEC_PR_plan = EEC_PR, 
				 ERC_PR_plan = ERC_PR, 
				 AL_plan     = AL, 
				 C_plan      = C, 
				 NC_plan     = NC, 
				 PR_plan     = PR, 
				 cola_actual, 
				 i.r) %>% 
	left_join(df_ea25, by = "year") %>% 
	group_by(runname, sim) %>% 
	mutate(
		     fct_dr       = 1/(1+dr_m1)^(year - 1),
		     EEC_indiv    = salary_indiv * EEC_PR_plan, 
				 NC_ER_indiv  = NCx.r_indiv + NCx.v_indiv - EEC_indiv,
				 SC_plan      = C_plan - NC_plan,
				 SC_ER_indiv1 = (SC_plan / PR_plan) * salary_indiv,
				 SC_ER_indiv2 =  SC_plan * ((ALx.r_indiv + ALx.v_indiv) / AL_plan),
				 ERC_indiv1   = NC_ER_indiv + SC_ER_indiv1,
				 ERC_indiv2   = NC_ER_indiv + SC_ER_indiv2,
				 cola_actual  = ifelse(age>=60, cola_actual, 0),
				 B.r_indiv    = ifelse(age >= 60, B.r_indiv[age == 60] * lag(cumprod(1+cola_actual)),0 )
				 )
df_ea25	



#*******************************************************************************
##         Measures:  Lifetime analysis 1 (PVB/PVC at ea)            ####
#*******************************************************************************


# Measure 1. Distribution of the PVB / PVC ratio at entry age: for a single cohort (ea = 25 or 30)
#   - PVC: PV of sum(salary_t * (ERCrate_t + EECrate_t) for t in working age
# 	- PVB: PV of sum(B_t) for t in retirement age

# discont rate for this measure
dr_m1 <- 0.075

df_ea25_m1 <- 
	df_ea25 %>% 
	# mutate(fct_dr = 1/(1+dr_m1)^(year - 1)) %>% 
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
		B_EEC  = B_PV / EEC_PV,
		B_NC   = B_PV / NC_PV
	)

df_ea25_m1_qtile <- 
df_ea25_m1 %>% 
	filter(sim >= 1) %>% 
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
						B_EEC_q10 = quantile(B_EEC, 0.1,na.rm = T),
						
						B_NC_q90 = quantile(B_NC, 0.90, na.rm = T),
						B_NC_q75 = quantile(B_NC, 0.75, na.rm = T),
						B_NC_q50 = quantile(B_NC, 0.50, na.rm = T),
						B_NC_q25 = quantile(B_NC, 0.25, na.rm = T),
						B_NC_q10 = quantile(B_NC, 0.1,na.rm = T)
						
						)

df_ea25_m1_qtile
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C1")) # salary based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C2")) # AL based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_EEC"))
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_NC"))

# sim 0, return assumption met
df_ea25_m1 %>% filter(sim ==0)


## cohort: ea = 25, yos = 0 in year 16, retirement year =  51
df_actives_ea25_y16 <- 
	df_actives %>% 
	filter(ea ==  25, year - (age-ea) == 16) %>% 
	select(ea, age, year, N = number.a, salary_indiv = sx, NCx, ALx)
df_actives_ea25_y16

df_retirees_ea25_y16 <- 
	df_retirees %>% filter(ea ==  25, year - (age-ea) == 16, year.retire == 51) %>% 
	select(ea, age, year, N = number.r, benefit = B.r)
df_retirees_ea25_y16

df_ea25_y16 <-  
	bind_rows(df_actives_ea25_y16, 
						df_retirees_ea25_y16) %>% 
	mutate(fct_dec = N /N[year == 16]) %>% 
	mutate_all(list(na2zero))
df_ea25_y16


# Merge to simulation results and calculate contributions and benefit 
df_ea25_y16 <- 
	results_all %>% 
	filter(sim >= 0, year %in% 16:91) %>% 
	select(runname, runname_wlabel, year, sim, policy_type, EEC_PR, ERC_PR, AL, C, NC, PR, cola_actual) %>% 
	left_join(df_ea25_y16, by = "year") %>% 
	group_by(runname, sim) %>% 
	mutate(EEC    = salary_indiv * EEC_PR, 
				 NC_ER  = NCx - EEC,
				 SC_tot = C - NC,
				 SC_ER1 = (SC_tot / PR) * salary_indiv, # Note PR is the total payroll of the plan
				 SC_ER2 = SC_tot * (ALx / AL),
				 ERC1   = NC_ER + SC_ER1,
				 ERC2   = NC_ER + SC_ER2,
				 cola_actual = ifelse(age>=60, cola_actual, 0),
				 benefit = ifelse(age >= 60, benefit[age == 60] * lag(cumprod(1+cola_actual)),0 )
	)
df_ea25_y16


# Measure 1. Distribution of the PVB / PVC ratio at entry age: for a single cohort (ea = 25 or 30)
#   - PVC: PV of sum(salary_t * (ERCrate_t + EECrate_t) for t in working age
# 	- PVB: PV of sum(B_t) for t in retirement age

# discont rate for this measure
dr_m1 <- 0.075

df_ea25_y16_m1 <- 
	df_ea25_y16 %>% 
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
		B_EEC  = B_PV / EEC_PV,
		B_NC   = B_PV / NC_PV
	)

df_ea25_y16_m1_qtile <- 
	df_ea25_y16_m1 %>% 
	filter(sim >= 1) %>% 
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
						B_EEC_q10 = quantile(B_EEC, 0.1,na.rm = T),
						
						B_NC_q90 = quantile(B_NC, 0.90, na.rm = T),
						B_NC_q75 = quantile(B_NC, 0.75, na.rm = T),
						B_NC_q50 = quantile(B_NC, 0.50, na.rm = T),
						B_NC_q25 = quantile(B_NC, 0.25, na.rm = T),
						B_NC_q10 = quantile(B_NC, 0.1,na.rm = T)
						
	)

df_ea25_y16_m1_qtile
df_ea25_y16_m1_qtile %>% select(runname_wlabel, starts_with("B_C1")) # salary based
df_ea25_y16_m1_qtile %>% select(runname_wlabel, starts_with("B_C2")) # AL based
df_ea25_y16_m1_qtile %>% select(runname_wlabel, starts_with("B_EEC"))
df_ea25_y16_m1_qtile %>% select(runname_wlabel, starts_with("B_NC"))


df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C1")) # salary based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_C2")) # AL based
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_EEC"))
df_ea25_m1_qtile %>% select(runname_wlabel, starts_with("B_NC"))


df_ea25_m1 %>% filter(sim ==0)
df_ea25_y16_m1 %>% filter(sim ==0)



# - Even for the cohort starting in year 16, the salary-based B/C ratios are still 
#   low, although they are higher than the ratios of the cohort starting in year 1.
# - Why is that? Does the B/C ratio vary a lot across ea groups? If so, is the ea = 25
#   group representative? 



#*******************************************************************************
##         Measures:  Lifetime analysis 2 (max age balance)                 ####
#*******************************************************************************

## Measure 2
# PV Final cohort balance / PVB at entry age  

# Final balances are calculated with
# 1. Cohort normal costs as cash inflow and benefit as cash outflow
# 2. Cohort contribution (salary-based) as cash inflow and benefit as cash outflow
# 3. Cohort contribution (liability-based) as cash inflow and benefit as cash outflow


# Construct cohort cash flow

df_ea25_m2 <- 
	df_ea25 %>% 
	mutate(PVB_ea    = (PVFBx.r * N)[age == ea],
		     NC_cohort = NCx * N,
				 C1_cohort = (EEC + ERC1) * N,
				 C2_cohort = (EEC + ERC2) * N,
				 B_cohort  = benefit * N,
				 balance_cohort = 0,
				 
				 balanceNC_cohort = getBalanceC(balance_cohort, NC_cohort, B_cohort, i.r), 
				 balanceC1_cohort = getBalanceC(balance_cohort, C1_cohort, B_cohort, i.r),
				 balanceC2_cohort = getBalanceC(balance_cohort, C2_cohort, B_cohort, i.r),
				 
				 balanceNC_cohort_PV = balanceNC_cohort / (1 + dr_m1)^(age - ea), 
				 balanceC1_cohort_PV = balanceC1_cohort / (1 + dr_m1)^(age - ea),
				 balanceC2_cohort_PV = balanceC2_cohort / (1 + dr_m1)^(age - ea),
				 
				 PV_ratioM2_NC =  balanceNC_cohort_PV /PVB_ea,
				 PV_ratioM2_C1 =  balanceC1_cohort_PV /PVB_ea,
				 PV_ratioM2_C2 =  balanceC2_cohort_PV /PVB_ea

				 ) %>% 
	select(runname, runname_wlabel, sim, year, ea, age, PVB_ea, ends_with("cohort"), ends_with("cohort_PV" ), starts_with("PV_ratio"))

df_ea25_m2 %>% 
	filter(year == max(year))

df_ea25_m2 %>% 
	select(runname, sim, year, 
											ends_with("cohort_PV"),
											starts_with("PV_ratio")
											) %>% 
	filter(year == max(year))


df_ea25_m2_qtile <- 
df_ea25_m2 %>% 
	filter(year == max(year), sim >=1) %>% 
	group_by(runname) %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						
						
						PV_ratioM2_NC_avg = mean(PV_ratioM2_NC, na.rm = T),
						PV_ratioM2_NC_q90 = quantile(PV_ratioM2_NC, 0.90, na.rm = T),
						PV_ratioM2_NC_q75 = quantile(PV_ratioM2_NC, 0.75, na.rm = T),
						PV_ratioM2_NC_q50 = quantile(PV_ratioM2_NC, 0.50, na.rm = T),
						PV_ratioM2_NC_q25 = quantile(PV_ratioM2_NC, 0.25, na.rm = T),
						PV_ratioM2_NC_q10 = quantile(PV_ratioM2_NC, 0.1, na.rm = T),
						
						PV_ratioM2_C1_q90 = quantile(PV_ratioM2_C1, 0.90, na.rm = T),
						PV_ratioM2_C1_q75 = quantile(PV_ratioM2_C1, 0.75, na.rm = T),
						PV_ratioM2_C1_q50 = quantile(PV_ratioM2_C1, 0.50, na.rm = T),
						PV_ratioM2_C1_q25 = quantile(PV_ratioM2_C1, 0.25, na.rm = T),
						PV_ratioM2_C1_q10 = quantile(PV_ratioM2_C1, 0.1, na.rm = T),
						
						PV_ratioM2_C2_q90 = quantile(PV_ratioM2_C2, 0.90, na.rm = T),
						PV_ratioM2_C2_q75 = quantile(PV_ratioM2_C2, 0.75, na.rm = T),
						PV_ratioM2_C2_q50 = quantile(PV_ratioM2_C2, 0.50, na.rm = T),
						PV_ratioM2_C2_q25 = quantile(PV_ratioM2_C2, 0.25, na.rm = T),
						PV_ratioM2_C2_q10 = quantile(PV_ratioM2_C2, 0.1, na.rm = T)
						
						# balanceNC_avg = mean(balanceNC_cohort, na.rm = T),
						# balanceNC_q90 = quantile(balanceNC_cohort, 0.90, na.rm = T),
						# balanceNC_q75 = quantile(balanceNC_cohort, 0.75, na.rm = T),
						# balanceNC_q50 = quantile(balanceNC_cohort, 0.50, na.rm = T),
						# balanceNC_q25 = quantile(balanceNC_cohort, 0.25, na.rm = T),
						# balanceNC_q10 = quantile(balanceNC_cohort, 0.1, na.rm = T),
						# 
						# balanceC1_q90 = quantile(balanceC1_cohort, 0.90, na.rm = T),
						# balanceC1_q75 = quantile(balanceC1_cohort, 0.75, na.rm = T),
						# balanceC1_q50 = quantile(balanceC1_cohort, 0.50, na.rm = T),
						# balanceC1_q25 = quantile(balanceC1_cohort, 0.25, na.rm = T),
						# balanceC1_q10 = quantile(balanceC1_cohort, 0.1, na.rm = T),
						# 
						# balanceC2_q90 = quantile(balanceC2_cohort, 0.90, na.rm = T),
						# balanceC2_q75 = quantile(balanceC2_cohort, 0.75, na.rm = T),
						# balanceC2_q50 = quantile(balanceC2_cohort, 0.50, na.rm = T),
						# balanceC2_q25 = quantile(balanceC2_cohort, 0.25, na.rm = T),
						# balanceC2_q10 = quantile(balanceC2_cohort, 0.1, na.rm = T)
						)
	
df_ea25_m2_qtile %>% select(runname, contains("NC"))
df_ea25_m2_qtile %>% select(runname, contains("C1"))
df_ea25_m2_qtile %>% select(runname, contains("C2"))


#*******************************************************************************
##         Measures:  Lifetime analysis 3 (ret age balance)                 ####
#*******************************************************************************

## Measure 3
# PV cohort ret age balance / PVB at ret age  

# Final balances are calculated with
# 1. Cohort normal costs as cash inflow and benefit as cash outflow
# 2. Cohort contribution (salary-based) as cash inflow and benefit as cash outflow
# 3. Cohort contribution (liability-based) as cash inflow and benefit as cash outflow


# Construct cohort cash flow

df_ea25_m3 <- 
	df_ea25 %>% 
	mutate(PVB_ret    = (PVFBx.r * N)[age == 60],
				 NC_cohort = NCx * N,
				 C1_cohort = (EEC + ERC1) * N,
				 C2_cohort = (EEC + ERC2) * N,
				 B_cohort  = benefit * N,
				 balance_cohort = 0,
				 
				 balanceNC_cohort = getBalanceC(balance_cohort, NC_cohort, B_cohort, i.r), 
				 balanceC1_cohort = getBalanceC(balance_cohort, C1_cohort, B_cohort, i.r),
				 balanceC2_cohort = getBalanceC(balance_cohort, C2_cohort, B_cohort, i.r),
				 
				 PV_ratioM3_NC =  balanceNC_cohort /PVB_ret,
				 PV_ratioM3_C1 =  balanceC1_cohort /PVB_ret,
				 PV_ratioM3_C2 =  balanceC2_cohort /PVB_ret
				 
	) %>% 
	select(runname, runname_wlabel, sim, year, ea, age, PVB_ret, ends_with("cohort"), starts_with("PV_ratio"))

df_ea25_m3 %>% 
	filter(age == 60)

df_ea25_m3 %>% 
	select(runname, sim, year, age,
				 ends_with("cohort"),
				 starts_with("PV_ratio")
	) %>% 
	filter(age == 60 )


df_ea25_m3_qtile <- 
	df_ea25_m3 %>% 
	filter(age == 60, sim >=1) %>% 
	group_by(runname) %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						
						PV_ratioM3_NC_avg = mean(PV_ratioM3_NC, na.rm = T),
						PV_ratioM3_NC_q90 = quantile(PV_ratioM3_NC, 0.90, na.rm = T),
						PV_ratioM3_NC_q75 = quantile(PV_ratioM3_NC, 0.75, na.rm = T),
						PV_ratioM3_NC_q50 = quantile(PV_ratioM3_NC, 0.50, na.rm = T),
						PV_ratioM3_NC_q25 = quantile(PV_ratioM3_NC, 0.25, na.rm = T),
						PV_ratioM3_NC_q10 = quantile(PV_ratioM3_NC, 0.1, na.rm = T),
						
						PV_ratioM3_C1_q90 = quantile(PV_ratioM3_C1, 0.90, na.rm = T),
						PV_ratioM3_C1_q75 = quantile(PV_ratioM3_C1, 0.75, na.rm = T),
						PV_ratioM3_C1_q50 = quantile(PV_ratioM3_C1, 0.50, na.rm = T),
						PV_ratioM3_C1_q25 = quantile(PV_ratioM3_C1, 0.25, na.rm = T),
						PV_ratioM3_C1_q10 = quantile(PV_ratioM3_C1, 0.1, na.rm = T),
						
						PV_ratioM3_C2_q90 = quantile(PV_ratioM3_C2, 0.90, na.rm = T),
						PV_ratioM3_C2_q75 = quantile(PV_ratioM3_C2, 0.75, na.rm = T),
						PV_ratioM3_C2_q50 = quantile(PV_ratioM3_C2, 0.50, na.rm = T),
						PV_ratioM3_C2_q25 = quantile(PV_ratioM3_C2, 0.25, na.rm = T),
						PV_ratioM3_C2_q10 = quantile(PV_ratioM3_C2, 0.1, na.rm = T)
	)

df_ea25_m3_qtile %>% select(runname, contains("NC"))
df_ea25_m3_qtile %>% select(runname, contains("C1"))
df_ea25_m3_qtile %>% select(runname, contains("C2"))



#*******************************************************************************
##              Distribution of COLA               ####
#*******************************************************************************

# Distribution of 40-year COLA with 75% initial funded ratio
df_cola <- 
	results_all %>% 
	filter(year %in% 1:40) %>% 
	# filter(year %in% 16:55) %>% 
	filter(sim >= 1, str_detect(runname, "cola|baseline") ) %>% 
	group_by(runname, sim) %>%
	summarise(runname_wlabel = unique(runname_wlabel),
						cola_avg  = prod(1 + cola_actual[-n()])^(1/(n() - 1)) - 1)

df_cola_qtiles <- 
	df_cola %>% 
	summarise(runname_wlabel = unique(runname_wlabel),
						cola_avg_q10 = quantile(cola_avg, 0.1),
						cola_avg_q25 = quantile(cola_avg, 0.25),
						cola_avg_q50 = quantile(cola_avg, 0.50),
						cola_avg_q75 = quantile(cola_avg, 0.75),
						cola_avg_q90 = quantile(cola_avg, 0.90)
						)

df_cola_qtiles_FR75 <- 
	df_cola_qtiles %>% 
	filter(!str_detect(runname, "FR100"))

levels(df_cola_qtiles_FR75$runname_wlabel)[df_cola_qtiles_FR75$runname]

df_cola_qtiles_FR100 <- 
	df_cola_qtiles %>% 
	filter(str_detect(runname, "FR100")) %>% 
	mutate(runname_wlabel = factor(runname, 
																 levels = runname, 
																 labels = levels(df_cola_qtiles_FR75$runname_wlabel)[df_cola_qtiles_FR75$runname])) 
df_cola_qtiles_FR100


fig_title    <- "Distributions of 40-year compound annual COLA \nunder different COLA policies"
fig_subtitle <- "Starting funded ratio: 75%"
fig_cola_qtiles_FR75 <- 
	df_cola_qtiles_FR75 %>% 
	# filter(runname %in% runname_stch) %>% 
	ggplot(aes(x = runname_wlabel)) + theme_yy() + 
	geom_boxplot(width = 0.3,
							 stat = "identity",
							 aes(ymin   = cola_avg_q10,
							 		lower  = cola_avg_q25,
							 		middle = cola_avg_q50,
							 		upper  = cola_avg_q75,
							 		ymax   = cola_avg_q90)
	) +
	geom_hline(yintercept =
						 	filter(df_cola_qtiles, runname == "baseline") %>% pull(cola_avg_q50),
						 linetype = 2) +
	coord_cartesian(ylim = c(0, 0.025)) + 
	scale_y_continuous(breaks = seq(0, 1, 0.0025), labels = percent) + 
	labs(title = fig_title,
			 subtitle = fig_subtitle,
			 x = NULL, 
			 y = "Compound annual COLA")
fig_cola_qtiles_FR75


fig_title    <- "Distributions of 40-year compound annual COLA \nunder different COLA policies"
fig_subtitle <- "Starting funded ratio: 100%"
fig_cola_qtiles_FR100 <- 
	df_cola_qtiles_FR100 %>% 
	# filter(runname %in% runname_stch) %>% 
	ggplot(aes(x = runname_wlabel)) + theme_yy() + 
	geom_boxplot(width = 0.3,
							 stat = "identity",
							 aes(ymin   = cola_avg_q10,
							 		lower  = cola_avg_q25,
							 		middle = cola_avg_q50,
							 		upper  = cola_avg_q75,
							 		ymax   = cola_avg_q90)
	) +
	geom_hline(yintercept =
						 	filter(df_cola_qtiles, runname == "baseline") %>% pull(cola_avg_q50),
						 linetype = 2) +
	coord_cartesian(ylim = c(0, 0.025)) + 
	scale_y_continuous(breaks = seq(0, 1, 0.0025), labels = percent) + 
	labs(title = fig_title,
			 subtitle = fig_subtitle,
			 x = NULL, 
			 y = "Compound annual COLA")
fig_cola_qtiles_FR100



# The impact of funded ratio can be very large for FR-based policies: 
#   - The legacy UAAL was largely accrued during the working years of the current retirees,
#   - so making the current retirees bear the cost is consistent with inter-generational equity.
#   - But current retirees will need to bear risk of with all NEW UAALs, which are associated
#   - with all members (active and retired). 
# 
# Return based cola is NOT affected by legacy underfunding.   


save_figure(fig_cola_qtiles_FR75, dir_outputs, w = 8,  h = 5)
save_figure(fig_cola_qtiles_FR100, dir_outputs, w = 8,  h = 5)


# Distribution of 40-year COLA with 100% initial funded ratio





