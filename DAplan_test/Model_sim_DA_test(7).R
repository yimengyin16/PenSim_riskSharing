

#*******************************************************************************
#                           ### Notes        ####                      
#*******************************************************************************

#' Test modeling a DA plan using the same inputs as the DB plans
#' 
#' Assumptions:
#'  - single retirement age 60. 
#'  - Mortality and termination rate
#'  - service retirement benefit only. (no deferred retirement benefit) 



#*******************************************************************************
#                           ### Initialization ####                      
#*******************************************************************************


run_sim_DA <- function(paramlist_ = paramlist,
											 Global_paramlist_ = Global_paramlist){
	

## Loading packages

# paramlist_ <- paramlist
# Global_paramlist_ <- Global_paramlist

assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(paramlist_,  envir = environment())

# bfactor_DA
# dr_DA
# infl
# idxFull_DA

ncore <- 6
nsim  <- 5



#******************************************************************************* 
#                 Loading data from Model_Main   ####
#*******************************************************************************

## loading demographic data
load("Inputs/riskShaing_demographics_100y.RData")

# df_actives
# df_retirees
# df_terms
# decrement



#******************************************************************************* 
#              Creating instrumental variables for valuation  ####
#*******************************************************************************

df_decrements_DA <- 
	decrement %>% 
	select(ea, age, qxm, qxt, qxd, qxr) %>% 
	group_by(ea) %>% 
	mutate(
		
		# Prob of survival in each year
		# For actives (age<60): decrements include mortality, termination and disability
		# For retirees (age>=60): mortality only
		pxT = ifelse(age < age_ret, 
								 1 - qxm - qxt - qxd,
								 1 - qxm),
		
		# Number of remaining members assuming 100 initial members
		year = 1:n(),
		N = ifelse(year == 1, 100, 100 * lag(cumprod(pxT))),
		
		
		# Prob of survival up to retirement (age 60)
		p_age2ret = order_by(-age, cumprod(ifelse(age >= age_ret, 1, pxT))),
		
		# Prob for retirees to survive up a certain age
		# note that pxT needs to be lagged, because it is the prob of surviving to the NEXT period.
		p_ret2age = cumprod(ifelse(age <= age_ret, 1, lag(pxT))), 
		
		
		# discount factor up to retirement (age 60), for actives only
		# PV at the current age of $1 at the retirement age
		fct_dr_age2ret =  ifelse(age >= age_ret, 1, 1/(1 + dr_DA)^(age_ret - age)),
		
		# discount factor: retirees up a certain age, for retirees only
		# PV at the retirement age of $1 at a certain age.
		fct_dr_ret2age = ifelse(age < age_ret, 1, 1/(1 + dr_DA)^(age - age_ret)),
		
		
		# expected benefit for $1 current accrued benefit at ret age (60) assuming full indexation
		# B1_age2ret_fullIdx = order_by(-age, cumprod(ifelse(age >= age_ret, 1, (1 + idxFull_DA)))),
		B1_age2ret_fullIdx   = ifelse(age > age_ret, 0, (1 + idxFull_DA)^(age_ret - age)),
		
		# expected benefit for $1 starting benefit at ret age (60) assuming full indexation
		B1_age2death_fullIdx = ifelse(age < age_ret, 0, (1 + idxFull_DA)^(age - age_ret)),
		
		
		# annuity value at current age for $1 current benefit, assuming full indexation 
		ax1_age2death_fullIdx = order_by(-age, cumsum(B1_age2death_fullIdx * fct_dr_ret2age * p_ret2age )) / (B1_age2death_fullIdx * fct_dr_ret2age * p_ret2age),
		ax1_age2death_fullIdx = ifelse(age >= age_ret, 	ax1_age2death_fullIdx, 0),
		
		# annuity value at current age for $1 current benefit, assuming no indexation
		ax1_age2death_noIdx = order_by(-age, cumsum(ifelse(age < age_ret, 0, fct_dr_ret2age * p_ret2age ))) / (fct_dr_ret2age * p_ret2age),
		ax1_age2death_noIdx = ifelse(age >= age_ret, 	ax1_age2death_noIdx, 0),
		
		
		
		# AL for $1 of current accrued benefit, full index
		fct_ALx_fullIdx = B1_age2ret_fullIdx * ax1_age2death_fullIdx[age == age_ret]  * p_age2ret / (1+dr_DA)^(age_ret-age),
		
		# AL for $1 of current accrued benefit, no index
		fct_ALx_noIdx = ax1_age2death_noIdx[age == age_ret] * p_age2ret / (1+dr_DA)^(age_ret-age),
		
		# Normal cost for $1 of newly accrued benefit 
		fct_NCx_fullIdx = (lead(B1_age2ret_fullIdx) * ax1_age2death_fullIdx[age == age_ret]) * p_age2ret / (1+dr_DA)^(age_ret-age)
	)


#*******************************************************************************
#                           ### select the group to model  ####                      
#*******************************************************************************


## Active members
df_actives_sim0 <- 
  df_actives %>% 
	mutate(start_year = year - (age-ea),
				 yos = age - ea,
				 ret_year = start_year + (age_ret - ea)) %>% 
	# filter(start_year %in% 1 & ea %in% c(30,31) |
	# 			 start_year %in% -1 & ea %in% c(20, 21)
	# 			 ) %>%
	filter(age < age_ret) %>% 
	select(start_year, ea, age, yos, year, ret_year, sx, n_actives = number.a) %>% 
	left_join(df_decrements_DA %>%
							select(-year),
						by = c("ea", "age"))


## Service retirees
# Notes: 
#  - For existing members in year 1, only the benefit values in year 1 will be used. 
#  - For all future members (start_year >= 2), only the benefit values at retirement age will be used. 
#  - ax.r is the annuity factor at age x (PV of future annuity payments for $1's payment at current age). 

df_servRet_sim0 <- 
  df_retirees %>% 
	filter(!is.na(year),
				 year <= nyear
	) %>% 
	rename(B  = B.r,
				 n_servRet  = number.r,
				 ret_year   = year.retire 
				 ) %>%
	mutate(start_year = year - (age - ea),
				 # ret_year   = start_year + (age_ret - ea), # This is the first retirement year (age 60) 
				 ret_age    = age - (year - ret_year)
				
				 ) %>% 
	filter(ret_year == 1 | ret_age == 60, 
				 !(ret_year < 1 & n_servRet == 0)    # excluding some unnecessary rows
         ) %>% 
	mutate_at(vars(B, n_servRet), list(na2zero)) %>% 
	select(start_year, ea, age, year, ret_age, ret_year, n_servRet, B) %>% 
	ungroup() %>% 
	arrange(year, ret_year, ea, age) # one-to-one mapping between retirement year and age with single retirement age, 
# df_servRet_sim0

# All starting years allowed
start_years <- c(df_servRet_sim0$start_year[df_servRet_sim0$year == 1], 1:nyear)
df_servRet_sim0 %<>%  filter( !(ret_year == 1 & (!start_year %in% start_years))) 


# select cohorts to model
df_servRet_sim0 %<>% 
	# filter(start_year %in%  1 & ea %in% c(30, 31)|
	# 			 start_year %in% -1 & ea %in% c(20, 21) |
	# 			 start_year %in% -10 & ea %in% c(49)
	# ) %>%
	
	# filter(start_year <= 40
	# 			 # start_year %in% -10 & ea %in% c(49)
	# ) %>%
	
	left_join(df_decrements_DA %>%
							select(-year),
						by = c("ea", "age"))


df_servRet_sim0 %>%
	arrange(start_year, ea) %>%
	filter(start_year == 3, ea == 20)


## combine actives and retirees
df_indiv_sim0 <- 
	bind_rows(
		df_actives_sim0,
		df_servRet_sim0
	) %>% 
	arrange(start_year, ea, age)





# TEMP: 
#   - accured benefits for initial members in year 1
#   - Use the current salary as the indexed salarly 
#   - assume full index in year 0   

# TODO: initial accrued benefit consistent with salary history before year 1 


df_indiv_sim0 %<>% 
	mutate(
		     
		    
		
		     # accrued benefit for initial active members
		     Bx          = ifelse(start_year <= 1 & year == 1 & age < age_ret, yos * bfactor_DA * sx, 0),
  			 Bx_new      = ifelse(start_year <= 1 & year == 1 & age < age_ret, bfactor_DA * sx * (1 + idxFull_DA), 0),
				 # benefit for initial retirees: deleting all benefit values for non-initial members
				 B           = ifelse(ret_year > 1 | year > 1, 0, B),
				 
				 ALx_fullIdx = ifelse(age< age_ret, Bx * fct_ALx_fullIdx, B * ax1_age2death_fullIdx),
	  		 ALx_noIdx   = ifelse(age< age_ret, Bx * fct_ALx_noIdx,   B * ax1_age2death_noIdx),
				 
				 NCx_fullIdx = Bx_new * fct_NCx_fullIdx,
				 
				 n_actives = na2zero(n_actives),
				 n_servRet = na2zero(n_servRet)
				 
		  	 ) %>% 
	relocate(start_year, ea,age, yos, year, n_actives, n_servRet, sx, B, Bx, Bx_new, ALx_fullIdx, ALx_noIdx) 


# df_indiv_sim0 %>% 
# 	filter(start_year == 1)
# df_indiv_sim0 


#df_actives_sim0 %>% group_by(start_year, ea) 
#df_servRet_sim0 %>% group_by(start_year, ea) 
# df_servRet_sim0 %>% filter(ret_year == 1, year== 1)


# df_actives_sim0 %>% 
# 	filter(ret_year <= 91) %>% 
# 	group_by(start_year, ea) 
# 
# df_servRet_sim0 %>% 
# 	filter(start_year >= 1) %>% 
# 	group_by(start_year, ea) 
# 
# df_servRet_sim0 %>% 
# 	filter(ret_year == 1, year == 1)
# 




#*******************************************************************************
#                           ### index and return ####                      
#*******************************************************************************

# nyear <- max(df_indiv_sim0$year)

## Create a series of hypothetical benefit indices:
ben_idx_vec <- rep(0.02, nyear)
# ben_idx_vec[c(6:10, 21:25, 36:40)] <- 0
# ben_idx_vec



## Asset-shock scenario
i.crisis <- rep(i.mean - i.sd^2/2, nyear)
i.crisis[2:5] <- c(-0.24, 0.12, 0.12, 0.12)


## Stochastic returns
set.seed(1234) ;i.r0 <- matrix(rnorm(nyear*nsim, i.mean, i.sd), nyear, nsim)
i.r0 <- cbind(rep(i.mean - i.sd^2/2, nyear), i.r0)
i.r0 <- cbind(i.crisis, i.crisis, i.r0)
colnames(i.r0) <- -2:nsim
# i.r_ <- i.r
# i.r0


#*******************************************************************************
#                           ### Simulation ####                      
#*******************************************************************************

# Need to calculat the following in each period
# Accrued benefit
# Nominal liability
# real liability
# New benefit accrual
# Normal cost
# index for current year. 



## Create a data frame for aggregate valuation results
df_agg_sim0 <- 
	data.frame(year   = 1:nyear) %>% 
	mutate(AL_fullIdx = 0,
				 AL_noIdx   = 0,
				 MA         = 0,
				 UAAL_fullIdx = 0,
				 FR_fullIdx = 0,
				 FR_noIdx   = 0,
				 NC         = 0,
				 SC         = 0,
				 
				 EEC        = 0,
				 ERC        = 0,
				 C          = 0,
				 
				 ADC    = 0, 
				 ADC.ER = 0,
				 C_ADC  = 0,
				 
				 B          = 0,
				 I.e        = 0,
				 I.r        = 0,
				 I.diff     = 0
				 )


## total payroll
df_agg_sim0 %<>% 
	left_join(df_indiv_sim0 %>% 
							group_by(year) %>% 
							summarise(PR = sum(n_actives * sx, na.rm = TRUE),
												.groups = "drop"),
						by = "year"
	)


cl <- makeCluster(ncore) 
registerDoParallel(cl)

penSim_results <- foreach(k = -2:nsim, .packages = c("dplyr", "tidyr", "magrittr", "Rcpp")) %dopar% {
	

	source("Functions.R")
	
	# k = 0
	
	i.r<- i.r0[, as.character(k)]
	
# Initialization 
df_indiv_sim <- df_indiv_sim0
df_agg_sim   <- df_agg_sim0

	
## Aggregate values in year 1
df_agg_temp_year1 <- filter(df_indiv_sim, year == 1) %>% 
	summarise(AL_fullIdx = sum((n_actives + n_servRet) * ALx_fullIdx, na.rm = TRUE),
						AL_noIdx = sum((n_actives + n_servRet) * ALx_noIdx, na.rm = TRUE),
						B  = sum(n_servRet * B, na.rm = TRUE),
						NC = sum(n_actives * NCx_fullIdx, na.rm = TRUE)
	) 


# filter(df_indiv_sim, year == 1) %>% 
# 	summarise(AL_fullIdx = sum((n_actives + n_servRet) * ALx_fullIdx, na.rm = TRUE),
# 						AL_noIdx = sum((n_actives + n_servRet) * ALx_noIdx, na.rm = TRUE),
# 						B  = sum(n_servRet * B, na.rm = TRUE),
# 						NC = sum(n_actives * NCx_fullIdx, na.rm = TRUE),
# 						n_ret = sum(n_servRet),
# 						n_act = sum(n_actives)
# 	) 

# cash flows
df_agg_sim$AL_fullIdx[1] <- df_agg_temp_year1$AL_fullIdx
df_agg_sim$AL_noIdx[1]   <- df_agg_temp_year1$AL_noIdx
df_agg_sim$NC[1]         <- df_agg_temp_year1$NC
df_agg_sim$B[1]          <- df_agg_temp_year1$B



df_agg_sim$MA[1]           <- df_agg_sim$AL_fullIdx[1] *  MA_0_pct


df_agg_sim$UAAL_fullIdx[1] <- df_agg_sim$AL_fullIdx[1] - df_agg_sim$MA[1]
df_agg_sim$SC[1] <- amort_LG(df_agg_sim$UAAL_fullIdx[1], dr_DA, m, salgrowth_amort, end = FALSE, method = amort_method)[1]

df_agg_sim$C[1]         <- df_agg_sim$NC[1] + df_agg_sim$SC[1]



# funded ratios
df_agg_sim$FR_fullIdx[1] <- with(df_agg_sim, MA[1] / AL_fullIdx[1])
df_agg_sim$FR_noIdx[1]   <- with(df_agg_sim, MA[1] / AL_noIdx[1])

# index 
ben_idx_vec[1] <- with(df_agg_sim, min(1, max(0, (MA[1] - AL_noIdx[1])/(AL_fullIdx[1] - AL_noIdx[1])))) * idxFull_DA


# converting to list for faster speed
df_agg_sim <- unclass(df_agg_sim)


for(j in 2:nyear){
	
	# j = 1
	
  # 1. Determining MA based on the cash flows in previous year
	
	# calculating investment income based on the updated balance of MA
	df_agg_sim$I.e[j-1]    <- with(df_agg_sim, dr_DA    * (MA[j-1] + C[j-1] - B[j-1]))
	df_agg_sim$I.r[j-1]    <- with(df_agg_sim, i.r[j-1] * (MA[j-1] + C[j-1] - B[j-1])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
	df_agg_sim$I.diff[j-1] <- with(df_agg_sim, I.r[j-1] - I.e[j-1])
	
	
	df_agg_sim$MA[j] <- with(df_agg_sim, (MA[j-1] + C[j-1] - B[j-1]) + I.r[j-1]   )
  
  # 2. Calculating nominal and real liabilities 	
	df_indiv_sim %<>% 
		mutate(
			     ## Updating accured benefits for active members 
			     #  all values are for a single member
			     Bx     = ifelse(year == j & (age-ea) != 0, lag((Bx + sx * bfactor_DA) * (1 + ben_idx_vec[j-1])) , Bx),
					
					 # Determine the starting benefit level upon retirement
					 # Note that do not need to do this for retirees in year 1 
					 B = ifelse(year == j & ret_year == j & ret_year > 1, Bx, B),
					 
					 # Update benefit with index
					 B = ifelse(year == j & year > ret_year, lag(B * (1 + ben_idx_vec[j-1])), B),
					 
					 
					 # Real liability: assuming full index
					 #ALx_fullIdx = ifelse(age < age_ret, Bx * fct_ALx_fullIdx, B * ax1_age2death_fullIdx),
					 
					 # Nominal liability assuming full index
					 #ALx_noIdx   = ifelse(age < age_ret, Bx * fct_ALx_noIdx,   B * ax1_age2death_noIdx),
					 )
	
	df_agg_temp1 <- filter(df_indiv_sim, year == j) %>% 
		              mutate(
		              	# Real liability: assuming full index
		              	ALx_fullIdx = ifelse(age < age_ret, Bx * fct_ALx_fullIdx, B * ax1_age2death_fullIdx),

		              	# Nominal liability assuming full index
		              	ALx_noIdx   = ifelse(age < age_ret, Bx * fct_ALx_noIdx,   B * ax1_age2death_noIdx)) %>%
		               
	                	summarise(AL_fullIdx = sum((n_actives + n_servRet) * ALx_fullIdx, na.rm = TRUE),
	               						AL_noIdx = sum((n_actives + n_servRet) * ALx_noIdx, na.rm = TRUE)
	               						) 
		 
	df_agg_sim$AL_fullIdx[j] <- df_agg_temp1$AL_fullIdx
	df_agg_sim$AL_noIdx[j]   <- df_agg_temp1$AL_noIdx
	
	df_agg_sim$UAAL_fullIdx[j] <- df_agg_sim$AL_fullIdx[j] - df_agg_sim$MA[j]
	
	

	# 3. Calculating nominal and real funded ratio
	df_agg_sim$FR_fullIdx[j] <- with(df_agg_sim, MA[j] / AL_fullIdx[j])
	df_agg_sim$FR_noIdx[j]   <- with(df_agg_sim, MA[j] / AL_noIdx[j])
	
	
	
	# 4. Determining benefit index in year j 
  ben_idx_vec[j] <- with(df_agg_sim, min(1, max(0, (MA[j] - AL_noIdx[j])/(AL_fullIdx[j] - AL_noIdx[j])))) * idxFull_DA
	
  
	# 5 and 6. determining benefit accrual and normal cost based on salary and indexation determined in the previous step
	# df_indiv_sim %<>% 
	# 	mutate(
	# 		## Updating accured benefits for active members 
	# 		#  all values are for a single member
	# 		Bx_new = ifelse(year == j, ((sx * bfactor_DA) * (1 + ben_idx_vec[j])), Bx_new),
	# 		
	# 		# Normal costs
	# 		NCx_fullIdx = Bx_new * fct_NCx_fullIdx
	# 	)
	
	
	## aggregate benefit and normal cost
	df_agg_temp2 <- filter(df_indiv_sim, year == j) %>% 
		mutate(
			## Updating accured benefits for active members 
			#  all values are for a single member
			Bx_new = ifelse(year == j, ((sx * bfactor_DA) * (1 + ben_idx_vec[j])), Bx_new),
			
			# Normal costs
			NCx_fullIdx = Bx_new * fct_NCx_fullIdx
		) %>% 
		summarise(B  = sum(n_servRet * B, na.rm = TRUE),
							NC = sum(n_actives * NCx_fullIdx, na.rm = TRUE)
		)
	df_agg_sim$B[j]  <- df_agg_temp2$B
	df_agg_sim$NC[j] <- df_agg_temp2$NC
  
	
	# Amortization cost
	df_agg_sim$SC[j] <- amort_LG(df_agg_sim$UAAL_fullIdx[j], dr_DA, m, salgrowth_amort, end = FALSE, method = amort_method)[1]
	
	
	# Total contribution
	
	## EEC is a fixed share of total payroll
	if(EEC_type == "fixed" & cola_type != "SDRS"){
		# Employee contribution, based on payroll. May be adjusted later. 
		df_agg_sim$EEC[j] <- with(df_agg_sim, PR[j] * EECrate_fixed)
		
		if(nonNegC){
			df_agg_sim$ADC[j]    <- with(df_agg_sim, max(0, NC[j] + SC[j])) 
			df_agg_sim$ADC.ER[j] <- with(df_agg_sim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
			
			# Adjustment of EEC
			if(!EEC_fixed) df_agg_sim$EEC[j] <- with(df_agg_sim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # df_agg_sim$EEC[j] <- with(df_agg_sim, EEC[j]) else
			
		} else {
			# Allow for negative ADC and C  
			df_agg_sim$ADC[j]    <- with(df_agg_sim, NC[j] + SC[j]) 
			
			if(EEC_fixed) {df_agg_sim$ADC.ER[j] <- with(df_agg_sim, ADC[j] - EEC[j]) # EEC is fixed
			# EEC is not fixed
			# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
			} else if(with(df_agg_sim, ADC[j] > EEC[j])) {
				df_agg_sim$ADC.ER[j] <- with(df_agg_sim, ADC[j] - EEC[j]) 
				# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
			} else if(with(df_agg_sim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
				df_agg_sim$ADC.ER[j] <- 0
				df_agg_sim$EEC[j]    <- with(df_agg_sim, ADC[j])
				# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
			} else if(with(df_agg_sim, ADC[j] <= 0)) {
				df_agg_sim$ADC.ER[j] <- with(df_agg_sim, ADC[j])
				df_agg_sim$EEC[j]    <- 0
			}
		}
		
		# ERC
		df_agg_sim$ERC[j] <- with(df_agg_sim, ADC.ER[j])
	}
	
	
	
	# df_agg_sim$C[j]         <- 	df_agg_sim$NC[j] + df_agg_sim$SC[j]  
  
	# C(j)
	df_agg_sim$C[j] <- with(df_agg_sim, EEC[j] + ERC[j])
	
	# C(j) - ADC(j)
	df_agg_sim$C_ADC[j] <- with(	df_agg_sim, C[j] - ADC[j])
}

as.data.frame(df_agg_sim)
	
}

stopCluster(cl)


bind_rows(penSim_results) %>%
	mutate(runname   = runname,
				 cola_type = cola_type,
				 policy_type = policy_type,
				 return_scn  = return_scn,
				 sim     = rep(-2:nsim, each = nyear)) %>% 
	group_by(sim) %>% 
	mutate(C_PR    = 100 * C / PR,
				 NC_PR   = NC / PR) %>% 
	relocate(runname, sim, year)

}


## Run simulation

{
	start_time <- Sys.time()	
	penSim_results <- run_sim_DA()
	print(Sys.time() - start_time)
	suppressMessages(gc())
}





#*******************************************************************************
#                           ### check results ####                      
#*******************************************************************************

penSim_results %>% 
	filter(sim == 0) %>% 
	select(sim, year, AL_fullIdx, NC_PR, C_PR, FR_fullIdx, PR)

# df_indiv_sim %>% filter(start_year == 1,  ea == 31)
# df_indiv_sim %>% filter(start_year == -10)



# load("Outputs/Outputs_baseline.RData")
# outputs_list$results %>% 
# 	filter(sim == 0) %>% 
# 	select(sim, year, AL, NC, NC_PR, salary, ERC_PR)
# 
# 
# load("Outputs/Outputs_baseline_FR100.RData")
# outputs_list$results %>% 
# 	filter(sim == 0) %>% 
# 	select(sim, year, AL, NC, SC, NC_PR, salary, ERC_PR, EEC_PR)
