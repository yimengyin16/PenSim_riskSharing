

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

## Loading packages

# paramlist_ <- paramlist
# Global_paramlist_ <- Global_paramlist

assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(paramlist_,  envir = environment())

bfactor_DA
dr_DA
infl
idxFull_act_DA
idxFull_ret_DA


## add an index variable

# bfactor <- 0.022 # the 
# dr      <- 0.05
# infl    <- 0.02 # assuming constant inflation rate, expected = actual
# salgrowth <- 0.03



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
		# B1_age2ret_fullIdx = order_by(-age, cumprod(ifelse(age >= age_ret, 1, (1 + idxFull_act_DA)))),
		B1_age2ret_fullIdx   = ifelse(age > age_ret, 0, (1 + idxFull_ret_DA)^(age_ret - age)),
		
		# expected benefit for $1 starting benefit at ret age (60) assuming full indexation
		B1_age2death_fullIdx = ifelse(age < age_ret, 0, (1 + idxFull_ret_DA)^(age - age_ret)),
		
		
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


df_decrements_DA %>% 
	filter(ea == 30)



#*******************************************************************************
#                           ### select the group to model  ####                      
#*******************************************************************************

# pick a single cohort to model:
  # - start_year: 1
  # - ea: 30

# Active members
df_actives_sim0 <- 
  df_actives %>% 
	mutate(start_year = year - (age-ea),
				 yos = age - ea,
				 ret_year = start_year + (age_ret - ea)) %>% 
	filter(start_year %in% 1 & ea %in% c(30,31)|
				 start_year %in% -1 & ea %in% c(20, 21)
				 ) %>%
	filter(age < age_ret) %>% 
	
	# # TEMP: accured benefits for initial members in year 1
	# #       Use the current salary as the indexed salarly 
	# mutate(Bx = ifelse(start_year <=1 & year == 1, yos * bfactor_DA * sx, 0)
	# 			 ) %>% 
	select(start_year, ea, age, yos, year, ret_year, sx, n_actives = number.a) %>% 
	left_join(df_decrements_DA %>%
							select(-year),
						by = c("ea", "age"))
# df_actives_sim0 %>% filter(start_year <= 1, year == 1) %>% pull(n_actives) %>% sum


# Service retirees

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
df_servRet_sim0

# All starting years allowed
start_years <- c(df_servRet_sim0$start_year[df_servRet_sim0$year == 1], 1:nyear)
df_servRet_sim0 %<>%  filter( !(ret_year == 1 & (!start_year %in% start_years))) 

# select cohorts to model
df_servRet_sim0 %<>% 
	filter(start_year %in%  1 & ea %in% c(30,31)|
				 start_year %in% -1 & ea %in% c(20, 21) |
				 start_year %in% -10 & ea %in% c(49)
	) %>%
	left_join(df_decrements_DA %>%
							select(-year),
						by = c("ea", "age"))


df_servRet_sim0 %>% 
	arrange(start_year, ea) %>% 
	filter(start_year == 1)



## combine actives and retirees

df_indiv_sim0 <- 
	bind_rows(
		df_actives_sim0,
		df_servRet_sim0
	) %>% 
	arrange(start_year, ea, age)



# TEMP: accured benefits for initial members in year 1
#       Use the current salary as the indexed salarly 
# TODO: initial accrued benefit consistent with salary history before year 1 

df_indiv_sim0 %<>% 
	mutate(
		     # accrued benefit for initial active members
		     Bx        = ifelse(start_year <= 1 & year == 1 & age < age_ret, yos * bfactor_DA * sx, 0),
  			 Bx_new    = 0,
				 # benefit for initial retirees: deleting all benefit values for non-initial members
				 B = ifelse(ret_year > 1 | year > 1, 0, B),
				 
				 ALx_fullIdx = ifelse(age< age_ret, Bx * fct_ALx_fullIdx, B * ax1_age2death_fullIdx),
	  		 ALx_noIdx   = ifelse(age< age_ret, Bx * fct_ALx_noIdx,   B * ax1_age2death_noIdx),
				 
				 NCx_fullIdx = 0,
				 
				 n_actives = na2zero(n_actives),
				 n_servRet = na2zero(n_servRet)
				 
		  	 ) %>% 
	relocate(start_year, ea,age, yos, year, n_actives, n_servRet, sx, B, Bx, Bx_new, ALx_fullIdx, ALx_noIdx) 


df_indiv_sim0 %>% 
	filter(start_year == -10)
df_indiv_sim0 


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
#                           ### valuation  ####                      
#*******************************************************************************

# Need to calculat the following in each period
# Accrued benefit
# Nominal liability
# real liability
# New benefit accrual
# Normal cost
# index for current year. 



nyear <- max(df_indiv_sim0$year)

## Create a series of hypothetical benefit indices:
ben_idx_vec <- data.frame(year = unique(df_actives$year),
													ben_idx = 0.02)
ben_idx_vec$ben_idx[c(6:10, 21:25, 36:40)] <- 0


df_indiv_sim0 %<>% 
	mutate(ben_idx = 0.02)




## Create a data frame for aggregate valuation results
df_agg_sim0 <- 
	data.frame(year =  1:nyear) %>% 
	mutate(AL_fullIdx = 0,
				 AL_noIdx   = 0,
				 MA         = 0,
				 FR_fullIdx = 0,
				 FR_noIdx   = 0,
				 NC         = 0,
				 SC         = 0,
				 C          = 0,
				 B          = 0
				 )
	
#df_agg_sim0$AL_fullIdx[1] <- filter(df_indiv_sim0, year == 1)$ALx_fullIdx %>% sum
#df_agg_sim0$AL_noIdx[1]   <- filter(df_indiv_sim0, year == 1)$ALx_noIdx   %>% sum


df_indiv_sim <- df_indiv_sim0
df_agg_sim   <- df_agg_sim0 %>% unclass()


for(j in 1:nyear){

	
	# df_indiv_sim0 %>%
	# 	filter(start_year == -1, ea == 20)
	# 
	# df_indiv_sim %>%
	# 	filter(start_year == -10)
	
 
	df_indiv_sim %<>% 
		mutate(
			     ## Updating accured benefits for active members 
			     #  all values are for a single member
			     Bx     = ifelse(year == j & j != 1, lag((Bx  + sx * bfactor_DA) * (1 + ben_idx)) , Bx),
					 Bx_new = ifelse(year == j, ((sx * bfactor_DA) * (1 + ben_idx)), Bx_new),
					 
					 
					 # Determine the starting benefit level upon retirement
					 # Note that do not need to do this for retirees in year 1 
					 B = ifelse(year == j & ret_year == j & ret_year > 1, Bx, B),
					 
					 # Update benefit with index
					 B = ifelse(year == j & year > ret_year, lag(B * (1 + ben_idx)), B),
					 
					 # Real liability: assuming full index
					 ALx_fullIdx = ifelse(age < age_ret, Bx * fct_ALx_fullIdx, B * ax1_age2death_fullIdx),
					 
					 # Nominal liability assuming full index
					 ALx_noIdx   = ifelse(age < age_ret, Bx * fct_ALx_noIdx,   B * ax1_age2death_noIdx),
					 
					 # Normal costs
					 NCx_fullIdx = Bx_new * fct_NCx_fullIdx
					 )
	
	df_agg_temp <- filter(df_indiv_sim, year == j) %>% 
	               	summarise(AL_fullIdx = sum((n_actives + n_servRet) * ALx_fullIdx, na.rm = TRUE),
	               						AL_noIdx = sum((n_actives + n_servRet) * ALx_noIdx, na.rm = TRUE),
	               						B  = sum(n_servRet * B, na.rm = TRUE),
	               						NC = sum(n_actives * NCx_fullIdx, na.rm = TRUE)
	               						) 
		 
	
	df_agg_sim$AL_fullIdx[j] <- df_agg_temp$AL_fullIdx
	df_agg_sim$AL_noIdx[j]   <- df_agg_temp$AL_noIdx
	df_agg_sim$B[j]          <- df_agg_temp$B
	df_agg_sim$NC[j]         <- df_agg_temp$NC
  
	
	
	
	if(j == 1) {
		df_agg_sim$MA[j] <- df_agg_sim$AL_fullIdx[j]
		} else {
		df_agg_sim$MA[j] <- 	with(df_agg_sim, (MA[j-1] + NC[j-1] - B[j-1]) * (1 + dr_DA))
	}
	
	df_agg_sim$FR_fullIdx[j] <- with(df_agg_sim, MA[j] / AL_fullIdx[j])
	df_agg_sim$FR_noIdx[j]   <- with(df_agg_sim, MA[j] / AL_noIdx[j])
	
}




df_indiv_sim %>% filter(start_year == -1, ea == 20)

df_indiv_sim %>% filter(start_year == 1, ea == 31)

df_indiv_sim %>% filter(start_year == -10)

df_agg_sim %>% as.data.frame()







# 
# 
# 
# # loop for actives
# for(j in 1:31){
# 	
# 	df_indiv1 %<>% 
# 	
# 		mutate(
# 		
# 		## Updating accured benefits for active members 
# 		#   all values are for a single member
# 		Bx     = ifelse(year == j & j != 1, ( (Bx  + sx * bfactor_DA) * (1 + ben_idx))[year == j-1], Bx),
# 	  
# 		Bx_new = ifelse(year == j, ((sx * bfactor_DA) * (1 + ben_idx)), Bx_new),
# 		
# 	  # Real liability: assuming full index
# 	  ALx_fullIdx = Bx * fct_ALx_fullIdx,
# 	  
# 	  # Nominal liability assuming full index
# 	  ALx_noIdx = Bx * fct_ALx_noIdx,
# 		
# 		# Normal costs
# 		NCx_fullIdx = Bx_new * fct_NCx_fullIdx,
# 		
# 		## Aggregate values
# 		# AL_fullIdx_tot = n_actives *  ALx_fullIdx,
# 		# AL_noIdx_tot   = n_actives *  ALx_noIdx,
# 		# NC_tot         = n_actives *  NCx_fullIdx,
# 		# B_tot          = n_actives *  B
# 		)
# 	
#    df_indiv1$AL_fullIdx_tot[j] = with(df_indiv1, n_actives[j] *  ALx_fullIdx[j])
#    df_indiv1$AL_noIdx_tot[j]   = with(df_indiv1, n_actives[j] *  ALx_noIdx[j])
#    df_indiv1$NC_tot[j]         = with(df_indiv1, n_actives[j] *  NCx_fullIdx[j])
#    df_indiv1$B_tot[j]          = with(df_indiv1, n_actives[j] *  B[j])
# 	
# 	if(j != 1) df_indiv1$MA[j] <- with(df_indiv1, (MA[j-1] + NC_tot[j-1] - B_tot[j-1]) * (1 + dr_DA) )
# 	
# }
# 
# 
# df_indiv1
# 
# df_indiv1 %<>% 
# 	mutate(B     = ifelse(age ==  60, Bx, 0),
# 				 B_tot = ifelse(age ==  60, n_servRet * B, B_tot)
# 				 )
# df_indiv1
# 
# # loop for retirees
# for(j in 31:71){
# 	
# 	df_indiv1 %<>% 
# 		# Updating accured benefits for active members  
# 		mutate(B = ifelse(year == j & j!= 31, (B * (1+ben_idx))[year == j-1], B),
# 					 
# 					 # Real liability: assuming full index
# 					 ALx_fullIdx = ifelse(year < j, ALx_fullIdx, B * ax1_age2death_fullIdx),
# 					 
# 					 # Nominal liability assuming full index
# 					 ALx_noIdx   = ifelse(year < j, ALx_noIdx,   B * ax1_age2death_noIdx),
# 					 
# 					 ## Aggregate values
# 					 # AL_fullIdx_tot = N *  ALx_fullIdx,
# 					 # AL_noIdx_tot   = N *  ALx_noIdx,
# 					 # NC_tot         = N * NCx_fullIdx,
# 					 # B_tot          = N * B
# 		)
# 	 
# 	 df_indiv1$AL_fullIdx_tot[j] = with(df_indiv1, n_servRet[j] * ALx_fullIdx[j])
# 	 df_indiv1$AL_noIdx_tot[j]   = with(df_indiv1, n_servRet[j] * ALx_noIdx[j])
# 	 df_indiv1$NC_tot[j]         = with(df_indiv1, n_servRet[j] * NCx_fullIdx[j])
# 	 df_indiv1$B_tot[j]          = with(df_indiv1, n_servRet[j] * B[j])
# 	 
# 	 if(j != 31) df_indiv1$MA[j] <- with(df_indiv1, (MA[j-1] + NC_tot[j-1] - B_tot[j-1]) * (1 + dr_DA) )
# 	
# }
# 
# 
# 
# 
# df_indiv1 %>% 
# 	select(-qxm, -qxt, -qxd, -qxr, -qxt, -starts_with("p_"), -starts_with("fct") )
# 
# 
# df_indiv1 %>% 
# 	filter(age>=30) %>% 
# 	mutate(FR_real = MA / AL_fullIdx_tot,
# 				 FR_nom  = MA / AL_noIdx_tot,
# 				 s_N = 1000*N/lag(N),
# 				 s_nret = 1000*n_servRet/lag(n_servRet)
# 				 
# 				 ) %>% 
# 	select(year, age, FR_real, FR_nom,  AL_noIdx_tot, AL_fullIdx_tot, MA, NC_tot, B_tot, N, B,Bx, fct_ALx_fullIdx, ax1_age2death_fullIdx, N, n_servRet, s_N, s_nret) # NCx_fullIdx, B) #ALx_fullIdx, B1_age2death_fullIdx ,fct_dr_ret2age, p_ret2age, ax1_age2death_fullIdx,pxT)
# 
