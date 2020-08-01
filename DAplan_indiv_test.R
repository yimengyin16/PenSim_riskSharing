

#*******************************************************************************
#                           ### Notes        ####                      
#*******************************************************************************

#' Test modeling of DA plan for an individual
#' 
#' Assumptions:
#'  - single retirement age 60. 
#'  - Mortality and termination rate
#'  - service retirement benefit only. (no deferred retirement benefit) 



#*******************************************************************************
#                           ### Initialization ####                      
#*******************************************************************************

## Loading packages
source("libraries.R")

## loading demographic data
load("Inputs/riskShaing_demographics_100y.RData")


#*******************************************************************************
#                           ### Global parameters  ####                      
#*******************************************************************************

bfactor <- 0.02 
dr      <- 0.05
infl    <- 0.02 # assuming constant inflation rate, expected = actual
# salgrowth <- 0.03




#*******************************************************************************
#                           ### Individual data  ####                      
#*******************************************************************************

# pick a single cohort to model:
  # - start_year: 1
  # - ea: 30


df_indiv <- 
	bind_rows(
	df_actives %>% 
		mutate(start_year = year - (age-ea),
					 yos = age - ea) %>% 
		filter(start_year == 1, ea == 30) %>% 
	  mutate(sx = 100 * sx / sx[yos==0]) %>% 
		select(year, ea, age, yos, sx),
	
	
	df_retirees %>% 
		mutate(start_year = year - (age-ea),
					 sx = 0) %>% 
		filter(start_year == 1, ea == 30, year.retire == 31) %>% 
		select(year, ea, age, sx = 0)
)



df_indiv %<>%  
	left_join(decrement %>% 
	          	filter(ea == 30) %>% 
							select(ea, age, qxm, qxt, qxd, qxr),
						by = c("ea", "age")
	)


df_indiv %<>% 
	mutate(
		# Prob of survival in each year
		pxT = ifelse(age < 60, 
								 1 - qxm - qxt - qxd,
								 1 - qxm),
		# Prob of survival up to retirement (age 60)
		p_age2ret = order_by(-age, cumprod(ifelse(age >= 60, 1, pxT))),
		
		# Prob for retirees to survive up a certain age
		p_ret2age = cumprod(ifelse(age <= 60, 1, pxT)),
		
		
		# discount factor up to retirement (age 60)
		fct_dr_x2ret =  ifelse(age >= 60, 1, 1/(1+dr)^(60-age)),
		
		# discount factor: retirees up a certain age
		fct_dr_ret2age = ifelse(age < 60, 1, 1/(1+dr)^(age-60)),
		
		# expected benefit for $1 current accrued benefit  at age 60 assuming full indexation
		B1_age2ret_fullIdx = order_by(-age, cumprod(ifelse(age >= 60, 1, (1+infl)))),
		
		# expected benefit for $1 starting benefit at age 60 assuming full indexation
		B1_age2death_fullIdx = ifelse(age < 60, 0, (1+infl)^(age-60)),
		
		
		# annuity value for $1 benefit at age 60, assuming full indexation 
		ax1_age2death_fullIdx = order_by(-age, cumsum(B1_age2death_fullIdx * fct_dr_ret2age * p_ret2age )) / (B1_age2death_fullIdx * fct_dr_ret2age * p_ret2age),
		ax1_age2death_fullIdx = ifelse(age >= 60, 	ax1_age2death_fullIdx, 0),
		
		# annuity value for $1 benefit at age 60, assuming no indexation
		ax1_age2death_noIdx = order_by(-age, cumsum(ifelse(age < 60, 0, fct_dr_ret2age * p_ret2age ))) / (fct_dr_ret2age * p_ret2age),
		ax1_age2death_noIdx = ifelse(age >= 60, 	ax1_age2death_noIdx, 0)
		)
	
df_indiv

# 1 + 1.02*0.708/(1+dr)
# 1 +  1.02 * 0.728/(1+dr) + 1.02^2 *0.708 * 0.728/(1+dr)^2

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


## Create a series of hypothetical benefit indices:
ben_idx_vec <- rep(infl, nrow(df_indiv))
ben_idx_vec[c(6:10, 21:25, 36:40)] <- 0

df_indiv %<>% 
	mutate(ben_idx = ben_idx_vec,
				 Bx = 0,
				 Bx_new = 0,
				 B  = 0)


df_indiv1 <- df_indiv

for(j in 2:31){
	
	df_indiv1 %<>% 
		# Updating accured benefits for active members  
		mutate(
		Bx = ifelse(year == j, ( (Bx  + sx * bfactor) * (1 + ben_idx))[year == j-1], Bx),
	  
		Bx_new = ifelse(year == j, ((sx * bfactor) * (1 + ben_idx))[year == j-1], Bx_new),
		
	  # Real liability: assuming full index
	  ALx_fullIdx = Bx * B1_age2ret_fullIdx * ax1_age2death_fullIdx[age == 60],
	  
	  # Nominal liability assuming full index
	  ALx_noIdx   = Bx * ax1_age2death_noIdx[age == 60]
		
		# Normal costs
		
		)
}

df_indiv1 %<>% 
	mutate(B = ifelse(age ==  60, Bx, 0))
df_indiv1

for(j in 32:71){
	
	df_indiv1 %<>% 
		# Updating accured benefits for active members  
		mutate(B = ifelse(year == j, (B * (1+ben_idx))[year == j-1], B),
					 
					 # Real liability: assuming full index
					 ALx_fullIdx = ifelse(year < j, ALx_fullIdx, B * ax1_age2death_fullIdx),
					 
					 # Nominal liability assuming full index
					 ALx_noIdx   = ifelse(year < j, ALx_noIdx,   B * ax1_age2death_noIdx)
		)
}




df_indiv1 %>% 
	select(-qxm, -qxt, -qxd, -qxr, -qxt, -pxT)















