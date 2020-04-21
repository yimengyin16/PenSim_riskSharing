

run_sim_regular <- function(paramlist_ = paramlist,
														Global_paramlist_ = Global_paramlist){

# paramlist_ <- paramlist
# Global_paramlist_ <- Global_paramlist

assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(paramlist_,  envir = environment())


#******************************************************************************* 
#                 Processing data from Model_Main   ####
#*******************************************************************************

load("Inputs/riskShaing_demographics.RData")
# df_actives
# df_retirees
# decrement


## Decrement table
df_decrement <- 
	decrement %>% 
	  ungroup %>% 
		# filter(ea == 20) %>% 
		select(age,ea, yos, qxm, qxm.r, qxt, qxd) %>%
	  mutate(qxT = qxm + qxt + qxd)
df_decrement %>% head



## Retirees 
df_retirees %<>% 
	filter(!is.na(year)) %>% 
  rename(B_ret  = B.r,
	  		 AL_ret = ALx.r,
		  	 n_ret  = number.r,
  			 ret_year = year.retire) %>%
	mutate(start_year = year - (age - ea),
				 ret_age    = age - (year - ret_year)) %>% 
	filter(ret_year == 1 | ret_age == 60, 
				 !(ret_year < 1 & n_ret == 0),    # excluding some unnecessary rows
				 year <= nyear) %>% 
	mutate_at(vars(B_ret, AL_ret, n_ret), list(na2zero)) %>% 
	select(start_year, ea, age, year,ret_age, ret_year, everything()) %>% 
	ungroup() %>% 
	arrange(year, ret_year, ea, age) # one-to-one mapping between retirement year and age with single retirement age, 
df_retirees %>% head

# Notes: 
#  - For existing members in year 1, only the benefit values in year 1 will be used. 
#  - For all future members (start_year >= 2), only the benefit values at retirement age will be used. 
#  - ax.r is the annuity factor at age x (PV of future annuity payments for $1's payment at current age). 


# All starting years allowed
start_years <- c(df_retirees$start_year[df_retirees$year == 1], 1:nyear)
df_retirees	%<>%  filter( !(ret_year == 1 & (!start_year %in% start_years))) 

# df_retirees %>% filter(year == 2) 
# df_retirees %>% filter(start_year == 1, ea ==20) 


## Active members
df_actives %<>% 
	rename(n_act  = number.a,
				 salary = sx,
				 AL_act = ALx,
				 NC_act = NCx,
				 PVB_act  =  PVFBx.r,
				 B_retAge = Bx.ret) %>%
	mutate(start_year = year - (age - ea),
				 ratio.NC_PVB = NC_act / PVB_act,
				 ratio.AL_PVB = AL_act / PVB_act) %>% 
	select(start_year, ea, age, year,  n_act, salary, B_retAge, PVB_act, AL_act, NC_act, ratio.NC_PVB, ratio.AL_PVB) %>% 
  as_tibble()
df_actives

# df_retirees %>% filter(start_year == 5, ea ==20)
# get_PVB_retiree(60, 1, 0.015, 0.075) %>% print

#df_actives %>% filter(start_year == 1, ea == 25) %>% select(ea, age, year, ratio.NC_PVB, ratio.AL_PVB)
#df_actives %>% filter(start_year == 5, ea == 25) %>% select(ea, age, year, ratio.NC_PVB, ratio.AL_PVB)






#*******************************************************************************
#                    Preparation: Active members  ####
#*******************************************************************************

# Valuations of active member can be done outside the loop if the baseline COLA rate
# is constant over time

sim_actives_yearsum <-
df_actives %>%
	group_by(year) %>%
	summarise(AL_act  = sum(AL_act * n_act),
						NC      = sum(NC_act * n_act),
						PVB_act = sum(PVB_act* n_act),
						salary  = sum(salary* n_act),
						n_act   = sum(n_act))
sim_actives_yearsum



#******************************************************************************* 
#                      Preparation: Retirees  ####
#*******************************************************************************

# Only keep B_ret for initial retirees at year 1 and future retirees at their retirement year
# Note that the retirement year for initial retirees is set to 1 for convenience.
# Therefore the condition of ret_year == year can cover both cases we want to keep

# Total liability for retirees in year 1, to be used to determine the year-1 asset value.
AL_ret_year1 <- 
	df_retirees %>% 
	filter(year == 1) %>% 
	summarise(AL_ret = sum(AL_ret * n_ret)) %>% 
	pull(AL_ret)

# Total number of retirees by year

df_n_ret <- 
df_retirees %>% 
	group_by(year) %>% 
	summarize(n_ret = sum(n_ret))
df_n_ret

# df to be used in the simulation
sim_retirees0 <- 
	df_retirees %>% 
	mutate(B_ret  = ifelse(ret_year == year , B_ret, 0), 
				 AL_ret = ifelse(year == 1 , AL_ret, 0)) %>% 
	group_by(start_year, ea)

#x <- sim_retirees %>% filter(ret_year == 1, year == 1)
#x$n_ret %>% sum
#sim_retirees %>% filter(ret_year == 2, year == 3)



#******************************************************************************* 
#                   COLA-AL relationship with "$1 benefit"   ####
#*******************************************************************************

## Calculate PVB of "unit benefit" at all ages in retirement with a grid of COLA rates

# cola grid from 0 to 3% by 0.1%
cola_grid <- seq(0, by = 0.001, len =31)

# PVB for each cola rate for $1 benefit
df_PVB_colaGrid <- sapply(cola_grid, function(x) mapply(get_PVB_retiree, age_ret:age_max, 
																												MoreArgs = list(benefit_init = 1, i = dr, cola_assumed = x, 
																																				decrement = df_decrement, age_max_ = age_max))) %>% t
colnames(df_PVB_colaGrid) <- age_ret:age_max

df_PVB_colaGrid %<>% 
  as_tibble() %>% 
	mutate(cola = cola_grid) %>% 
	select(cola, everything()) %>% 
	gather(age, PVB1, -cola) %>% 
	mutate(age = factor(age, levels = age_ret:age_max)) %>% 
	select(age, cola, PVB1)
df_PVB_colaGrid


## COLA-AL relationship for retirees (group by age)

# Fit the COLA-AL curve for each age group using polynomials of order 3
p_coeff_retirees <- 
	df_PVB_colaGrid %>% 
	split(df_PVB_colaGrid$age) %>% 
	map( ~ lm(PVB1 ~ cola + I(cola^2) + I(cola^3), data = .)) %>% 
	map(coefficients) %>% 
	bind_rows() %>% 
	t %>% 
  as_tibble() %>% 
	mutate(age = age_ret:age_max) %>% 
	rename(b0 = 1, b1=2, b2=3, b3 =4)

p_coeff_retirees



## COLA-AL relationship for actives (group by (ea, age))

# PV of $1 benefit at the retirement age
ax.r_ret <- filter(df_retirees ,age == age_ret)[1,] %>% pull(ax.r)

# calculate AL as a percentage of PVB up retirement (PVBr) for all (ea, age) groups
df_actives %<>% 
	mutate(PVBr = B_retAge * ax.r_ret,
				 ratio.AL_PVBr = AL_act / PVBr)


# For each (ea, age), create a grid for COLAs, 
#   and add the PVBr associated with those COLA values,
#   then calculate AL for each cell. 
df_AL.act_colaGrid <- 
	ldply(cola_grid, 
				 function(x){
				 	df_actives %>% 
				 		filter(year == 1) %>% # 820 cells
				 		select(ea, age, ratio.AL_PVBr) %>% 
				 		mutate(cola = x)}
				 ) %>% 
	left_join(filter(df_PVB_colaGrid, age == age_ret) %>% select(cola, PVB1r = PVB1), by = "cola") %>% 
	mutate(AL1 = PVB1r * ratio.AL_PVBr) %>% 
	unite(ea_age, ea, age, sep = "_")

# Estimate the coeffients of the polynomial for the AL-COLA relationship. 
p_coeff_actives <-  
df_AL.act_colaGrid %>% 
	split(df_AL.act_colaGrid$ea_age) %>% 
	map( ~ lm(AL1 ~ cola + I(cola^2) + I(cola^3), data = .)) %>% 
	map(coefficients) 
grp_names <- names(p_coeff_actives)
p_coeff_actives %<>% 
	bind_rows() %>% 
	t %>% 
	as_tibble %>% 
	mutate(ea_age = grp_names) %>% 
	separate(ea_age, c("ea", "age"), sep = "_", convert = TRUE) %>% 
	rename(b0 = 1, b1=2, b2=3, b3 =4)

p_coeff_actives



#******************************************************************************* 
#                           Preparation: loop  ####
#*******************************************************************************

# Setup df for annual values 

penSim0 <- 
	sim_actives_yearsum %>% 
	mutate(
		     AL_ret = ifelse(year == 1, AL_ret_year1, 0),
		     B      = 0,
		     n_ret  = df_n_ret$n_ret,
		     
		     cola_actual = 0,
		     AL     = ifelse(year == 1, AL_act + AL_ret, 0),
				 MA     = 0,
				 AA     = 0,
				 UAAL   = 0,
				 FR_MA  = 0,
				 FR_AA  = 0,
				 
				 EUAAL  = 0,
				 LG     = 0,
				 AM     = 0, # amortization basis
				 
				 SC     = 0,
				 EEC    = 0,
				 ERC    = 0,
				 C      = 0,
				 ADC    = 0, 
				 ADC.ER = 0,
				 
				 C_ADC  = 0,
				 I.r    = 0,                         
				 I.e    = 0, 
				 I.dif  = 0,
				 Ia     = 0,
				 Ib     = 0,
				 Ic     = 0,
				 
				 dr     = dr,
				 i.r    = 0,
				 
				 # DC_MA  = 0,
				 
				 i.r_geoReturn   = 0,
				 infl_stoch      = 0,
				 
				 # For PSERS policy
				 sharedRisk.rate = 0,
				 SharedRiskEval  = 0,
				 
				 # For SDRS
				 AL_act_baseline = 0,
				 AL_ret_baseline = 0,
				 
				 AL_solved   = 0,
				 AL_baseline = 0,
				 
				 UAAL_SDRS  = 0,
				 FR_MA_solved   = 0,
				 FR_MA_baseline = 0,
				 
				 AM_baseline  = 0, #based on baseline COLA
				 AM_min       = 0, #based on minimum COLA
				 
				 SC_SDRS      = 0,
				 
				 NC.EE = 0, 
				 NC.ER = 0,
				 SC.EE = 0,
				 SC.ER = 0
				 ) 

penSim0


## Evaluation year for shared risk EEC: PASERS policy
penSim0$SharedRiskEval <- seq_len(nyear) %% 3 == 0  # TRUE in the year to determine if the EEC rate should be changed



#******************************************************************************* 
#                     Amortization and asset smoothing                      ####
#*******************************************************************************

## matrix for amortization costs
SC_amort0 <- matrix(0, nyear + m, nyear + m)

## asset smoothing
s.vector <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector  # a vector containing the porportion of 



#******************************************************************************* 
#                     Investment returns                                    ####
#*******************************************************************************

# Asset-shock scenario
i.crisis <- rep(i.mean - i.sd^2/2, nyear)
i.crisis[2:5] <- c(-0.24, 0.12, 0.12, 0.12)


# Stochastic returns
set.seed(1234) ;i.r <- matrix(rnorm(nyear*nsim, i.mean, i.sd), nyear, nsim)
i.r <- cbind(rep(i.mean - i.sd^2/2, nyear), i.r)
i.r <- cbind(i.crisis, i.r)
colnames(i.r) <- -1:nsim
i.r_ <- i.r


## Geometric return for shared-risk employee contribution 
# (currently only used in PSERS policy)

# i.r_supplement <-  
# 	cbind(rep(paramlist$i, 5),
# 				matrix(c(0.0343, 0.0796, 0.1491, 0.0304, 0.0129), 5, Global_paramlist$nsim + 1))

i.r_supplement <- matrix(dr, 5, Global_paramlist$nsim + 2)

i.r_geoReturn <- rbind(i.r_supplement, i.r) %>% 
	as.data.frame %>% 
	mutate_all(list(~ get_rollingReturns(., "moving", 10)))

i.r_geoReturn[1:9, ] <- rbind(i.r_supplement, i.r)[1:9,] %>% 
	as.data.frame %>% 
	mutate_all(list(~ get_rollingReturns(., "expanding"))) %>% 
	as.matrix()

i.r_geoReturn <- i.r_geoReturn[-(1:5),]
i.r_geoReturn_ <- i.r_geoReturn
#i.r_geoReturn 


## Generating stochastic inflation
set.seed(1234)
infl_stoch <- matrix(rnorm(nyear*(nsim), infl_mean, infl_sd), nyear, nsim)
infl_stoch <- cbind(matrix(infl_mean, nyear, 2), infl_stoch)
colnames(infl_stoch) <- -1:nsim

infl_stoch_ <- infl_stoch


#******************************************************************************* 
#                              Simulation                                   ####
#*******************************************************************************

cl <- makeCluster(ncore) 
registerDoParallel(cl)


penSim_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr", "magrittr", "Rcpp")) %dopar% {

	# k <- -1 # for simulation runs
	
	penSim   <- penSim0
	SC_amort <- SC_amort0 
	sim_retirees <- sim_retirees0
	
	penSim[["i.r"]] <- i.r_[, as.character(k)]
	penSim[["i.r_geoReturn"]] <- i.r_geoReturn_[, as.character(k)]
	penSim[["infl_stoch"]] <- infl_stoch_[, as.character(k)]
	
	source("Functions.R")

	
for (j in 1:nyear){
	 
	
	# j <- 1
	
	#***********************************
	#           Asset value            # 
	#***********************************
	
	# MA(j) and EAA(j) 
	if(j == 1) {
		
		penSim$MA[j]  <- ifelse(k == -1, penSim$AL[j],
																			switch(init_MA, 
																						 MA = MA_0,                        # Use preset value
																						 AL = penSim$AL[j],                # Assume inital fund equals inital liability.
																						 AL_pct = penSim$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
														)
		
		penSim$AA[j]  <- switch(smooth_method,
		  											method1 =  with(penSim, MA[j]),   # we may want to allow for a preset initial AA.
			  										method2 =  with(penSim, MA[j])
				  									)
		
		} else {
		
		penSim$MA[j]  <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
		
		penSim$AA[j]  <- switch(smooth_method,
														method1 = with(penSim, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),
														method2 = with(penSim, MA[j]) 
														)
		}
	
 

	#**********************************************
	#       Liability and funded status           # 
	#**********************************************
	
	# Determine current year's benefit payment and AL for retirees
	if(j > 1) {
			sim_retirees <- 
			mutate(sim_retirees, 
						 B_ret  = ifelse(year == j & year > ret_year, B_ret[year == j - 1] * (1 + penSim$cola_actual[j - 1]), B_ret),
						 AL_ret = B_ret * ax.r)
	} 
	
	sim_retirees0
	
	##Exploring faster ways to update B and AL for retirees **********************
	# 
	# if(j > 1) {
	# 
	# 	sim_retirees %<>% mutate(IfUpdate = (year == j) & (year > ret_year))
	#    
	# 	cola_actual_current <- penSim$cola_actual[j - 1]
	# 	
	# 	sim_retirees <-
	# 		mutate(sim_retirees, B_ret  = ifelse(IfUpdate, B_ret[year == j - 1] * (1 + cola_actual_current), B_ret),
	# 					 AL_ret = B_ret * ax.r)
	# }
	# 
	# 
	# sim_retirees %>% filter(start_year == 4, ea == 44)
	# 
	# sim_retirees %>% mutate(B_ret_update = B_ret* cola_actual_current)
	# 
	# n <- 2
	# filter(sim_retirees, year == n) 
	# filter(sim_retirees, year == n+1, year> ret_year)
	# 
	# sim_retirees[sim_retirees$year == n]
	# sim_retirees %>% filter(year == 4)
	# sim_retirees %>% filter(year == 2, year > ret_year)
	# sim_retirees %>% filter(start_year == -33, ea == 25)	
	#
	##****************************************************************************
	
  # Calculate total benefit and AL
	penSim$AL_ret[j] <- filter(sim_retirees, year == j) %>% ungroup() %>% summarise(AL_ret = sum(AL_ret * n_ret)) %>% pull(AL_ret)
	penSim$B[j]      <- filter(sim_retirees, year == j) %>% ungroup() %>% summarise(B      = sum(B_ret * n_ret))  %>% pull(B)
	
	
	# Total liability: actives and retirees  
	penSim$AL[j] <- with(penSim, AL_act[j] + AL_ret[j])
	
	
	# Funded ratios
	penSim$FR_MA[j] <- with(penSim, MA[j] / AL[j])
	penSim$FR_AA[j] <- with(penSim, AA[j] / AL[j])
	
	
	# UAAL
	penSim$UAAL[j] <- with(penSim, AL[j] - AA[j]) 
	
	
	
	
	#**********************************************
	#       Determining (contingent) COLA         # 
	#**********************************************
	# cola determined based on previous year's conditioning variable(s) and inflation 
	
	# Assume that cola for year 1 is equal to the baseline cola
	if(j == 1) penSim$cola_actual[j] <- cola_baseline
	
	# Constant cola:
	if(cola_type == "constant") {penSim$cola_actual[j] <- cola_baseline}
	
	
	# COLA indexed to inflation
	if(cola_type == "Infl") penSim$cola_actual[j] <- max(min(penSim$infl_stoch[j-1], cola_max_FR))
	
	
	# return based COLA
	if(j > 1 & cola_type == "return"){
		
		if(penSim$i.r[j-1] >= dr) penSim$cola_actual[j - 1] <- cola_max_return else penSim$cola_actual[j - 1] <- cola_min_return
		
	}
	
	# funded ratio based COLA
	if(j > 1 & cola_type == "FR" & infl_type == "constant"){
		if(penSim$FR_MA[j-1] >= FR_threshold_FR) penSim$cola_actual[j - 1] <- cola_max_FR else penSim$cola_actual[j-1] <- cola_min_FR
	}
	
	if(j > 1 & cola_type == "FR" & infl_type == "stochastic"){
		if(penSim$FR_MA[j-1] >= FR_threshold_FR) penSim$cola_actual[j - 1] <- 
				max(min(penSim$infl_stoch[j-1], cola_max_FR), cola_min_FR) else penSim$cola_actual[j-1] <- cola_min_FR
	}
	
	
	# funded ratio based COLA: ramp 1	
	if(j > 1 & cola_type == "FRramp1"){
		penSim$cola_actual[j - 1] <-  max(cola_min_FRramp, cola_max_FRramp - FRstepLength_FRramp * max(0, (FR_threshold_FRramp - penSim$FR_MA[j-1]))/FRstep_FRramp)
	}
	
	
	# funded ratio based COLA: ramp 2
	if(j > 1 & cola_type == "FRramp2"){
		penSim$cola_actual[j - 1] <- max(cola_min_FRramp2, cola_max_FRramp2 - FRstepLength_FRramp2 * max(0, (FR_threshold_FRramp2 - penSim$FR_MA[j-1]))/FRstep_FRramp2)
	}
	
	
	#**********************************************
	#                SDRS COLA policy          # 
	#**********************************************
	
	#### Baseline liability	  
	
	penSim$AL_baseline[j] <- with(penSim, AL[j]) # Note that the regular AL is calculated with the baseline COLA (ax.r based on baseline cola)
	penSim$FR_MA_baseline[j] <- with(penSim, MA[j] / AL_baseline[j])
	# penSim$FR_MA_baseline[j]
	
	
	#### Solve for COLA payable
	
	# weights for constructing a single curve
	weight_actives  <- filter(df_actives, year == j) %>% mutate(weight = n_act * B_retAge) %>% select(ea, age, weight)
	weight_retirees <- filter(sim_retirees, year == 1) %>% group_by(age) %>% summarise(weight = sum(B_ret * n_ret)) 
	
	
	# Constructing a single COLA-AL curve
	p_coeff_single <- 
  bind_rows(
	left_join(p_coeff_actives,  weight_actives,  by = c("ea", "age")),
	left_join(p_coeff_retirees, weight_retirees, by = c("age"))
  ) %>% 
		mutate(weight = na2zero(weight)) %>% 
		summarise(b0 = sum(b0 * weight),
							b1 = sum(b1 * weight),
							b2 = sum(b2 * weight),
							b3 = sum(b3 * weight)
		)
	# p_coeff_single
	# predict(polynomial(p_coeff_single), 0.015)/penSim$AL[j]	# very close
	
	
	# Using the single curve to calculate cola for a target value of AL
	
	# update the polynomial coefficients for the target AL
	p_coeff_single_solve <- p_coeff_single
	p_coeff_single_solve[1] <- p_coeff_single[1] - penSim$MA[j]
	
	# Define and solve polynomial
	p_single <- polynomial(p_coeff_single_solve)
	y_single <- solve(p_single)
	
	# Extract the real root (3rd degree polynomials always have at least one real root, and given the complex coefficients it is unlikely that all three roots are real)
	cola_solved <- Re(y_single[Im(y_single)==0])  
	
	
	# Determine COLA for the next year
	
	if(penSim$FR_MA_baseline[j] >= FR_threshold_SDRS) {
		penSim$cola_actual[j]  <- cola_max_SDRS
		penSim$AL_solved[j]    <-  predict(polynomial(p_coeff_single), cola_baseline)
		penSim$FR_MA_solved[j] <- with(penSim, MA[j] / AL_solved[j])
		
	}
	
	if(penSim$FR_MA_baseline[j] < FR_threshold_SDRS)  {
		
		if(cola_solved >= cola_min_SDRS) {
			
			penSim$cola_actual[j] <- cola_solved
			penSim$AL_solved[j] <-  predict(polynomial(p_coeff_single), cola_solved)
			penSim$FR_MA_solved[j] <- with(penSim, MA[j] / AL_solved[j])  
			
			
		} else {
			
			penSim$cola_actual[j]  <- cola_min_SDRS
			penSim$AL_solved[j]    <-  predict(polynomial(p_coeff_single), cola_min_SDRS)
			penSim$FR_MA_solved[j] <- with(penSim, MA[j] / AL_solved[j]) 
		}  
	}		
	
	
	# Determine corrective Action costs
	# Assuming the difference between MA and the AL under cola_min are amortized using a 15 year open, level dollar method 
	
	# if(correctiveAction == 1) penSim$UAAL[j] <- max(0, predict(polynomial(p_coeff_single), cola_min_SDRS) - penSim$MA[j])
	# 
	# if(correctiveAction == 2) penSim$UAAL[j] <- ifelse(penSim$FR_MA_solved[j] < 1,  max(0, predict(polynomial(p_coeff_single), cola_baseline) - penSim$MA[j]), 0)
	
	
	penSim$AM_min[j]        <- ifelse(penSim$FR_MA_solved[j] < 1,  max(0, predict(polynomial(p_coeff_single), cola_min_SDRS) - penSim$MA[j]), 0)
	penSim$AM_baseline[j]   <- ifelse(penSim$FR_MA_solved[j] < 1,  max(0, predict(polynomial(p_coeff_single), cola_baseline) - penSim$MA[j]), 0)
	
	if(correctiveAction == 1) penSim$SC_SDRS[j] <- amort_LG(penSim$AM_baseline[j], dr, m, salgrowth_amort, end = FALSE, method = amort_method)[1]
	if(correctiveAction == 2) penSim$SC_SDRS[j] <- amort_LG(penSim$AM_baseline[j], dr, 5, salgrowth_amort, end = FALSE, method = amort_method)[1]
	
	
	penSim$UAAL_SDRS[j] <- penSim$AM_min[j]  #??
	
	
	
	#**********************************************
	#             Amortization costs              # 
	#**********************************************
	
	## Losses and gains
	# Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
	if (j == 1){
		penSim$EUAAL[j] <- 0
		penSim$LG[j] <- with(penSim, UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
		penSim$AM[j] <- with(penSim, LG[j])
		
	} else {
		penSim$EUAAL[j] <- with(penSim, (UAAL[j - 1] + NC[j - 1])*(1 + dr[j - 1]) - C[j - 1] - Ic[j - 1])
		penSim$LG[j]    <- with(penSim,  UAAL[j] - EUAAL[j])
		penSim$AM[j]    <- with(penSim,  LG[j] - (C_ADC[j - 1]) * (1 + dr[j - 1]))
	}   
	
	
	## Amortize LG(j)
	if(amort_type == "closed") SC_amort[j, j:(j + m - 1)] <- amort_LG(penSim$AM[j], dr, m, salgrowth_amort, end = FALSE, method = amort_method)  
	
	## Supplemental cost in j
	penSim$SC[j] <- switch(amort_type,
												 closed = sum(SC_amort[, j]),
												 open   = amort_LG(penSim$UAAL[j], dr, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
	
	
	# Employee contribution, based on payroll. May be adjusted later. 
	#penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate)
	
	
	#****************************************************
	#                    EEC and ERC                    #
	#****************************************************
	
	## EEC is a fixed share of total payroll
	if(EEC_type == "fixed"){
		# Employee contribution, based on payroll. May be adjusted later. 
		penSim$EEC[j] <- with(penSim, salary[j] * EECrate_fixed)
		
		if(nonNegC){
			penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
			penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
			
			# Adjustment of EEC
			if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
			
		} else {
			# Allow for negative ADC and C  
			penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
			
			if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
			# EEC is not fixed
			# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
			} else if(with(penSim, ADC[j] > EEC[j])) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
				# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
			} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
				penSim$ADC.ER[j] <- 0
				penSim$EEC[j]    <- with(penSim, ADC[j])
				# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
			} else if(with(penSim, ADC[j] <= 0)) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j])
				penSim$EEC[j]    <- 0
			}
		}
		
		
		# ERC
		penSim$ERC[j] <- with(penSim, ADC.ER[j])
		 
	}
	
	
	## Employees share a fixed proportion of ADC
	if(EEC_type == "sharedADC"){
		
		# Note: 
		#  - Negative EEC and ERC are NOT allowed: EEC = ERC = 0 if NC + SC < 0 
		#  - May want to set floors for EEC and/or ERC
		
		penSim$ADC[j] <- with(penSim, max(0, NC[j] + SC[j])) 
		penSim$EEC[j] <- with(penSim, ifelse(ADC[j] < 0, 0, ADC[j] * EECshare_sharedADC))
		penSim$ERC[j] <- with(penSim, ADC[j] - EEC[j])
		
	}
	
	
	## EEC is a fixed share of NC
	if(EEC_type == "sharedNC"){
	  
		# Note:
		#  - EEC is fixed share of NC, which is almost always positive
		#  - EEC may have very limited variation over time in this model. 
		
		penSim$EEC[j] <- with(penSim, NC[j] * EECshare_sharedNC)
		
		if(nonNegC){
			penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
			penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
			
			# Adjustment of EEC
			if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
			
		} else {
			# Allow for negative ADC and C  
			penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
			
			if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
			# EEC is not fixed
			# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
			} else if(with(penSim, ADC[j] > EEC[j])) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
				# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
			} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
				penSim$ADC.ER[j] <- 0
				penSim$EEC[j]    <- with(penSim, ADC[j])
				# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
			} else if(with(penSim, ADC[j] <= 0)) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j])
				penSim$EEC[j]    <- 0
			}
		}
		
		# ERC
		penSim$ERC[j] <- with(penSim, ADC.ER[j])
		
	}
	

	## EEC is contingent upon the investment return in the previous year
	if(EEC_type == "EEC_return"){
		
		# Note:
		#  - The range of EEC rate is set to [EECrate_min_EEC_return, EECrate_max_EEC_return]
		#  - EEC rate is set to EECrate_min_EEC_return when i.r[j-1] >= returnMax_EEC_return
		#  - EEC rate is set to EECrate_max_EEC_return when i.r[j-1] <= returnMin_EEC_return
		#  - EEC rate is set to middle point of its range in year 1
		
		# if(j == 1){
		# 	penSim$EEC[j] <- with(penSim, salary[j] * 	(EECrate_min_EEC_return+EECrate_max_EEC_return)/2)
		# } else {
		# 	
		# EEC_rate <- ifelse(penSim$i.r[j-1] > returnMax_EEC_return, EECrate_min_EEC_return,
		# 									 ifelse(penSim$i.r[j-1] < returnMin_EEC_return, EECrate_max_EEC_return,
		# 									 			 EECrate_min_EEC_return + (EECrate_max_EEC_return - EECrate_min_EEC_return) * (returnMax_EEC_return - penSim$i.r[j-1])/ (returnMax_EEC_return - returnMin_EEC_return)
		# 									 			 )
		# 									 )
		# 
		# penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate)
		# 
		# }
		
		j_EEC <- ifelse(j == 1, j, j-1)
			
		EEC_rate <- ifelse(penSim$i.r[j_EEC] > returnMax_EEC_return, EECrate_min_EEC_return,
											 ifelse(penSim$i.r[j_EEC] < returnMin_EEC_return, EECrate_max_EEC_return,
											 			 EECrate_min_EEC_return + (EECrate_max_EEC_return - EECrate_min_EEC_return) * (returnMax_EEC_return - penSim$i.r[j_EEC])/ (returnMax_EEC_return - returnMin_EEC_return)
											 )
		            )                 
		
		penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate) 
		 
	
		
		if(nonNegC){
			penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
			penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
			
			# Adjustment of EEC
			if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
			
		} else {
			# Allow for negative ADC and C  
			penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
			
			if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
			# EEC is not fixed
			# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
			} else if(with(penSim, ADC[j] > EEC[j])) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
				# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
			} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
				penSim$ADC.ER[j] <- 0
				penSim$EEC[j]    <- with(penSim, ADC[j])
				# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
			} else if(with(penSim, ADC[j] <= 0)) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j])
				penSim$EEC[j]    <- 0
			}
		}
		
		
		# ERC
		penSim$ERC[j] <- with(penSim, ADC.ER[j])
		
	}
	
	
	## EEC is contingent upon the funded ratio (MA based) in the previous year
	if(EEC_type == "EEC_FR"){
		
		# Note:
		#  - The range of EEC rate is set to [EECrate_min_EEC_FR, EECrate_max_EEC_FR]
		#  - EEC rate is set to EECrate_min_EEC_FR when i.r[j-1] >= FRMax_EEC_FR
		#  - EEC rate is set to EECrate_max_EEC_FR when i.r[j-1] <= FRMin_EEC_FR
		#  - EEC rate is set to middle point of its range in year 1
		# 
		# if(j == 1){
		# 	penSim$EEC[j] <- with(penSim, salary[j] * 	(EECrate_min_EEC_FR+EECrate_max_EEC_FR)/2)
		# } else {
		# 	
		# 	EEC_rate <- ifelse(penSim$FR_MA[j-1] > FRMax_EEC_FR, EECrate_min_EEC_FR,
		# 										 ifelse(penSim$FR_MA[j-1] < FRMin_EEC_FR, EECrate_max_EEC_FR,
		# 										 			 EECrate_min_EEC_FR + (EECrate_max_EEC_FR - EECrate_min_EEC_FR) * (FRMax_EEC_FR - penSim$FR_MA[j-1])/ (FRMax_EEC_FR - FRMin_EEC_FR)
		# 										 )
		# 	)
		# 	
		# 	penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate)
		# 	
		# }
		
		
		j_EEC <- ifelse(j == 1, j, j-1)
		
		EEC_rate <- ifelse(penSim$FR_MA[j_EEC] > FRMax_EEC_FR, EECrate_min_EEC_FR,
											 ifelse(penSim$FR_MA[j_EEC] < FRMin_EEC_FR, EECrate_max_EEC_FR,
											 			 EECrate_min_EEC_FR + (EECrate_max_EEC_FR - EECrate_min_EEC_FR) * (FRMax_EEC_FR - penSim$FR_MA[j_EEC])/ (FRMax_EEC_FR - FRMin_EEC_FR)
											 )
		)
			
		penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate)
			
		if(nonNegC){
			penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
			penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
			
			# Adjustment of EEC
			if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
			
		} else {
			# Allow for negative ADC and C  
			penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
			
			if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
			# EEC is not fixed
			# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
			} else if(with(penSim, ADC[j] > EEC[j])) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
				# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
			} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
				penSim$ADC.ER[j] <- 0
				penSim$EEC[j]    <- with(penSim, ADC[j])
				# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
			} else if(with(penSim, ADC[j] <= 0)) {
				penSim$ADC.ER[j] <- with(penSim, ADC[j])
				penSim$EEC[j]    <- 0
			}
		}
		
		
		# ERC
		penSim$ERC[j] <- with(penSim, ADC.ER[j])
		
	}
	
	
	if(EEC_type == "PSERS"){
		if(j > 1){
			
			# in the re-evaluation year
			if(penSim$SharedRiskEval[j - 1]){
				
				penSim$sharedRisk.rate[j] <-    ifelse(penSim$i.r_geoReturn[j - 1] >= dr,                penSim$sharedRisk.rate[j - 1] - 0.005,
																							 ifelse(penSim$i.r_geoReturn[j - 1] < (dr - 0.01), penSim$sharedRisk.rate[j - 1] + 0.005, 
																							 			 penSim$sharedRisk.rate[j - 1]))
				
				penSim$sharedRisk.rate[j] <- ifelse(            penSim$sharedRisk.rate[j] >  SharedRisk_cap, SharedRisk_cap,
																												ifelse( penSim$sharedRisk.rate[j] < -SharedRisk_cap, -SharedRisk_cap,
																																penSim$sharedRisk.rate[j])
				)
				
				
				penSim$sharedRisk.rate[j] <- ifelse(penSim$AL[j - 1] == 0, 0,
																						ifelse(penSim$FR_MA[j - 1] > 1 & penSim$sharedRisk.rate[j] > 0, 0, penSim$sharedRisk.rate[j])
				)
				
			} else {
				# Not in the re-evaluation year  
				penSim$sharedRisk.rate[j] <-  penSim$sharedRisk.rate[j - 1]
			}
		} 
		
			penSim$EEC[j] <-  (EEC_rate + penSim$sharedRisk.rate[j]) * penSim$salary[j]

			if(nonNegC){
				penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
				penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
				
				# Adjustment of EEC
				if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
				
			} else {
				# Allow for negative ADC and C  
				penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
				
				if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
				# EEC is not fixed
				# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
				} else if(with(penSim, ADC[j] > EEC[j])) {
					penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
					# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
				} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
					penSim$ADC.ER[j] <- 0
					penSim$EEC[j]    <- with(penSim, ADC[j])
					# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
				} else if(with(penSim, ADC[j] <= 0)) {
					penSim$ADC.ER[j] <- with(penSim, ADC[j])
					penSim$EEC[j]    <- 0
				}
			}
		
			
			# ERC
			penSim$ERC[j] <- with(penSim, ADC.ER[j])
				
		} 
		
	
	if(cola_type == "SDRS"){
		
		penSim$ADC[j]   <- with(penSim, NC[j] + SC[j])
		
		penSim$NC.ER[j] <- penSim$NC[j] * (1 - EECshare_NC) 
		penSim$NC.EE[j] <- penSim$NC[j] * EECshare_NC 
		
		penSim$SC.ER[j] <- penSim$SC[j] * (1 - EECshare_correctiveAction) 
		penSim$SC.EE[j] <- penSim$SC[j] * EECshare_correctiveAction 
		
		penSim$ERC[j] <- penSim$NC.ER[j] + penSim$SC.ER[j]
		penSim$EEC[j] <- penSim$NC.EE[j] + penSim$SC.EE[j]
		
	}
	
	
	
	#**************************************************************************************************************
	
	# if(nonNegC){
	# 	penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
	# 	penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
	# 	
	# 	# Adjustment of EEC
	# 	if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
	# 	
	# } else {
	# 	# Allow for negative ADC and C  
	# 	penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
	# 	
	# 	if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
	# 	# EEC is not fixed
	# 	# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
	# 	} else if(with(penSim, ADC[j] > EEC[j])) {
	# 		penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
	# 		# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
	# 	} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
	# 		penSim$ADC.ER[j] <- 0
	# 		penSim$EEC[j]    <- with(penSim, ADC[j])
	# 		# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
	# 	} else if(with(penSim, ADC[j] <= 0)) {
	# 		penSim$ADC.ER[j] <- with(penSim, ADC[j])
	# 		penSim$EEC[j]    <- 0
	# 	}
	# }
	# 
	# 
	# 
	# # ERC
	# penSim$ERC[j] <- with(penSim, ADC.ER[j])
	
	
	# C(j)
	penSim$C[j] <- with(penSim, EEC[j] + ERC[j])
	
	# C(j) - ADC(j)
	penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
	
	
	#****************************************************
	#                Investment returns                 #
	#****************************************************

	# Ia(j), Ib(j), Ic(j)
	penSim$Ia[j] <- with(penSim,  MA[j] * dr[j])
	penSim$Ib[j] <- with(penSim,  B[j] * dr[j])
	penSim$Ic[j] <- with(penSim,  C[j] * dr[j])
	
	
	# I.e(j)
	penSim$I.e[j] <- with(penSim, dr[j] *(MA[j] + C[j] - B[j]))
	
	# I.r(j)
	penSim$I.r[j] <- with(penSim, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
	
	# I.dif(j) = I.r(j) - I.e(j)
	penSim$I.dif[j] <- with(penSim, I.r[j] - I.e[j])
	
	
	# #**************************************************
	# #           Defined contribution component        #
	# #**************************************************
	# 
	# 
	# # MA(j) and EAA(j) 
	# if(j == 1) {penSim$DC_MA[j]  <- switch(DC_initBalanceType, 
	# 																			 preSet = DC_initBalance_preSet,     # Use preset value
	# 																		   ALpct  = penSim$AL[j] * DC_initBalance_ALpct,                # Assume inital fund equals inital liability.
	# 																		   MApct  = penSim$MA[j] * DC_initBalance_MApct # Inital MA is a proportion of inital AL
	#                                 ) 
	# 
	# } else {
	# 	penSim$DC_MA[j]  <- with(penSim, (DC_MA[j - 1] + salary[j - 1] * (DC_EECrate + DC_ERCrate)) * (1 + i.r[j - 1]))
	# }

}

as.data.frame(penSim)
}


stopCluster(cl)



# penSim_results %<>% mutate(cola_type = cola_type)  %>% select(cola_type, everything())

penSim_results <- 
	bind_rows(penSim_results) %>% 
	mutate(runname   = runname,
		     cola_type = cola_type,
				 policy_type = policy_type,
				 return_scn  = return_scn,
		     sim     = rep(-1:nsim, each = nyear)) %>% 
	group_by(sim) %>% 
	mutate(
				 AL_std  = AL / AL[year == 1],
				 B_std   = B / B[year == 1],
				 C_std   = C / C[year == 1],
				 ERC_PR  = ERC/ salary,
				 EEC_PR  = EEC/ salary,
				 NC_PR   = NC / salary
				 ) %>% 
	select(runname, cola_type, sim, year, AL, MA, FR_MA, ERC_PR,EEC_PR, NC_PR, AL_std, B_std, C_std, # DC_MA, 
				 everything()) %>% 
	as_tibble()

}


Global_paramlist$nsim <- 10


{
	start_time <- Sys.time()	
	penSim_DB_results <- run_sim_regular()
	print(Sys.time() - start_time)
	suppressMessages(gc())
}

penSim_DB_results %>% filter(sim == 1) %>% print()




# penSim$AL

# penSim$C %>% sum
# penSim$C %>% sd

# contingent cola: total 1028717234; sd = 29779508
# constant   cola: total  948162721; sd = 28538920


# penSim_constant <- penSim
# penSim_return <- penSim
# penSim_FR1 <- penSim
# penSim_FR2 <- penSim

# penSim_constant %>% head(10)
# penSim_FR1%>% head(10)


# penSim_comparison <-
# 	bind_rows(penSim_constant,penSim_return,penSim_FR1, penSim_FR2) %>%
# 	mutate(cola_type = factor(cola_type, levels = c("constant", "return", "FR1", "FR2")))
# 
# 
# penSim_comparison %>%
# 	select(cola_type, year, AL, MA) %>%
# 	gather(variable, value, -year, -cola_type) %>%
# 
# 	qplot(x = year, y = value, color = variable, data = ., geom = c("line", "point")) + facet_wrap(.~cola_type, nrow = 2) +
# 	coord_cartesian(ylim = c(0, 3.4e+09))
# 
# 
# penSim_comparison %>%
# 	group_by(cola_type) %>%
# 	summarize(C_tot = sum(C))
# 












