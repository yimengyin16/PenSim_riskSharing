
# #*********************************************** 
# #     Functions and tools  ####
# #***********************************************
# 
# # ## Function for calculating PVB for retirees 
# # get_PVB_retiree <- function(age_fn, benefit_init, cola_assumed, i, decrement = df_decrement, age_max_ = age_max) {
# # 	# calculates actuarial PV for given age, initial benefit, 
# # 	# cola assumption, and mortality 
# # 	
# # 	# age_fn: current age
# # 	# benefit_init: current benefit payment
# # 	# cola_assumed: assumed future annual cola
# # 	# i: discount rate
# # 	
# # 	
# # 	nyear_ret <- age_max_ - age_fn + 1
# # 	
# # 	decrement_fn <- filter(decrement, age >= age_fn, ea == min(ea))$qxm
# # 	
# # 	PVB <- sum(((1 + i)^-(0:(nyear_ret - 1))) * c(1, cumprod(1-decrement_fn)[-nyear_ret]) * (benefit_init * (1 + cola_assumed)^(0:(nyear_ret-1))))
# # }
# # 
# # 
# # ## Function for calculating PVB for actives   
# # get_PVB_active <- function(age_fn, ea_fn, age_ret, benefit_init, cola_assumed, i, decrement = df_decrement, age_max_ = age_max) {
# # 	# calculates actuarial PV for an active plan member.  
# # 	
# # 	# age_fn: current age
# # 	# benefit_init: benefit payment at age age_ret
# # 	# cola_assumed: assumed future annual cola
# # 	# i: discount rate
# # 	# decrement: decrement table. qxm is the mortality rate for retirees, qxT is the total separation rate for actives (mortality included) 
# # 	
# # 	# 	mortality_ = mortality
# # 	# 	age_max_ = age_max
# # 	#   i <- 0.075
# # 	
# # 	nyear_ret <- age_max_ - age_ret + 1
# # 	
# # 	decrement_ret <- filter(decrement, age >= age_ret, ea == ea_fn)$qxm
# # 	decrement_act <- filter(decrement, age>= age_fn, age< age_ret, ea == ea_fn)$qxT
# # 	
# # 	PVB <- sum(((1 + i)^-(0:(nyear_ret - 1))) * c(1, cumprod(1-decrement_ret)[-nyear_ret]) * (benefit_init * (1 + cola_assumed)^(0:(nyear_ret-1)))) *
# # 		((1 + i)^-((age_ret - age_fn)) * prod(1-decrement_act))
# # 	
# # 	
# # }
# 
# 
# #get_PVB_retiree(60, 593489.63, 0.03, 0.075)  %>% print
# #get_PVB_active(21, 60, 2739765, 0.03, 0.075) %>% print
# 
# 
# 
# 



#*********************************************** 
# Processing data from Model_Main   ######## 
#***********************************************

run_sim_regular <- function(paramlist_ = paramlist,
														Global_paramlist_ = Global_paramlist){

# paramlist_ <- paramlist
# Global_paramlist_ <- Global_paramlist
	
assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(paramlist_,  envir = environment())


load("Data/SteadyState2.RData")
# ss_actives
# ss_retirees
# ss_decrement

ss_actives  %>% summarise(w.age = weighted.mean(age, number.a),
						  						w.yos  = weighted.mean(age - ea, number.a))
ss_retirees %>% summarise(w.age = weighted.mean(age, number.r))


## Decrement table
df_decrement <- 
	decrement %>% 
	  ungroup %>% 
		# filter(ea == 20) %>% 
		select(age,ea, yos, qxm, qxm.r, qxt, qxd) %>%
	  mutate(qxT = qxm + qxt + qxd)


## Steady state retirees
ss_retirees %<>% 
	rename(B_ret  = B.r,
				 AL_ret = ALx.r,
				 n_ret  = number.r) %>% 
	select(-year) %>% 
	ungroup() %>% 
	arrange(year.retire, ea, age) # one-to-one mapping between retirement year and age with single retirement age, 
	
ss_retirees %>% head
	
 # grouping retirees by age
ss_retirees2 <- 
	ss_retirees %>% group_by(age) %>% 
	summarize(B_ret  = sum(n_ret * B_ret)  / sum(n_ret),
						AL_ret = sum(n_ret * AL_ret) / sum(n_ret),
						n_ret  = sum(n_ret)) %>% 
	select(age, B_ret, n_ret)
ss_retirees2 %>% head


## Active members
ss_actives %<>% 
	rename(n_act  = number.a,
				 salary = sx,
				 AL_act = ALx,
				 NC_act = NCx,
				 ratio.NC_PVB = NC_PVB,
				 ratio.AL_PVB = AL_PVB,
				 PVB_act  =  PVFBx.r,
				 B_retAge = Bx.ret) %>% 
	select(ea, age, n_act, salary, B_retAge, ratio.NC_PVB, ratio.AL_PVB) # , PVB_act, AL_act, NC_act) %>% 
	as_tibble()
ss_actives %>% head()

# Relationship between benefits of actives and retirees
  # Ratio of benefit at age 60 to expected first year benefit for ea = age = 20  
  # Will be used to determine benefit level for new retirees at age 60.
ratio.ret_act <- ss_retirees2[ss_retirees2$age == 60, "B_ret"] / ss_actives[ss_actives$age == 20 & ss_actives$ea == 20, "B_retAge"]
# ratio.ret_act



# df_decrement
# 
# ss_actives
# ss_retirees2
# 
# ss_retirees
# ratio.ret_act




#*************************************************** 
#              Investment returns  ####
#***************************************************

# set.seed(1210);  i.r <- rnorm(nyear, i.mean, i.sd); prod(1+i.r)^(1/nyear)
i.r <- rep(dr, nyear)

# cola_actual <- rep(cola_baseline, nyear )

# Stress scenario:
# i.r[3:4] <- 0


#cola_actual[5:6] <- 0
# i.r[2:5] <- c(-0.24, 0.12, 0.12, 0.12)


#*************************************************** 
#          Preparation: Active members  ####
#***************************************************

# The the current version, valuations of active member can be done
# outside the loop. 


 # Calculate PVB, AL and NC. 
 # PVB is calculated 

ss_actives$PVB_act <-  
	mapply(get_PVB_active, ss_actives$age, ss_actives$ea, 60, ss_actives$B_retAge , MoreArgs = list(i = dr, cola_assumed = cola_baseline, decrement = df_decrement, age_max_ = age_max))


ss_actives %<>% mutate(AL_act = PVB_act * ratio.AL_PVB,
											 NC_act = PVB_act * ratio.NC_PVB) 


 # Growth of PVB, AL and NC over time
sim_actives <- 
	expand.grid(age = 20:(age_ret-1), ea = 20:(age_ret - 1), year = 1:nyear) %>% 
	filter(age >= ea) %>% 
	left_join(ss_actives, by = c("ea", "age")) %>% 
	
	# Salary, B_retAge, PVB, NC, AL in all cells growth at the same annual rate 
	mutate_at(vars(salary, B_retAge, PVB_act, AL_act, NC_act), funs(. * ( 1 + g_startSal)^(year - 1))) 

sim_actives %>% head

 # calculate annual totals
sim_actives_yearsum <- 
	sim_actives %>% 
	group_by(year) %>% 
	summarise(AL_act  = sum(AL_act * n_act),
						NC      = sum(NC_act * n_act),
						PVB_act = sum(PVB_act* n_act),
						salary  = sum(salary* n_act),
						n_act   = sum(n_act))
sim_actives_yearsum




#*************************************************** 
#             Preparation: Retirees  ####
#***************************************************

 # Adjust benefit in year 1 for past cola 
ss_retirees  %<>% mutate(B_ret = (1 + cola_baseline)^(age - 60) * B_ret)
ss_retirees2 %<>% mutate(B_ret = (1 + cola_baseline)^(age - 60) * B_ret)

 # Calculate year 1 AL for retirees
ss_retirees2$AL_ret <- mapply(get_PVB_retiree, ss_retirees2$age, ss_retirees2$B_ret , MoreArgs = list(cola_assumed = cola_baseline, i = dr, decrement = df_decrement, age_max_ = age_max))

 # Construct df for nyear years. values for year > 1 will be calculated in the loop.  
sim_retirees <- 
	expand.grid(age = age_ret:age_max, year = 1:nyear) %>% 
	left_join(ss_retirees2, by = "age") %>% 
	mutate(AL_ret = ifelse(year == 1, AL_ret, 0),
				 B_ret  = ifelse(year == 1, B_ret, 0)) %>% 
	select(year, age, everything())

 # Step df for annual totals ( filled by 0s, values to be calculated later)
sim_retirees_yearsum <- 
	sim_retirees %>% 
	group_by(year) %>% 
	summarise(AL_ret  = sum(AL_ret * n_ret),
						B       = sum(B_ret  * n_ret),
						n_ret   = sum(n_ret)) 
sim_retirees_yearsum

 # convert to list for the loop. 
lsim_retirees0 <- split(sim_retirees, sim_retirees$year) 



#*************************************************** 
#             Preparation: loop  ####
#***************************************************

# Setep df for annual totals 
penSim0 <- 
	left_join(sim_actives_yearsum, sim_retirees_yearsum, by = "year") %>% 
	mutate(
		     cola_actual = 0,
		     AL     = ifelse(year == 1, AL_act + AL_ret, 0),
				 MA     = 0,
				 AA     = 0,
				 UAAL   = 0,
				 FR_MA  = 0,
				 FR_AA  = 0,
				 
				 EUAAL  = 0,
				 LG     = 0,
				 AM     = 0,
				 
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
				 
				 
				 i.r_geoReturn = 0,
				 sharedRisk.rate = 0
				 
				 ) 

penSim0


# Evaluation year for shared risk EEC
penSim0$SharedRiskEval <- seq_len(nyear) %% 3 == 0  # TRUE in the year to determine if the EEC rate should be changed




SC_amort0 <- matrix(0, nyear + m, nyear + m)
s.vector <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector  # a vector containing the porportion of 






i.crisis <- rep(i.mean - i.sd^2/2, nyear)
i.crisis[2:5] <- c(-0.24, 0.12, 0.12, 0.12)

set.seed(1234) ;i.r <- matrix(rnorm(nyear*nsim, i.mean, i.sd), nyear, nsim)
i.r <- cbind(rep(i.mean - i.sd^2/2, nyear), i.r)
i.r <- cbind(i.crisis, i.r)

colnames(i.r) <- -1:nsim

i.r_ <- i.r

# cola_type <- c("constant", "return", "FR", "FRramp1", "FRramp2", "SDRS")[3]



# Geometric return for shared-risk employee contribution

# i.r_supplement <-  
# 	cbind(rep(paramlist$i, 5),
# 				matrix(c(0.0343, 0.0796, 0.1491, 0.0304, 0.0129), 5, Global_paramlist$nsim + 1))

i.r_supplement <- matrix(dr, 5, Global_paramlist$nsim + 2)


i.r_geoReturn <- rbind(i.r_supplement, i.r) %>% 
	as.data.frame %>% 
	mutate_all(funs(get_rollingReturns(., "moving", 10)))

i.r_geoReturn[1:9, ] <- rbind(i.r_supplement, i.r)[1:9,] %>% 
	as.data.frame %>% 
	mutate_all(funs(get_rollingReturns(., "expanding"))) %>% 
	as.matrix()

i.r_geoReturn <- i.r_geoReturn[-(1:5),]
i.r_geoReturn_ <- i.r_geoReturn

#i.r_geoReturn 


# Generating stochastic inflation
set.seed(1234)
infl_stoch <- matrix(rnorm(nyear*(nsim), infl_mean, infl_sd), nyear, nsim)
infl_stoch <- cbind(matrix(infl_mean, nyear, 2), infl_stoch)
colnames(infl_stoch) <- -1:nsim

infl_stoch_ <- infl_stoch


#*************************************************** 
#                    Simulation            ####
#***************************************************

cl <- makeCluster(ncore) 
registerDoParallel(cl)


penSim_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
	# k <- 0
	# initialize
	# penSim   <- penSim0
	# SC_amort <- SC_amort0
	# 
	# if(k == -1) SC_amort[,] <- 0
	# 
	# penSim[["i.r"]] <- i.r_[, as.character(k)]
	# # penSim[["i.r_geoReturn"]] <- i.r_geoReturn_[, as.character(k)]
	
	# k <- 1 # for simulation runs
	
	penSim   <- penSim0
	SC_amort <- SC_amort0 
	lsim_retirees <- lsim_retirees0
	
	penSim[["i.r"]] <- i.r_[, as.character(k)]
	penSim[["i.r_geoReturn"]] <- i.r_geoReturn_[, as.character(k)]
	penSim[["infl_stoch"]] <- infl_stoch_[, as.character(k)]
	
	source("Functions.R")

	
for (j in 1:nyear){
	 # j <- 2
	
	
	#### 1.  Asset value 
	
	# MA(j) and EAA(j) 
	if(j == 1) {penSim$MA[j]  <- ifelse(k == -1, penSim$AL[j],
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
	

	#### 2. Determining (contingent COLA) based on previous year's conditioning variable(s) and inflation 
	
	if(j == 1) penSim$cola_actual[j] <- cola_baseline
	
	# Constant cola:
	if(cola_type == "constant") penSim$cola_actual[j] <- cola_baseline
	
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
		

		
	#### 3. Liability and funded status
	

	# Determine current year's benefit payment and AL for retirees
	
	if(j > 1) lsim_retirees[[j]]$B_ret <- c(lsim_retirees[[j-1]]$B_ret[1] * (1 + g_startSal), lsim_retirees[[j-1]]$B_ret[-nrow(lsim_retirees[[j]])] * (1 + penSim$cola_actual[j-1] ))
	lsim_retirees[[j]]$AL_ret <- mapply(get_PVB_retiree, lsim_retirees[[j]]$age, lsim_retirees[[j]]$B_ret , MoreArgs = list(cola_assumed = cola_baseline, i = dr, age_max_ = age_max, decrement = df_decrement))
	# lsim_retirees[[j]]
	
	penSim$AL_ret[j] <- sum(lsim_retirees[[j]]$AL_ret * lsim_retirees[[j]]$n_ret) 
	penSim$B[j]      <- sum(lsim_retirees[[j]]$B_ret  * lsim_retirees[[j]]$n_ret)
	
	# Total liability  
	penSim$AL[j] <- with(penSim, AL_act[j] + AL_ret[j])
	
	# Funded ratios
	penSim$FR_MA[j] <- with(penSim, MA[j] / AL[j])
	penSim$FR_AA[j] <- with(penSim, AA[j] / AL[j])
	
	# UAAL
	penSim$UAAL[j] <- with(penSim, AL[j] - AA[j]) 
	
	# penSim
	
	
	# 4. Losses and gains
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
	
	
	# Amortize LG(j)
	if(amort_type == "closed") SC_amort[j, j:(j + m - 1)] <- amort_LG(penSim$AM[j], dr, m, salgrowth_amort, end = FALSE, method = amort_method)  
	
	# Supplemental cost in j
	penSim$SC[j] <- switch(amort_type,
												 closed = sum(SC_amort[, j]),
												 open   = amort_LG(penSim$UAAL[j], dr, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
	
	
	# Employee contribution, based on payroll. May be adjusted later. 
	#penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate)
	
	
	#**************************************************************************************************************
	#                                        PSERS: shared-risk EEC rate 
	#**************************************************************************************************************
	
	if(EEC_type == "sharedRisk"){
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

		} else {
		
		# Employee contribution, based on payroll. May be adjusted later. 
		penSim$EEC[j] <- with(penSim, salary[j] * EEC_rate)
	}
	
	#**************************************************************************************************************
	
	

	
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
	
	
	# C(j)
	penSim$C[j] <- with(penSim, EEC[j] + ERC[j])
	
	# C(j) - ADC(j)
	penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
	
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
	
}


# penSim_results[[k]] <- penSim
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
				 EEC_PR  = EEC/ salary
				 ) %>% 
	select(runname, cola_type, sim, year, AL, MA, FR_MA, ERC_PR,EEC_PR, AL_std, B_std, C_std, everything()) %>% 
	as_tibble()

}

penSim_results <- run_sim_regular()

penSim_results %>% filter(sim == -1) %>% print





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












