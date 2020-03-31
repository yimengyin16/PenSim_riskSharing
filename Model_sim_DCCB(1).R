

run_sim_DC <- function(paramlist_ = paramlist,
														Global_paramlist_ = Global_paramlist){

#paramlist_ <- paramlist
#Global_paramlist_ <- Global_paramlist

assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(paramlist_,  envir = environment())


#' Notes
#' Currently we only model DC and CB account balances for active members
#' Variables :
#'   - Indices: start_year, ea, age (20-60) 
#'   - salary
#'   - DC_EECrate, DC_ERCrate
#'   - i.r_DC: investment return to DC
#' 
#' Data:
#'   - Year-1 balances for initial active members 





#******************************************************************************* 
#                 Processing data from Model_Main   ####
#*******************************************************************************


load("Inputs/riskShaing_demographics.RData")
# df_actives
# df_retirees
# df_decrement


# ## Decrement table
# df_decrement <- 
# 	decrement %>% 
# 	  ungroup %>% 
# 		# filter(ea == 20) %>% 
# 		select(age,ea, yos, qxm, qxm.r, qxt, qxd) %>%
# 	  mutate(qxT = qxm + qxt + qxd)
# df_decrement %>% head



# ## Retirees 
# df_retirees %<>% 
# 	filter(!is.na(year)) %>% 
#   rename(B_ret  = B.r,
# 	  		 AL_ret = ALx.r,
# 		  	 n_ret  = number.r,
#   			 ret_year = year.retire) %>%
# 	mutate(start_year = year - (age - ea),
# 				 ret_age    = age - (year - ret_year)) %>% 
# 	filter(ret_year == 1 | ret_age == 60, !(ret_year < 1&n_ret == 0), year <= nyear) %>% 
# 
# 	mutate_at(vars(B_ret, AL_ret, n_ret), list(na2zero)) %>% 
# 	select(start_year, ea, age, year,ret_age, ret_year, everything()) %>% 
# 	ungroup() %>% 
# 	arrange(year, ret_year, ea, age) # one-to-one mapping between retirement year and age with single retirement age, 
# 
# 
# # All starting years allowed
# start_years <- c(df_retirees$start_year[df_retirees$year == 1], 1:nyear)
# df_retirees	%<>%  filter( !(ret_year == 1 & (!start_year %in% start_years))) 
# 
# df_retirees %>% filter(year == 2) 
# df_retirees %>% filter(start_year == 1, ea ==20) 


## Active members

load("Inputs/riskShaing_demographics.RData")

df_actives %<>% 
	rename(n_act  = number.a,
				 salary = sx,
				 AL_act = ALx,
				 NC_act = NCx,
				 # ratio.NC_PVB = NC_PVB,
				 # ratio.AL_PVB = AL_PVB,
				 PVB_act  =  PVFBx.r,
				 B_retAge = Bx.ret) %>%
	mutate(start_year = year - (age - ea)) %>% 
	select(start_year, ea, age, year,  n_act, salary) %>% 
  as_tibble()
df_actives %>% head()


# df_retirees %>% filter(start_year == 5, ea ==20)
# get_PVB_retiree(60, 1, 0.015, 0.075) %>% print




#******************************************************************************* 
#                    Generate investment returns  ####
#*******************************************************************************

## Generate investment returns

i.crisis <- rep(i.mean - i.sd^2/2, nyear)
i.crisis[2:5] <- c(-0.24, 0.12, 0.12, 0.12)

set.seed(1234) ;i.r <- matrix(rnorm(nyear*nsim, i.mean, i.sd), nyear, nsim)
i.r <- cbind(rep(i.mean - i.sd^2/2, nyear), i.r)
i.r <- cbind(i.crisis, i.r)

colnames(i.r) <- -1:nsim

df_i.r <- i.r %>% as_tibble() %>% 
	mutate(year = 1:nyear) %>% 
	select(year, everything()) %>% 
	gather(sim, i.r, -year) %>% 
	mutate(sim = as.numeric(sim))





#******************************************************************************* 
#                    Preparation: Active members  ####
#*******************************************************************************
sim_actives0 <- 
	df_actives %>% 
	mutate(DC_balance = ifelse(year == 1, (age-ea)*0.15*salary, 0),
				 DC_EEC = salary * DC_EECrate,
				 DC_ERC = salary * DC_ERCrate,
				 DC_C = DC_EEC + DC_ERC
				 ) %>% 
	group_by(start_year, ea)
	
sim_actives <- 
	sim_actives0 %>% 
  left_join(filter(df_i.r, sim == 1), by = "year")

# for (j in 2:50){
# # j <- 2
# 
# sim_actives %<>% 
# 	mutate(DC_balance = ifelse(year == j & age > ea, (DC_balance[year == j-1] + DC_C[j-1])*(1 + i.r[j-1]), DC_balance))
# }

# sim_actives %>% filter(start_year == 0, ea == 30)



#******************************************************************************* 
#                    Preparation: Active members  ####
#*******************************************************************************

# # Valuations of active member can be done outside the loop if the baseline COLA rate
# # is constant over time
# 
# sim_actives_yearsum <-
# df_actives %>% 
# 	group_by(year) %>% 
# 	summarise(AL_act  = sum(AL_act * n_act),
# 						NC      = sum(NC_act * n_act),
# 						PVB_act = sum(PVB_act* n_act),
# 						salary  = sum(salary* n_act),
# 						n_act   = sum(n_act))
# sim_actives_yearsum


#******************************************************************************* 
#                              Simulation                                   ####
#*******************************************************************************

cl <- makeCluster(ncore) 
registerDoParallel(cl)


penSim_DC_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr", "magrittr", "Rcpp")) %dopar% {

	# k <- 0 # for simulation runs
	
	sim_actives <- 
		sim_actives0 %>% 
		left_join(filter(df_i.r, sim == k), by = "year") %>% 
		mutate(sim = k)
	
	source("Functions.R")

	
for (j in 2:nyear){
	sim_actives %<>% 
		mutate(DC_balance = ifelse(year == j & age > ea, (DC_balance[year == j-1] + DC_C[year == j-1])*(1 + i.r[year == j-1]), DC_balance))
}
	
# penSim_results[[k]] <- penSim
  sim_actives
}


stopCluster(cl)


# penSim_DC_results


penSim_DC_results_indiv <- 
	bind_rows(penSim_DC_results) %>% 
	mutate(runname   = runname,
		     cola_type = cola_type,
				 policy_type = policy_type,
				 return_scn  = return_scn) %>% 
	# group_by(sim) %>% 
	select(runname, cola_type, sim, year, everything()) %>% 
	as_tibble()

penSim_DC_results_indiv %>% 
	filter(is.na(DC_balance))

penSim_DC_results_indiv %>% 
	filter( start_year == 3, ea == 58)

penSim_DC_results_yearsum <-
  penSim_DC_results_indiv %>%
	group_by(sim, year) %>%
	summarise(DC_EEC  = sum(DC_EEC * n_act),
						DC_ERC  = sum(DC_ERC * n_act),
						DC_balance = sum(DC_balance * n_act),
						salary  = sum(salary* n_act),
						n_act   = sum(n_act))
penSim_DC_results_yearsum

penSim_DC_outputs <- 
	list(DC_actives_indiv  = penSim_DC_results_indiv,
			 DC_actives_yearsum = penSim_DC_results_yearsum
	)

}

# paramlist$cola_type <- "FRramp1"

Global_paramlist$nsim <- 10
Global_paramlist$nyear

start_time <- Sys.time()	
penSim_DC_results <- run_sim_DC()
Sys.time() - start_time

penSim_DC_results$DC_actives_indiv %>% filter(sim == -1) %>% print
penSim_DC_results$DC_actives_yearsum %>% filter(sim == 0)











