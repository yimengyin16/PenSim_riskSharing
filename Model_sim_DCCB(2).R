
run_sim_DC <- function(paramlist_ = paramlist,
											 Global_paramlist_ = Global_paramlist){

# paramlist_ <- paramlist
# Global_paramlist_ <- Global_paramlist

assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(paramlist_,  envir = environment())


##' Notes
#' Currently we only model DC and CB account balances for active members
#' 
#' Key model inputs:
#'   - Year-1 balances for initial active members (assumption needed)  
#'   - Salary
#'   - DC_EECrate, DC_ERCrate
#'   - i.r_DC: investment return to DC
#' 
#' Indices of active members: 
#'   - start_year, ea, age (20-60) 
#'   - Note: df_actives only include age 20-59, need add age 60 to each start_year/ea group.




#******************************************************************************* 
#                 Processing data from Model_Main   ####
#*******************************************************************************


# load("Inputs/riskShaing_demographics.RData")
# df_actives
# df_retirees
# decrement


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
	rename(
		     n_act  = number.a,
				 salary = sx,
				 AL_act = ALx,
				 NC_act = NCx,
				 # ratio.NC_PVB = NC_PVB,
				 # ratio.AL_PVB = AL_PVB,
				 PVB_act  =  PVFBx.r,
				 B_retAge = Bx.ret) %>%
	mutate(start_year = year - (age - ea),
				 yos = age - ea) %>% 
	select(start_year, ea, age,yos, year,  n_act, salary) %>% 
  as_tibble()

# add the retirement year (age 60)
df_actives_retAge <- 
  df_actives %>% 
	group_by(start_year, ea) %>% 
	mutate(age_add =  max(age) + 1,
				 keep    =  max(age) == age_ret - 1) %>% 
	       # only keep the groups that include age 59 in the simulation period. 
	filter((start_year >= 2 & ea == age) | (start_year <= 1 & year == 1), 
				 keep) %>% 
	mutate(age = age_add,
				 yos = age - ea,
				 year = start_year + yos,
				 n_act = 0,
				 salary = 0) %>% 
	select(-keep, -age_add)

df_actives <- bind_rows(df_actives, df_actives_retAge) %>% 
	arrange(start_year, ea)

# df_actives %>% head()


#******************************************************************************* 
#                    Generate investment returns  ####
#*******************************************************************************

# Asset-shock scenario
i.crisis <- rep(i.mean - i.sd^2/2, nyear)
i.crisis[2:5] <- c(-0.24, 0.12, 0.12, 0.12)

# Stochastic returns
set.seed(1234) ;i.r <- matrix(rnorm(nyear*nsim, i.mean, i.sd), nyear, nsim)
i.r <- cbind(rep(i.mean - i.sd^2/2, nyear), i.r)
i.r <- cbind(i.crisis, i.r)
colnames(i.r) <- -1:nsim

# Turn the matrix into data frame 
df_i.r <- i.r %>% as_tibble() %>% 
	mutate(year = 1:nyear) %>% 
	select(year, everything()) %>% 
	gather(sim, i.r, -year) %>% 
	mutate(sim = as.numeric(sim))



#**************************************************************************************** 
#  Notes: Fast calculation of DC/CB balances when salary scale is a function of YOS  ####
#****************************************************************************************

#' Within each simulation run:
#' 
#' For future employees who enter the plan in or after year 1:
#'   - The schedule of DC/CB balance for each dollar's starting salary only depends on the start year.
#'       - Within each start year, entry age does not affect future path of salary: salary scale only depends on yos; 
#'       - The start year determines the path of investment return. (which year to start with in the i.r[k] series)      
#'   - For each start-year group, multiplying the DC/CB schedule with the starting salaries of entrants in that year (for all entry ages) 
#'     gives the full DC/CB schedules for those entrants. 
#'     
#' For existing active members in year 1: 
#'   - They have different YOS in year 1 and are on different points of the salary scale
#'   - Approach 1:
#'      - So we need to track the DC/CB schedule for 1 dollar's salary for each year-1 YOS. 
#'      - Calculate the full schedules of DC/CB by multiplying the schdules for $1's year-1 salary with the 1-year salaries 
#'     of the initial active members. 
#'   - Approach 2(currently used in the model):
#'      - Simply track each age-yos cell of the year-1 members over the entire sim period.



#******************************************************************************* 
#     DC balance for new employees who enter the plan in or after year 2    ####
#*******************************************************************************

# Salary and contribution schedule for a starting salary of $1 
df_salary_std <- 
	df_actives %>% 
	filter(start_year == 1, ea == 20) %>% 
	mutate(salary_std = salary / salary[age == ea],
				 yos = age- ea) %>% 
	select(yos, salary_std) %>% 
	mutate(# DC_EEC = salary_std * DC_EECrate,
				 # DC_ERC = salary_std * DC_ERCrate,
				 # DC_C = DC_EEC + DC_ERC,
		     DC_C  = salary_std * (DC_ERCrate + DC_EECrate),
				 DC_balance_std = 0
	)
df_salary_std


# Set up the data frame for all simulation runs and all years
df_DC_std <- 
	expand.grid(yos = df_salary_std$yos, start_year =  2:nyear, sim = unique(df_i.r$sim)) %>% 
	left_join(df_salary_std, by = "yos") %>% 
	mutate(year = start_year + yos) %>% 
	left_join(df_i.r, by = c("sim", "year")) %>% 
	group_by(sim, start_year)
df_DC_std


# The Rcpp version (in Functions.R) of the following function is used in the loop
# fn <- function(balance_, C_, i.r_){
# 	# balance_ = filter(df_DC_std, sim == -1, start_year == 2)$DC_balance
# 	# C_ = filter(df_DC_std, sim == -1, start_year == 2)$DC_C
# 	# i.r_ = filter(df_DC_std, sim == -1, start_year == 2)$i.r
# 
# 	N <- length(balance_)
#   for (j in 2:N){
#   	balance_[j] <- (balance_[j-1] + C_[j-1]) * (1 + i.r_[j-1])
#   }
# 
# 	balance_
# }


# Paths of DC balance for all start-year and all sims
# a <- Sys.time()
df_DC_std %<>% 
	mutate(DC_balance_std = fnC(DC_balance_std, DC_C, i.r)) 
# Sys.time() - a

# df_DC_std %>% filter(sim == 1, start_year == 2)
# df_DC_std


# Create df for all active members and all sims
sim_actives_indiv <- 
	llply(unique(df_i.r$sim), function(x) mutate(df_actives, sim = x)) %>% 
	bind_rows()

# Merge the df for active members and DC balance for $1's starting salary 
sim_actives_indiv %<>% 
	left_join(select(df_DC_std, -year), by = c("sim", "start_year", "yos")) %>% 
	filter(start_year >= 2)


rm(df_DC_std) # to save memory
suppressMessages(gc())

# Calculate actual DC balances by multiplying the starting salaries with 
#   the balance for $1's starting salary
sim_actives_indiv %<>% 
	group_by(sim, start_year, ea) %>% 
	mutate(DC_balance = DC_balance_std * salary[yos == 0]
				 # DC_EEC = salary * DC_EECrate,
				 # DC_ERC = salary * DC_ERCrate,
				 # DC_C = DC_EEC + DC_ERC
				 ) %>% 
	ungroup()


# x <- sim_actives1 %>%
# 	filter(sim == 1, start_year ==20, ea == 30) %>%
# 	mutate(DC_EEC = salary * DC_EECrate,
# 				 DC_ERC = salary * DC_ERCrate,
# 				 DC_C = DC_EEC + DC_ERC) %>%
# 	select(sim, start_year, ea, age, salary, DC_C,DC_balance, i.r)
# x



#******************************************************************************* 
#                   DC balance for initial (year-1) members                 ####
#*******************************************************************************


df_actives_init <- 
	df_actives %>%
	filter(start_year <= 1) %>% 
	mutate(
		     #DC_EEC = salary * DC_EECrate,
				 #DC_ERC = salary * DC_ERCrate,
				 #DC_C = DC_EEC + DC_ERC,
				 DC_C  = salary * (DC_ERCrate + DC_EECrate),
				 DC_balance = ifelse(year == 1, (age-ea)*0.15*salary, 0)
	)
	

# Create df for all simulation runs
sim_actives_init <- 
	llply(unique(df_i.r$sim), function(x) mutate(df_actives_init, sim = x)) %>% 
	bind_rows() %>% 
	left_join(df_i.r, by = c("sim", "year")) %>% 
	group_by(sim, start_year, ea)
sim_actives_init

# Calculate paths of DC balances for all cells of initial actives in all sim runs
# a <- Sys.time()
sim_actives_init %<>% 
	mutate(DC_balance = fnC(DC_balance, DC_C, i.r))    
# Sys.time() - a


# sim_actives_init
# df_actives_init %>% filter(year == 1) %>% pull(yos) %>% unique
# df_actives_init %>% filter(start_year ==-10, ea == 21)
# df_actives_init



#******************************************************************************* 
#                           Producing final outputs                         ####
#*******************************************************************************


## Combine results for initial and future members
sim_actives_indiv <- 
	bind_rows(sim_actives_init, 
						sim_actives_indiv) %>% 
	mutate(runname   = runname,
				 cola_type = cola_type,
				 policy_type = policy_type,
				 return_scn  = return_scn) %>% 
	select(runname, cola_type, sim, year, everything()) %>% 
	as_tibble()

rm(sim_actives_init) # to save memory
suppressMessages(gc())


# ## Final outputs
# sim_actives_indiv %<>% 
# 	# ungroup() %>% 
# 	mutate(runname   = runname,
# 		     cola_type = cola_type,
# 				 policy_type = policy_type,
# 				 return_scn  = return_scn) %>% 
# 	select(runname, cola_type, sim, year, everything()) %>% 
# 	as_tibble()

# penSim_DC_results_indiv %>% 
# 	filter(sim == 1, start_year == 3, ea == 28)


sim_actives_yearsum <-
	sim_actives_indiv %>%
	filter(year <= nyear) %>% 
	mutate(
		DC_EEC = salary * DC_EECrate,
		DC_ERC = salary * DC_ERCrate) %>% 
	group_by(sim, year) %>%
	summarise(
		        DC_EEC  = sum(DC_EEC * n_act),
						DC_ERC  = sum(DC_ERC * n_act),
						DC_balance = sum(DC_balance * n_act),
						salary  = sum(salary* n_act),
						n_act   = sum(n_act))
sim_actives_yearsum


penSim_DC_outputs <- 
	list(
		   # DC_actives_indiv   = sim_actives_indiv,
			 DC_actives_yearsum = sim_actives_yearsum
	)

}



Global_paramlist$nsim <- 1000


{
	start_time <- Sys.time()	
	penSim_DC_results <- run_sim_DC1()
	print(Sys.time() - start_time)
	suppressMessages(gc())
}


# penSim_DC_results$DC_actives_indiv %>% filter(sim == -1) %>% print
penSim_DC_results$DC_actives_yearsum %>% filter(sim == 0)


save(penSim_DC_results, file = "Outputs/penSim_DC_results_dev.RData")








