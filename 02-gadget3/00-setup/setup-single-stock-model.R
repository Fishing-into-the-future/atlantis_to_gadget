## -----------------------------------------------------------------------------
##
## Runner to set up stocks parameters and actions using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------

## Note regarding setup in Atlantis:
# SPAWNING 
# number of times an age-structured group spawn per year given by NumSpawns in functional_groups file
# All spawn produced in one time step, day of year is set by Time_Spawn
# recruitment delays set by spawn_period in prm file

# RECRUITMENT:
# Recruits arrive in the model after a set larval period
# From this point recruits continually arrive for Recruit_Period days
# larval period = Recruit_time + Spawn_Period
#
# Cod - NumSpawns = 1; TimeSpawn = 60; Spawn_Period = 60; Recruit_Time = 30; Recruit_period = 60
#
# So cod spawn on 1st of March and recruits start arriving 30 days later (March 31st) and arrive continually for 60 days until May 30th
# So should recruit into g3 model step 3

# 73 - March 14th - step 1
# 146 - May 26th - step 2
# 219 - August 7th - step 3

## SCHEDULE
# All cod age on February 28th - step 1
# Cod spawn 1st March - step 1
# Recruitment starts 31st March - step 1
# Recruitment ends 30th May - step - 

## TIMEVARYING GROWTH?
if (decadal_growth){
  vonb_K <- g3_formula(if (cur_year < 1970) K1 else
                       if (cur_year > 1969 && cur_year < 1980) K2 else
                       if (cur_year > 1979 && cur_year < 1990) K3 else
                       if (cur_year > 1989 && cur_year < 2000) K4 else K5,
                       K1 = g3_parameterized('K_60s', by_stock = stocks),
                       K2 = g3_parameterized('K_70s', by_stock = stocks),
                       K3 = g3_parameterized('K_80s', by_stock = stocks),
                       K4 = g3_parameterized('K_90s', by_stock = stocks),
                       K5 = g3_parameterized('K_00s', by_stock = stocks))
  # vonb_K <- g3_formula(if (cur_year < ty) K1 else K2,
  #                      ty = 2000,
  #                      K1 = g3_parameterized('K_early', by_stock = stocks),
  #                      K2 = g3_parameterized('K_late', by_stock = stocks))
}else{
  vonb_K <- g3_parameterized('K', by_stock = stocks)
}



g3a_renewal_vonb_t0_ageoffset <- 
  gadget3:::f_substitute(
    quote(Linf * (1 - exp(-1 * K * (recage - t0)))),
    list(Linf = g3_parameterized("Linf", by_stock = TRUE), 
         K = g3_parameterized("K", by_stock = TRUE), 
         t0 = g3_parameterized("t0", by_stock = TRUE),
         recage = g3_stock_def(single_stock, 'minage') + 1))


## Initial numbers
# One parameter per age group per stock
#initvonb <- g3a_renewal_vonb(recage = g3_stock_def(single_stock, 'minage'), by_stock = stocks)

# Use a CV for initial sds
#initsd <- :f_substitute(~x * y, list(x = g3_parameterized('lencv', by_stock = TRUE, optimise = FALSE),
#                                              y = initvonb))

naturalmortality <- g3_parameterized('M', 
                                     by_stock = stocks, 
                                     by_age = ifelse(random_M, FALSE, TRUE),
                                     by_year = ifelse(random_M, TRUE, FALSE),
                                     optimise = FALSE, 
                                     random = ifelse(random_M, TRUE, FALSE),
                                     ifmissing = NaN)#, 
                                     # offset = g3_parameterized('M_offset', 
                                     #                           optimise = estimate_M_offset,
                                     #                           by_stock = TRUE,
                                     #                           value = 0),
                                     # scale = g3_parameterized('M_scale', 
                                     #                          optimise = estimate_M_scale, 
                                     #                          by_stock = TRUE,
                                     #                          value = 1))

initabun <- g3a_renewal_initabund(M = naturalmortality)
if (simple_initial_conditions){
  tmp <- ~1L#if (age == 0) 0L else 1L
  initabun <- g3a_renewal_initabund(
    scalar = g3_parameterized('init.scalar', by_stock = stocks, scale = 1e3, value = 1),
    M = naturalmortality, 
    init = tmp, 
    recage = g3_stock_def(single_stock, 'minage'))
}

if (decadal_growth){
  grow_actions <- 
    
    # list(
    #   g3a_growmature(single_stock,
    #                  ## Growth
    #                  impl_f = g3a_grow_impl_bbinom(
    #                    delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_early', by_stock = stocks), by_stock = stocks),
    #                    delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
    #                    beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
    #                    maxlengthgroupgrowth = maxlengthgroupgrowth
    #                  )
    #                  , run_f = ~cur_year < 2000),
    #   g3a_growmature(single_stock,
    #                  ## Growth
    #                  impl_f = g3a_grow_impl_bbinom(
    #                    delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_late', by_stock = stocks), by_stock = stocks),
    #                    delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
    #                    beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
    #                    maxlengthgroupgrowth = maxlengthgroupgrowth
    #                  )
    #                  , run_f = ~cur_year > 1999)
    
    
    list(
      g3a_growmature(single_stock,
                     ## Growth
                     impl_f = g3a_grow_impl_bbinom(
                       delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_60s', by_stock = stocks), by_stock = stocks),
                       delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
                       beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                       maxlengthgroupgrowth = maxlengthgroupgrowth
                    )
                    , run_f = ~cur_year < 1970),
      g3a_growmature(single_stock,
                     ## Growth
                     impl_f = g3a_grow_impl_bbinom(
                       delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_70s', by_stock = stocks), by_stock = stocks),
                       delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
                       beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                       maxlengthgroupgrowth = maxlengthgroupgrowth
                     )
                     , run_f = ~cur_year > 1969 && cur_year < 1980),
      g3a_growmature(single_stock,
                     ## Growth
                     impl_f = g3a_grow_impl_bbinom(
                       delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_80s', by_stock = stocks), by_stock = stocks),
                       delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
                       beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                       maxlengthgroupgrowth = maxlengthgroupgrowth
                     )
                     , run_f = ~cur_year > 1979 && cur_year < 1990),
      g3a_growmature(single_stock,
                     ## Growth
                     impl_f = g3a_grow_impl_bbinom(
                       delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_90s', by_stock = stocks), by_stock = stocks),
                       delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
                       beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                       maxlengthgroupgrowth = maxlengthgroupgrowth
                     )
                     , run_f = ~cur_year > 1989 && cur_year < 2000),
      g3a_growmature(single_stock,
                     ## Growth
                     impl_f = g3a_grow_impl_bbinom(
                       delta_len_f = g3a_grow_lengthvbsimple(kappa_f = g3_parameterized('K_00s', by_stock = stocks), by_stock = stocks),
                       delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
                       beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                       maxlengthgroupgrowth = maxlengthgroupgrowth
                     )
                     , run_f = ~cur_year > 1999)
  )
}else{
  grow_actions <-
    list(
      g3a_growmature(single_stock,
                     ## Growth
                     impl_f = g3a_grow_impl_bbinom(
                       delta_len_f = g3a_grow_lengthvbsimple(kappa_f = vonb_K, by_stock = stocks),
                       delta_wgt_f = g3a_grow_weightsimple(by_stock = stocks),
                       beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                       maxlengthgroupgrowth = maxlengthgroupgrowth
                     )
      )
    )
}
  

## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

stock_actions <- 
  c(
    list(
      ## Initial conditions
      g3a_initialconditions_normalcv(single_stock,
                                     factor_f = initabun,
                                     mean_f = g3a_renewal_vonb_t0(K = vonb_K,
                                                                  by_stock = stocks),
                                     cv_f = g3_parameterized("lencv", 
                                                             by_stock = stocks, 
                                                             by_age = TRUE,
                                                             value = 0.1, 
                                                             optimise = FALSE)),
      ## Natural mortality
      g3a_naturalmortality(single_stock, g3a_naturalmortality_exp(param_f = naturalmortality)),
      ## Ageing
      g3a_age(single_stock, run_f = ~cur_step == 2)#, run_at = 7),  
      ),
    grow_actions,
    list(
      # Renewal
      g3a_renewal_normalcv(single_stock, 
                           mean_f = g3a_renewal_vonb_t0(K = vonb_K, 
                                                        by_stock = stocks),
                           cv_f = g3_parameterized(paste0("lencv.", g3_stock_def(single_stock, 'minage')), 
                                                   by_stock = stocks, 
                                                   value = 0.1),
                           run_step = 3, run_at = 2) ## Recruitment happens at beginning of 3rd step  
    ),
    list()
  )
