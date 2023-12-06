## -----------------------------------------------------------------------------
##
## Runner to set up stocks parameters and actions using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------


## Initial numbers
# One parameter per age group per stock
#initvonb <- gadget3::g3a_renewal_vonb(recage = gadget3::g3_stock_def(single_stock, 'minage'), by_stock = stocks)
initvonb <- g3a_renewal_vonb_t0(K = g3_parameterized("K", by_stock = TRUE),
                                t0 = g3_parameterized('t0', by_stock = TRUE))
# Use a CV for initial sds
initsd <- gadget3:::f_substitute(~x * y, list(x = gadget3::g3_parameterized('lencv', optimise = FALSE),
                                              y = initvonb))


naturalmortality <- g3_parameterized('M', by_stock = TRUE, by_age = TRUE, 
                                     optimise = FALSE, random = FALSE, 
                                     offset = g3_parameterized(paste0(single_stock$name, '_M_offset'), 
                                                               optimise = estimate_M_offset, 
                                                               random = random_M,
                                                               value = 0),
                                     scale = g3_parameterized(paste0(single_stock$name, '_M_scale'), 
                                                              optimise = estimate_M_scale, 
                                                              random = random_M,
                                                              value = 1))


## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

stock_actions <- 
  list(
    ## Initial conditions
    g3a_initialconditions_normalparam(single_stock,
                                      factor_f = g3a_renewal_initabund(M = naturalmortality),
                                      mean_f = initvonb,
                                      stddev_f = initsd),
    ## Natural mortality
    gadget3::g3a_naturalmortality(single_stock, gadget3::g3a_naturalmortality_exp(param_f = naturalmortality)),
    ## Ageing
    gadget3::g3a_age(single_stock, run_f = ~cur_step == 2),
    ## Growth and maturity
    gadget3::g3a_growmature(single_stock,
                            ## Growth
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(by_stock = stocks),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(by_stock = stocks),
                              maxlengthgroupgrowth = maxlengthgroupgrowth,
                              by_stock = stocks 
                            )
                            ),
    # Renewal
    gadget3::g3a_renewal_normalparam(single_stock, mean_f = initvonb, run_step = 2),
    
    list()
    
  )
