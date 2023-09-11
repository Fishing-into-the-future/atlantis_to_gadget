## -----------------------------------------------------------------------------
##
## Runner to set up stocks parameters and actions using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------

## Initial numbers
# One parameter per age group per stock
initvonb <- gadget3::g3a_renewal_vonb(recage = gadget3::g3_stock_def(single_stock, 'minage'), by_stock = stocks)
# Use a CV for initial sds
initsd <- gadget3:::f_substitute(~x * y, list(x = gadget3::g3_parameterized('lencv', optimise = FALSE),
                                 y = initvonb))


## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

stock_actions <- 
  list(
    ## Initial conditions
    g3a_initialconditions_normalparam(single_stock,
                                      factor_f = g3a_renewal_initabund(M = g3_parameterized('M', by_stock = TRUE, by_age = TRUE)),
                                      mean_f = initvonb,
                                      stddev_f = initsd),
    ## Natural mortality
    gadget3::g3a_naturalmortality(single_stock, gadget3::g3a_naturalmortality_exp(by_age = TRUE)),
    ## Ageing
    gadget3::g3a_age(single_stock),
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
    gadget3::g3a_renewal_normalparam(single_stock, mean_f = initvonb),
    
    list()
    
  )
