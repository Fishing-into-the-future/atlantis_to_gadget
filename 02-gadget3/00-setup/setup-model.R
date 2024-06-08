## -----------------------------------------------------------------------------
##
## Runner to set up stocks parameters and actions using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------

## Initial numbers
# One parameter per age group per stock
initabun <- gadget3::g3a_renewal_initabund(by_stock = TRUE, by_stock_f = stocks)
if (simple_initial_conditions){
  tmp <- ~if (age == 0) 0L else 1L
  initabun <- gadget3::g3a_renewal_initabund(by_stock = TRUE, by_stock_f = stocks, init = tmp)
}
initvonb <- gadget3::g3a_renewal_vonb(recage = gadget3::g3_stock_def(imm_stock, 'minage'), by_stock = stocks)

# Use a CV for initial sds
initsd <- gadget3:::f_substitute(~x * y, list(x = gadget3::g3_parameterized('lencv', optimise = FALSE),
                                 y = initvonb))


## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

## IMMATURE ACTIONS

imm_actions <- 
  list(
    ## Initial conditions
    g3a_initialconditions_normalparam(imm_stock,
                                      factor_f = initabun,
                                      mean_f = initvonb,
                                      stddev_f = initsd),
    ## Natural mortality
    gadget3::g3a_naturalmortality(imm_stock, gadget3::g3a_naturalmortality_exp(by_stock = stocks, by_age = TRUE)),
    ## Ageing
    gadget3::g3a_age(imm_stock, output_stocks = list(mat_stock)),
    ## Growth and maturity
    gadget3::g3a_growmature(imm_stock,
                            ## Growth
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(by_stock = stocks),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(by_stock = stocks),
                              maxlengthgroupgrowth = maxlengthgroupgrowth,
                              by_stock = stocks 
                            ),
                            ## Maturity
                            maturity_f = gadget3::g3a_mature_continuous(),
                            output_stocks = list(mat_stock),
                            transition_f = ~TRUE
                            ),
    # Renewal
    gadget3::g3a_renewal_normalparam(imm_stock, mean_f = initvonb),
    
    list()
    
  )

## MATURE ACTIONS

mat_actions <- 
  list(
    ## Initial conditions
    g3a_initialconditions_normalparam(mat_stock,
                                      factor_f = initabun,
                                      mean_f = initvonb,
                                      stddev_f = initsd),
    ## Natural mortality
    gadget3::g3a_naturalmortality(mat_stock, gadget3::g3a_naturalmortality_exp(by_stock = stocks, by_age = TRUE)),
    ## Ageing
    gadget3::g3a_age(mat_stock),
    ## Growth
    gadget3::g3a_growmature(mat_stock,
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(by_stock = stocks),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(by_stock = stocks),
                              maxlengthgroupgrowth = maxlengthgroupgrowth)
    ),
    list()
  )

## Compile stock actions
stock_actions <- c(imm_actions, mat_actions, list())



