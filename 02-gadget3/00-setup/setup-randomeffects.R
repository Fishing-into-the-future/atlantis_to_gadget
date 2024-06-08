## Setup random effects
## Natural mortality
random_actions <- 
  c(
    {if (random_recruitment)
      list(
        g3l_random_walk(nll_name = 'rnd_recruitment',
                        param_f = substitute(log(avoid_zero(x)),
                                             list(x = g3_parameterized(paste0(stocks[[1]]$name, '.rec'), 
                                                                       by_year = TRUE, 
                                                                       random = TRUE,
                                                                       ifmissing = NaN))),
                        sigma_f = g3_parameterized(paste0(stocks[[1]]$name, '.recruitment_sigma')))
      )
    else NULL},
    {if (random_M)
      if (random_walk_M){
        list(
          g3l_random_walk(nll_name = 'rnd_natural_mortality',
                          param_f = substitute(x,
                                               list(x = g3_parameterized(paste0(stocks[[1]]$name, '.M'), 
                                                                         by_year = TRUE, 
                                                                         random = TRUE,
                                                                         ifmissing = NaN))),
                          sigma_f = g3_parameterized(paste0(stocks[[1]]$name, '.M_sigma')))  
        )
      }else{
        list(
          g3l_random_dnorm(nll_name = 'rnd_natural_mortality',
                           param_f = g3_parameterized(paste0(stocks[[1]]$name, '.M'), 
                                                      by_year = TRUE, 
                                                      random = TRUE,
                                                      ifmissing = NaN),
                           mean_f = g3_parameterized('zero', scale = 0),
                           sigma_f = g3_parameterized(paste0(stocks[[1]]$name, '.M_sigma')))
        )
      }
    else NULL},
    list()
  )
