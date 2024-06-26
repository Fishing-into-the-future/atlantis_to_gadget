## -----------------------------------------------------------------------------
##
## Setup likelihood
##
## -----------------------------------------------------------------------------

fix_name_length <- function(name){
  return(
    paste0('len', 
           paste(rep('0', 6-nchar(name)), collapse = ''),
           gsub('len', '', name))
  )
}

if (boot_catchcomp){
  boot_repl_cc <- boot_repl
}else{
  boot_repl_cc <- 1
}


nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

## Aggregate ldist.com to yearly
if (aggregate_comm_ldist){
  ldist.sea2 <- aggregate(number ~ year + area + age + length, ldist.sea[[boot_repl_cc]], FUN = sum)
  attributes(ldist.sea2)$year <- attributes(ldist.sea[[1]])$year
  attributes(ldist.sea2)$area <- attributes(ldist.sea[[1]])$area
  attributes(ldist.sea2)$age <- attributes(ldist.sea[[1]])$age
  attributes(ldist.sea2)$length <- attributes(ldist.sea[[1]])$length
}else{
  ldist.sea2 <- ldist.sea[[boot_repl]]
}

likelihood_actions <- list(
  g3l_understocking(stocks, nll_breakdown = nll_breakdown, weight = 1e6),

  # g3l_catchdistribution(
  #   'aldist_comm',
  #   aldist.sea[[boot_repl]] %>% mutate(area = '1') ,
  #   fleets = list(comm),
  #   stocks = stocks,
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  
  g3l_catchdistribution(
    'ldist_comm',
    (ldist.sea2 %>% 
       mutate(area = '1') %>% 
       filter(year <= max(year_range))),
    fleets = list(comm),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
   g3l_catchdistribution(
     'ldist_igfs',
     (ldist.igfs[[boot_repl_cc]] %>% 
        mutate(area = '1') %>% 
        filter(year >= igfs_start_year) %>% 
        filter(year <= max(year_range))), 
     fleets = list(igfs),
     stocks = stocks,
     g3l_distribution_sumofsquares(),
     nll_breakdown = nll_breakdown,
     report = lik_report),

  g3l_catchdistribution(
    'aldist_igfs',
    (aldist.igfs[[boot_repl_cc]] %>% 
       mutate(area = '1') %>% 
       filter(year >= igfs_start_year) %>% 
       filter(year <= max(year_range))), 
    fleets = list(igfs),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'aldist_aut',
    (aldist.aut[[boot_repl_cc]] %>% 
       mutate(area = '1') %>% 
       filter(year >= aut_start_year) %>% 
       filter(year <= max(year_range))), 
    fleets = list(aut),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_aut',
    (ldist.aut[[boot_repl_cc]] %>% 
       mutate(area = '1') %>% 
       filter(year >= aut_start_year) %>% 
       filter(year <= max(year_range))), 
    fleets = list(aut),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  list()
)
if (numSIs){
  
  ## If estimating catchability
  alpha <- NULL
  if (estimate_catchability) alpha = 1
  
  surv.lik <- list()
  ## Autumn
  for (i in 1:length(numindices_aut[[boot_repl]])){
    
    beta <- 1
    if (include_si_slope){
      if (names(numindices_aut[[boot_repl]])[[i]] %in% paste0('len', c(80))) beta <- NULL
    }
    if (all_si_slope) beta <- NULL
    
    #alpha <- g3_parameterized(paste0('alpha_', i), lower = -10, upper = 10, value = 0, optimise = TRUE)
    #beta <- g3_parameterized(paste0('beta_', i), lower = -10, upper = 10, value = 0, optimise = TRUE)
    
    if (survey_fleet){
      tmp <- 
        list(
          g3l_catchdistribution(
            paste0(fix_name_length(names(numindices_aut[[boot_repl]])[[i]]), '_survQ4'),
            obs_data = numindices_aut[[boot_repl]][[i]] %>% 
              mutate(area = '1', step = as.numeric(step)) %>% 
              filter(year >= aut_start_year) %>% filter(year <= max(year_range)),
            fleets = list(aut),
            stocks = stocks,
            function_f = g3l_distribution_surveyindices_log(alpha = alpha, beta = beta),
            nll_breakdown = nll_breakdown,     
            report = lik_report)
        )
    }else{
      tmp <- 
        list(
          g3l_abundancedistribution(
            paste0(fix_name_length(names(numindices_aut[[boot_repl]])[[i]]), '_survQ4'),
            obs_data = numindices_aut[[boot_repl]][[i]] %>% 
              mutate(area = '1', step = as.numeric(step)) %>% 
              filter(year >= aut_start_year) %>% filter(year <= max(year_range)),
            fleets = list(),
            stocks = stocks,
            function_f = g3l_distribution_surveyindices_log(alpha = alpha, beta = beta),
            nll_breakdown = nll_breakdown,     
            report = lik_report)
        )  
    }
    
    #print(beta)
    surv.lik <- c(surv.lik, tmp)
    
  }
  ## IGFS
  for (i in 1:length(numindices_igfs[[boot_repl]])){

    beta <- 1
    if (include_si_slope){
      if (names(numindices_igfs[[boot_repl]])[[i]] %in% paste0('len', c(80))) beta <- NULL
    }
    if (all_si_slope) beta <- NULL

    if (survey_fleet){
      tmp <-
        list(
          g3l_catchdistribution(
            paste0(fix_name_length(names(numindices_igfs[[boot_repl]])[[i]]), '_survQ1'),
            obs_data = numindices_igfs[[boot_repl]][[i]] %>%
              mutate(area = '1', step = as.numeric(step)) %>%
              filter(year >= igfs_start_year) %>% filter(year <= max(year_range)),
            fleets = list(igfs),
            stocks = stocks,
            function_f = g3l_distribution_surveyindices_log(alpha = alpha, beta = beta),
            nll_breakdown = nll_breakdown,
            report = lik_report)
        )
    }else{
      tmp <-
        list(
          g3l_abundancedistribution(
            paste0(fix_name_length(names(numindices_igfs[[boot_repl]])[[i]]), '_survQ1'),
            obs_data = numindices_igfs[[boot_repl]][[i]] %>%
              mutate(area = '1', step = as.numeric(step)) %>%
              filter(year >= igfs_start_year) %>% filter(year <= max(year_range)),
            fleets = list(),
            stocks = stocks,
            function_f = g3l_distribution_surveyindices_log(beta = beta),
            nll_breakdown = nll_breakdown,
            report = lik_report)
        )
    }

    #print(beta)
    surv.lik <- c(surv.lik, tmp)

  }
  surv.lik <- c(surv.lik, list())
}else{
  surv.lik <- 
    list(
      g3l_abundancedistribution(
        'survQ1',
        (indices_igfs[[boot_repl]] %>% 
           mutate(area = '1',
                  weight = total_weight,
                  step = as.numeric(step)) %>%
           select(-total_weight)),
        fleets = list(),
        stocks = stocks,
        g3l_distribution_surveyindices_log(beta = if (slope_SIs) NULL else 1),
        nll_breakdown = nll_breakdown,
        report = lik_report),
      
      g3l_abundancedistribution(
        'survQ3',
        (indices_aut[[boot_repl]] %>%
           mutate(area = '1',
                  weight = total_weight,
                  step = as.numeric(step)) %>%
           select(-total_weight)),
        fleets = list(),
        stocks = stocks,
        g3l_distribution_surveyindices_log(beta = if (slope_SIs) NULL else 1),
        nll_breakdown = nll_breakdown,
        report = lik_report),
      
      list()
    )
}

likelihood_actions <- c(likelihood_actions, surv.lik)
