## -----------------------------------------------------------------------------
##
## Runner to build a Gadget3 model for cod
##
## Trying to replicate Taylor et als model (2007)
##
## -----------------------------------------------------------------------------

library(mfdb)
library(gadget3)
library(tidyverse)
library(gadgetutils)
library(g3experiments)
library(gadgetplots)

## Model directory
base_dir <- '02-gadget3'

## Model version
vers <- 'models/13_baseline'
#vers <- 'TMP'
dbname <- file.path('db', 'atlantisiceland_v6.duckdb')

simBiolPar <- readRDS("ms-keyrun/data-raw/atlantisoutput/Out/v6_effic1_cv00/simBiolPar.rds")

## -----------------------------------------------------------------------------
## OPTIONS
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- FALSE

## Which model diagnostics to run
run_iterative <- FALSE
run_retro <- FALSE
run_bootstrap <- TRUE
run_mprofile <- FALSE
run_jitter <- FALSE
run_leaveout <- FALSE

## Model settings:
single_stock_model <- TRUE
maxlengthgroupgrowth <- 10

## Fleet options:
dome_comm <- FALSE
dome_survey <- FALSE

## Likelihood options:
aggregate_comm_ldist <- TRUE
numSIs <- TRUE
include_si_slope <- TRUE
all_si_slope <- FALSE

## Optimisation settings:
# Control list for stats::optim(method = 'BFGS'), used in the g3 iterative, retro, jitter, leaveout functions
optim_controls <- list(maxit = 1000, reltol = .Machine$double.eps^2)
include_bound_penalty <- TRUE

## Bootstrap: 
boot_name <- 'BOOTSTRAP_SHORT_PARFINAL'
nreps <- 120
boot_cores <- 45
boot_short <- TRUE
boot_repl <- 1 ## Don't change

# M options:
Lorenzen_M <- FALSE
estimate_M_offset <- FALSE
estimate_M_scale <- FALSE

## M profile options:
profile_name <- 'M_PROFILE_ITER'
M_range <- seq(0.1, 0.3, by = 0.01)
profile_short <- FALSE

## Jitter options:
jitter_name <- 'JITTER'
number_of_jitters <- 100
jitter_optimised_pars <- FALSE

## Leaveout options
leaveout_name <- 'LOCV'

## Iterative re-weighting options:
cv_floor <- 0               ## see ?g3_iterative
groupallSI <- FALSE

## Random effects
M_value <- 0.2
random_M <- FALSE


# control list for inner optimisations via TMB::newton
newton_control <- list(maxit = 50, tol = 1e-08, tol10 = 0)

## -----------------------------------------------------------------------------
## PARAMETERS
## -----------------------------------------------------------------------------

year_range <- 1948:2020
species_name <- 'COD'

# Following WGSAM 
sppLookup <- data.frame(iceSpp = c("FCD"),
                        mfdbSpp = c("COD"),
                        ageGrpSize = c(1),
                        ageMax = c(16),
                        minLen = c(4),
                        maxLen = c(150),
                        iniScale = 1,
                        recScale = 1)
sppList <- left_join(simBiolPar, sppLookup %>% rename(Code=iceSpp))
sppListi <- sppList %>% filter(mfdbSpp==species_name)


defaults <- list(
#  area = mfdb_group("1" = paste0('Box', 0:52)),
  timestep = mfdb_timestep_quarterly,
  year = year_range,
  species = toupper(species_name))

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year),
           end_year = max(defaults$year),
           mfdb_group('1'=1:3, '2'=4:5, '3'=6:8, '4'=9:10, '5'=11:12)
           #defaults$timestep
           ),
  list())

areas <- structure(1, names = "1")

## Data and model folders
fs::dir_create(file.path(base_dir, c('data', 'models', vers)))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup/setup-stocks.R'))
source(file.path(base_dir, '00-setup/setup-initial_parameters.R'))

if (single_stock_model){
  source(file.path(base_dir, '00-setup/setup-single-stock-model.R'))  # Generates mat_stock_actions / imm_stock_actions
}else{
  source(file.path(base_dir, '00-setup/setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions  
}

## ----------------------------------------------------------------------------
## Load data objects ----------------------------------------------------------
## ----------------------------------------------------------------------------

if(read_data){
  nboots <- 1
  mdb <- mfdb::mfdb(dbname)
  source(file.path(base_dir, '00-setup/setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup/setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup/setup-indices.R'))
  mfdb::mfdb_disconnect(mdb)
} else {
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    stringr::str_subset('bootstrap', negate = TRUE) %>% 
    lapply(load,.GlobalEnv)
}

## Configure model actions ------------------------------------------------------------

source(file.path(base_dir, '00-setup/setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup/setup-likelihood.R'))  # Generates likelihood_actions
source(file.path(base_dir, '00-setup', 'setup-randomeffects.R'))  # Generates random actions

## -----------------------------------------------------------------------------
## Bootstrap data
## -----------------------------------------------------------------------------

if (read_data && run_bootstrap){
  
  nboots <- nreps
  mdb <- mfdb::mfdb(dbname)
  
  ## Catch distributions noise generated through multinomial sampling
  source(file.path(base_dir, '00-setup/setup-catchdistribution.R'))
  
  ## Indices done using spatial bootstrap, however, going to filter out areas with 0 fish as casuing bias in early length replicates
  bootboxes <- mfdb_sample_count(mdb, c('length', 'area'), 
                                 c(list(data_source = 'atlantis_survey_numbers_rep1',
                                        sampling_type = c('AUT', 'IGFS'),
                                        length = mfdb_interval('len', c(5,100), open_ended = c('lower', 'upper')),
                                        area = paste0('Box', 0:52)),
                                   defaults))[[1]]$area %>% unique()
  
  defaults <- 
    within(defaults,
           {area = mfdb_bootstrap_group(nboots,
                                        mfdb_group('1' = bootboxes),
                                        seed = 8217)})
  
  source(file.path(base_dir, '00-setup/setup-indices.R'))
  mfdb::mfdb_disconnect(mdb)
}


##### Compile the r- and tmb-based models ######################################

## Collate actions
actions <- c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  if (random_M) random_actions else NULL,
  time_actions
  #random_actions
)

# Turn actions into an R function
model <- g3_to_r(actions)#, strict = TRUE, trace = TRUE)

# You can edit the model code with:
#model <- edit(model)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(actions)

## Fill in the parameter template
tmb_param <-
  attr(tmb_model, 'parameter_template') %>%
  g3_init_guess('\\.rec',500, 0.001, 1000, 1) %>%
  g3_init_guess('\\.init', 500, 0.001, 1000, 1) %>%
  g3_init_guess('lencv', 0.2, 0.1, 0.3, 0) %>%
  g3_init_guess('recl', 5, 1, 30, 1) %>%
  g3_init_guess('rec.sd', 5, 0.01, 20, 1) %>%
  g3_init_guess('recage', g3_stock_def(single_stock, 'minage'), 0, 20, 0) %>%
  g3_init_guess('rec.scalar', 100, 0.001, 200, 1) %>%
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>%
  g3_init_guess('init.scalar', 100, 0.001, 200, 1) %>%
  g3_init_guess('Linf', 120, 100, 180, 1) %>%
  g3_init_guess('\\.K', 0.15, 0.04, 0.2, 1) %>%
  g3_init_guess('\\.t0', 1, -5, 5, 1) %>%
  g3_init_guess('bbin', 60, 1e-07, 1000, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>%
  g3_init_guess('l50', 50, 10, 100, 1) %>%
  g3_init_guess('init.F', 0, 0.1, 1, 0) %>%
  g3_init_guess('\\.M', M_value, 0.001, 1, 0) %>%
  g3_init_guess('\\.M.[0-9]', M_value, 0.001, 1, 0) %>%
  g3_init_guess('M_offset', 0, -M_value*0.9, M_value*4, as.numeric(estimate_M_offset)) %>%
  g3_init_guess('M_scale', 1, 0.9, 1.1, as.numeric(estimate_M_scale)) %>%
  #  g3_init_guess('mat_initial_alpha', 1, 0.5, 2, 1) %>%
  #  g3_init_guess('mat_initial_a50', mat.a50$a50, 3, 15, 0) %>%
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat.alpha', 0.07, 0.01, 0.2, 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>%
  g3_init_guess('mat.l50', 50, 25, 120, 1) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  #g3_init_guess('sigma_alpha', init.sigma.coef[['alpha']], -1, 1, 0) %>%
  #g3_init_guess('sigma_beta', init.sigma.coef[['beta']], 0, 2, 0) %>%
  #g3_init_guess('sigma_gamma', init.sigma.coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('walpha', lw.constants$a, lw.constants$a*0.9, lw.constants$a*1.1, 0) %>%
  g3_init_guess('wbeta', lw.constants$b, lw.constants$b*0.9, lw.constants$b*1.1, 0) %>%
  g3_init_guess('p0', 0.5,0,1,1) %>%
  g3_init_guess('p1', 0.5,0,1,1) %>%
  g3_init_guess('p2', 0.5,0,1,1) %>%
  g3_init_guess('p3', 50,0.01,100,1) %>%
  g3_init_guess('p4', 50,0.01,100,1) %>%
  g3_init_guess('LP', 100,10,300,1) %>% 
  ## RANDOM EFFECTS
  g3_init_guess('M_random', 0,-1,1,0) %>% 
  g3_init_guess('M_sigma', 0.2,0.01,0.4,1) %>% 
  g3_init_guess('rnd_M_weight', 1, 0.01, 100, 0) %>% 
  g3_init_guess('zero', 0, -1, 1, 0) 
  

if (Lorenzen_M){
  ## Scale so terminal M is 0.2
  mscale <- 0.2/Ma[Ma$age == 12,'M']
  tmb_param$value[paste0('cod.M.', 0:3)] <- Ma[Ma$age == 3,'M']
  tmb_param$value[paste0('cod.M.', 4:12)] <- Ma[Ma$age %in% 4:12,'M']
  tmb_param <- g3_init_guess(tmb_param, 'cod_M_scale', mscale, 0.0001, 1.2, as.numeric(estimate_M_scale))
}

## Initial sd's
if (any(grepl('\\.init\\.sd', tmb_param$switch))){

  tmb_param[grepl('mat\\.init\\.sd', tmb_param$switch), 'value'] <- 1
  tmb_param[grepl('imm\\.init\\.sd', tmb_param$switch), 'value'] <- 1
  ## Turn off optimisation
  tmb_param <-
    tmb_param %>%
    mutate(optimise = case_when(grepl('init.sd', switch) ~ FALSE,
                                grepl('.M.[\\.[0-9]', switch) ~ FALSE,
                                TRUE~optimise))

}

## --------------------------------------------------------------------------

if (include_bound_penalty){
  actions <- c(actions, list(g3l_bounds_penalty(tmb_param %>% filter(!grepl('_sigma$|_sigma_exp$', switch)))))
  model <- g3_to_r(actions)
  tmb_model <- g3_to_tmb(actions)
  tmb_param <- tmb_param[match(attr(tmb_model, 'parameter_template')$switch, tmb_param$switch),]
}

## Run the R-model
result <- model(tmb_param$value)
print(result[[1]])

#gadgetplots::gadget_plots(g3_fit(model, tmb_param), file_type = 'html', path = 'TEST_v3')

# List all available reports
#print(names(attributes(result)))

## Write out parameters and both models
save(tmb_param, file = file.path(base_dir, vers, 'tmb_param.Rdata'))
save(model, file = file.path(base_dir, vers, 'r_model.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------

# Compile and generate TMB ADFun (see ?TMB::MakeADFun)
obj.fun <- g3_tmb_adfun(tmb_model, tmb_param)

if (!numSIs){
  groupings <- list(sind = c('survQ1', 'survQ3'))  
}else{
  if (groupallSI){
    groupings <- list(
      igfs = paste0('len', c('05',35,45,60,80), '_survQ1'),
      #igfs = paste0('survQ1_len', c(5,20,35,45,60,80)),
      aut = paste0('len', c('05',35,45,60,80), '_survQ3'))
    #aut = paste0('survQ3_len', c(5,20,35,45,60,80)))
  }else{
    groupings <- list(#igfs1 = paste0('survQ1_len', c(5,20,35)),
      igfs1 = paste0('len', c('05',35), '_survQ1'),
      igfs2 = paste0('len', c(45,60,80), '_survQ1'),
      #aut1 = paste0('survQ3_len', c(5,20,35)),
      aut1 = paste0('len', c('05',35), '_survQ3'),
      aut2 = paste0('len', c(45,60,80), '_survQ3')) 
  }
}

## Iterative re-weighting
if (run_iterative){
  
  ## Run iterative re-weighting
  params.out <- g3_iterative(file.path(base_dir, vers),
                             wgts = 'WGTS',
                             model = tmb_model,
                             params.in = tmb_param,
                             grouping = groupings,
                             method = 'BFGS',
                             control = optim_controls,
                             use_parscale = TRUE,
                             shortcut = FALSE,
                             cv_floor = cv_floor,
                             resume_final = FALSE)
  
  ## Try ADREPORTS
  # sdout <- TMB::sdreport(obj.fun, gadget3::g3_tmb_par(params.out))
  
  
  ## Get the model fit
  fit <- g3_fit(model, params.out)
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html')
  
}else{
  load(file = file.path(base_dir, vers, 'fit.Rdata'))
  
  if (random_M){
    
    fs::dir_create(file.path(base_dir, vers, 'RANDOM'))
    
    load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
    tmp <- params_final[grepl('weight$', params_final$switch), 'switch']
    tmp <- tmp[tmp %in% tmb_param$switch]
    tmb_param$value[tmp] <- params_final$value[tmp]
    rm(tmp)
    rmultinom(10, size = 12, prob = c(0.1,0.2,0.8))
    # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
    obj.fun <- g3_tmb_adfun(tmb_model, 
                            jitter_params(tmb_param), 
                            inner.control = newton_control)
    
    obj.fun$env$tracepar <- TRUE
    
    out <- optim(par = obj.fun$par, 
                 fn = obj.fun$fn, 
                 gr = obj.fun$gr,
                 method = 'BFGS',
                 control = c(random_control, 
                             list(parscale = g3_tmb_parscale(tmb_param)))
    )
    sdout <- TMB::sdreport(obj.fun, out$par)
    
    save(tmb_model, tmb_param, 
         obj.fun,
         out, sdout, file = file.path(base_dir, vers, 'RANDOM/randomout.Rdata'))
    
    ## Merge estimated parameters into template
    pars <- summary(sdout, 'all')[,1]
    tmbpars <- g3_tmb_relist(tmb_param, pars[match(names(g3_tmb_par(tmb_param)), names(pars))])
    modcpp <- g3_to_tmb(c(attr(tmb_model, 'actions'), list(g3a_report_detail(attr(tmb_model, 'actions')))))
    newpars <- attr(modcpp, 'parameter_template')
    newpars$value[names(tmbpars)] <- tmbpars
    newpars$value$report_detail <- 1L
    
    ## Fit
    fit <- g3_fit(g3_to_r(attr(modcpp, 'actions')), newpars)
    save(fit, file = file.path(base_dir, vers, 'RANDOM/fit.Rdata'))
    gadget_plots(fit, file.path(base_dir, vers, 'RANDOM/figs'), file_type = 'html')
    
  }
  
}

## -----------------------------------------------------------------------------
## Jitter
## -----------------------------------------------------------------------------

if (run_jitter){
  
  ## Load the parameters that resulted from iterative re-weighting
  load(file.path(base_dir, vers,'WGTS','params_final.Rdata'))
  jitpar <- params_final
  
  ## Re-adjust parameters to initial values
  if (!jitter_optimised_pars){
    jitpar$value[params_final$switch[(!grepl('_weight$', params_final$switch))]] <- 
      tmb_param$value[params_final$switch[(!grepl('_weight$', params_final$switch))]]
  } 
  
  jitter_out <- gadgetutils::g3_jitter(file.path(base_dir, vers),
                                       jitter_name,
                                       tmb_model,
                                       tmb_param,
                                       njits = number_of_jitters,
                                       control = optim_controls,
                                       mc.cores = 35)
  
  jitter_fit <- 1:number_of_jitters %>% purrr::map(function(x) try(g3_fit(model = tmb_model, params = jitter_out[[x]])))
  save(jitter_fit, file = file.path(base_dir, vers, jitter_name, 'jitter_fit.Rdata'))
  
}

## -----------------------------------------------------------------------------
## Leave-out analysis
## -----------------------------------------------------------------------------

if (run_leaveout){
  
  ## Load the parameters that resulted from iterative re-weighting
  load(file.path(base_dir, vers,'WGTS','params_final.Rdata'))
  jitpar <- params_final
  
  ## Re-adjust parameters to initial values
  if (!jitter_optimised_pars){
    jitpar$value[params_final$switch[(!grepl('_weight$', params_final$switch))]] <- 
      tmb_param$value[params_final$switch[(!grepl('_weight$', params_final$switch))]]
  } 
  
  leaveout_out <- gadgetutils::g3_leaveout(file.path(base_dir, vers),
                                           leaveout_name,
                                           tmb_model,
                                           tmb_param,
                                           grouping = groupings,
                                           control = optim_controls,
                                           mc.cores = 30)
  
  leaveout_fit <- 1:length(leaveout_out) %>% purrr::map(function(x) try(g3_fit(model = tmb_model, params = leaveout_out[[x]])))
  save(leaveout_fit, file = file.path(base_dir, vers, leaveout_name, 'leaveout_fit.Rdata'))
  
}

## -----------------------------------------------------------------------------
## profile M
## -----------------------------------------------------------------------------

if (run_mprofile){
  
  fs::dir_create(file.path(base_dir, vers, 'M_profile'))
  
  ## Take weights from params final
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  
  if (profile_short){
    tmb_param$value[grepl('_weight$', params_final$switch)] <-  
      params_final$value[grepl('_weight$', params_final$switch)]
  }
  
  ## Create input parameters for each m
  pars.in <- list()
  for (i in seq_along(M_range)){
    pars.in[[i]] <- 
      tmb_param %>% 
      g3_init_guess('\\.M', M_range[i], 0.001, 1, 0) %>%
      g3_init_guess('\\.M.[0-9]', M_range[i], 0.001, 1, 0)
  }
  names(pars.in) <- paste0('M', M_range)
  
  profile_run <- 
    parallel::mclapply(setNames(names(pars.in), names(pars.in)),function(x) {
      if(profile_short){
        try(g3_optim(tmb_model, pars.in[[x]], control = optim_controls))
      } else {
        try(g3_iterative(file.path(base_dir, vers),
                         wgts = file.path(profile_name, paste0('WGTS_', x)),
                         tmb_model,
                         pars.in[[x]],
                         grouping = groupings,
                         cv_floor = cv_floor,
                         #resume_final = TRUE,
                         control = optim_controls,
                         use_parscale = TRUE,
                         mc.cores = 1))
      }
      }, mc.cores = 40)
  
  write.g3.file(bind_rows(lapply(profile_run, function(x) attr(x, 'summary')), .id = "id"), base_dir, file.path(vers, profile_name, 'summary.txt'))
  
  save(profile_run, file = file.path(base_dir, vers, profile_name, 'profile_run.Rdata')) 
  
  profile_fit <- 
    1:length(profile_run) %>%  
    set_names(paste0('m',1:length(profile_run))) %>% 
    purrr::map(function(x) try(g3_fit(model = model, params = profile_run[[x]])))
  
  save(profile_fit, file = file.path(base_dir, vers, profile_name, 'profile_fit.Rdata'))  
  
}

## -----------------------------------------------------------------------------
## Bootstrap
## -----------------------------------------------------------------------------

if (run_bootstrap){
  
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    stringr::str_subset('bootstrap') %>% 
    lapply(load,.GlobalEnv)
  
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  
  #params_final$value[!(grepl('_weight$', tmb_param$switch))] <- 
  #  tmb_param$value[!(grepl('_weight$', tmb_param$switch))]
  
  boot_model <- list()
  for(boot_repl in 1:nreps){
    source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actions
    boot_actions <- 
      c(stock_actions,
        fleet_actions,
        likelihood_actions,
        time_actions,
        list(g3l_bounds_penalty(tmb_param %>% filter(!grepl('_sigma$|_sigma_exp$', switch)))))
    boot_model[[boot_repl]] <- g3_to_tmb(boot_actions)
  } 
  fs::dir_create(file.path(base_dir, vers, boot_name))
  save(boot_model, file=file.path(base_dir, vers, boot_name,'boot_model.Rdata'))
  boot_run <- 
    parallel::mclapply(1:nreps,function(x) {
      if(boot_short){
        try(g3_optim(boot_model[[x]], 
                     gadgetutils::jitter_params(params_final), 
                     control = optim_controls))
      } else {
        try(g3_iterative(file.path(base_dir, vers),
                         wgts = paste0(boot_name, '/WGTS_' ,x),
                         boot_model[[x]],
                         tmb_param,
                         grouping = groupings,
                         cv_floor = cv_floor,
                         #resume_final = TRUE,
                         control = optim_controls,
                         use_parscale = TRUE,
                         mc.cores = 1))
      }
    },
    mc.cores = boot_cores)#parallel::detectCores())
  
  save(boot_run, file = file.path(base_dir, vers, boot_name, 'boot_run.Rdata')) 
  save(boot_model, file = file.path(base_dir, vers, boot_name, 'boot_model.Rdata')) 
  
  boot_fit <- 
    1:nreps %>%  
    set_names(paste0('bs',1:nreps)) %>% 
    purrr::map(function(x) try(g3_fit(model = boot_model[[x]] ,params = boot_run[[x]])))
  
  do.call('rbind', 
          lapply(setNames(names(boot_fit), names(boot_fit)), function(x){
            if ("gadget.fit" %in% class(boot_fit[[x]])){
              return(attr(boot_fit[[x]], 'summary') %>% mutate(id = x))
            }else{
              return(data.frame(type = 'g3_optim', id = x,
                                method = 'BFGS', maxiter = optim_controls$maxit, reltol = optim_controls$reltol,
                                optim_complete = 0, fn_calls = NA, gd_calls = NA,
                                convergence = NA, score = NA, return_complete = FALSE, comment = 'FAILED'))
            }
            })) %>% write.g3.file(file.path(base_dir, vers, boot_name), 'summary.txt')
  
  
  save(boot_fit, file = file.path(base_dir, vers, boot_name, 'boot_fit.Rdata'))     
}



