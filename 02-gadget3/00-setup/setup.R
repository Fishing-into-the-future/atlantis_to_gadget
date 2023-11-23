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
vers <- 'models/10_baseline'
#vers <- 'TMP'
dbname <- file.path('db', 'atlantisiceland_baseline_v3.duckdb')

simBiolPar <- readRDS("ms-keyrun/data-raw/atlantisoutput/Out/v3_effic1_cv00_rep1/simBiolPar.rds")

## -----------------------------------------------------------------------------
## OPTIONS
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- TRUE

## Whether or not to run iterative reweighting
run_iterative <- TRUE
run_retro <- FALSE
run_bootstrap <- FALSE

## Model settings:
single_stock_model <- TRUE
maxlengthgroupgrowth <- 10

## Parameter option:
include_bound_penalty <- TRUE

## Bootstrap: 
boot_name <- 'BOOTSTRAP_ITER'
nreps <- 100
boot_short <- FALSE
boot_repl <- 1 ## Don't change

## Fleet options:
dome_comm <- FALSE
dome_survey <- FALSE

## Likelihood options:
aggregate_comm_ldist <- TRUE
numSIs <- TRUE
include_si_slope <- FALSE
all_si_slope <- FALSE
groupallSI <- FALSE

## Random effects
M_value <- 0.2
random_M <- FALSE
estimateM <- FALSE

## Optimisation settings:
cv_floor <- 0               ## For iterative re-weighting
optim_controls <- list(maxit = 1000, reltol = .Machine$double.eps^2)

# For g3_optim, BFGS in iterative, retro, jitter, leaveout
random_control <- list(maxit = 1000, reltol = 1e-10)#.Machine$double.eps^2)#1e-10)
# TMB::newton optimisations
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
           defaults$timestep),
  list())

areas <- structure(1, names = "1")

## Data and model folders
fs::dir_create(file.path(base_dir, c('data', 'models', vers)))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup/setup-stocks.R'))

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
  #source(file.path(base_dir, 'setup-initial_parameters.R'))
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
  defaults <- 
    within(defaults,
           {area = mfdb_bootstrap_group(nboots,
                                        mfdb_group('1' = paste0('Box', 0:52)),
                                        seed = 2459)})
  
  mdb <- mfdb::mfdb(dbname)
  source(file.path(base_dir, '00-setup/setup-catchdistribution.R'))
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
  g3_init_guess('\\.rec',50, 0.001, 100, 1) %>%
  g3_init_guess('\\.init', 50, 0.001, 100, 1) %>%
  g3_init_guess('lencv', 0.2, 0.1, 0.3, 0) %>%
  g3_init_guess('recl', 5, 1, 30, 1) %>%
  g3_init_guess('rec.sd', 5, 0.01, 20, 1) %>%
  g3_init_guess('recage', g3_stock_def(single_stock, 'minage'), 0, 20, 0) %>%
  g3_init_guess('rec.scalar', 5, 0.001, 20, 1) %>%
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>%
  g3_init_guess('init.scalar', 5, 0.001, 20, 1) %>%
  g3_init_guess('Linf', 140, 100, 180, 1) %>%
  g3_init_guess('\\.K', 0.09, 0.04, 0.2, 1) %>%
  g3_init_guess('\\.t0', -1, -5, 5, 1) %>%
  g3_init_guess('bbin', 60, 1e-07, 1000, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>%
  g3_init_guess('l50', 50, 10, 100, 1) %>%
  g3_init_guess('init.F', 0, 0.1, 1, 0) %>%
  g3_init_guess('\\.M', M_value, 0.001, 1, 0) %>%
  g3_init_guess('\\.M.[0-9]', M_value, 0.001, 1, 0) %>%
  g3_init_guess('M_offset', 0, -M_value*0.9, M_value*4, as.numeric(estimateM)) %>%
  #  g3_init_guess('mat_initial_alpha', 1, 0.5, 2, 1) %>%
  #  g3_init_guess('mat_initial_a50', mat.a50$a50, 3, 15, 0) %>%
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat.alpha', 0.07, 0.01, 0.2, 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>%
  g3_init_guess('mat.l50', 50, 25, 120, 1) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  #g3_init_guess('sigma_alpha', init.sigma.coef[['alpha']], -1, 1, 0) %>%
  #g3_init_guess('sigma_beta', init.sigma.coef[['beta']], 0, 2, 0) %>%
  #g3_init_guess('sigma_gamma', init.sigma.coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('walpha', simBiolPar$WLa, 1e-10, 1, 0) %>%
  g3_init_guess('wbeta', simBiolPar$WLb, 2, 4, 0) %>%
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
print(names(attributes(result)))

## Write out parameters and both models
save(tmb_param, file = file.path(base_dir, vers, 'tmb_param.Rdata'))
save(model, file = file.path(base_dir, vers, 'r_model.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------

# Compile and generate TMB ADFun (see ?TMB::MakeADFun)
obj.fun <- g3_tmb_adfun(tmb_model, tmb_param)

## Iterative re-weighting
if (run_iterative){
  
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
## Bootstrap
## -----------------------------------------------------------------------------

if (run_bootstrap){
  
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    stringr::str_subset('bootstrap') %>% 
    lapply(load,.GlobalEnv)
  
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  
  params_final$value[!(grepl('_weight$', params_final$switch))] <- 
    tmb_param$value[!(grepl('_weight$', params_final$switch))]
  
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
        try(g3_optim(boot_model[[x]], params_final, control = optim_controls))
      } else {
        try(g3_iterative(file.path(base_dir, vers),
                         wgts = paste(boot_name, '/WGTS' ,x,sep='_'),
                         boot_model[[x]],
                         params_final,
                         grouping = list(sind = c('survQ1', 'survQ3')),
                         cv_floor = cv_floor,
                         #resume_final = TRUE,
                         control = optim_controls,
                         use_parscale = TRUE,
                         mc.cores = 1))
      }
    },
    mc.cores = 50)#parallel::detectCores())
  
  save(boot_run, file = file.path(base_dir, vers, boot_name, 'boot_run.Rdata')) 
  save(boot_model, file = file.path(base_dir, vers, boot_name, 'boot_model.Rdata')) 
  
  boot_fit <- 
    1:nreps %>%  
    set_names(paste0('bs',1:nreps)) %>% 
    purrr::map(function(x) try(g3_fit(model = boot_model[[x]] ,params = boot_run[[x]]%>% g3_init_guess('project_years',1))) )
  
  save(boot_fit, file = file.path(base_dir, vers, boot_name, 'boot_fit.Rdata'))     
}


