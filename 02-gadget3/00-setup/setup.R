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
vers <- 'models/37_baseline_lengroup_cv01_IGFS'

dbname <- file.path('db', 'atlantisiceland_v16_Scv02_reps100.duckdb')

simBiolPar <- readRDS("01-atlantis_to_mfdb/data/v16_Scv02_reps100/simBiolPar.rds")

## -----------------------------------------------------------------------------
## OPTIONS
## -----------------------------------------------------------------------------

## Model dimensions and stock
year_range <- 1960:2010
species_name <- 'COD'

## Whether or not to call the setup-data scripts
read_data <- FALSE
read_bootstrap_data <- FALSE

## Which model diagnostics o run
run_iterative <- FALSE
read_weights_and_optimise <- FALSE
rescale_weights <- FALSE
run_retro <- FALSE
run_bootstrap <- TRUE
run_mprofile <- FALSE
run_jitter <- FALSE
run_leaveout <- FALSE

## Model settings:
single_stock_model <- TRUE
maxlengthgroupgrowth <- 8
simple_initial_conditions <- TRUE
decadal_survey_selection <- FALSE
decadal_growth <- FALSE
threshold_year <- 2000
exponentiate_bbin <- TRUE
lencv <- 0.2
M_value <- 0.2

## Fleet options:
timevarying_selectivity <- TRUE ## Commercial fleet only, doesnt work with dome shaped atm
dome_comm <- FALSE
dome_survey <- FALSE
survey_fleet <- FALSE             ## If TRUE, the SI likelihood will incorporate fleet selectivity
estimate_catchability <- FALSE

## Likelihood options:
drop_AUT_survey <- TRUE
drop_IGFS_survey <- FALSE
aggregate_comm_ldist <- TRUE
numSIs <- TRUE
igfs_start_year <- 1985  # min(year_range)
aut_start_year <- 2000
include_si_slope <- FALSE
all_si_slope <- FALSE
si_len_groups <- c(5,35,45,60,80,150)
#si_len_groups <- c(5,35,45,60,80,100,150)
#si_len_groups <- c(5,35,45,60,75,95,140)
#si_len_groups <- c(5,30,45,60,75,90,140)
#si_len_groups <- c(5,30,45,60,80,100,140)

## Optimisation settings:
# Control list for stats::optim(method = 'BFGS'), used in the g3 iterative, retro, jitter, leaveout functions
optim_controls <- list(maxit = 2000, reltol = 1e-10)#.Machine$double.eps)#^2)
include_bound_penalty <- TRUE

## Bootstrap:
## 100 replicates are stored under unique sampling_ids in the duckdb
## we apply a single spatial bootstrap to each replicate
## this is in addition to the baseline, hence 101 nreps
boot_name <- 'BOOTSTRAP'
boot_params_folder <- 'WGTS'  #M_PROFILE_ITER/WGTS_M0.21'
nreps <- 100
boot_cores <- 35
boot_short <- TRUE
boot_repl <- 1 ## Don't change
boot_catchcomp <- TRUE

# M options:
Lorenzen_M <- FALSE
estimate_M_offset <- FALSE
estimate_M_scale <- FALSE

## M profile options:
profile_name <- 'M_PROFILE_ITER'
profile_reps <- 1
M_range <- rep(seq(0.15, 0.25, by = 0.01), each = profile_reps)
profile_short <- FALSE

## Jitter options:
jitter_name <- 'JITTER'
number_of_jitters <- 100
jitter_optimised_pars <- FALSE

## Leaveout options
leaveout_name <- 'LOCV'

## Iterative re-weighting options:
cv_floor <- 0.01             ## see ?g3_iterative
groupallSI <- FALSE
groupbysurvey <- FALSE
groupbylen <- TRUE


## Random effects
random_M <- FALSE
random_walk_M <- FALSE
random_recruitment <- FALSE


# control list for inner optimisations via TMB::newton
newton_control <- list(maxit = 50, tol = 1e-08, tol10 = 0)

## -----------------------------------------------------------------------------
## PARAMETERS
## -----------------------------------------------------------------------------

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


defaults_base <- list(area = mfdb_group("1" = paste0('Box', 0:52)),
                      timestep = mfdb_group('1'=1:3, '2'=4:5, '3'=6:8, '4'=9:10, '5'=11:12),
                      year = year_range,
                      species = toupper(species_name),
                      sampling_type = '0')

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(g3a_time(start_year = min(defaults_base$year),
                              end_year = max(defaults_base$year),
                              defaults_base$timestep
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
  defaults <- list(defaults_base)
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

##### Compile the r- and tmb-based models ######################################

## Collate actions
actions <- c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  if (random_M || random_recruitment) random_actions else NULL,
  time_actions,
  NULL
)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(c(actions,
                         list(g3a_report_detail(actions)),
                         list(gadget3::g3l_bounds_penalty(actions)),
                         NULL
                         ))

# Turn actions into an R function
model <- g3_to_r(attr(tmb_model, 'actions'))#, strict = TRUE, trace = TRUE)

## Fill in the parameter template
tmb_param <-
  attr(tmb_model, 'parameter_template') %>%

  ## Recruitment and initial conditions
  g3_init_val('*.rec.#', 500, lower = 0.001, upper = 1000, random = ifelse(random_recruitment, TRUE, FALSE)) %>%
  g3_init_val('*.init.#', 500, lower = 0.001, upper = 1000) %>%
  g3_init_val('*.rec.scalar', 100, spread = 0.99) %>%
  g3_init_val('*.init.scalar',
              ifelse(simple_initial_conditions, init.num.age1/1e3, 100),
              lower = ifelse(simple_initial_conditions, init.num.age1*0.7/1e3, 0.001),
              upper = ifelse(simple_initial_conditions, init.num.age1*1.3/1e3, 200)) %>%
  g3_init_val('init.F',
              ifelse(simple_initial_conditions, 0.1, 0),
              lower = {if (simple_initial_conditions) 0.05 else NULL},
              upper = {if (simple_initial_conditions) 0.375 else NULL}) %>%
  g3_init_val('*.lencv.#', lencv) %>%
  g3_init_val('*.lencv.1', lencv, lower = 0.01, upper = 0.8) %>%
  g3_init_val('*.rec.sd', 2, lower = 0.1, upper = 10) %>%

  ## Growth
  g3_init_val('*.Linf', 150, lower = 100, upper = 300) %>%
  g3_init_val('*.K', 0.13, spread = 0.5) %>%
  g3_init_val('*.K_60s', 0.13, spread = 0.5) %>%
  g3_init_val('*.K_70s', 0.13, spread = 0.5) %>%
  g3_init_val('*.K_80s', 0.13, spread = 0.5) %>%
  g3_init_val('*.K_90s', 0.13, spread = 0.5) %>%
  g3_init_val('*.K_00s', 0.13, spread = 0.5) %>%
  g3_init_val('*.t0', 0, lower = -5, upper = 1) %>%
  # g3_init_val('*.recl', 15, spread = 0.99) %>%       Using t0 (g3a_renewal_vonb_t0) instead of recl (g3a_renewal_vonb_recl)
  #g3_init_val('*.bbin', 50, lower = 1e-05, upper = 100) %>%
  g3_init_val('*.bbin', 50, lower = 1e-03, upper = 100) %>%
  g3_init_val('*.M.#', M_value, random = {if (random_M) TRUE else NULL}) %>%
  g3_init_val('*.M_scale', 1, spread = {if (estimate_M_scale) 0.1 else NULL}) %>%
  g3_init_val('*.M_offset', 0, lower = -0.2, upper = 0.2) %>%
  g3_init_val('recage', g3_stock_def(single_stock, 'minage'), 0, 20, 0) %>%

  ## Fleet selection parameters
  #  S-shaped
  g3_init_val('*.alpha', 0.25, spread = 0.99) %>%
  g3_init_val('*.alpha.#', 0.25, spread = 0.99) %>%
  g3_init_val('*.igfs.alpha.*', 0.25, spread = 0.99) %>%
  g3_init_val('*.aut.alpha.*', 0.25, spread = 0.99) %>%
  g3_init_val('*.l50', 50, spread = 0.6) %>%
  g3_init_val('*.l50.#', 50, spread = 0.6) %>%
  g3_init_val('*.igfs.l50.*', 50, spread = 0.6) %>%
  g3_init_val('*.aut.l50.*', 50, spread = 0.6) %>%
  #  Dome-shaped
  g3_init_val('*.p1', log(2), lower = 0, upper = 3) %>%
  g3_init_val('*.p3', 0.1, lower = 0.001, upper = 10) %>%
  g3_init_val('*.p4', 0.02, lower = 0.001, upper = 1e3) %>%
  g3_init_val('*.catchability', 10, lower = 0.001, upper = 1e4,1) %>%

  # ## RANDOM EFFECTS
  g3_init_val('*.M_sigma', 0.02, spread = 0.5) %>%
  #g3_init_val('*.rnd_M_weight', 1) %>%
  g3_init_val('*.recruitment_sigma', 0.2, spread = 0.5) %>%
  #g3_init_val('*.rnd_recruitment_weight', 1) %>%
  g3_init_val('zero', 0) %>%

  # Weight-length
  g3_init_val('*.walpha', lw.constants$a) %>%
  g3_init_val('*.wbeta', lw.constants$b)

if (random_recruitment){
  tmb_param[tmb_param$switch %in% c('cod.rec.1960'), 'random'] <- FALSE
  #tmb_param[tmb_param$switch %in% c('cod.rec.1970'), c('optimise', 'random')] <- c(TRUE, FALSE)
}
if (random_M){
 # tmb_param[tmb_param$switch %in% c('cod.M.1960'), 'random'] <- FALSE
#  tmb_param[tmb_param$switch %in% c('cod.M.2020'), c('optimise', 'random')] <- c(TRUE, FALSE)
}


if (Lorenzen_M){
  ## Scale so terminal M is 0.2
  mscale <- 0.2/Ma[Ma$age == 12,'M']
  tmb_param$value[paste0('cod.M.', 0:3)] <- Ma[Ma$age == 3,'M']
  tmb_param$value[paste0('cod.M.', 4:12)] <- Ma[Ma$age %in% 4:12,'M']
  tmb_param <- g3_init_val(tmb_param, 'cod_M_scale', mscale, 0.0001, 1.2, as.numeric(estimate_M_scale))
}

if (drop_AUT_survey){
  tmb_param[grepl('survQ4_weight$|ldist_aut_weight$', tmb_param$switch), 'value'] <- 0
}
if (drop_IGFS_survey){
  tmb_param[grepl('survQ1_weight$|ldist_igfs_weight$', tmb_param$switch), 'value'] <- 0
}

## --------------------------------------------------------------------------

## Run the R-model
result <- model(tmb_param$value)
print(result[[1]])

#tmp <- g3_fit(model, tmb_param)
#gadgetplots::gadget_plots(tmp, file_type = 'html', path = "TEST")

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
      si = c(paste0('len', c('005','035','045','060','080'), '_survQ1'),
             paste0('len', c('005','035','045','060','080'), '_survQ4')))
      #igfs = paste0('len', c('005','035','045','060','080'), '_survQ2'),
      #aut = paste0('len', c('005','035','045','060','080'), '_survQ4'))
  }else{
    groupings <- list(igfs1 = paste0('len', c('005','035'), '_survQ1'),
                     igfs2 = paste0('len', c('045','060','080'), '_survQ1'),
                     aut1 = paste0('len', c('005','035'), '_survQ4'),
                     aut2 = paste0('len', c('045','060','080'), '_survQ4')) 
 #   groupings <- list(igfs1 = paste0('len', c('005','030','045'), '_survQ2'),
#                      igfs2 = paste0('len', c('060','075','090'), '_survQ2'),
#                      aut1 = paste0('len', c('005','030','045'), '_survQ4'),
#                      aut2 = paste0('len', c('060','075','090'), '_survQ4')) 
    }
}

if (groupbylen){
  groupings <- list(
    small = c(
      if (!drop_IGFS_survey) paste0('len', c('005','035'), '_survQ1'),
      if (!drop_AUT_survey) paste0('len', c('005','035'), '_survQ4'),
      NULL
    ),
    large = c(
      if (!drop_IGFS_survey) paste0('len', c('045','060','080'), '_survQ1'),
      if (!drop_AUT_survey) paste0('len', c('045','060','080'), '_survQ4'),
      NULL
    )
  )
}

#if (groupbysurvey){
#  groupings <- list(
#    igfs = c(paste0('len', c('005','035','045','060','080'), '_survQ2'), 'ldist_igfs', 'aldist_igfs'),
#    aut = c(paste0('len', c('005','035','045','060','080'), '_survQ4'), 'ldist_aut', 'aldist_aut'))
#}

#groupings <- list(si = c(paste0('len', c('005','035','045','060','080'), '_survQ2'),
#                         paste0('len', c('005','035','045','060','080'), '_survQ4')))

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
}

if (read_weights_and_optimise){
  
  load('02-gadget3/data/tmp.wgts.Rdata')
  tmb_param$value[tmp.wgts$switch] <- tmp.wgts$value
  
  ## Run iterative re-weighting
  params.out <- g3_optim(tmb_model,
                         tmb_param,
                         control = optim_controls)
  
  ## Try ADREPORTS
  # sdout <- TMB::sdreport(obj.fun, gadget3::g3_tmb_par(params.out))
  
  ## Get the model fit
  fs::dir_create(file.path(base_dir, vers, 'WGTS_MEAN'))
  save(params.out, file.path(base_dir, vers, 'WGTS_MEAN', 'params.out.Rdata'))
  fit <- g3_fit(model, params.out)
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html')
  
  
  
}

if (rescale_weights){
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  par.in <- params_final
  #par.in$value[params_final$switch[(grepl('_weight$', params_final$switch))]] <- 
  #  params_final$value[params_final$switch[(grepl('_weight$', params_final$switch))]]
  par.in$value[par.in$switch[(grepl('survQ4', params_final$switch))]] <- 
    map(par.in$value[par.in$switch[(grepl('survQ4', params_final$switch))]], function(x) x/10)
  
  pout <- g3_optim(tmb_model, 
                   gadgetutils::jitter_params(par.in),
                   control = optim_controls)
  
  fs::dir_create(file.path(base_dir, vers, 'WGTS3'))
  save(pout, file = file.path(base_dir, vers, 'WGTS3/pout.Rdata'))
  
  ## Get the model fit
  fit <- g3_fit(model, pout)
  save(fit, file = file.path(base_dir, vers, 'fit3.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'figs3'), file_type = 'html')
  
}

if (random_M){
  fs::dir_create(file.path(base_dir, vers, 'RANDOM'))
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  rnd_param <- tmb_param
  tmp <- params_final[grepl('weight$', params_final$switch), 'switch']
  tmp <- tmp[tmp %in% rnd_param$switch]
  rnd_param$value[tmp] <- params_final$value[tmp]
  rm(tmp)
  
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model, 
                          jitter_params(rnd_param), 
                          inner.control = newton_control)
  
  obj.fun$env$tracepar <- TRUE
  
  out <- optim(par = obj.fun$par, 
               fn = obj.fun$fn, 
               gr = obj.fun$gr,
               method = 'BFGS',
               control = c(optim_controls, 
                           list(parscale = g3_tmb_parscale(rnd_param)))
  )
  sdout <- TMB::sdreport(obj.fun, out$par)
  
  save(tmb_model, rnd_param, 
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
  
  fs::dir_create(file.path(base_dir, vers, profile_name))
  
  if (profile_short){
    ## Take weights from params final
    load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
    tmb_param$value[grepl('_weight$', params_final$switch)] <-  
      params_final$value[grepl('_weight$', params_final$switch)]
  }
  
  ## Create input parameters for each m
  pars.in <- list()
  for (i in seq_along(M_range)){
    pars.in[[i]] <- tmb_param %>% g3_init_val('*.M.#', M_range[i])
  }
  names(pars.in) <- paste0('M', M_range)
  
  profile_run <- 
    parallel::mclapply(setNames(names(pars.in), names(pars.in)),function(x) {
      if(profile_short){
        try(g3_optim(tmb_model, 
                     gadgetutils::jitter_params(pars.in[[x]]), control = optim_controls))
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

if (read_bootstrap_data){
  nboots <- nreps
  mdb <- mfdb::mfdb(dbname)
  ## Indices done using spatial bootstrap, however, going to filter out areas with 0 fish as casuing bias in early length replicates
  bootboxes <- mfdb_sample_count(mdb, c('length', 'area'), 
                                 c(list(data_source = c('iceland-ldist-IGFS','iceland-ldist-AUT'),
                                        length = mfdb_interval('len', c(5,100), open_ended = c('lower', 'upper')),
                                        area = paste0('Box', 0:52)),
                                   defaults_base))[[1]]$area %>% unique()
  defaults <- list()
  for (i in 1:nreps){
    defaults[[i]] <- within(defaults_base,
                            {area = mfdb_bootstrap_group(1,
                                                         mfdb_group('1' = bootboxes),
                                                         seed = NULL)})
    ## Note, -1 to include the baseline
    defaults[[i]]$sampling_type <- as.character(i-1)
  }
  
  ## Using spatial bootstrap not noise generated through multinomial samplingf
  source(file.path(base_dir, '00-setup/setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup/setup-indices.R'))
  
  mfdb::mfdb_disconnect(mdb)
}

# ## Code to check bootstrap replicates
# load("~/Fishing-into-the-future/atlantis_to_gadget/02-gadget3/data/indices.Rdata")
# numindices_aut2 <- numindices_aut
# load("~/Fishing-into-the-future/atlantis_to_gadget/02-gadget3/data/bootstrap_indices.Rdata")
# lapply(1:101, function(x){ return(numindices_aut[[x]][['len60']] %>% 
#                                     mutate(id = x))}) %>% 
#   bind_rows(.id = 'id') %>% 
#   ggplot(aes(year, number)) + 
#   geom_line(aes(col = id), show.legend = FALSE) + 
#   geom_line(data = numindices_aut2$`0.0.0.0`$len60)



if (run_bootstrap){
  
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    stringr::str_subset('bootstrap') %>% 
    lapply(load,.GlobalEnv)
  
  if (boot_short){
    load(file = file.path(base_dir, vers, boot_params_folder ,'params_final.Rdata'))
    #params_final <- params.out
    boot_params <- params_final
    boot_params$value[!(grepl('_weight$', tmb_param$switch))] <- 
      tmb_param$value[!(grepl('_weight$', tmb_param$switch))]
  }else{
    boot_params <- tmb_param
  }
  
  
  ## Lets try constained the initial conditions to the base fit
  # boot_params <-
  #   boot_params %>%
  #   g3_init_val('init.scalar',
  #                 params_final$value$cod.init.scalar,
  #                 params_final$value$cod.init.scalar*0.9,
  #                 params_final$value$cod.init.scalar*1.1, 1) %>%
  #   g3_init_val('init.F',
  #                 params_final$value$init.F,
  #                 params_final$value$init.F*0.9,
  #                 params_final$value$init.F*1.1, 1)
  
  boot_model <- list()
  for(boot_repl in 1:nreps){
    source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actions
    boot_actions <- 
      c(stock_actions,
        fleet_actions,
        likelihood_actions,
        time_actions)
    
    boot_actions <- 
      c(boot_actions,
        list(g3a_report_detail(boot_actions)),
        list(gadget3::g3l_bounds_penalty(boot_actions)),
        NULL
        )
    
    boot_model[[boot_repl]] <- g3_to_tmb(boot_actions)
  } 
  fs::dir_create(file.path(base_dir, vers, boot_name))
  save(boot_model, file=file.path(base_dir, vers, boot_name,'boot_model.Rdata'))
  boot_run <- 
    parallel::mclapply(1:nreps,function(x) {
      if(boot_short){
        try(g3_optim(boot_model[[x]], 
                     gadgetutils::jitter_params(boot_params), 
                     control = optim_controls))
      } else {
        try(g3_iterative(file.path(base_dir, vers),
                         wgts = paste0(boot_name, '/WGTS_' ,x),
                         boot_model[[x]],
                         boot_params,
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
  
  save(boot_fit, file = file.path(base_dir, vers, boot_name, 'boot_fit.Rdata'))  
  
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
  
  
     
}


