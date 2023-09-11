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
vers <- 'models/01_TEST'

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
boot_name <- 'BOOTSTRAP'
nboots <- 100
boot_short <- TRUE
boot_repl <- 1 ## Don't change

## Fleet options:
dome_comm <- FALSE
dome_survey <- FALSE

## Likelihood options:
slope_SIs <- FALSE
aggregate_comm_ldist <- TRUE

## Optimisation settings:
cv_floor <- 0               ## For iterative re-weighting
optim_controls <- list(maxit = 2000, reltol = sqrt(.Machine$double.eps))


## -----------------------------------------------------------------------------
## PARAMETERS
## -----------------------------------------------------------------------------

year_range <- 1948:2012
species_name <- 'FCD'

## -----------------------------------------------------------------------------

defaults <- list(
#  area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
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


## Load data objects ----------------------------------------------------------
if(read_data){
  mdb <- mfdb::mfdb(file.path('db', 'atlantisiceland2.duckdb'))
  #mdb <- mfdb("../../mfdb/copy/iceland.duckdb")
  source(file.path(base_dir, '00-setup/setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup/setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup/setup-indices.R'))
  #source(file.path(base_dir, 'setup-initial_parameters.R'))
  mfdb::mfdb_disconnect(mdb)
} else {
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}



## Configure model actions ------------------------------------------------------------

source(file.path(base_dir, '00-setup/setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup/setup-likelihood.R'))  # Generates likelihood_actions

##### Compile the r- and tmb-based models ######################################

## Collate actions
actions <- c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
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
  g3_init_guess('\\.rec',100, 0.001, 500, 1) %>%
  g3_init_guess('\\.init', 100, 0.001, 500, 1) %>%
  g3_init_guess('lencv', 0.2, 0.1, 0.3, 0) %>%
  g3_init_guess('recl', 5, 1, 30, 1) %>%
  g3_init_guess('rec.sd', 5, 4, 20, 1) %>%
  g3_init_guess('recage', minage, 0, 20, 0) %>%
  g3_init_guess('rec.scalar', 50, 1, 100, 1) %>%
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>%
  g3_init_guess('init.scalar', 50, 1, 100, 1) %>%
  g3_init_guess('Linf', 140, 100, 200, 1) %>%
  g3_init_guess('\\.K', 0.09, 0.04, 0.2, 1) %>%
  g3_init_guess('bbin', 60, 1e-07, 1000, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>%
  g3_init_guess('l50', 50, 10, 100, 1) %>%
  g3_init_guess('init.F', 0, 0.1, 1, 0) %>%
  g3_init_guess('\\.M', 0.2, 0.001, 1, 0) %>%
  g3_init_guess('\\.M.[0-9]', 0.2, 0.001, 1, 0) %>%
  #  g3_init_guess('mat_initial_alpha', 1, 0.5, 2, 1) %>%
  #  g3_init_guess('mat_initial_a50', mat.a50$a50, 3, 15, 0) %>%
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat.alpha', 0.07, 0.01, 0.2, 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>%
  g3_init_guess('mat.l50', 50, 25, 120, 1) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  #g3_init_guess('sigma_alpha', init.sigma.coef[['alpha']], -1, 1, 0) %>%
  #g3_init_guess('sigma_beta', init.sigma.coef[['beta']], 0, 2, 0) %>%
  #g3_init_guess('sigma_gamma', init.sigma.coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('walpha', 0.000004832201, 1e-10, 1, 0) %>%
  g3_init_guess('wbeta', 3.151656234727, 2, 4, 0) %>%
  g3_init_guess('p0', 0.5,0,1,1) %>%
  g3_init_guess('p1', 0.5,0,1,1) %>%
  g3_init_guess('p2', 0.5,0,1,1) %>%
  g3_init_guess('p3', 50,0.01,100,1) %>%
  g3_init_guess('p4', 50,0.01,100,1) %>%
  g3_init_guess('LP', 100,10,300,1)


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
result[[1]]

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
  
  ## Run iterative re-weighting
  params.out <- g3_iterative(file.path(base_dir, vers),
                             wgts = 'WGTS',
                             model = tmb_model,
                             params.in = tmb_param,
                             grouping = list(sind = c('log_survQ1', 'log_survQ3')),
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
}


