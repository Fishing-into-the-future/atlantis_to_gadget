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

## Model directory
base_dir <- 'cod_demo/gadget'

## Model version
vers <- 'models/TEST'

## -----------------------------------------------------------------------------
## OPTIONS
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- TRUE

## Whether or not to run iterative reweighting
run_iterative <- FALSE
run_retro <- FALSE
bootstrap <- FALSE

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
  species = 'FCD')

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

source(file.path(base_dir, 'setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions

## This setup-model script specified each action explicitly instead of through the model_actions function
#source(file.path(base_dir, '00-setup', 'setup-model_custom.R'))

## Load data objects ----------------------------------------------------------
if(read_data){
  mdb <- mfdb(file.path('cod_demo', 'db', 'atlantis_iceland_test_db.duckdb'))
  #mdb <- mfdb("../../mfdb/copy/iceland.duckdb")
  source(file.path(base_dir, 'setup-fleet-data.R'))
  source(file.path(base_dir, 'setup-catchdistribution.R'))
  source(file.path(base_dir, 'setup-indices.R'))
  #source(file.path(base_dir, 'setup-initial_parameters.R'))
} else {
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

## Configure model actions ------------------------------------------------------------


source(file.path(base_dir, 'setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, 'setup-likelihood.R'))  # Generates ling_likelihood_actions
#source(file.path(base_dir, '00-setup', 'setup-randomeffects.R'))  # Generates ling_likelihood_actions
#source(file.path(base_dir, '00-setup', 'setup-randomeffects_new.R'))  # Generates ling_likelihood_actions

## setup_likelihood_alphabeta creates parameters for the SIs that are optimised
#  source(file.path(base_dir, '00-setup', 'setup-likelihood_alphabeta.R')) # Generates likelihood_actions (alpha and beta optimized)

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

# Get the parameter template
tmb_param <- attr(tmb_model, 'parameter_template')

## Fill in the parameter template
tmb_param <-
  tmb_param %>%
  #g3_init_guess('retro', 5, NA, NA, 0) %>%
  g3_init_guess('\\.rec',100, 0.001, 1000, 1) %>%
  g3_init_guess('\\.init', 100, 0.001, 1000, 1) %>%
  g3_init_guess('recl', 5, 1, 30, 1) %>%
  g3_init_guess('rec.sd', 5, 4, 20, 1) %>%
  g3_init_guess('rec.scalar', 100, 1, 500, 1) %>%
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>%
  g3_init_guess('init.scalar', 100, 1, 500, 1) %>%
  g3_init_guess('Linf', 140, 100, 200, 1) %>%
  g3_init_guess('\\.K', 90, 40, 200, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 100, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>%
  g3_init_guess('l50', 50, 10, 100, 1) %>%
  g3_init_guess('init.F', 0.4, 0.1, 1, 1) %>%
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>%
  #  g3_init_guess('mat_initial_alpha', 1, 0.5, 2, 1) %>%
  #  g3_init_guess('mat_initial_a50', mat.a50$a50, 3, 15, 0) %>%
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat1', 70, 10, 200, 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>%
  g3_init_guess('mat2', 25, 75, 120, 1) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
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

if (!run_iterative){

  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model, tmb_param)
  # writeLines(TMB::gdbsource(g3_tmb_adfun(tmb_model, tmb_param, compile_flags = "-g", output_script = TRUE)))

  # Run model once, using g3_tmb_par to reshape tmb_param into param vector.
  # Will return nll
  obj.fun$fn(g3_tmb_par(tmb_param))

  # Run model once, returning model report
  obj.fun$report(g3_tmb_par(tmb_param))

  # tmp <- nlminb(g3_tmb_par(tmb_param),
  #                   obj.fun$fn,
  #                   obj.fun$gr,
  #                   upper = g3_tmb_upper(tmb_param),
  #                   lower = g3_tmb_lower(tmb_param),
  #                   control = list(trace = 2, iter.max=1000, rel.tol = .Machine$double.eps))
  #
  # tmp <- opm(g3_tmb_par(tmb_param),
  #                  obj.fun$fn,
  #                  obj.fun$gr,
  #                  upper = g3_tmb_upper(tmb_param),
  #                  lower = g3_tmb_lower(tmb_param),
  #                  method = 'ALL',
  #                  control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))

  # Run model through R optimiser, using bounds set in tmb_param
  fit.opt <- optim(g3_tmb_par(tmb_param),
                   obj.fun$fn,
                   obj.fun$gr,
                   method = 'BFGS',
                   control = list(trace = 2, maxit = 1000, reltol = .Machine$double.eps^2,
                                  parscale = g3_tmb_parscale(tmb_param)))

  save(model, fit.opt, tmb_param, file = file.path(base_dir, vers, 'fit.opt.Rdata'))

  source("~/WORK/gadget-framework/atlantisom/R/g3_fit_test.R")

  fit <- g3_fit(model, g3_tmb_relist(tmb_param, fit.opt$par))
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))


}else{

  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model,tmb_param)

  ## Run iterative re-weighting
  params.out <- g3_iterative(file.path(base_dir, vers),
                             wgts = 'WGTS',
                             model,
                             tmb_model,
                             tmb_param,
                             grouping = list(),
                             opt_method = 'BFGS',
                             use_parscale = TRUE)


  ## Get the model fit
  fit <- gadget3:::g3_fit(model, params.out)
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))

  ## Plot gadget2 vs gadget3 comparisons, and get all model plots
  gadget_plots(fit, file.path(base_dir, vers, 'figs'))
  #source(file.path(base_dir, "src/g3_vs_assessmentmodel_graphs.R"))

  ## Run the retro
  if (run_retro){
    retro <- g3_retro(file.path(base_dir, vers),
                      tmb_model,
                      params.out,
                      num.years = 10)
  }
}

## Iterative re-weighting step-by-step
if (FALSE){
  res1 <-  g3_lik_out(model, tmb_param)
  res2 <-  g3_iterative_setup(res1, grouping = list(sind1 = c('log_si_aut_1',
                                                              'log_si_aut_2a',
                                                              'log_si_aut_2b'),
                                                    sind2 = c('log_si_aut_3a',
                                                              'log_si_aut_3b',
                                                              'log_si_aut_3c',
                                                              'log_si_aut_3d')))

  res3 <-  parallel::mclapply(res2$params, function(x) g3_iterative_run(x,
                                                                        tmb_model), mc.cores = parallel::detectCores())
  res4 <-  parallel::mclapply(res3, function(x) g3_lik_out(model,x), mc.cores = parallel::detectCores())
  res5 <-  g3_iterative_final(res4)
  res6 <-  parallel::mclapply(res5, function(x) g3_iterative_run(x, tmb_model), mc.cores = parallel::detectCores())
  res7 <-  parallel::mclapply(res6, function(x) g3_lik_out(model,x), mc.cores = parallel::detectCores())

  lapply(res6, function(x) attr(x, 'summary')) %>% dplyr::bind_rows(.id = 'group')

}
