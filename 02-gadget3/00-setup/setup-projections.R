################################################################################
##
## Runner for gadget model projections
##
################################################################################

library(tidyverse)
library(gadget3)
library(gadgetutils)
library(g3experiments)
library(mfdb)

source('src/functions_gadg.R')

## -----------------------------------------------------------------------------

## Model directory
base_dir <- '02-gadget3'
## Model version
vers <- '37_baseline_lengroup_cv01_IGFS'

## Variables
single_stock_model <- TRUE

## Load the model, fit and bootstrap
load(file = file.path(base_dir, 'models', vers, 'fit.Rdata'))
load(file = file.path(base_dir, 'models', vers, 'BOOTSTRAP', 'boot_fit.Rdata'))
load(file = file.path(base_dir, 'models', vers, 'tmb_model.Rdata'))
params_final <- fit$params

## -----------------------------------------------------------------------------
## MODEL SETUP
blim <- min(fit$res.by.year$total.biomass)
num_project_years <- 10
species_name <- 'COD'
mc_cores <- 20
rec_start_year <- 1988


year_range <- min(fit$res.by.year$year):(max(fit$res.by.year$year) + num_project_years)
defaults <- list(area = mfdb_group("1" = paste0('Box', 0:52)),
                 timestep = mfdb_group('1'=1:3, '2'=4:5, '3'=6:8, '4'=9:10, '5'=11:12),
                 year = year_range,
                 species = toupper(species_name),
                 sampling_type = NULL)

areas <- structure(1, names = "1")

## -----------------------------------------------------------------------------
## Bootstrap parameters

param_list <- c(list(baseline = fit$params), boot_fit %>% discard(~class(.)[1] == 'try-error') %>% map('params')) 
param_list <- lapply(param_list, function(x){
  tmp <- x %>% filter(switch != 'report_detail')
  return(tmp)
})
rec_list <- c(list(base = fit), 
              boot_fit %>% 
                discard(~class(.)[1] == 'try-error')) %>% 
                map(.f = g3_fix_recruitment) %>% 
                map('stock.recruitment') %>% 
  map(filter, recruitment > 0, year >= rec_start_year, year < 2020)

## -----------------------------------------------------------------------------
## Stock actions:

## Get stock objects
source(file.path(base_dir, '00-setup/setup-stocks.R'))

decadal_growth <- any(grepl('K_60s', names(fit$params$value)))

if (decadal_growth){
  vonb_K <- g3_formula(if (cur_year < 1970) K1 else
    if (cur_year > 1969 && cur_year < 1980) K2 else
      if (cur_year > 1979 && cur_year < 1990) K3 else
        if (cur_year > 1989 && cur_year < 2000) K4 else K5,
    K1 = g3_parameterized('K_60s', by_stock = stocks),
    K2 = g3_parameterized('K_70s', by_stock = stocks),
    K3 = g3_parameterized('K_80s', by_stock = stocks),
    K4 = g3_parameterized('K_90s', by_stock = stocks),
    K5 = g3_parameterized('K_00s', by_stock = stocks))
}else{
  vonb_K <- g3_parameterized('K', by_stock = stocks)
}

## Setup the projection actions
base_proj_actions <- 
  c(attributes(tmb_model)$actions,
    proj_stock_actions(num_project_years,
                       mat = single_stock))

## -----------------------------------------------------------------------------
## Fleet actions

dbname <- file.path('db', 'atlantisiceland_v16_Scv02_reps100.duckdb')
mdb <- mfdb::mfdb(dbname)
total_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(data_source = 'atlantis_fisheries'), defaults))
total_landings$`0.0.0` <- total_landings$`0.0.0` %>% mutate(area = '1')
mfdb::mfdb_disconnect(mdb)

comm_proj <- g3_fleet('comm_proj') %>% g3s_livesonareas(areas[c('1')])

if (any(grepl('alpha.2008', names(fit$params$value)))){
  sel_alpha <- ~g3_param('cod.comm.alpha.2008')
  sel_l50 <- ~g3_param('cod.comm.l50.2008')
}else{
  sel_alpha <- ~g3_param('cod.comm.alpha')
  sel_l50 <- ~g3_param('cod.comm.l50')
}

proj_fleet_actions <- 
  list(
    comm_proj %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x){
                            gadget3::g3_suitability_exponentiall50(
                              alpha = sel_alpha,
                              l50 = sel_l50
                              )
                          }),
                        catchability_f = g3a_predate_catchability_totalfleet(
                          g3_timeareadata('total_landings_proj', total_landings[[1]] %>%
                                            mutate(area = as.numeric(area),
                                                   step = as.numeric(step),
                                                   year = as.numeric(year)) %>% 
                                            filter(year > max(fit$res.by.year$year)))),
                        run_f = ~cur_year_projection)
  )

## -----------------------------------------------------------------------------

proj_actions <- c(base_proj_actions, proj_fleet_actions)
proj_actions <- c(proj_actions, list(gadget3::g3a_report_detail(proj_actions, run_f = ~TRUE)))

## Build models
tmb_proj <- g3_to_tmb(proj_actions)

## -----------------------------------------------------------------------------

## Get parameter template and fill in values with the optimised values from the assessment model
base.par.proj <- attributes(tmb_proj)$parameter_template
tmp <- params_final %>% filter(switch != 'report_detail')

par.proj <- base.par.proj
par.proj$value[tmp$switch] <- tmp$value
par.proj <- 
  par.proj %>% 
  g3_init_val('project_years', value = num_project_years) %>% 
  g3_init_val('blim', value = blim) %>% 
  g3experiments::g3p_project_rec(fit$stock.recruitment %>% 
                                   filter(year %in% 1988:2009),
                                 method = 'bootstrap')

## -----------------------------------------------------------------------------
## RUN THE PROGNOSIS
## -----------------------------------------------------------------------------

adfun <- g3_tmb_adfun(tmb_proj, par.proj, type = 'Fun')

pars_in <- 
  lapply(rec_list, function(x, par.proj){
    return(
      par.proj %>% g3experiments::g3p_project_rec(x, method = 'bootstrap')
    )
  }, par.proj = par.proj)

tres <- 
  parallel::mclapply(setNames(names(pars_in), names(pars_in)), function(x, adfun){
    return(
      run_proj(adfun, pars_in[[x]])
    )
  }, adfun = adfun, mc.cores = mc_cores) %>% bind_rows(.id = "id")

projout <-
  list(
    total.biomass = 
      tres %>% 
      filter(step == 1) %>% 
      mutate(Scenario = vers,
             idsc = paste(id, Scenario, sep = '-')) %>% 
      select(year, id, Scenario, value = total.biomass, idsc),
    ref.biomass = 
      tres %>% 
      filter(step == 1) %>% 
      mutate(Scenario = vers,
             idsc = paste(id, Scenario, sep = '-')) %>% 
      select(year, id, Scenario, value = ref.biomass, idsc),
    recruitment = 
      tres %>% 
      filter(step == 4) %>% 
      mutate(Scenario = vers,
             idsc = paste(id, Scenario, sep = '-')) %>% 
      select(year, id, Scenario, value = rec, idsc),
    fbar = 
      tres %>%
      group_by(year, id) %>% 
      summarise(fbar = mean(fbar)) %>% 
      mutate(Scenario = vers,
             idsc = paste(id, Scenario, sep = '-')) %>% 
      select(year, id, Scenario, value = fbar, idsc)
    )

save(projout, file = paste0('03-analyses/data/projout__', vers, '.Rdata'))
  