## -----------------------------------------------------------------------------
##
## (1) Initialise the Atlantis to gadget scripts
## (2) Read in the Atlantis truth
##
## -----------------------------------------------------------------------------

## Libraries
library(atlantisom)
library(gadget3)
library(gadgetutils)
library(tidyverse)
library(mfdbatlantis)

## Read in local scripts - modified scripts from atlantisom allowing > 10 cohorts
source('src/run_truth_local.R')
source('src/load_nc_cohort.R')
#source('src/load_nc_local.R')
source('src/load_biolprm_local.R')
source('src/om_list_isl.R')
source('src/om_comps_isl.R')
source('src/sample_ages_isl.R')
source('src/calc_age2length_isl.R')
source('src/sample_survey_biomass_box.R')
source('src/sample_survey_numbers_box.R')
source('src/compile_lengthindices.R')


## Some variables
atlantis_dir <- '../../Atlantis/AtlantisIceland/v6610'
base_dir <- '01-atlantis_to_mfdb'

nreps <- 1
sampling_id <- 'TEST'# 'v8_effic1'

species_ss <- 'Cod'
origin_year <- 1948

## Set Atlantis file locations
config_file <- file.path('config', 'v6610.R')
source(config_file)

## For mfdbatlantis
is_dir <- atlantis_directory(path = file.path(atlantis_dir, 'Out'),
                             xml_bio = mfdbatlantis:::first_file(file.path(atlantis_dir, 'Out'), '*[Bb]*.xml'),
                             start_year = origin_year)

## -----------------------------------------------------------------------------
## STEP 2: read in the Atlantis truth
## -----------------------------------------------------------------------------

## This is a modified version of atlantisom::om_init
## Essentially it is a wrapper for run_truth that appends additional information
## to the output. Using a local version as the load_biolprm and run_truth scripts
## need to account for > 10 cohorts.
## NOTE, the option of annage = TRUE should do this but not present in the Icelandic simulations

## Iceland operating model
iceom <- om_init(config_file)
print(names(iceom))

## Subset to specific species
iceom_ms <- atlantisom::om_species(species_ss, iceom, save = FALSE)
iceom_ms$time_lookup <- 
  data.frame(simtime = seq(0, iceom$runpar$tstop, by = iceom$runpar$toutinc)) %>% 
  mutate(time = 0:(n()-1),
         simyear = ceiling(simtime/365),
         year = local(origin_year) - 1 + simyear,
         doy = simtime - (simyear-1)*365,
         month = as.numeric(format(as.Date(.data$doy-1, origin = '2023-01-01'), '%m')), ## Using non-leap year as origin
         step = 1,
         step = ifelse(month %in% 4:5, 2, step),
         step = ifelse(month %in% 6:8, 3, step),
         step = ifelse(month %in% 9:10, 4, step),
         step = ifelse(month %in% 11:12, 5, step))

saveRDS(iceom_ms, file.path(atlantis_dir, paste0(scenario.name, "omlist_ss.rds")))
print(names(iceom_ms))

## Biomass based indices
iceom_ms_ind <- om_index_isl(usersurvey = c("config/isl_survey_igfs.R", "config/isl_survey_aut.R"),
                             userfishery = NULL, #c("config/isl_fishery.R"),
                             omlist_ss = iceom_ms, 
                             n_reps = 1, 
                             save = TRUE)

## Numbers per length interval
iceom_ms_ind$survObsNumB <- compile_lengthindices(usersurvey = c("config/isl_survey_igfs.R", 
                                                                 "config/isl_survey_aut.R"),
                                                  omlist_ss = iceom_ms)

## Catch compositions
iceom_ms_comp <- om_comps_isl(usersurvey = c("config/isl_survey_igfs.R", "config/isl_survey_aut.R"),
                              userfishery = c("config/isl_fishery.R"),
                              omlist_ss = iceom_ms, 
                              n_reps = nreps, 
                              save = TRUE)

fs::dir_create(path = file.path('ms-keyrun/data-raw/atlantisoutput', v.name, sampling_id))
save(iceom, iceom_ms, file = file.path('ms-keyrun/data-raw/atlantisoutput', v.name, sampling_id, 'iceom.Rdata'))
save(iceom_ms_ind, iceom_ms_comp, 
     file = file.path('ms-keyrun/data-raw/atlantisoutput', v.name, sampling_id, 'iceom_samples.Rdata'))



# NOBAom_ms_diet <- om_diet(config = here("simulated-data/config", "NOBA2config.R"),
#                           dietfile = "NOBADetDiet.gz",
#                           usersurvey = c(here("simulated-data/config/mssurvey_spring.R"), 
#                                          here("simulated-data/config/mssurvey_fall.R")), 
#                           omlist_ss = icesom_ms, 
#                           n_reps = 1, 
#                           save = TRUE)

#unlink(here("docs/config"), recursive = TRUE)


