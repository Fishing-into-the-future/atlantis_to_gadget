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


## Some variables
atlantis_dir <- '../../Atlantis/AtlantisIceland'
#atlantis_vers <- 'test_run_90d'
atlantis_vers <- 'test_run'
base_dir <- '01-atlantis_to_mfdb'

species_ss <- 'Cod'

## Set Atlantis file locations
source(file.path('config', 'config.R'))

## For mfdbatlantis
is_dir <- atlantis_directory(path = file.path(atlantis_dir, atlantis_vers),
                             xml_bio = mfdbatlantis:::first_file(file.path(atlantis_dir, atlantis_vers), '*[Bb]*.xml'),
                             start_year = 1948)

## -----------------------------------------------------------------------------
## STEP 2: read in the Atlantis truth
## -----------------------------------------------------------------------------

## This is a modified version of atlantisom::om_init
## Essentially it is a wrapper for run_truth that appends additional information
## to the output. Using a local version as the load_biolprm and run_truth scripts
## need to account for > 10 cohorts.
## NOTE, the option of annage = TRUE should do this but not present in the Icelandic simulations

iceom <- om_init(file.path('config', 'config.R'), atlantis_dir)
print(names(iceom))
iceom_ms <- om_species(species_ss, iceom, save = FALSE)
saveRDS(iceom_ms, file.path(atlantis_dir, paste0(scenario.name, "omlist_ss.rds")))
print(names(iceom_ms))


iceom_ms_ind <- om_index_isl(usersurvey = c("config/isl_survey_igfs.R", "config/isl_survey_aut.R"),
                         userfishery = c("config/isl_fishery.R"),
                         omlist_ss = iceom_ms, 
                         n_reps = 1, 
                         save = FALSE)

iceom_ms_comp <- om_comps(usersurvey = c("config/isl_survey_igfs.R", "config/isl_survey_aut.R"),
                           userfishery = c("config/isl_fishery.R"),
                           omlist_ss = iceom_ms, 
                           n_reps = 1, 
                           save = FALSE)

# NOBAom_ms_diet <- om_diet(config = here("simulated-data/config", "NOBA2config.R"),
#                           dietfile = "NOBADetDiet.gz",
#                           usersurvey = c(here("simulated-data/config/mssurvey_spring.R"), 
#                                          here("simulated-data/config/mssurvey_fall.R")), 
#                           omlist_ss = icesom_ms, 
#                           n_reps = 1, 
#                           save = TRUE)

#unlink(here("docs/config"), recursive = TRUE)


