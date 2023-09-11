## -----------------------------------------------------------------------------
##
## Initialise the Atlantis to gadget scripts
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


## Some variables
atlantis_dir <- '../../Atlantis/AtlantisIceland'
#atlantis_vers <- 'test_run_90d'
atlantis_vers <- 'test_run'
base_dir <- '01-atlantis_to_mfdb'

species_ss <- 'Cod'

## Set Atlantis file locations
source(file.path('config', 'test_run.R'))

## For mfdbatlantis
is_dir <- atlantis_directory(path = file.path(atlantis_dir, atlantis_vers),
                             xml_bio = mfdbatlantis:::first_file(file.path(atlantis_dir, atlantis_vers), '*[Bb]*.xml'),
                             start_year = 1948)
