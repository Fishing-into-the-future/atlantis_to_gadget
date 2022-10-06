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

#library(mfdb) Load later

## Read in local scripts - modified scripts from atlantisom allowing > 10 cohorts
source('cod_demo/src/run_truth_local.R')
source('cod_demo/src/load_nc_local.R')
source('cod_demo/src/load_biolprm_local.R')


## Some variables
atlantis_dir <- '../../Atlantis/AtlantisIceland'
#atlantis_vers <- 'test_run_90d'
atlantis_vers <- 'test_run'
base_dir <- 'cod_demo'

species_ss <- 'Cod'

## Set Atlantis file locations
source(file.path(base_dir, 'config', 'config.R'))

## For mfdbatlantis
is_dir <- atlantis_directory(path = file.path(atlantis_dir, atlantis_vers),
                             xml_bio = mfdbatlantis:::first_file(file.path(atlantis_dir, atlantis_vers), '*[Bb]*.xml'),
                             start_year = 1948)
