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
#library(mfdbatlantis)
library(mfdb)

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
#source('src/compile_lengthindices.R')
source('src/create_polygon_scale.R')
source('src/sample_fish_box.R')

source('src/compile_surveyreplicates.R')
source('src/compile_lengthdists.R')
source('src/compile_surveycomps.R')
source('src/compile_fisherycomps.R')
source('src/create_sim_biolpar.R')
source('src/create_sim_startpars.R')

load(file = "01-atlantis_to_mfdb/data/selection_by_age.Rdata")


## Some variables
atlantis_dir <- '../../Atlantis/AtlantisIceland/v6610'
base_dir <- '01-atlantis_to_mfdb'

nreps <- 100
sampling_id <- paste0('v16_Scv02_reps', nreps)

species_ss <- 'Cod'
origin_year <- 1948

## Set Atlantis file locations
simName <- 'v6610'
config_file <- file.path('config', paste0(simName, '.R'))
source(config_file)

out_path <- file.path('01-atlantis_to_mfdb', 'data', sampling_id)
fs::dir_create(out_path)

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
  dplyr::mutate(time = 0:(n()-1),
                simyear = ceiling(simtime/365),
                year = local(origin_year) - 1 + simyear,
                doy = simtime - (simyear-1)*365,
                month = as.numeric(format(as.Date(.data$doy-1, origin = '2023-01-01'), '%m')), ## Using non-leap year as origin
                step = 1,
                step = ifelse(month %in% 4:5, 2, step),
                step = ifelse(month %in% 6:8, 3, step),
                step = ifelse(month %in% 9:10, 4, step),
                step = ifelse(month %in% 11:12, 5, step))

saveRDS(iceom_ms, file.path(out_path, "omlist_ss.rds"))
print(names(iceom_ms))

################################################################################

## Trying a new approach (1) composition data is tied to a specific survey replicate (2) parallelised a lot of the code
## Tested on single species so far...
## Step 1: create a list of surveys 'nreps' long. Element '0' is the baseline with 0 obs.error, the proceeding 'nreps' elements are variants with observational error added
## Step 2: convert each survey's age composition to length intervals for the survey indices
## Step 3: 

## SURVEYS
surveys <- list('IGFS' = compile_surveyreplicates("config/isl_survey_igfs.R", iceom_ms, nreps),
                'AUT'  = compile_surveyreplicates("config/isl_survey_aut.R", iceom_ms, nreps))
gc()

#survey_indices <- list('IGFS' = compile_lengthdists(surveys$IGFS, iceom_ms, ncores = 50),
#                       'AUT'  = compile_lengthdists(surveys$AUT, iceom_ms, ncores = 50))
#gc()

composition_data <- list()
composition_data[['IGFS']] <- compile_surveycomps(surveys$IGFS, iceom_ms, ncores = 50)
composition_data[['AUT']] <- compile_surveycomps(surveys$AUT, iceom_ms, ncores = 50)
composition_data[['SEA']] <- compile_fisherycomps(c("config/isl_fishery.R"), iceom_ms, nreps, ncores = 50)

gc()

## Save data
saveRDS(surveys, file = file.path(out_path, 'surveys.rds'))
#saveRDS(survey_indices, file = file.path(out_path, 'survey_indices.rds'))
saveRDS(composition_data, file = file.path(out_path, 'composition_data.rds'))

################################################################################
##
## Load the sampled data along with simulation info into a MFDB database (using duckdb)
##
################################################################################

if (FALSE){
  mfdb(file.path('db', paste0('atlantisiceland_', sampling_id, '.duckdb')), destroy_schema = TRUE)
}
mdb <- mfdb(file.path('db', paste0('atlantisiceland_', sampling_id, '.duckdb')))

## -----------------------------------------------------------------------------
## Species lookup
## -----------------------------------------------------------------------------

## Get the simulation's biological parameters using the mskeys function
simBiolPar <- create_sim_biolpar(config_file, saveToData=FALSE)
saveRDS(simBiolPar, file = file.path(out_path, 'simBiolPar.rds'))
simStartPars <- create_sim_startpars(config_file, fitstart=61, fitend=365, saveToData=FALSE)

sppLookup <- data.frame(Code    = 'FCD', 
                        mfdbSpp = 'COD')
sppList <- left_join(simBiolPar, sppLookup)

## -----------------------------------------------------------------------------
## Areas
## -----------------------------------------------------------------------------

area_data <- NULL
for (i in seq_along(iceom_ms$boxpars$boxes)){
  tmp <- data.frame(id = i,
                    name = iceom_ms$boxpars$boxes[[i]]$label,
                    size = round(iceom_ms$boxpars$boxes[[i]]$area/1e6,0))
  area_data <- rbind(area_data, tmp)
}
## Into MFDB
mfdb_import_area(mdb, area_data)

## -----------------------------------------------------------------------------
## Temperature
## -----------------------------------------------------------------------------

## atlantisom option?
#is_temp <- atlantis_temperature(is_dir, area_data)
#mfdb_import_temperature(mdb, is_temp[is_temp$depth == 1,])

## Time to year month lookup - quick hack for now
# noutsteps <- floor(iceom$runpar$tstop/iceom$runpar$outputstep)
# time_to_yearmonth <-
#   data.frame(time = 0:(noutsteps),
#              month = rep(c(1,4,7,10), length(0:noutsteps)/4),
#              year = rep(1948:(1948+(length(0:noutsteps)/4)-1), each = 4))

## -----------------------------------------------------------------------------
## Functional groups
## -----------------------------------------------------------------------------

mfdb_import_species_taxonomy(mdb,
                             iceom$funct.groups %>%
                               select(name = Code, species = Name, description = LongName))


## -----------------------------------------------------------------------------
## Sampling types
## -----------------------------------------------------------------------------

## Note, the sampling id will now be used to indicate replicates, data_sources will
## be used to identify survey types... more efficient this way as negates looping 
## over replicates

## Sampling types
mfdb_import_sampling_type(mdb, data.frame(id = 0:nreps,
                                          name = 0:nreps,
                                          description = c('baseline', paste0('replicate', 1:nreps))))

## -----------------------------------------------------------------------------
## Subannual landings
## -----------------------------------------------------------------------------

cat('IMPORTING SUBANNUAL LANDINGS\n')
## Gear taxonomies
mfdb_import_cs_taxonomy(mdb,
                        'gear',
                        tibble(id = unique(iceom_ms$truecatchtons_ss$fleet),
                               name = unique(iceom_ms$truecatchtons_ss$fleet),
                               description = unique(iceom_ms$truecatchtons_ss$fleet)))

## Discards present for haddock and cod only
## name == "All" will sum both catch and discard
mfdb_import_tow_taxonomy(mdb, data.frame(
  name = c("All", "C", "D"),
  t_group = c(NA, "All", "All"),
  stringsAsFactors = FALSE))

tmp <- 
  iceom_ms$truecatchtons_ss %>% 
  dplyr::mutate(tow = 'C') %>% 
  dplyr::bind_rows(
    iceom_ms$truedisctons_ss %>% 
      dplyr::mutate(tow = 'D')
  ) %>% 
  dplyr::left_join(iceom_ms$time_lookup, by = 'time') %>% 
  dplyr::left_join(sppList %>% 
                     dplyr::rename(species = Code) %>% 
                     dplyr::select(species, mfdbSpp), 
                   by = 'species')

mfdb_import_survey(mdb,
                   data.frame(year = tmp$year,
                              month = tmp$month,
                              areacell = paste0('Box', tmp$polygon),
                              gear = tmp$fleet,
                              tow = tmp$tow,
                              species = tmp$mfdbSpp,
                              weight_total = tmp$atoutput*1e3, ## to kg
                              stringsAsFactors = TRUE),
                   data_source = 'atlantis_fisheries')


## -----------------------------------------------------------------------------
## Survey data
## -----------------------------------------------------------------------------

## No longer doing SIs this way... just counting the ldist frequencies now
if (FALSE){
  cat('IMPORTING SURVEY INDICES NUMBERS\n')
  cat('Igfs\n\n')
  
  mfdb::mfdb_import_survey(
    mdb,
    survey_indices$IGFS %>% 
      bind_rows(.id = 'sampling_type') %>% 
      left_join(iceom_ms$time_lookup, by = 'time') %>% 
      mutate(areacell = paste0('Box', polygon),
             species = toupper(species)) %>% 
      rename(length = midlength, count = atoutput) %>% 
      select(year,month,areacell,species,sampling_type,length,count),
    data_source = 'atlantis_igfs_si')
  
  cat('Aut\n\n')
  
  mfdb::mfdb_import_survey(
    mdb,
    survey_indices$AUT %>% 
      bind_rows(.id = 'sampling_type') %>% 
      left_join(iceom_ms$time_lookup, by = 'time') %>% 
      mutate(areacell = paste0('Box', polygon),
             species = toupper(species)) %>% 
      rename(length = midlength, count = atoutput) %>% 
      select(year,month,areacell,species,sampling_type,length,count),
    data_source = 'atlantis_aut_si')
}

## -----------------------------------------------------------------------------
## Compositions
## -----------------------------------------------------------------------------

cat('IMPORTING LENGTH DISTRIBUTIONS\n')
cat('Igfs\n')

mfdb::mfdb_import_survey(
  mdb,
  composition_data$IGFS$survObsLenComp %>% 
    bind_rows(.id = 'sampling_type') %>% 
    left_join(iceom_ms$time_lookup, by = 'time') %>% 
    mutate(areacell = paste0('Box', polygon),
           species = toupper(species)) %>% 
    rename(length = midlength, count = atoutput) %>% 
    select(year,month,areacell,species,sampling_type,length,count),
  data_source = 'iceland-ldist-IGFS')

cat('Aut\n')

mfdb::mfdb_import_survey(
  mdb,
  composition_data$AUT$survObsLenComp %>% 
    bind_rows(.id = 'sampling_type') %>% 
    left_join(iceom_ms$time_lookup, by = 'time') %>% 
    mutate(areacell = paste0('Box', polygon),
           species = toupper(species)) %>% 
    rename(length = midlength, count = atoutput) %>% 
    select(year,month,areacell,species,sampling_type,length,count),
  data_source = 'iceland-ldist-AUT')

cat('Sea\n')

mfdb::mfdb_import_survey(
  mdb,
  composition_data$SEA$fishObsLenComp %>% 
    bind_rows(.id = 'sampling_type') %>% 
    left_join(iceom_ms$time_lookup, by = 'time') %>% 
    mutate(areacell = paste0('Box', polygon),
           species = toupper(species)) %>% 
    rename(length = midlength, count = atoutput) %>% 
    select(year,month,areacell,species,sampling_type,length,count),
  data_source = 'iceland-ldist-SEA')

cat('IMPORTING AGE-LENGTH DISTRIBUTIONS\n')
cat('Igfs\n')

mfdb::mfdb_import_survey(
  mdb,
  composition_data$IGFS$survObsAgeLenComp %>% 
    bind_rows(.id = 'sampling_type') %>% 
    left_join(iceom_ms$time_lookup, by = 'time') %>% 
    mutate(areacell = paste0('Box', polygon),
           species = toupper(species),
           agecl = as.integer(agecl)) %>% 
    rename(length = midlength, count = atoutput, age = agecl) %>% 
    select(year,month,areacell,age,species,sampling_type,length,count),
  data_source = 'iceland-aldist-IGFS')

cat('Aut\n')

mfdb::mfdb_import_survey(
  mdb,
  composition_data$AUT$survObsAgeLenComp %>% 
    bind_rows(.id = 'sampling_type') %>% 
    left_join(iceom_ms$time_lookup, by = 'time') %>% 
    mutate(areacell = paste0('Box', polygon),
           species = toupper(species),
           agecl = as.integer(agecl)) %>% 
    rename(length = midlength, count = atoutput, age = agecl) %>% 
    select(year,month,areacell,age,species,sampling_type,length,count),
  data_source = 'iceland-aldist-AUT')

cat('Sea\n')

if (FALSE){
  mfdb::mfdb_import_survey(
    mdb,
    composition_data$SEA$fishObsAgeLenComp %>% 
      bind_rows(.id = 'sampling_type') %>% 
      left_join(iceom_ms$time_lookup, by = 'time') %>% 
      mutate(areacell = paste0('Box', polygon),
             species = toupper(species)) %>% 
      rename(length = midlength, count = atoutput, age = agecl) %>% 
      select(year,month,areacell,age,species,sampling_type,length,count),
    data_source = 'iceland-aldist-SEA')
}

# ---------------------------------------------------
# IMPORT INITIAL POPULATION BY AGE
# ---------------------------------------------------

tmp <- simStartPars %>%
  mutate(units=NULL) %>%
  filter(variable == "Natage") %>%
  left_join(sppList) %>%
  rename(age = agecl) %>%
  mutate(year=1960, month = 1) %>% # initial year is 40
  mutate(area = NA) 

#ggplot(tmp %>% filter(age!=1)) + geom_point(aes(age,value)) + facet_wrap(~mfdbSpp, scale="free") + ylim(0,NA)

mfdb_import_survey(mdb,
                   data_source = 'atlantis_anumb_init',
                   data.frame(
                     year = tmp$year,
                     month = tmp$month,
                     areacell = tmp$area,
                     species = tmp$mfdbSpp,
                     sampling_type = 0,
                     age = tmp$age,
                     count = tmp$value,
                     stringsAsFactors = TRUE))

# ---------------------------------------------------
# IMPORT AVERAGE RECRUITMENT
# ---------------------------------------------------

tmp <- 
  simStartPars %>%
  mutate(units=NULL) %>%
  filter(variable == "AvgRec") %>%
  left_join(sppList) %>%
  mutate(age = cut(RecruitMonth, breaks=c(1,12,24,36), labels=0:2, include.lowest=T)) %>% # recruitment assumed at age0 ***CHECK
  mutate(year=1960, month = 1) %>% # use year1 to store avg recr
  mutate(area = NA)

mfdb_import_survey(mdb,
                   data_source = 'atlantis_logrec_avg',
                   data.frame(
                     year = tmp$year,
                     month = tmp$month,
                     areacell = tmp$area,
                     species = tmp$mfdbSpp,
                     sampling_type = 0,
                     age = tmp$age,
                     count = tmp$value,
                     stringsAsFactors = TRUE))

# ---------------------------------------------------
# IMPORT INITIAL POPULATION AGE-LENGTH
# ---------------------------------------------------

tmp <- simStartPars %>%
  mutate(units=NULL) %>%
  filter(variable == "Natlen") %>%
  left_join(sppList) %>%
  mutate(age = agecl) %>% #ifelse(ageGrpSize == 2, agecl*2-1, # assign the first age in the age class
  #ifelse(ageGrpSize == 4, agecl*4-1, agecl))) %>%
  mutate(year=1960, month = 1) %>% # *** verify that initial year is 40
  mutate(area = NA)

mfdb_import_survey(mdb,
                   data_source = 'atlantis_alnumb_init',
                   data.frame(
                     year = tmp$year,
                     month = tmp$month,
                     areacell = tmp$area,
                     species = tmp$mfdbSpp,
                     sampling_type = 0,
                     length = tmp$lenbin,
                     age = tmp$age,
                     count = tmp$value,
                     stringsAsFactors = TRUE))

mfdb_disconnect(mdb)








