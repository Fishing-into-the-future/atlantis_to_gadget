## -----------------------------------------------------------------------------
##
## Runner sample the Atlantis truth
##
## -----------------------------------------------------------------------------

## We samples of:

# 1. survey indices
# 2. length distributions
# 3. age-length distributions

## Check if the Truth exists
if (!exists("stock_truth")) load(file.path(atlantis_dir, atlantis_vers, paste0(atlantis_vers, '_TRUTH.Rdata')))

## -----------------------------------------------------------------------------
## 1. CREATE SURVEYS
## -----------------------------------------------------------------------------

## TODO add selectivity functions, these should vary between surveys and fisheries sampling

# IGFS
survey_igfs <- create_survey(dat = stock_truth$nums,
                             time = survey_params$IGFS$survey_sample_full,
                             species = species_ss,
                             boxes = boxall,
                             effic = effic,
                             selex = sel)

# Autumn
survey_aut <- create_survey(dat = stock_truth$nums,
                            time = survey_params$AUT$survey_sample_full,
                            species = species_ss,
                            boxes = boxall,
                            effic = effic,
                            selex = sel)

## -----------------------------------------------------------------------------
## 2. SURVEY INDICES
## -----------------------------------------------------------------------------

# TODO: pooled over age classes atm, need to sample per length grouping? Or age class?

# IGFS
si_igfs <- sample_survey_numbers(survey_igfs,
                                 cv = data.frame(species = species_ss, cv = survey_params$CVs$survey))

# Autumn
si_aut <- sample_survey_numbers(survey_aut,
                                cv = data.frame(species = species_ss, cv = survey_params$CVs$survey))

################################################################################

## -----------------------------------------------------------------------------
## 3. AGE LENGTH DISTRIBUTIONS
## -----------------------------------------------------------------------------

survey_aldists <- list('IGFS' = list(),
                       'AUT' = list())


## Need to convert ages into lengths and do this using
## residual and structural nitrogen and the age-to-length relationship

for (i in c('IGFS', 'AUT')){

  if (i == 'IGFS') tmp <- survey_igfs else tmp <- survey_aut

  ## Fist get the age composition
  survey_aldists[[i]]$age_comp <-
    sample_fish(tmp, data.frame(species = species_ss,
                                effN = survey_params$surveyEffN))

  ## Aggregate residual nitrogen
  survey_aldists[[i]]$survey_aggresnstd <-
    aggregateDensityData(dat = stock_truth$resn,
                         time = survey_params[[i]]$survey_sample_full,
                         species = species_ss,
                         boxes = boxall)

  ## Aggregate structural nitrogen
  survey_aldists[[i]]$survey_aggstructnstd <-
    aggregateDensityData(dat = stock_truth$structn,
                         time = survey_params[[i]]$survey_sample_full,
                         species = species_ss,
                         boxes = boxall)


  survey_aldists[[i]]$ss_resnstd <- sample_fish(survey_aldists[[i]]$survey_aggresnstd,
                                                survey_params$surveyEffN,
                                                sample = FALSE)

  survey_aldists[[i]]$ss_structnstd <- sample_fish(survey_aldists[[i]]$survey_aggstructnstd,
                                                   survey_params$surveyEffN,
                                                   sample = FALSE)
  ## Get age length distributions
  survey_aldists[[i]]$ss_length_stdsurv <-
    calc_age2length(structn = survey_aldists[[i]]$ss_structnstd,
                    resn = survey_aldists[[i]]$ss_resnstd,
                    nums = survey_aldists[[i]]$age_comp,
                    biolprm = stock_truth$biolprm,
                    fgs = stock_truth$fgs,
                    CVlenage = survey_params$CVs$lenage,
                    remove.zeroes = TRUE)

}

################################################################################

## -----------------------------------------------------------------------------
## SEA SAMPLING
## -----------------------------------------------------------------------------

survey_sea <- create_survey(dat = stock_truth$nums,
                            time = survey_params$SEA$survey_sample_full,
                            species = species_ss,
                            boxes = boxall,
                            effic = effic,
                            selex = sel)
# SEA
sea_age <- sample_fish(survey_sea, data.frame(species = species_ss,
                                              effN = survey_params$fisheryEffN))


# Get composition inputs for SS (survey and fishery catch at age, survey and fishery lengths, survey and fishey weight at age).
# Because catch composition information goes in to the assessment as a proportion,
# we can use fishery catch at age from this legacy codebase even with absolute catch numbers likely half what they should be overall.

# Survey length comps and wtage done above to get survey ts using nums*wtage approach

# Fisheries sampled continuously through year
effN <- data.frame(species = species_ss,
                   effN = survey_params$fisheryEffN/fstepperyr)

#catch at age each timestep summed over polygons
# catch at age by area and timestep
catch_numbers <-  create_fishery_subset(dat = stock_truth$catch,
                                        time = fish_times,
                                        species = species_ss,
                                        boxes = boxall)

catch_numsss_samp <- sample_fish(catch_numbers, effN)

#Get weights
# aggregate true resn per fishery subset design
catch_aggresnss <- aggregateDensityData(dat = stock_truth$resn,
                                        time = fish_times,
                                        species = species_ss,
                                        boxes = boxall)

# aggregate true structn fishery subsetdesign
catch_aggstructnss <- aggregateDensityData(dat = stock_truth$structn,
                                           time = fish_times,
                                           species = species_ss,
                                           boxes = boxall)

# dont sample these, just aggregate them using median
catch_structnss_samp <- sample_fish(catch_aggstructnss, effN, sample = FALSE)
catch_resnss_samp <-  sample_fish(catch_aggresnss, effN, sample = FALSE)

# these fishery lengths and weight at age are each output timestep
catch_lengthwt_samp <- calc_age2length(structn = catch_structnss_samp,
                                       resn = catch_resnss_samp,
                                       nums = catch_numsss_samp,
                                       biolprm = stock_truth$biolprm,
                                       fgs = stock_truth$fgs,
                                       maxbin = maxbin,
                                       CVlenage = survey_params$CVs$lenage, remove.zeroes=TRUE)


