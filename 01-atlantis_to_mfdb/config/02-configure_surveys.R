## -----------------------------------------------------------------------------
##
## Setup survey parameters
##
## -----------------------------------------------------------------------------

## Initialise an object to store the parameters
survey_params <- list('sea' = list(),
                      'aut' = list(),
                      'igfs' = list()
                      )

## STOCK SPECIFIC PARAMETERS
# Age classes of your species
survey_parameters$age_classes <-
  age_classes <- 1:(stock_truth$fgs |> filter(Name == species_ss) |> pull(NumCohorts))

# Selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1,length(age_classes))

# Efficiency
survey_params$eff_case <- 0.5

## Timings
# 1. Steps per year
survey_params$timestep <- stepperyr
# 2. Steps of surveys
survey_params$sea$survey_sample_time <- 1
survey_params$aut$survey_sample_time <- 3
survey_params$igfs$survey_sample_time <- 1

# The last timestep to sample
survey_params$total_sample <- noutsteps-1

# Vector of indices of survey times to pull
survey_params$sea$survey_sample_full <-
  seq(survey_params$sea$survey_sample_time, survey_params$total_sample, by = survey_params$timestep)
survey_params$aut$survey_sample_full <-
  seq(survey_params$aut$survey_sample_time, survey_params$total_sample, by = survey_params$timestep)
survey_params$igfs$survey_sample_full <-
  seq(survey_params$igfs$survey_sample_time, survey_params$total_sample, by = survey_params$timestep)

# Effective sample size for composition data (annual total samples)
survey_params$surveyEffN <- 1000
survey_params$fisheryEffN <- 1000

#CVs for length at age, catch, and survey
survey_params$CVs <- list('lenage'=0.1, 'fishery'=0.01, 'survey'=0.1)

#Number of years of data to pull
nyears <- 50

#Atlantis initialization period in years
burnin <- 30

#Maximum size of a fish in cm
maxbin <- 200

# Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample)
fish_burnin <- burnin*fstepperyr+1
fish_nyears <- nyears*fstepperyr
fish_times <- fish_sample_full[fish_burnin:(fish_burnin+fish_nyears-1)]
fish_timesteps <- seq(fish_times[5], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times/fstepperyr))
#Years to survey, assuming survey is once per year
survey_years <- survey_sample_full[burnin:(burnin+nyears-1)]

#Month of survey/fishing
survey_month <- 7
fishing_month <- 1

#Efficiency of the survey
effic <- data.frame(species=species, efficiency=eff_case)

# Assume uniform selectivity, same as selex1 here
sel<- data.frame(species=rep(species,length(age_classes)),
                 agecl=age_classes,
                 selex=sel_by_age)
