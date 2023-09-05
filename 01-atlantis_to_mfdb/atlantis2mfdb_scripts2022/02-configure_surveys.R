## -----------------------------------------------------------------------------
##
## Setup survey parameters
##
## -----------------------------------------------------------------------------

## Initialise an object to store the parameters
survey_params <- list('SEA' = list(),
                      'AUT' = list(),
                      'IGFS' = list()
                      )

## STOCK SPECIFIC PARAMETERS
# Age classes of your species
survey_params$age_classes <-
  age_classes <- 1:(stock_truth$fgs |> filter(Name == species_ss) |> pull(NumCohorts))

# Selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1,length(age_classes))

# Efficiency
survey_params$eff_case <- 0.5

## Timings
noutsteps <- floor(runpar$tstop / runpar$outputstep)
stepperyr <- if(runpar$outputstepunit == "days") round(365 / runpar$toutinc, 0)
# learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(runpar$outputstepunit=="days") round(365 / runpar$toutfinc, 0)

# 1. Steps per year
survey_params$timestep <- stepperyr
# 2. Steps of surveys
survey_params$SEA$survey_sample_time <- 1
survey_params$AUT$survey_sample_time <- 3
survey_params$IGFS$survey_sample_time <- 1

# The last timestep to sample
survey_params$total_sample <- noutsteps-1

# Vector of indices of survey times to pull
survey_params$SEA$survey_sample_full <-
  seq(survey_params$SEA$survey_sample_time, survey_params$total_sample, by = survey_params$timestep)
survey_params$AUT$survey_sample_full <-
  seq(survey_params$AUT$survey_sample_time, survey_params$total_sample, by = survey_params$timestep)
survey_params$IGFS$survey_sample_full <-
  seq(survey_params$IGFS$survey_sample_time, survey_params$total_sample, by = survey_params$timestep)

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
fish_sample_full <- c(0:(noutsteps-1))
fish_burnin <- burnin*fstepperyr+1
# All steps proceeding the burnin
fish_times <- fish_sample_full[fish_burnin:(length(fish_sample_full)+1)]
fish_timesteps <- fish_times # seq(fish_times[floor(fstepperyr)], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times/fstepperyr))
#Years to survey, assuming survey is once per year
#survey_years <- survey_sample_full[burnin:(burnin+nyears-1)]

#Efficiency of the survey
effic <- data.frame(species=species_ss, efficiency=survey_params$eff_case)

# Assume uniform selectivity, same as selex1 here
sel<- data.frame(species=rep(species_ss,length(age_classes)),
                 agecl=age_classes,
                 selex=sel_by_age)

