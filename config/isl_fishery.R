## -----------------------------------------------------------------------------
##
## Setup survey parameters
##
## -----------------------------------------------------------------------------

# Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Need to define survey season, area, efficiency, selectivity

# Survey name
fishery.name <- "SEA"

# Inherits species from input omlist_ss
fishspp <- omlist_ss$species_ss 

#Number of years of data to pull
nyears <- 73

#Atlantis initialization period in years
burnin <- 12

# same time dimensioning parameters as in surveycensus.R
#Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:(noutsteps-1))  #total_sample defined in sardinesurvey.R
fish_burnin <- burnin*fstepperyr+1
fish_nyears <- nyears*fstepperyr
fish_times <- fish_burnin:(fish_nyears)#fish_sample_full[fish_burnin:(fish_nyears)]
fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times/fstepperyr)) #from Christine's new sardine_config.R

fishtime <- fish_times


# fishery sampling area
# should return all model areas, this assumes you see everything that it caught
fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1))#[!(c(0:(omlist_ss$boxpars$nbox - 1)) %in% c(0:4,8:13,24:29,38:52))]

# effective sample size needed for sample_fish
# this effective N is divided by the number of annual timesteps below, so 200 per time
# use as input to the length samples, ages can be a subset
fisheffN <- data.frame(species=survspp, effN=rep(250000, length(survspp)))

age_prop <- data.frame(species = 'Cod', prop = 0.1)
# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200
lenage_cv <- 0.13

#this adjusts for subannual fishery output so original effN is for whole year
fisheffN$effN <- fisheffN$effN/fstepperyr 

# fishery catch cv can be used in sample_survey_biomass
# perfect observation
fish_cv <- data.frame(species=survspp, cv=0)
