## -----------------------------------------------------------------------------
##
## Setup survey parameters
##
## -----------------------------------------------------------------------------

# Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name <- "IGFS"

# Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr # 4

# Which atlantis timestep does the survey run in?
# Icelandic model uses quarterly steps

# No, timestep 0 is initial condition and should be ignored to align 
# snapshots (biomass, numbers) with
# cumulative outputs (fishery catch, numbers)

#survey_sample_time <- 1 # autumn survey
survey_sample_time <- 3 # autumn survey
survey_start_year <- 1980

# The last timestep to sample
#total_sample <- noutsteps-1 # 262

#Vector of indices of survey times to pull
#survey_sample_full <- seq(survey_sample_time, total_sample, by=timestep)
survey_sample_full <- 
  omlist_ss$time_lookup %>% 
  filter(month == survey_sample_time,
         year >= survey_start_year,
         simyear > 0) %>% 
  pull(time)

survtime <- survey_sample_full

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                        efficiency=rep(0.5, length(survspp)))

# survey selectivity (agecl based)
# this is by age class, need to change to use with ANNAGEBIO output
if (FALSE){
  survselex <- data.frame(species=rep(names(age_classes), each=n_age_classes),
                       agecl=rep(c(1:n_age_classes),length(survspp)),
                       selex=rep(1.0,length(survspp)*n_age_classes))
}else{
  # for annage output uses names(annages) NOT alphabetical survspp
  survselex <- data.frame(species=rep(names(annages), n_annages), #  
                          agecl=unlist(sapply(n_annages,seq)),
                          selex=c(0.1,0.3,0.5,0.7,0.8,0.9,0.95,1,1,1,1,1,1,1,1,1)) #c(rep(0, 3), rep(1.0,sum(n_annages-3))))
  
}


survselex.agecl <- survselex


# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(85000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))
#surv_cv <- data.frame(species=survspp, cv=rep(0,length(survspp)))

age_prop <- data.frame(species = survspp, prop = 0.05)

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- data.frame(species=rep(names(annages), n_annages), #  
                        agecl=unlist(sapply(n_annages,seq)),
                        cvlenage=c(0.17,0.16,0.15,0.14,0.13,0.13,0.12,0.11,0.11,0.12,0.13,0.13,0.13,0.13,0.13,0.13))
#lenage_cv <- 0.135
#lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10000000
unidprey <- 0

