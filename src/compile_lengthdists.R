compile_lengthdists <- function(surveys, omlist_ss, fishery = FALSE, ncores = parallel::detectCores()){
  
  ## Add lengths compositions from ages distributions
  ## But only if length data does not already exist
  if ('midlength' %in% names(surveys[[1]])){
    tmp <- surveys
  }else{
    tmp <- compile_age2length(surveys, omlist_ss, fishery, ncores)
  }
  
  ## Aggregate over lengths
  out <- 
    tmp %>% 
    map(function(x){
      return(
        x %>% 
          dplyr::group_by(species, polygon, time, midlength) %>%
          dplyr::summarise(atoutput = sum(atoutput), .groups = 'drop')
      )
    })
  return(out)
}

compile_age2length <- function(surveys, omlist_ss, fishery = FALSE, ncores = parallel::detectCores()){
  
  # Script for dimension parameters
  source("config/omdimensions.R", local = TRUE)
  # Survey information
  source(attr(surveys, 'configfile'), local = TRUE)
  
  if (fishery){
    survtime <- fishtime
    survboxes <- fishboxes
  }
  
  ## CONVERT TO LENGTH BASED NUMBERS
  # weights needed for weight at age and length comp calcs
  # aggregate true resn per survey design
  survey_aggresn <- 
    atlantisom::aggregateDensityData(dat = omlist_ss$trueresn_ss,
                                     time = survtime,
                                     species = survspp,
                                     boxes = survboxes) %>% 
    dplyr::mutate(layer = NA) %>% 
    dplyr::rename(atoutput = medatoutput)
  
  # aggregate true structn per survey design
  survey_aggstructn <- 
    atlantisom::aggregateDensityData(dat = omlist_ss$truestructn_ss,
                                     time = survtime,
                                     species = survspp,
                                     boxes = survboxes) %>% 
    dplyr::mutate(layer = NA) %>% 
    dplyr::rename(atoutput = medatoutput)
  
  ## Convert to lengths
  out <- parallel::mclapply(surveys, function(x){
    
    return(
      calc_age2length_isl(structn = survey_aggstructn, 
                          resn = survey_aggresn,
                          nums = x,
                          biolprm = omlist_ss$biol,
                          fgs = omlist_ss$funct.group_ss,
                          maxbin = maxbin,
                          CVlenage = lenage_cv,
                          remove.zeroes=TRUE)$natlength %>%
        dplyr::mutate(midlength = (lower.bins + upper.bins)/2) %>% 
        dplyr::select(species, agecl, polygon, time, midlength, atoutput)
    )
    
  }, mc.cores = ncores)
  
  ## Inherits survey file attribute
  attributes(out)$configfile <- attr(surveys, 'configfile')
  return(out)
}

