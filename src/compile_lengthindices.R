compile_lengthindices <- function(usersurvey, omlist_ss){
  
  # one script for dimension parameters to be used in multiple functions
  source("config/omdimensions.R", local = TRUE)
  
  out <- list()
  for (s in usersurvey){
    
    print(s)
    
    # survey information
    source(s, local = TRUE)
    
    ## NUMBERS
    survey_N <- atlantisom::create_survey(dat = omlist_ss$truenums_ss,
                                          time = survtime,
                                          species = survspp,
                                          boxes = survboxes,
                                          effic = surveffic,
                                          selex = survselex)  
    
    ## CONVERT TO LENGTH BASED NUMBERS
    ## Not doing anything with observation error here yet, will do within gadget
    
    # weights needed for weight at age and length comp calcs
    # aggregate true resn per survey design
    survey_aggresn <- atlantisom::aggregateDensityData(dat = omlist_ss$trueresn_ss,
                                                       time = survtime,
                                                       species = survspp,
                                                       boxes = survboxes)
    
    # aggregate true structn per survey design
    survey_aggstructn <- atlantisom::aggregateDensityData(dat = omlist_ss$truestructn_ss,
                                                          time = survtime,
                                                          species = survspp,
                                                          boxes = survboxes)
    ## Calculate age-length
    tmp <- calc_age2length_isl(structn = survey_aggstructn %>% 
                                 dplyr::mutate(layer = NA) %>% 
                                 dplyr::rename(atoutput = medatoutput),
                               resn = survey_aggresn %>% 
                                 dplyr::mutate(layer = NA) %>% 
                                 dplyr::rename(atoutput = medatoutput),
                               nums = survey_N,
                               biolprm = omlist_ss$biol,
                               fgs = omlist_ss$funct.group_ss,
                               maxbin = maxbin,
                               CVlenage = lenage_cv,
                               remove.zeroes=TRUE)
    
    out[[survey.name]][[1]] <- 
      tmp$natlength %>% 
      dplyr::group_by(species, polygon, layer, time, lower.bins, upper.bins) %>% 
      dplyr::summarise(atoutput = sum(atoutput), .groups = 'drop')
    
    saveRDS(out[[survey.name]], file.path(d.name, paste0(scenario.name, "_", survey.name, "surveyN.rds")))  
  }
  return(out)
}
  
 