compile_surveycomps <- function(surveys, omlist_ss, ncores = parallel::detectCores()){
  
  # Script for dimension parameters
  source("config/omdimensions.R", local = TRUE)
  # Survey information
  source(attr(surveys, 'configfile'), local = TRUE)
  
  ## Create polygon*time scale for survey s
  #survey_scale <- create_polygon_scale(omlist_ss, survey = TRUE, surveffN)
  
  ## (1) Sample age composition data
  agecomps <- parallel::mclapply(surveys, function(x, survey_scale){
    
    ## Create polygon*time scale for survey s
    survey_scale <- 
      x %>% 
      group_by(species, time, polygon) %>% 
      summarise(effN = sum(atoutput), .groups = 'drop')
    
    return(sample_fish_box(x, survey_scale))
    
  }, survey_scale = survey_scale, mc.cores = ncores)
  
  ## Add attribute
  attributes(agecomps)$configfile <- attr(surveys, 'configfile')
  
  ## (2) Distribute age data to length intervals
  agelencomps <- compile_age2length(agecomps, omlist_ss, ncores = ncores)
  
  ## (3) Strip age away for length distributions
  lencomps <- compile_lengthdists(agelencomps)
  
  ## (4) Sub-sample for age-length keys
  agelenkeys <- parallel::mclapply(agelencomps, function(x, age_prop){
    return(
      sample_ages_isl(x, age_prop) %>% 
        dplyr::select(-atoutput) %>% 
        dplyr::rename(atoutput = numAtAgeSamp)
    )
  }, age_prop = age_prop, mc.cores = ncores)
  
  return(
    list(survObsLenComp = lencomps,
         survObsAgeLenComp = agelenkeys))
  
}
