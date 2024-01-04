compile_lengthindices <- function(surveys, omlist_ss){
  
  # Script for dimension parameters
  source("config/omdimensions.R", local = TRUE)
  # Survey information
  source(attr(surveys, 'surveyfile'), local = TRUE)
    
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
            dplyr::group_by(species, polygon, layer, time, midlength) %>% 
            dplyr::summarise(atoutput = sum(atoutput), .groups = 'drop')
        )
        
      }, mc.cores = parallel::detectCores())
    
    ## Inherits survey file attribute
    attributes(out)$surveyfile <- attr(surveys, 'surveyfile')
    return(out)
}
  
 