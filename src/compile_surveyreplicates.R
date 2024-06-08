compile_surveyreplicates <- function(survey, omlist_ss, nreps){
  
  # Script for dimension parameters
  source("config/omdimensions.R", local = TRUE)
  # Survey information
  source(survey, local = TRUE)
  
  # NUMBERS
  survey_N <- atlantisom::create_survey(dat = omlist_ss$truenums_ss,
                                        time = survtime,
                                        species = survspp,
                                        boxes = survboxes,
                                        effic = surveffic,
                                        selex = survselex) 
  
  out <- list()
  # BASELINE - cv = 0
  # This function returns the same numbers but formats same as replicates
  out[['0']] <- sample_survey_numbers_box(survey_N, 
                                        data.frame(species = surv_cv$species,
                                                   cv = 0))
  
  # REPLICATES
  tmp <- NULL
  if (nreps > 1){
    tmp <- parallel::mclapply(setNames(1:nreps, 1:nreps), function(x){
      return(
        sample_survey_numbers_box(survey_N, surv_cv)
      )
    }, mc.cores = parallel::detectCores())
  }
  
  out <- c(out, tmp)
  ## Round to whole numbers
  out <- lapply(out, function(x){
    x$atoutput <- round(x$atoutput, 0) 
    return(x)
  })
  attributes(out)$configfile <- survey
  return(out)
  
}