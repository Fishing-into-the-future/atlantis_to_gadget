compile_fisherycomps <- function(fishery, omlist_ss, nreps, ncores = parallel::detectCores()){
  
  # Script for dimension parameters
  source("config/omdimensions.R", local = TRUE)
  # Survey information
  source(fishery, local = TRUE)
  
  catch_numbers <-  atlantisom::create_fishery_subset(dat = omlist_ss$truecatchnum_ss,
                                                      time = fishtime,
                                                      fleets = NULL,
                                                      species = survspp,
                                                      boxes = fishboxes)
  
  ## Create polygon*time scale for survey s
  fishery_scale <- create_polygon_scale(omlist_ss, survey = FALSE, fisheffN, boxes = fishboxes)
  
  ## (1) Sample age composition data
  agecomps <- parallel::mclapply(setNames(0:nreps, 0:nreps), function(x, fishery_scale){
    return(
      sample_fish_box(catch_numbers, fishery_scale)
    )
  }, fishery_scale = fishery_scale, mc.cores = ncores)
  
  ## Add attribute
  attributes(agecomps)$configfile <- fishery
 
  ## (2) Distribute age data to length intervals
  agelencomps <- compile_age2length(agecomps, omlist_ss, fishery = TRUE, ncores)
  
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
    list(fishObsLenComp = lencomps,
         fishObsAgeLenComp = agelenkeys))
}