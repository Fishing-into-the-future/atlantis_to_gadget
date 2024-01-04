compile_catchnumbers <- function(fishery, omlist_ss, nreps){
  
  # Script for dimension parameters
  source("config/omdimensions.R", local = TRUE)
  # Survey information
  source(fishery, local = TRUE)
  
  out <-  atlantisom::create_fishery_subset(dat = omlist_ss$truecatchnum_ss,
                                            time = fishtime,
                                            fleets = NULL,
                                            species = survspp,
                                            boxes = fishboxes)
  
  attributes(out)$configfile <- fishery
  return(out)
  
}