#' @title Sample a numbers index of abundance from an atlantis scenario
#'
#' @description The function takes numbers-at-age data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc}}
#'   within \code{\link{run_truth}}. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#' @details
#'   This function simply sums numbers-at-age over polygons,
#'   and then applies user defined error to the total numbers.
#'   The result is a coastwide numbers estimate from the survey
#'   Improvements could be to provide polygon specific numbers,
#'   but the cv will need to be thought about.
#' @author Poseidon
#' @export
#'
#' @template dat
#' @param cv      Coefficient of variation for the entire species specific abundance
#'                    a matrix with columns: species, cv
#'
#' @return The standard dataframe as specified used in \code{dat}.
#'   The function sums over layers and makes \code{$layers} is {NA}.
#'
#' @examples
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' species <- c("Pisciv_T_Fish","Pisciv_S_Fish")
#' truth <- run_truth(scenario = "outputs",
#'   dir = d,
#'   file_fgs = "Functional_groups.csv",
#'   file_bgm = "Geography.bgm",
#'   select_groups = species,
#'   file_init = "Initial_condition.nc",
#'   file_biolprm = "Biology.prm",
#'   file_runprm = "Run_settings.xml")
#'
#'  boxes <- 1:3
#'	effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_S_Fish"), efficiency=c(0.3,0.1))
#'	selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
#'                agecl=c(1:10,1:10),
#'	                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))
#'
#'	tmp <- create_survey(dat=truth$nums, time=seq(10,55,3), species=species, boxes=boxes, effic=effic, selex=selex)
#'
#'
#'	cv <- data.frame(species=species, cv=c(0.2,0.3))
#'
#'	survObsN <- sample_survey_numbers(dat=tmp,cv=cv)



sample_survey_numbers_box <- function(dat,cv) {
 
  #calculate total numbers and partition to boxes
  ###  otherwise some assumptions about box-specific cv have to be made
  ###  this makes sure that box-specific numbers add up to total observed numbers
  ### use create_survey to subset the boxes and time
  # no conversion, just rename for clarity
  dat$numbers <- dat$atoutput
  
  #sum over boxes and ages (the sampled boxes were already subset in create functions)
  totN <- aggregate(dat$numbers,list(dat$species,dat$time),sum)
  names(totN) <- c("species","time","numbers")
  
  #add observation error
  totNobs <- merge(totN,cv,by="species",all.x=T)
  totNobs$var <- log(totNobs$cv^2+1)
  totNobs$obsNumbers <- rlnorm(nrow(totNobs), log(totNobs$numbers)-totNobs$var/2, sqrt(totNobs$var))
  
  
  #THIS CODE BELOW IS A METHOD TO CALCULATE BIOMASS PER POLYGON (needs testing)
  #totBobs is the total biomass for a species aggregated over boxes
  #split that into boxes using the true proportion of biomass in each box
  
  #now split that into the boxes based on the true proportion of biomass by species in each box
  #sum over boxes and ages (the sampled boxes were already subset in create functions)
  # Bbox <- aggregate(dat2$biomass,list(dat2$species,dat2$polygon),sum)
  # names(Bbox) <- c("species","polygon","biomassBox")
  Nbox <- aggregate(list(numbersBoxAge = dat$numbers),
                    by = list(species = dat$species, 
                              polygon = dat$polygon,
                              agecl = dat$agecl,
                              time = dat$time),
                    sum)
  
  # Bbox2 <- merge(Bbox,totB,by="species",all.x=TRUE)
  # Bbox2$propB <- Bbox2$biomassBox/Bbox2$biomass
  
  Nbox2 <- merge(Nbox, totN, by = c('species', 'time'), all.x = TRUE)
  Nbox2$propN <- Nbox2$numbersBox/Nbox2$numbers
  
  # Bbox3 <- merge(Bbox2,totBobs,by="species",all.x=TRUE)
  # Bbox3$obsBbox <- Bbox3$obsBiomass*Bbox3$propB
  
  Nbox3 <- merge(Nbox2,totNobs,by=c("species", 'time', 'numbers'),all.x=TRUE)
  Nbox3$obsNbox <- Nbox3$obsNumbers*Nbox3$propN
  
  #output (observed biomass by box, which adds up to the appropriate total biomass)
  out <- data.frame(species=Nbox3$species,
    	              agecl = Nbox3$agecl,
    	              polygon=Nbox3$polygon,
    	              layer=NA, 
                    time=Nbox3$time,
    	              atoutput=Nbox3$obsNbox)
  
  return(out)
}




if(F) {
  #to check how multiple sampling can work
  a <- seq(10,100,10)
  b <- seq(0.1,1,0.1)
  
  x <- matrix(rlnorm(length(a)*1000,log(a)-(b^2)/2,b),ncol=10,byrow=T)
  apply(x,2,mean)
  
  
  directory <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
  scenario <- "SETAS"
  groups <- load_fgs(dir = directory, "functionalGroups.csv")
  groups <- groups[groups$IsTurnedOn > 0, "Name"]
  results <- run_truth(scenario = scenario,
                       dir = directory,
                       file_fgs = "functionalGroups.csv",
                       file_bgm = "VMPA_setas.bgm",
                       select_groups = groups,
                       file_init = "INIT_VMPA_Jan2015.nc",
                       file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")
  
  species=c("Pisciv_T_Fish","Pisciv_S_Fish")
  boxes <- 1:3
  effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_S_Fish"), efficiency=c(0.3,0.1))
  selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
                      agecl=c(1:10,1:10),
                      selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))
  
  tmp <- create_survey(dat=results$nums, time=seq(70,82,3), species=species, boxes=boxes, effic=effic, selex=selex)
  
  cv <- data.frame(species=species, cv=c(0.2,0.3))
  
  survObsNum <- sample_survey_numbers(dat=tmp,cv=cv)
  
}