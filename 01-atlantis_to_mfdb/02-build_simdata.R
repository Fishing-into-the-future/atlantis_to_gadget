#' Sarah's notes for building simulated dataset
#' 
#' all atlantis files are local on my computer in folder
#' ms-keyrun/simlulated-data/atlantisoutput
#' 
#' see SimData.Rmd for how these are generated using atlantisom
#' 
#' to make data for package
#' source these files in data-raw/R:
#'
#' create_sim_focal_species.R
#' get_sim_survey_index.R
#' 
#' run from ms-keyrun directory

library(tidyverse)

for (i in dir('ms-keyrun/data-raw/R')) source(file.path('ms-keyrun/data-raw/R', i))

atlmod <- file.path("config/test_run.R")
fitstart <- 20
fitend <- 65
saveToData <- FALSE
outputfolder <- file.path('ms-keyrun/data-raw/atlantisoutput', v.name)
if (!dir.exists(outputfolder)) dir.create(outputfolder)

## -----------------------------------------------------------------------------

#create_sim_focal_species(atlmod)

## Need to check predator/prey ratio (not included atm)
simBiolPar <- create_sim_biolpar(atlmod, saveToData=saveToData)
saveRDS(simBiolPar, file.path(outputfolder, "simBiolPar.rds"))

simSurveyInfo <- create_sim_survey_info(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyInfo, file.path(outputfolder, "simSurveyInfo.rds"))

simSurveyIndex <- create_sim_survey_index(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyIndex, file.path(outputfolder, "simSurveyIndex.rds"))

## SEE BOTTOM FOR LANDINGS
simCatchIndexSubannual <- create_sim_fishery_index_isl(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData) #creates subannual amd aggregate
saveRDS(simCatchIndexSubannual, file.path(outputfolder, "simCatchIndexSubannual.rds"))

## Age composition
#create_sim_survey_agecomp(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)

#create_sim_fishery_agecomp(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)

#create_sim_fishery_agecomp_subannual(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)

## Length composition data
simSurveyLencomp <- create_sim_survey_lencomp(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyLencomp, file.path(outputfolder, "simSurveyLencomp.rds"))

simFisheryLencomp <- create_sim_fishery_lencomp(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryLencomp, file.path(outputfolder, "simFisheryLencomp.rds"))

simFisheryLencompSubannual <- create_sim_fishery_lencomp_subannual(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryLencompSubannual, file.path(outputfolder, "simFisheryLencompSubannual.rds"))

## Diet
#simSurveyDietcomp <- create_sim_survey_dietcomp(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyDietcomp, file.path(outputfolder, "simSurveyDietcomp.rds"))

## Temperature
#simSurveyBottemp <- create_sim_survey_bottemp(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyBottemp, file.path(outputfolder, "simSurveyBottemp.rds"))

simFisheryWtatAge <- create_sim_fishery_wtage(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryWtatAge, file.path(outputfolder, "simFisheryWtatAge.rds"))

simFisheryWtatAgeSubannual <- create_sim_fishery_wtage_subannual(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryWtatAgeSubannual, file.path(outputfolder, "simFisheryWtatAgeSubannual.rds"))

simSurveyWtatAge <- create_sim_survey_wtage(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyWtatAge, file.path(outputfolder, "simSurveyWtatAge.rds"))

simSurveyAgeLencomp <- create_sim_survey_agelen(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyAgeLencomp, file.path(outputfolder, "simSurveyAgeLencomp.rds"))

simFisheryAgeLencomp <- create_sim_fishery_agelen(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryAgeLencomp, file.path(outputfolder, "simFisheryAgeLencomp.rds"))

#simPerCapCons <- create_sim_percapconsumption(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simPerCapCons, file.path(outputfolder, "simPerCapCons.rds"))

simStartPars <- create_sim_startpars(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simStartPars, file.path(outputfolder, "simStartPars.rds"))

# food web model specific datasets add other species

#simSurveyIndexFW <- create_sim_survey_index_fw(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyIndexFW, file.path(outputfolder, "simSurveyIndexFW.rds"))

#simCatchIndexSubannualFW <- create_sim_fishery_index_fw(atlmod, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simCatchIndexSubannualFW, file.path(outputfolder, "simCatchIndexSubannualFW.rds"))

# below combines already loaded mskeyrun datasets,  
# outputs of create_sim_survey_agelen and create_sim_survey_dietcomp
# ensure that these are up to date before running

#create_sim_survey_lendietcomp()

iceom$truth$catch %>% filter(species %in% species_ss)



## LANDINGS
area_data <- NULL
for (i in seq_along(iceom_ms$boxpars$boxes)){
  tmp <- data.frame(id = i,
                    name = iceom_ms$boxpars$boxes[[i]]$label,
                    size = round(iceom_ms$boxpars$boxes[[i]]$area/1e6,0))
  area_data <- rbind(area_data, tmp)
}

## Going to go with mfdbatlantis here...
## Because the stock_truth$catch looks odd... to do with the log.txt?

## Read fishery types & upload as gears
is_fisheries <- 
  mfdbatlantis:::fetch_xml_attributes(XML::xmlParse(attr(is_dir, "xml_fisheries")), "Fishery", 
                                       c("Code", "Index", "Name", "IsRec", "NumSubFleets"), stringsAsFactors = FALSE)

fisherydata <- NULL
for (fisheryCode in is_fisheries$Code){
  
  fishery <- is_fisheries[is_fisheries$Code %in% fisheryCode,]
  cat("Importing fishery", fisheryCode, "\n")
  #print(unique(is_catch$species))
  
  ## Catch
  is_catch <- atlantis_fisheries_catch(is_dir, area_data, fishery)
  if (nrow(is_catch) == 0) is_catch <- NULL else is_catch$tow <- 'C'
  ## Discards
  is_discard <- atlantis_fisheries_discard(is_dir, area_data, fishery)
  if (nrow(is_discard) == 0) is_discard <- NULL else is_discard$tow <- 'D'
  
  ## Collate and add relevant info
  cd <- bind_rows(is_catch, is_discard)
  cd$species <- cd$functional_group
  cd$fisheryCode <- fisheryCode
  
  fisherydata <- rbind(fisherydata, cd)
  
}








