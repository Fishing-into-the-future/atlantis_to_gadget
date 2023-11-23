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
#' 
#' ### ---------------------------------------------------



library(tidyverse)

for (i in dir('ms-keyrun/data-raw/R')) source(file.path('ms-keyrun/data-raw/R', i))

fitstart <- NULL
fitend <- NULL
saveToData <- FALSE
outputfolder <- file.path('ms-keyrun/data-raw/atlantisoutput', v.name, sampling_id)
if (!dir.exists(outputfolder)) dir.create(outputfolder, recursive = TRUE)

## -----------------------------------------------------------------------------

#create_sim_focal_species(config_file)

## Need to check predator/prey ratio (not included atm)
simBiolPar <- create_sim_biolpar(config_file, saveToData=saveToData)
saveRDS(simBiolPar, file.path(outputfolder, "simBiolPar.rds"))

## (1) Initial population by age, (2) initial population by age-length, (3) average recruitment
simStartPars <- create_sim_startpars(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simStartPars, file.path(outputfolder, "simStartPars.rds"))

## Temperature

# NOTE:
# Warning message from atlantisom::load_nc_physics
# In atlantisom::load_nc_physics(dir = file.path(d.name, v.name),  :
# 0% of entries are min-pools (0, 1e-08, 1e-16)

#simSurveyBottemp <- create_sim_survey_bottemp(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyBottemp, file.path(outputfolder, "simSurveyBottemp.rds"))

## -----------------------------------------------------------------------------
## SURVEY DATASETS
## -----------------------------------------------------------------------------

## General info
simSurveyInfo <- create_sim_survey_info(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyInfo, file.path(outputfolder, "simSurveyInfo.rds"))

## Biomass index
simSurveyIndex <- create_sim_survey_index(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyIndex, file.path(outputfolder, "simSurveyIndex.rds"))

simSurveyIndexNums <- create_sim_survey_index_numbers(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyIndexNums, file.path(outputfolder, "simSurveyIndexNums.rds"))

# simSurveyIndex %>% 
#   filter(variable == 'biomass') %>% 
#   group_by(year, replicate, survey) %>% 
#   summarise(val = sum(value)) %>% 
#   ggplot(aes(year, val)) + geom_line(aes(col = as.factor(replicate))) + facet_wrap(~survey)

## Length composition 
simSurveyLencomp <- create_sim_survey_lencomp(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyLencomp, file.path(outputfolder, "simSurveyLencomp.rds"))

## Age composition 
## No ANNAGE files
#simSurveyAgecomp <- create_sim_survey_agecomp(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyAgecomp, file.path(outputfolder, "simSurveyAgecomp.rds"))

## ALK
simSurveyAgeLencomp <- create_sim_survey_agelen(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyAgeLencomp, file.path(outputfolder, "simSurveyAgeLencomp.rds"))

# simSurveyAgeLencomp %>% 
#   group_by(year, survey, replicate) %>% 
#   summarise(ml = (sum(lenbin*value)/sum(value)),
#             ma = (sum(agecl*value)/sum(value))) -> qq# %>% 
#   pivot_longer(cols = c(ml, ma), names_to = 'var') %>% 
#   ggplot(aes(year, value)) + 
#   geom_line(aes(col = as.factor(replicate))) + 
#   facet_wrap(~survey + var, scales = "free_y")

## Weight at age
simSurveyWtatAge <- create_sim_survey_wtage(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simSurveyWtatAge, file.path(outputfolder, "simSurveyWtatAge.rds"))

## -----------------------------------------------------------------------------
## COMMERCIAL DATASETS
## -----------------------------------------------------------------------------

## Length composition, just subannual needed
simFisheryLencomp <- create_sim_fishery_lencomp(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryLencomp, file.path(outputfolder, "simFisheryLencomp.rds"))

## Length composition subannual
simFisheryLencompSubannual <- create_sim_fishery_lencomp_subannual(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryLencompSubannual, file.path(outputfolder, "simFisheryLencompSubannual.rds"))

## Age composition
## No ANNAGE files
#simFisheryAgecomp <- create_sim_fishery_agecomp(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simFisheryAgecomp, file.path(outputfolder, "simFisheryAgecomp.rds"))

## Age composition subannual
#simFisheryAgecompSubannual <- create_sim_fishery_agecomp_subannual(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simFisheryAgecompSubannual, file.path(outputfolder, "simFisheryAgecompSubannual.rds"))

## ALK
simFisheryAgeLencomp <- create_sim_fishery_agelen(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryAgeLencomp, file.path(outputfolder, "simFisheryAgeLencomp.rds"))

## age-length distributions
simFisheryAgeLencompSubannual <- create_sim_fishery_agelen_subannual(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryAgeLencompSubannual, file.path(outputfolder, "simFisheryAgeLencompSubannual.rds"))

## Weight at age
simFisheryWtatAge <- create_sim_fishery_wtage(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryWtatAge, file.path(outputfolder, "simFisheryWtatAge.rds"))

## Weight at age subanual
simFisheryWtatAgeSubannual <- create_sim_fishery_wtage_subannual(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
saveRDS(simFisheryWtatAgeSubannual, file.path(outputfolder, "simFisheryWtatAgeSubannual.rds"))

## -----------------------------------------------------------------------------
## LANDINGS
## -----------------------------------------------------------------------------

## The create_sim_fishery_index utilises the catch.txt, however,
## this only contains annual catches and does not match the .nc output
## therefore using mfdbatlantis for catches

## Going to go with mfdbatlantis here...
## Because the stock_truth$catch looks odd... to do with the log.txt?

#simCatchIndexSubannual <- create_sim_fishery_index(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData) #creates subannual amd aggregate
#saveRDS(simCatchIndexSubannual, file.path(outputfolder, "simCatchIndexSubannual.rds"))

area_data <- NULL
for (i in seq_along(iceom_ms$boxpars$boxes)){
  tmp <- data.frame(id = i,
                    name = iceom_ms$boxpars$boxes[[i]]$label,
                    size = round(iceom_ms$boxpars$boxes[[i]]$area/1e6,0))
  area_data <- rbind(area_data, tmp)
}

## Read fishery types & upload as gears
is_fisheries <- 
  mfdbatlantis:::fetch_xml_attributes(XML::xmlParse(attr(is_dir, "xml_fisheries")), "Fishery", 
                                      c("Code", "Index", "Name", "IsRec", "NumSubFleets"), stringsAsFactors = FALSE)
saveRDS(is_fisheries, file.path(outputfolder, "is_fisheries.rds"))


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

fisherydata <- 
  fisherydata %>% 
  mutate(ModSim = unique(simFisheryLencomp$ModSim),
         simyear = ceiling((time/60/60/24)/365)) %>%
  select(-year) %>% 
  left_join(iceom$time_lookup %>% select(simyear, year) %>% distinct(), by = "simyear") %>% 
  rename(fishMonth = month,
         Code = functional_group) %>% 
  select(-species)

saveRDS(fisherydata, file.path(outputfolder, "fisherydata.rds"))

## -----------------------------------------------------------------------------
## Food web and diet
## -----------------------------------------------------------------------------

## Diet
#simSurveyDietcomp <- create_sim_survey_dietcomp(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyDietcomp, file.path(outputfolder, "simSurveyDietcomp.rds"))

#simPerCapCons <- create_sim_percapconsumption(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simPerCapCons, file.path(outputfolder, "simPerCapCons.rds"))

# food web model specific datasets add other species
#simSurveyIndexFW <- create_sim_survey_index_fw(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simSurveyIndexFW, file.path(outputfolder, "simSurveyIndexFW.rds"))

#simCatchIndexSubannualFW <- create_sim_fishery_index_fw(config_file, fitstart=fitstart, fitend=fitend, saveToData=saveToData)
#saveRDS(simCatchIndexSubannualFW, file.path(outputfolder, "simCatchIndexSubannualFW.rds"))

# below combines already loaded mskeyrun datasets,  
# outputs of create_sim_survey_agelen and create_sim_survey_dietcomp
# ensure that these are up to date before running

#create_sim_survey_lendietcomp()
