## -----------------------------------------------------------------------------
##
## Runner to load output of atlantis om into mfdb
##
## -----------------------------------------------------------------------------

## NOTES from WGSAM (courtesy of VB) - try to standardise approaches to a degree

## Import mskeyrun data into mfdb
##
## - CATCHES ::simCatchIndex  ---> simCatchIndexSubannual 
# no annage file for Iceland, subannual catches collated using mfdbatlantis becsuae catch.txt is only at an annual level
# use fisherydata instead 
## - CATCH LENGTH DISTRIBUTIONS ::simFisheryLencomp ---> simFisheryLencompSubannual 
# simFisheryLencompSubannual
## - CATCH AGE DISTRIBUTIONS :: simFisheryAgecomp  ---> simFisheryAgecompSubannual 
# not included
## - CATCH ALK :: simFisheryAgeLencomp
# simFisheryAgeLencomp
## - CATCH WGT@AGE ::simFisheryWtatAge  ---> simFisheryWtatAgeSubannual
# simFisheryWtatAgeSubannual
## - SURVEY INDICES ::simSurveyIndex
# simSurveyIndex
## - SURVEY LENGTH DISTRIBUTIONS ::simSurveyLencomp
# simSurveyLencomp
## - SURVEY AGE DISTRIBUTIONS ::simSurveyAgecomp
# Not included
## - SURVEY ALK ::simSurveyAgeLencomp
# simSurveyAgeLencomp
## - SURVEY WGT@AGE ::simSurveyWtatAge
# simSurveyWtatAge
## - DIET DATA BY AGE ::simSurveyDietcomp
# not included yet
## - DIET DATA BY LEN ::simSurveyLenDietcomp
# not included yet
## - INITIAL POPULATION BY AGE ::simStartPars
# simStartPars
## - AVERAGE RECRUITMENT ::simStartPars
# simStartPars
## - INITIAL POPULATION AGE-LENGTH ::simStartPars
# simStartPars
# ---------------------------------------------------


library(mfdb)
library(mfdbatlantis)
library(tidyverse)


sampling_id <- 'v3_effic1_cv00_rep1'
for (i in dir(file.path('ms-keyrun/data-raw/atlantisoutput/Out', sampling_id))){
  if (grepl('rds', i)){
    assign(gsub('\\.rds', '', i), readRDS(file.path('ms-keyrun/data-raw/atlantisoutput/Out', sampling_id, i)))
  }
}
load(file = file.path('ms-keyrun/data-raw/atlantisoutput/Out', sampling_id, 'iceom.Rdata'))

if (FALSE){
  mfdb(file.path('db', 'atlantisiceland_baseline_v3.duckdb'), destroy_schema = TRUE)
}
mdb <- mfdb(file.path('db', 'atlantisiceland_baseline_v3.duckdb'))


## -----------------------------------------------------------------------------
## Species lookup
## -----------------------------------------------------------------------------

sppLookup <- data.frame(Code = 'FCD',
                        mfdbSpp = 'COD')
sppList <- left_join(simBiolPar, sppLookup)

## Simulation name
simName <- unique(simSurveyIndex$ModSim)


## -----------------------------------------------------------------------------
## Areas
## -----------------------------------------------------------------------------

area_data <- NULL
for (i in seq_along(iceom_ms$boxpars$boxes)){
  tmp <- data.frame(id = i,
                    name = iceom_ms$boxpars$boxes[[i]]$label,
                    size = round(iceom_ms$boxpars$boxes[[i]]$area/1e6,0))
  area_data <- rbind(area_data, tmp)
}
## Into MFDB

mfdb_import_area(mdb, area_data)

## -----------------------------------------------------------------------------
## Temperature
## -----------------------------------------------------------------------------

## atlantisom option?
#is_temp <- atlantis_temperature(is_dir, area_data)
#mfdb_import_temperature(mdb, is_temp[is_temp$depth == 1,])

## Time to year month lookup - quick hack for now
# noutsteps <- floor(iceom$runpar$tstop/iceom$runpar$outputstep)
# time_to_yearmonth <-
#   data.frame(time = 0:(noutsteps),
#              month = rep(c(1,4,7,10), length(0:noutsteps)/4),
#              year = rep(1948:(1948+(length(0:noutsteps)/4)-1), each = 4))

## -----------------------------------------------------------------------------
## Functional groups
## -----------------------------------------------------------------------------

mfdb_import_species_taxonomy(mdb,
                            iceom$funct.groups %>%
                               select(name = Code, species = Name, description = LongName))

## -----------------------------------------------------------------------------
## Sampling types
## -----------------------------------------------------------------------------

## Sampling types
mfdb_import_sampling_type(mdb,
                          data.frame(id = 1:4,
                                     name = c('SEA','IGFS','AUT', 'INIT'),
                                     description = c('Sea sampling', 'Icelandic ground fish survey',
                                                     'Icelandic autumn survey', 'Initial population parameters')))


## -----------------------------------------------------------------------------
## Subannual landings
## -----------------------------------------------------------------------------

# tmp <- simCatchIndexSubannual %>%
#   ## filter(variable=="catch")
#   mutate(units=NULL,
#          area = paste0('Box', area)) %>%
#   group_by(ModSim, year, fishMonth, Code, Name, fishery, variable) %>% 
#   summarise(value = sum(value), .groups = 'drop') %>% 
#   tidyr::spread(variable,value) %>%
#   left_join(sppList) %>% 
#   mutate(area = NA)
# 
# 
# mfdb_import_survey(mdb,
#                    data.frame(year = tmp$year,
#                               month = tmp$fishMonth,
#                               areacell = tmp$area,
#                               #gear = fisherydata$fisheryCode,
#                               #tow = fisherydata$tow,
#                               species = tmp$mfdbSpp,
#                               weight_total =tmp$catch,
#                               weight_var = tmp$cv,
#                               sampling_type = tmp$fishery,
#                               stringsAsFactors = TRUE),
#                    data_source = 'atlantis_fisheries')



## OLD VERSION

# ## Going to go with mfdbatlantis here...
# ## Because the stock_truth$catch looks odd... to do with the log.txt?
# 
# ## Read fishery types & upload as gears
# is_fisheries <- mfdbatlantis:::fetch_xml_attributes(XML::xmlParse(attr(is_dir, "xml_fisheries")), 
#                                                       "Fishery", 
#                                                       c("Code", "Index", "Name", "IsRec", "NumSubFleets"), 
#                                                     stringsAsFactors = FALSE)
# 
# 
# # is_fisheries <- mfdbatlantis::atlantis_fisheries(is_dir)
# 
mfdb_import_cs_taxonomy(mdb,
                        'gear',
                        tibble(id = 1:nrow(is_fisheries),
                               name = is_fisheries$Code,
                               description = is_fisheries$Name))

## Discards present for haddock and cod only
## name == "All" will sum both catch and discard
mfdb_import_tow_taxonomy(mdb, data.frame(
  name = c("All", "C", "D"),
  t_group = c(NA, "All", "All"),
  stringsAsFactors = FALSE))

tmp <- 
  fisherydata %>% 
  right_join(sppList) %>% 
  select(year, fishMonth, area, fisheryCode, tow, species = mfdbSpp, weight_total)

mfdb_import_survey(mdb,
                   data.frame(year = tmp$year,
                              month = tmp$fishMonth,
                              areacell = tmp$area,
                              gear = tmp$fisheryCode,
                              tow = tmp$tow,
                              species = tmp$species,
                              weight_total = tmp$weight_total*1e3, ## to kg
                              stringsAsFactors = TRUE),
                   data_source = 'atlantis_fisheries')



## -----------------------------------------------------------------------------
## Survey data
## -----------------------------------------------------------------------------

cat('IMPORTING SURVEY INDICES BIOMASS\n\n')
for (i in unique(simSurveyIndex$replicate)){
  print(i)
  si <- 
    simSurveyIndex %>%
    filter(replicate == i) %>% 
    mutate(units=NULL,
           value = value*1e3) %>% ## kg
    left_join(sppList) %>%
    spread(variable,value) %>%
    left_join(simSurveyInfo %>%
                select(survey,survMonth) %>%
                unique()) %>%
    mutate(area = NA) %>% 
    rename(simyear = year) %>% 
    left_join(iceom$time_lookup %>% select(simyear, year) %>% distinct(), by = 'simyear')
  
  mfdb::mfdb_import_survey(
    mdb,
    data.frame(year = si$year,
               month = si$survMonth,
               areacell = si$area,
               species = si$mfdbSpp,
               sampling_type = si$survey,
               weight_total = si$biomass,
               weight_var = si$cv,
               stringsAsFactors = TRUE),
    data_source = paste0('atlantis_survey_biomass_rep', i)
  )
}

cat('IMPORTING SURVEY INDICES NUMBERs\n\n')
for (i in unique(simSurveyIndexNums$replicate)){
  print(i)
  si <- 
    simSurveyIndexNums %>%
    filter(replicate == i) %>% 
    mutate(units=NULL) %>% 
    left_join(sppList) %>%
    spread(variable,value) %>%
    left_join(simSurveyInfo %>%
                select(survey,survMonth) %>%
                unique()) %>%
    mutate(area = NA) %>% 
    rename(simyear = year) %>% 
    left_join(iceom$time_lookup %>% select(simyear, year) %>% distinct(), by = 'simyear')
  
  mfdb::mfdb_import_survey(
    mdb,
    data.frame(year = si$year,
               month = si$survMonth,
               areacell = paste0('Box', si$polygon),
               species = si$mfdbSpp,
               sampling_type = si$survey,
               length = (si$lower.bins + si$upper.bins)/2,
               length_var = si$cv,
               count = si$numbers,
               stringsAsFactors = TRUE),
    data_source = paste0('atlantis_survey_numbers_rep', i)
  )
}


  
  # ## Numbers
  # tmp <- 
  #   iceom_ms_ind$survObsNumB$IGFS_allbox_effic1[[1]] %>% 
  #   mutate(sampling_type = 'IGFS') %>%
  #   bind_rows(iceom_ms_ind$survObsNumB$AUT_allbox_effic1[[1]] %>%
  #               mutate(sampling_type = 'AUT')) %>%
  #   left_join(time_to_yearmonth, by = 'time') %>%
  #   rename(areacell = polygon,
  #          count = atoutput) %>%
  #   mutate(species = 'FCD') %>%
  #   select(-agecl, -layer)
  # 
  # mfdb_import_survey(mdb, tmp, data_source = 'atlantis_survey_nums')
  
  ## -----------------------------------------------------------------------------
  ## Length distributions
  ## -----------------------------------------------------------------------------
  
cat('IMPORTING LENGTH DISTRIBUTIONS\n\n')
for (i in unique(simSurveyIndex$replicate)){
  print(i)
  ldists <- 
    simSurveyLencomp %>%
    filter(replicate == i) %>% 
    mutate(units = NULL) %>% 
    left_join(sppList) %>% 
    left_join(simSurveyInfo %>%
                select(survey,survMonth) %>%
                unique()) %>% 
    rename(month = survMonth, fishery = survey) %>%
    bind_rows(
      simFisheryLencompSubannual %>% 
        filter(replicate == i) %>% 
        left_join(sppList) %>% 
        rename(month = fishMonth)
    ) %>% 
    mutate(area = NA) %>% 
    rename(simyear = year) %>% 
    left_join(iceom$time_lookup %>% select(simyear, year) %>% distinct(), by = 'simyear')
  
  
  mfdb_import_survey(mdb, 
                     data.frame(
                       year = ldists$year,
                       month = ldists$month,
                       areacell = ldists$area,
                       species = ldists$mfdbSpp,
                       sampling_type = ldists$fishery,
                       length = ldists$lenbin,
                       count = ldists$value,
                       stringsAsFactors = TRUE
                     ),
                     data_source = paste0('atlantis_survey_ldists_rep', i)
  )
}
  
  ## -----------------------------------------------------------------------------
  ## Age-length distributions
  ## -----------------------------------------------------------------------------
  
cat('IMPORTING AGE LENGTH DISTRIBUTIONS\n\n')
for (i in unique(simSurveyIndex$replicate)){
  print(i)
  aldists <- 
    simSurveyAgeLencomp %>%
    filter(replicate == i) %>% 
    mutate(units = NULL) %>% 
    left_join(sppList %>% select(ModSim,Code,mfdbSpp,SpawnMonth,RecruitMonth)) %>% 
    left_join(simSurveyInfo %>%
                select(survey,survMonth) %>%
                unique()) %>%
    # add age calendar (birthday 1 Jan)
    mutate(ageCal = ifelse(RecruitMonth <= 12 & survMonth >= RecruitMonth, agecl-1, agecl)) %>% 
    rename(month = survMonth, fishery = survey) %>%
    bind_rows(
      simFisheryAgeLencompSubannual %>%
        filter(replicate == i) %>% 
        left_join(sppList %>% select(ModSim,Code,mfdbSpp,SpawnMonth,RecruitMonth)) %>%
        mutate(ageCal = ifelse(RecruitMonth <= 12 & fishMonth >= RecruitMonth, agecl-1, agecl)) %>% 
        rename(month = fishMonth)
    ) %>%
    mutate(area = NA) %>% 
    rename(simyear = year) %>% 
    left_join(iceom$time_lookup %>% select(simyear, year) %>% distinct(), by = 'simyear')
  
  
  mfdb_import_survey(mdb, 
                     data.frame(
                       year = aldists$year,
                       month = aldists$month,
                       areacell = aldists$area,
                       species = aldists$mfdbSpp,
                       sampling_type = aldists$fishery,
                       #age = aldists$ageCal,
                       age = aldists$agecl,
                       length = aldists$lenbin,
                       count = aldists$value,
                       stringsAsFactors = TRUE
                     ),
                     data_source = paste0('atlantis_survey_aldists_rep', i)
  )
}
  
  
  ## --------------------------------------------------
  ## Weight at age
  ## --------------------------------------------------
 
cat('IMPORTING WEIGHT AT AGE\n\n')
for (i in unique(simSurveyIndex$replicate)){
  
  wgtage <- 
    simSurveyWtatAge %>% 
    filter(replicate == i) %>% 
    mutate(units = NULL) %>% 
    left_join(sppList %>% select(ModSim,Code,mfdbSpp,SpawnMonth,RecruitMonth)) %>% 
    left_join(simSurveyInfo %>%
                select(survey,survMonth) %>%
                unique()) %>%
    # add age calendar (birthday 1 Jan)
    mutate(ageCal = ifelse(RecruitMonth <= 12 & survMonth >= RecruitMonth, age-1, age)) %>% 
    rename(month = survMonth, fishery = survey) %>%
    bind_rows(
      simFisheryWtatAgeSubannual %>% 
        filter(replicate == i) %>% 
        left_join(sppList %>% select(ModSim,Code,mfdbSpp,SpawnMonth,RecruitMonth)) %>%
        # add age calendar (birthday 1 Jan)
        mutate(ageCal = ifelse(RecruitMonth <= 12 & fishMonth >= RecruitMonth, age-1, age)) %>% 
        rename(month = fishMonth)
    ) %>%
    mutate(area = NA) %>% 
    rename(simyear = year) %>% 
    left_join(iceom$time_lookup %>% select(simyear, year) %>% distinct(), by = 'simyear')
  
  
  mfdb_import_survey(mdb, 
                     data.frame(
                       year = wgtage$year,
                       month = wgtage$month,
                       areacell = wgtage$area,
                       species = wgtage$mfdbSpp,
                       sampling_type = wgtage$fishery,
                       #age = wgtage$ageCal,
                       age = wgtage$age,
                       weight = wgtage$value/1e3, ## kg
                       stringsAsFactors = TRUE
                     ),
                     data_source = paste0('atlantis_wgtage_rep', i)
  )
}


# ---------------------------------------------------
# IMPORT INITIAL POPULATION BY AGE
# ---------------------------------------------------

tmp <- simStartPars %>%
  mutate(units=NULL) %>%
  filter(variable == "Natage") %>%
  left_join(sppList) %>%
  rename(age = agecl) %>%
  mutate(year=1948, month = 1) %>% # initial year is 40
  mutate(area = NA) 

ggplot(tmp %>% filter(age!=1)) + geom_point(aes(age,value)) + facet_wrap(~mfdbSpp, scale="free") + ylim(0,NA)

mfdb_import_survey(mdb,
                   data_source = 'atlantis_anumb_init',
                   data.frame(
                     year = tmp$year,
                     month = tmp$month,
                     areacell = tmp$area,
                     species = tmp$mfdbSpp,
                     sampling_type = 'INIT',
                     age = tmp$age,
                     count = tmp$value,
                     stringsAsFactors = TRUE))

# ---------------------------------------------------
# IMPORT AVERAGE RECRUITMENT
# ---------------------------------------------------

tmp <- 
  simStartPars %>%
  mutate(units=NULL) %>%
  filter(variable == "AvgRec") %>%
  left_join(sppList) %>%
  mutate(age = cut(RecruitMonth, breaks=c(1,12,24,36), labels=0:2, include.lowest=T)) %>% # recruitment assumed at age0 ***CHECK
  mutate(year=1948, month = 1) %>% # use year1 to store avg recr
  mutate(area = NA)

mfdb_import_survey(mdb,
                   data_source = 'atlantis_logrec_avg',
                   data.frame(
                     year = tmp$year,
                     month = tmp$month,
                     areacell = tmp$area,
                     species = tmp$mfdbSpp,
                     sampling_type = 'INIT',
                     age = tmp$age,
                     count = tmp$value,
                     stringsAsFactors = TRUE))

# ---------------------------------------------------
# IMPORT INITIAL POPULATION AGE-LENGTH
# ---------------------------------------------------

tmp <- simStartPars %>%
  mutate(units=NULL) %>%
  filter(variable == "Natlen") %>%
  left_join(sppList) %>%
  mutate(age = agecl) %>% #ifelse(ageGrpSize == 2, agecl*2-1, # assign the first age in the age class
                      #ifelse(ageGrpSize == 4, agecl*4-1, agecl))) %>%
  mutate(year=1948, month = 1) %>% # *** verify that initial year is 40
  mutate(area = NA)

mfdb_import_survey(mdb,
                   data_source = 'atlantis_alnumb_init',
                   data.frame(
                     year = tmp$year,
                     month = tmp$month,
                     areacell = tmp$area,
                     species = tmp$mfdbSpp,
                     sampling_type = 'INIT',
                     length = tmp$lenbin,
                     age = tmp$age,
                     count = tmp$value,
                     stringsAsFactors = TRUE))

mfdb_disconnect(mdb)

