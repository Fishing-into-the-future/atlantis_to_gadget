## -----------------------------------------------------------------------------
##
## Runner to load output of atlantis om into mfdb
##
## -----------------------------------------------------------------------------

library(mfdb)
library(mfdbatlantis)

if (FALSE){
  mfdb(file.path('db', 'atlantisiceland.duckdb'), destroy_schema = TRUE)
}
mdb <- mfdb(file.path('db', 'atlantisiceland.duckdb'))


noutsteps <- floor(iceom$runpar$tstop/iceom$runpar$outputstep)

## -----------------------------------------------------------------------------
## Species lookup
## -----------------------------------------------------------------------------

sppLookup <- data.frame(Code = 'FCD',
                        mfdbSpp = 'COD')
sppList <- left_join(simBiolPar, sppLookup)

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
is_temp <- atlantis_temperature(is_dir, area_data)
mfdb_import_temperature(mdb, is_temp[is_temp$depth == 1,])

## Time to year month lookup - quick hack for now
time_to_yearmonth <-
  data.frame(time = 0:(noutsteps),
             month = rep(c(1,4,7,10), length(0:noutsteps)/4),
             year = rep(1948:(1948+(length(0:noutsteps)/4)-1), each = 4))

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
                          data.frame(id = 1:3,
                                     name = c('SEA','IGFS','AUT'),
                                     description = c('Sea sampling', 'Icelandic ground fish survey',
                                                     'Icelandic autumn survey')))


## -----------------------------------------------------------------------------
## Survey data
## -----------------------------------------------------------------------------

si <- 
  simSurveyIndex %>%
  mutate(units=NULL) %>%
  left_join(sppList) %>%
  spread(variable,value) %>%
  left_join(simSurveyInfo %>%
              select(survey,survMonth) %>%
              unique()) %>%
  mutate(area = NA)


mfdb::mfdb_import_survey(
  mdb,
  data.frame(year = si$year,
             month = si$survMonth,
             areacell = si$area,
             species = si$mfdbSpp,
             sampling_type = do.call('c', lapply(strsplit(si$survey, '_'), function(x) x[[1]])),
             weight_total = si$biomass,
             weight_var = si$cv,
             stringsAsFactors = TRUE),
  data_source = 'atlantis_survey_biomass'
)

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

ldists <- 
  simSurveyLencomp %>%
  mutate(units = NULL) %>% 
  left_join(sppList) %>% 
  left_join(simSurveyInfo %>%
              select(survey,survMonth) %>%
              unique()) %>% 
  mutate(fishery = do.call('c', lapply(strsplit(simSurveyLencomp$survey, '_'), function(x) x[[1]]))) %>% 
  rename(month = survMonth) %>% 
  select(-survey) %>% 
  bind_rows(
    simFisheryLencompSubannual %>% 
      left_join(sppList) %>% 
      rename(month = fishMonth)
  ) %>% 
  mutate(area = NA) 


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
                   data_source = 'atlantis_survey_ldists')

## -----------------------------------------------------------------------------
## Age-length distributions
## -----------------------------------------------------------------------------


aldists <- 
  simSurveyAgeLencomp %>%
  mutate(units = NULL) %>% 
  left_join(sppList) %>% 
  left_join(simSurveyInfo %>%
              select(survey,survMonth) %>%
              unique()) %>%
  # add age calendar (birthday 1 Jan)
  mutate(ageCal = ifelse(RecruitMonth <= 12 & survMonth >= RecruitMonth, agecl-1, agecl)) %>% 
  mutate(fishery = do.call('c', lapply(strsplit(simSurveyAgeLencomp$survey, '_'), function(x) x[[1]]))) %>% 
  rename(month = survMonth) %>% 
  select(-survey) %>% 
  # bind_rows(
  #   simFisheryAgeLencompSubannual %>% 
  #     left_join(sppList) %>% 
  #     rename(month = fishMonth)
  # ) %>% 
  mutate(area = NA) 


mfdb_import_survey(mdb, 
                   data.frame(
                     year = aldists$year,
                     month = aldists$month,
                     areacell = aldists$area,
                     species = aldists$mfdbSpp,
                     sampling_type = aldists$fishery,
                     age = aldists$ageCal,
                     length = aldists$lenbin,
                     count = aldists$value,
                     stringsAsFactors = TRUE
                   ),
                   data_source = 'atlantis_survey_aldists')


## -----------------------------------------------------------------------------
## Subannual landings
## -----------------------------------------------------------------------------

tmp <- simCatchIndexSubannual %>%
  ## filter(variable=="catch")
  mutate(units=NULL,
         area = paste0('Box', area)) %>%
  group_by(ModSim, year, fishMonth, Code, Name, fishery, variable) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  tidyr::spread(variable,value) %>%
  left_join(sppList) %>% 
  mutate(area = NA)


mfdb_import_survey(mdb,
                   data.frame(year = tmp$year,
                              month = tmp$fishMonth,
                              areacell = tmp$area,
                              #gear = fisherydata$fisheryCode,
                              #tow = fisherydata$tow,
                              species = tmp$mfdbSpp,
                              weight_total =tmp$catch,
                              weight_var = tmp$cv,
                              sampling_type = tmp$fishery,
                              stringsAsFactors = TRUE),
                   data_source = 'atlantis_fisheries')


mfdb_disconnect(mdb)

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
# mfdb_import_cs_taxonomy(mdb,
#                         'gear',
#                         tibble(id = 1:nrow(is_fisheries),
#                                name = is_fisheries$Code,
#                                description = is_fisheries$Name))
# 
# ## Discards present for haddock and cod only
# ## name == "All" will sum both catch and discard
# mfdb_import_tow_taxonomy(mdb, data.frame(
#   name = c("All", "C", "D"),
#   t_group = c(NA, "All", "All"),
#   stringsAsFactors = FALSE))
# 
# fisherydata <- NULL
# for (fisheryCode in is_fisheries$Code){
# 
#   fishery <- is_fisheries[is_fisheries$Code %in% fisheryCode,]
#   cat("Importing fishery", fisheryCode, "\n")
#   #print(unique(is_catch$species))
# 
#   ## Catch
#   is_catch <- atlantis_fisheries_catch(is_dir, area_data, fishery)
#   if (nrow(is_catch) == 0) is_catch <- NULL else is_catch$tow <- 'C'
#   ## Discards
#   is_discard <- atlantis_fisheries_discard(is_dir, area_data, fishery)
#   if (nrow(is_discard) == 0) is_discard <- NULL else is_discard$tow <- 'D'
# 
#   ## Collate and add relevant info
#   cd <- bind_rows(is_catch, is_discard)
#   cd$species <- cd$functional_group
#   cd$fisheryCode <- fisheryCode
# 
#   fisherydata <- rbind(fisherydata, cd)
# 
# }
# 
# mfdb_import_survey(mdb,
#                    data.frame(year = fisherydata$year,
#                               month = fisherydata$month,
#                               areacell = fisherydata$area,
#                               gear = fisherydata$fisheryCode,
#                               tow = fisherydata$tow,
#                               species = fisherydata$species,
#                               weight_total = fisherydata$weight_total,
#                               stringsAsFactors = TRUE),
#                    data_source = 'atlantis_fisheries')









