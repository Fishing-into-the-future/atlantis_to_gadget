## -----------------------------------------------------------------------------
##
## Runner to load output of atlantis om into mfdb
##
## -----------------------------------------------------------------------------

library(mfdb)
library(mfdbatlantis)


#mfdb(file.path(base_dir, 'db', 'atlantis_iceland_test_db.duckdb'), destroy_schema = TRUE)
mdb <- mfdb(file.path(base_dir, 'db', 'atlantis_iceland_test_db.duckdb'))

## -----------------------------------------------------------------------------
## Areas
## -----------------------------------------------------------------------------

area_data <- NULL
for (i in seq_along(boxpars$boxes)){
  tmp <- data.frame(id = i,
                    name = boxpars$boxes[[i]]$label,
                    size = round(boxpars$boxes[[i]]$area/1e6,0))
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
                             stock_truth$fgs %>%
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
## Survey Indices
## -----------------------------------------------------------------------------

tmp <-
  si_igfs %>%
  mutate(sampling_type = 'IGFS') %>%
  bind_rows(si_aut %>%
              mutate(sampling_type = 'AUT')) %>%
  left_join(time_to_yearmonth, by = 'time') %>%
  rename(areacell = polygon,
         count = atoutput) %>%
  mutate(species = 'FCD') %>%
  select(-agecl, -layer)

mfdb_import_survey(mdb, tmp, data_source = 'atlantis_survey_nums')

## -----------------------------------------------------------------------------
## Age-length distributions
## -----------------------------------------------------------------------------

## IGFS
tmp <-
  survey_aldists$IGFS$ss_length_stdsurv$natlength %>%
  mutate(sampling_type = 'IGFS') %>%
  bind_rows(
    survey_aldists$AUT$ss_length_stdsurv$natlength %>%
      mutate(sampling_type = 'AUT')) %>%
  bind_rows(
    catch_lengthwt_samp$natlength %>%
      mutate(sampling_type = 'SEA')) %>%
  left_join(time_to_yearmonth, by = 'time') %>%
  rename(age = agecl,
         areacell = polygon,
         count = atoutput,
         length_min = lower.bins,
         length_max = upper.bins) %>%
  mutate(length = (length_min + length_max)/2) %>%
  mutate(species = 'FCD') %>%
  select(-layer, -time)

mfdb_import_survey(mdb, tmp, data_source = 'atlantis_survey_aldists')

## -----------------------------------------------------------------------------
## Landings
## -----------------------------------------------------------------------------

## Going to go with mfdbatlantis here...
## Because the stock_truth$catch looks odd... to do with the log.txt?

## Read fishery types & upload as gears
is_fisheries <- atlantis_fisheries(is_dir)

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

mfdb_import_survey(mdb,
                   data.frame(year = fisherydata$year,
                              month = fisherydata$month,
                              areacell = fisherydata$area,
                              gear = fisherydata$fisheryCode,
                              tow = fisherydata$tow,
                              species = fisherydata$species,
                              weight_total = fisherydata$weight_total,
                              stringsAsFactors = TRUE),
                   data_source = 'atlantis_fisheries')









