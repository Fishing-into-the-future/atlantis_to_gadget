## -----------------------------------------------------------------------------
##
## Catches by fleet
##
## -----------------------------------------------------------------------------

## Surveys
aut_landings <-
  structure(data.frame(year = defaults[[1]]$year, step = 4, area = 1, total_weight = 1))

## Surveys
igfs_landings <-
  structure(data.frame(year = defaults[[1]]$year, step = 1, area = 1, total_weight = 1))

## All Landings
defaults2 <- defaults[[1]]
defaults2$sampling_type <- NULL
total_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(data_source = 'atlantis_fisheries'), defaults2))
total_landings$`0.0.0` <- total_landings$`0.0.0` %>% mutate(area = '1')

if (TRUE){
  save(total_landings,
       aut_landings, igfs_landings,
       file = file.path(base_dir, "data", "fleet_data.Rdata"))
}
