## -----------------------------------------------------------------------------
##
## Catches by fleet
##
## -----------------------------------------------------------------------------

## Surveys
aut_landings <-
  structure(data.frame(year = defaults$year, step = 4, area = 1, total_weight = 1))

## Surveys
igfs_landings <-
  structure(data.frame(year = defaults$year, step = 2, area = 1, total_weight = 1))

## All Landings
total_landings <- mfdb_sample_totalweight(mdb,
                                          NULL,
                                          c(list(tow = 'All'),
                                            defaults))
total_landings$`0.0.0` <- total_landings$`0.0.0` %>% mutate(area = '1')

if (TRUE){
  save(total_landings,
       aut_landings, igfs_landings,
       file = file.path(base_dir, "data", "fleet_data.Rdata"))
}
