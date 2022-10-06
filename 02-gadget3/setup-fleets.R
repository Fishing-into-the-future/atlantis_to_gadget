## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------

############ Configure fleets ##################################################

## Survey(s)
total <-
  g3_fleet('total') %>%
  g3s_livesonareas(areas[c('1')])

igfs <-
  g3_fleet('igfs') %>%
  g3s_livesonareas(areas[c('1')])

aut <-
  g3_fleet('aut') %>%
  g3s_livesonareas(areas[c('1')])


## -----------------------------------------------------------------------------
## Create fleet actions

fleet_actions <-
  list(
    total %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('total.alpha', by_stock = 'species'),
                                                                        g3_parameterized('total.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('total_landings', total_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),

    ## Surveys
    aut %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('aut.alpha', by_stock = 'species'),
                                                                        g3_parameterized('aut.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('aut_landings', aut_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    igfs %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('igfs.alpha', by_stock = 'species'),
                                                                        g3_parameterized('igfs.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_landings', igfs_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))))
