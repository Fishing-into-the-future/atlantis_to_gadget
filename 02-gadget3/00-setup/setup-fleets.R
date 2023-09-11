## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------

############ Configure fleets ##################################################

## Survey(s)
comm <-
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
    comm %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x){
                            if (dome_comm){ 
                              g3_suitability_andersen(
                                p0 = g3_parameterized('andersen.p0', by_stock = 'species'),
                                p1 = g3_parameterized('com.p1', by_stock = 'species'),
                                p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
                                p3 = g3_parameterized('com.p3', by_stock = 'species'),
                                p4 = g3_parameterized('com.p4', by_stock = 'species'),
                                p5 = g3_parameterized('andersen.L', by_stock = 'species')
                              )
                            }else{ ## S-shaped selectivity
                              gadget3::g3_suitability_exponentiall50(
                                alpha = g3_parameterized('com.alpha', by_stock = 'species'),
                                l50 = g3_parameterized('com.l50', by_stock = 'species')
                              ) 
                            }
                          }),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('total_landings', total_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),

    ## Surveys
    igfs %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x){
                            if (dome_survey){
                              g3_suitability_andersen(
                                p0 = g3_parameterized('andersen.p0', by_stock = 'species'),
                                p1 = g3_parameterized('surQ1.p1', by_stock = 'species'),
                                p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
                                p3 = g3_parameterized('surQ1.p3', by_stock = 'species'),
                                p4 = g3_parameterized('surQ1.p4', by_stock = 'species'),
                                p5 = g3_parameterized('andersen.L', by_stock = 'species')
                              )
                            }else{
                              gadget3::g3_suitability_exponentiall50(
                                alpha = g3_parameterized('surQ1.alpha', by_stock = 'species'),
                                l50 = g3_parameterized('surQ1.l50', by_stock = 'species')
                              )
                            }
                          }),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_landings', igfs_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    aut %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x){
                            if (dome_survey){
                              g3_suitability_andersen(
                                p0 = g3_parameterized('andersen.p0', by_stock = 'species'),
                                p1 = g3_parameterized('surQ3.p1', by_stock = 'species'),
                                p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
                                p3 = g3_parameterized('surQ3.p3', by_stock = 'species'),
                                p4 = g3_parameterized('surQ3.p4', by_stock = 'species'),
                                p5 = g3_parameterized('andersen.L', by_stock = 'species')
                              )
                            }else{
                              gadget3::g3_suitability_exponentiall50(
                                alpha = g3_parameterized('surQ3.alpha', by_stock = 'species'),
                                l50 = g3_parameterized('surQ3.l50', by_stock = 'species')
                              ) 
                            }
                          }),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('aut_landings', aut_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))))
