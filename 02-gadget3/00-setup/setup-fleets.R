## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------

############ Configure fleets ##################################################

# FYI, selectivity changes in time, both slope and inflection point
#   simtime time simyear year doy month step
# 141   10220  140      28 1975 365    12    5
# 211   15330  210      42 1989 365    12    5
# 301   21900  300      60 2007 365    12    5



## Survey(s)
comm <-
  g3_fleet('comm') %>%
  g3s_livesonareas(areas[c('1')])

igfs <-
  g3_fleet('igfs') %>%
  g3s_livesonareas(areas[c('1')])

aut <-
  g3_fleet('aut') %>%
  g3s_livesonareas(areas[c('1')])


## -----------------------------------------------------------------------------
## Create fleet actions

## Setup up commercial fleet first as we may want time-varying selectivtiy
if (timevarying_selectivity){
  
  selform <- ~if (cur_year < 1976L) a else if (cur_year > 1975L && cur_year < 1990L) b else if (cur_year > 1989L && cur_year < 2008L) c else d
  
  sel_alpha <- gadget3:::f_substitute(selform, list(a = g3_parameterized('alpha.1948', by_stock = TRUE, by_predator = TRUE),
                                                    b = g3_parameterized('alpha.1976', by_stock = TRUE, by_predator = TRUE),
                                                    c = g3_parameterized('alpha.1990', by_stock = TRUE, by_predator = TRUE),
                                                    d = g3_parameterized('alpha.2008', by_stock = TRUE, by_predator = TRUE)))
  
  sel_l50 <- gadget3:::f_substitute(selform, list(a = g3_parameterized('l50.1948', by_stock = TRUE, by_predator = TRUE),
                                                  b = g3_parameterized('l50.1976', by_stock = TRUE, by_predator = TRUE),
                                                  c = g3_parameterized('l50.1990', by_stock = TRUE, by_predator = TRUE),
                                                  d = g3_parameterized('l50.2008', by_stock = TRUE, by_predator = TRUE)))
}else{
  sel_alpha <- g3_parameterized('comm.alpha', by_stock = 'species')
  sel_l50 <- g3_parameterized('comm.l50', by_stock = 'species')
}

if (decadal_survey_selection){
  
  selform_s <- ~if (cur_year < 1970L) a else 
                     if (cur_year > 1969L && cur_year < 1980L) b else 
                     if (cur_year > 1979L && cur_year < 1990L) c else 
                     if (cur_year > 1989L && cur_year < 2000L) d else e
  
  sel_alpha_s <- gadget3:::f_substitute(selform_s, list(a = g3_parameterized('alpha.60s', by_stock = TRUE, by_predator = TRUE),
                                                        b = g3_parameterized('alpha.70s', by_stock = TRUE, by_predator = TRUE),
                                                        c = g3_parameterized('alpha.80s', by_stock = TRUE, by_predator = TRUE),
                                                        d = g3_parameterized('alpha.90s', by_stock = TRUE, by_predator = TRUE),
                                                        e = g3_parameterized('alpha.00s', by_stock = TRUE, by_predator = TRUE)))
  
  sel_l50_s <- gadget3:::f_substitute(selform_s, list(a = g3_parameterized('l50.60s', by_stock = TRUE, by_predator = TRUE),
                                                      b = g3_parameterized('l50.70s', by_stock = TRUE, by_predator = TRUE),
                                                      c = g3_parameterized('l50.80s', by_stock = TRUE, by_predator = TRUE),
                                                      d = g3_parameterized('l50.90s', by_stock = TRUE, by_predator = TRUE),
                                                      e = g3_parameterized('l50.00s', by_stock = TRUE, by_predator = TRUE)))
}else{
  sel_alpha_s <- g3_parameterized('alpha', by_stock = TRUE, by_predator = TRUE)
  sel_l50_s <- g3_parameterized('l50', by_stock = TRUE, by_predator = TRUE)
}

fleet_actions <-
    list(
      comm %>%
        g3a_predate_fleet(stocks,
                          suitabilities =
                            stocks %>%
                            set_names(.,map(.,'name')) %>%
                            map(function(x){
                              if (dome_comm){ 
                                g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen'))
                                )
                              }else{ ## S-shaped selectivity
                                gadget3::g3_suitability_exponentiall50(
                                  alpha = sel_alpha,
                                  l50 = sel_l50
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
                                g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen'))
                                )
                              }else{
                                gadget3::g3_suitability_exponentiall50(alpha = sel_alpha_s, 
                                                                       l50 = sel_l50_s)
                              }
                            }),
                          if (survey_fleet){
                            g3a_predate_catchability_effortfleet(E = 5e-10,
                                                                 catchability_fs = 
                                                                   if (estimate_catchability) 
                                                                     g3_parameterized('catchability', 
                                                                                      by_stock = TRUE,
                                                                                      by_predator = TRUE) else 1L)
                          }else{
                            g3a_predate_catchability_totalfleet(
                              g3_timeareadata('igfs_landings',
                                              igfs_landings %>%
                                                mutate(area = as.numeric(area),
                                                       step = as.numeric(step),
                                                       year = as.numeric(year)) %>% 
                                                filter(year >= igfs_start_year)))}),
      aut %>%
        g3a_predate_fleet(stocks,
                          suitabilities =
                            stocks %>%
                            set_names(.,map(.,'name')) %>%
                            map(function(x){
                              if (dome_survey){
                                g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen'))
                                )
                              }else{
                                gadget3::g3_suitability_exponentiall50(alpha = sel_alpha_s, 
                                                                       l50 = sel_l50_s) 
                              }
                            }),
                          catchability_f = 
                            if (survey_fleet){
                                g3a_predate_catchability_effortfleet(E = 5e-10,
                                                                     catchability_fs = 
                                                                       if (estimate_catchability) 
                                                                         g3_parameterized('catchability', 
                                                                                          by_stock = TRUE,
                                                                                          by_predator = TRUE) else 1L)
                              }else{
                                g3a_predate_catchability_totalfleet(
                                  g3_timeareadata('aut_landings', 
                                                  aut_landings %>%
                                                    mutate(area = as.numeric(area),
                                                           step = as.numeric(step),
                                                           year = as.numeric(year)) %>% 
                                                    filter(year >= aut_start_year)))}),
      list()
      )
