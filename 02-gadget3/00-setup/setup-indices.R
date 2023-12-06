## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

indices_igfs <-
  mfdb_sample_totalweight(mdb, "data_source",
                    c(list(data_source = 'atlantis_survey_biomass_rep1',
                           sampling_type = 'IGFS'),
                      defaults))[[1]] %>% 
  split(.$data_source) %>% 
  map(dplyr::select, -data_source)

indices_aut <-
  mfdb_sample_totalweight(mdb, "data_source",
                    c(list(data_source = 'atlantis_survey_biomass_rep1',
                           sampling_type = 'AUT'),
                      defaults))[[1]] %>% 
  split(.$data_source) %>% 
  map(dplyr::select, -data_source)


numindices_igfs <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'atlantis_survey_numbers_rep1',
                      sampling_type = 'IGFS',
                      #length = mfdb_interval('len', c(5,20,35,45,60,80,100), open_ended = c('lower', 'upper'))),
                      length = mfdb_interval('len', c(5,35,45,60,80,100), open_ended = c('lower', 'upper'))),
                      defaults)) %>% 
  purrr::map(function(y){
    y %>% 
      split(.,~length) %>% 
      purrr::map(function(x){
        structure(x, length = attr(x, 'length')[unique(x$length)])
      })
  }) 

numindices_aut <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'atlantis_survey_numbers_rep1',
                      sampling_type = 'AUT',
                      #length = mfdb_interval('len', c(5,20,35,45,60,80,100), open_ended = c('lower', 'upper'))),
                      length = mfdb_interval('len', c(5,35,45,60,80,100), open_ended = c('lower', 'upper'))),
                      defaults)) %>% 
  purrr::map(function(y){
    y %>% 
      split(.,~length) %>% 
      purrr::map(function(x){
        structure(x, length = attr(x, 'length')[unique(x$length)])
      })
  })



if (run_bootstrap){
  save(indices_igfs, numindices_igfs,
       indices_aut, numindices_aut,
       file = file.path(base_dir, 'data', 'bootstrap_indices.Rdata'))
}else{
  save(indices_igfs, numindices_igfs,
       indices_aut, numindices_aut,
       file = file.path(base_dir, 'data', 'indices.Rdata'))
}

