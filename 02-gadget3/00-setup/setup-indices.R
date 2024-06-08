## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

if (FALSE){
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
}

numindices_igfs <- list()
for (i in 1:length(defaults)){
  numindices_igfs_tmp <- 
    mfdb_sample_count(mdb, c('length'), c(list(
      data_source = 'iceland-ldist-IGFS',
      length = mfdb_interval('len', si_len_groups, open_ended = c('lower', 'upper'))),
      defaults[[i]])
      ) %>% 
    purrr::map(function(y){
      y %>%
        split(.,~length) %>% 
        purrr::map(function(x){
          structure(x, length = attr(x, 'length')[unique(x$length)])
        })
    }) 
  numindices_igfs <- c(numindices_igfs, numindices_igfs_tmp)
}
rm(numindices_igfs_tmp)

numindices_aut <- list()
for (i in 1:length(defaults)){
    numindices_aut_tmp <- 
    mfdb_sample_count(mdb, c('length'), c(list(
      data_source = 'iceland-ldist-AUT',
      length = mfdb_interval('len', si_len_groups, open_ended = c('lower', 'upper'))),
      defaults[[i]])
      ) %>% 
    purrr::map(function(y){
      y %>% 
        split(.,~length) %>% 
        purrr::map(function(x){
          structure(x, length = attr(x, 'length')[unique(x$length)])
        })
    }) 
    numindices_aut <- c(numindices_aut, numindices_aut_tmp)
}
rm(numindices_aut_tmp)



if (length(defaults) > 1){
  save(numindices_igfs,#indices_igfs, 
       numindices_aut,#indices_aut, 
       file = file.path(base_dir, 'data', 'bootstrap_indices.Rdata'))
}else{
  save(numindices_igfs,#indices_igfs, 
       numindices_aut,#indices_aut, 
       file = file.path(base_dir, 'data', 'indices.Rdata'))
}

