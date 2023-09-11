## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

indices_igfs <-
  mfdb_sample_totalweight(mdb, NULL,
                    c(list(data_source = 'atlantis_survey_biomass',
                           sampling_type = 'IGFS'),
                      defaults))

indices_aut <-
  mfdb_sample_totalweight(mdb, NULL,
                    c(list(data_source = 'atlantis_survey_biomass',
                           sampling_type = 'AUT'),
                      defaults))


if (run_bootstrap){
  save(indices_igfs,
       indices_aut,
       file = file.path(base_dir, 'data', 'bootstrap_indices.Rdata'))
}else{
  save(indices_igfs,
       indices_aut,
       file = file.path(base_dir, 'data', 'indices.Rdata'))
}

