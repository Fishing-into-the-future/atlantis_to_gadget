## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

indices_igfs <-
  mfdb_sample_count(mdb, NULL,
                    c(list(data_source = 'atlantis_survey_nums',
                           sampling_type = 'IGFS'),
                      defaults))

indices_aut <-
  mfdb_sample_count(mdb, NULL,
                    c(list(data_source = 'atlantis_survey_nums',
                           sampling_type = 'AUT'),
                      defaults))



if (TRUE){
  save(indices_igfs,
       indices_aut,
       file = file.path(base_dir, 'data', 'indices.Rdata'))
}
