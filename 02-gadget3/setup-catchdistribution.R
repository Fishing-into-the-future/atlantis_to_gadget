## -----------------------------------------------------------------------------
## Catch age and length distributions:
## -----------------------------------------------------------------------------

minage <- gadget3:::stock_definition(imm_stock, 'stock__minage')
maxage <- gadget3:::stock_definition(mat_stock, 'stock__maxage')
minlength <- 4
maxlength <- 150
dl <- gadget3:::stock_definition(mat_stock, 'stock__dl') %>% min()

aldist.igfs <-
  mfdb_sample_count(mdb, c("age", "length"),
                    c(list(data_source = 'atlantis_survey_aldists',
                           age = mfdb_interval('age',c(1:16),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(1, 150, by = 2),
                                                  open_ended = c("upper","lower")),
                           sampling_type = 'IGFS'),
                      defaults))

aldist.aut <-
  mfdb_sample_count(mdb, c("age", "length"),
                    c(list(data_source = 'atlantis_survey_aldists',
                           age = mfdb_interval('age',c(1:16),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(1, 150, by = 2),
                                                  open_ended = c("upper","lower")),
                           sampling_type = 'AUT'),
                      defaults))

aldist.sea <-
  mfdb_sample_count(mdb, c("age", "length"),
                    c(list(data_source = 'atlantis_survey_aldists',
                           age = mfdb_interval('age',c(1:16),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(1, 150, by = 2),
                                                  open_ended = c("upper","lower")),
                           sampling_type = 'SEA'),
                      defaults))




if (TRUE){
  save(aldist.igfs,
       aldist.aut,
       aldist.sea,
       file = file.path(base_dir, 'data', 'catchdistribution_data.Rdata'))
}
