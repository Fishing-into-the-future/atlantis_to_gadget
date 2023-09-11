## -----------------------------------------------------------------------------
## Catch age and length distributions:
## -----------------------------------------------------------------------------

minage <- lapply(stocks, gadget3::g3_stock_def, 'minage') |> unlist() |> min()
maxage <- lapply(stocks, gadget3::g3_stock_def, 'maxage') |> unlist() |> max()
minlen <- lapply(stocks, gadget3::g3_stock_def, 'minlen') |> unlist() |> min()
maxlen <- 
  lapply(stocks, gadget3::g3_stock_def, 'maxlen') |> 
  unlist() |> (\(.) replace(., is.infinite(.), NA))() |> max(na.rm = TRUE)
dl <- lapply(stocks, function(x) diff(gadget3::g3_stock_def(x, 'minlen'))) |> unlist() |> min()


ldist.igfs <-
  mfdb_sample_count(mdb, 
                    c("age", "length"),
                    c(list(
                      data_source = 'atlantis_survey_aldists',
                      sampling_type = 'IGFS',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len",
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.igfs <-
  mfdb_sample_count(mdb, 
                    c("age", "length"),
                    c(list(
                      data_source = 'atlantis_survey_aldists',
                      sampling_type = 'IGFS',
                      age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                               open_ended = "upper"),
                      length = mfdb_interval("len", 
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

## AUTUMN
ldist.aut <-
  mfdb_sample_count(mdb, 
                    c("age", "length"),
                    c(list(
                      data_source = 'atlantis_survey_aldists',
                      sampling_type = 'AUT',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len",
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.aut <-
  mfdb_sample_count(mdb, 
                    c("age", "length"),
                    c(list(
                      data_source = 'atlantis_survey_aldists',
                      sampling_type = 'AUT',
                      age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                               open_ended = "upper"),
                      length = mfdb_interval("len", 
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

## COMMERCIAL
ldist.sea <- 
  mfdb_sample_count(mdb, 
                    c("age", "length"),
                    c(list(
                      data_source = 'atlantis_survey_aldists',
                      sampling_type = 'SEA',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len",
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.sea <-
  mfdb_sample_count(mdb, 
                    c("age", "length"),
                    c(list(
                      data_source = 'atlantis_survey_aldists',
                      sampling_type = 'SEA',
                      age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                               open_ended = "upper"),
                      length = mfdb_interval("len", 
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


if (run_bootstrap){
  save(aldist.igfs, ldist.igfs,
       aldist.aut, ldist.aut,
       aldist.sea, ldist.sea,
       file = file.path(base_dir, 'data', 'catchdistribution_bootstrap_data.Rdata'))
}else{
  save(aldist.igfs, ldist.igfs,
       aldist.aut, ldist.aut,
       aldist.sea, ldist.sea,
       file = file.path(base_dir, 'data', 'catchdistribution_data.Rdata'))
}

