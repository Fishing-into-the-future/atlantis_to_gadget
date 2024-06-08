## -----------------------------------------------------------------------------
## Catch age and length distributions:
## -----------------------------------------------------------------------------

minage <- lapply(stocks, gadget3::g3_stock_def, 'minage') |> unlist() |> min()
maxage <- lapply(stocks, gadget3::g3_stock_def, 'maxage') |> unlist() |> max()
minlen <- lapply(stocks, gadget3::g3_stock_def, 'minlen') |> unlist() |> min()
maxlen <- max(sapply(stocks, gadget3::g3_stock_def, 'maxmidlen'))
dl <- lapply(stocks, function(x) diff(gadget3::g3_stock_def(x, 'minlen'))) |> unlist() |> min()


ldist.igfs <- aldist.igfs <- ldist.aut <- aldist.aut <- ldist.sea <- aldist.sea <- list()

for (i in 1:length(defaults)){
  
  ldist.igfs_tmp <-
    mfdb_sample_count(mdb, 
                      c("age", "length"),
                      c(list(
                        data_source = 'iceland-ldist-IGFS',
                        age = mfdb_interval("all",c(minage,maxage),
                                            open_ended = c("upper","lower")),
                        length = mfdb_interval("len",
                                               seq(minlen, maxlen, by = dl),
                                               open_ended = c("upper","lower"))),
                        defaults[[i]]))
  
  ldist.igfs <- c(ldist.igfs, ldist.igfs_tmp)
  
  aldist.igfs_tmp <-
    mfdb_sample_count(mdb, 
                      c("age", "length"),
                      c(list(
                        data_source = 'iceland-aldist-IGFS',
                        age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                 open_ended = "upper"),
                        length = mfdb_interval("len", 
                                               seq(minlen, maxlen, by = dl),
                                               open_ended = c("upper","lower"))),
                        defaults[[i]]))
  
  aldist.igfs_tmp[[1]]$age <- paste0('age', gsub('age', '', aldist.igfs_tmp[[1]]$age) %>% as.integer())
  aldist.igfs <- c(aldist.igfs, aldist.igfs_tmp)
  
  ## AUTUMN
  ldist.aut_tmp <-
    mfdb_sample_count(mdb, 
                      c("age", "length"),
                      c(list(
                        data_source = 'iceland-ldist-AUT',
                        age = mfdb_interval("all",c(minage,maxage),
                                            open_ended = c("upper","lower")),
                        length = mfdb_interval("len",
                                               seq(minlen, maxlen, by = dl),
                                               open_ended = c("upper","lower"))),
                        defaults[[i]]))
  
  ldist.aut <- c(ldist.aut, ldist.aut_tmp)
  
  aldist.aut_tmp <-
    mfdb_sample_count(mdb, 
                      c("age", "length"),
                      c(list(
                        data_source = 'iceland-aldist-AUT',
                        age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                 open_ended = "upper"),
                        length = mfdb_interval("len", 
                                               seq(minlen, maxlen, by = dl),
                                               open_ended = c("upper","lower"))),
                        defaults[[i]]))
  
  aldist.aut_tmp[[1]]$age <- paste0('age', gsub('age', '', aldist.aut_tmp[[1]]$age) %>% as.integer())
  aldist.aut <- c(aldist.aut, aldist.aut_tmp)
  
  ## COMMERCIAL
  ldist.sea_tmp <- 
    mfdb_sample_count(mdb, 
                      c("age", "length"),
                      c(list(
                        data_source = 'iceland-ldist-SEA',
                        age = mfdb_interval("all",c(minage,maxage),
                                            open_ended = c("upper","lower")),
                        length = mfdb_interval("len",
                                               seq(minlen, maxlen, by = dl),
                                               open_ended = c("upper","lower"))),
                        defaults[[i]]))
  
  ldist.sea <- c(ldist.sea, ldist.sea_tmp)
  
  aldist.sea_tmp <-
    mfdb_sample_count(mdb, 
                      c("age", "length"),
                      c(list(
                        data_source = 'iceland-aldist-SEA',
                        age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                 open_ended = "upper"),
                        length = mfdb_interval("len", 
                                               seq(minlen, maxlen, by = dl),
                                               open_ended = c("upper","lower"))),
                        defaults[[i]]))
  
  aldist.sea <- c(aldist.sea, aldist.sea_tmp)
  
  
}

rm(aldist.aut_tmp, ldist.aut_tmp,
   aldist.igfs_tmp, ldist.igfs_tmp,
   aldist.sea_tmp, ldist.sea_tmp)

if (length(defaults) > 1){
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

