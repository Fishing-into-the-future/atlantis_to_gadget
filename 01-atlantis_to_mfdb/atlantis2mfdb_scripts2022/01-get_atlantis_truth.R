## -----------------------------------------------------------------------------
##
## Runner to get the truth from an Atlantis run
##
## -----------------------------------------------------------------------------

## Get the parameters to run the model
runpar <- load_runprm(atlantis_dir, run.prm.file)

# Model areas
boxpars <- load_box(atlantis_dir, box.file)
boxall <- c(0:(boxpars$nbox - 1))

if (TRUE){
  stock_truth <- run_truth(scenario = scenario.name,
                           dir = atlantis_dir,
                           file_fgs = functional.groups.file,
                           file_bgm = box.file,
                           select_groups = species_ss,
                           file_init = initial.conditions.file,
                           file_biolprm = biol.prm.file,
                           file_runprm = run.prm.file,
                           file_fish = fisheries.file,
                           verbose = TRUE)
  
  save(stock_truth,
       runpar,
       boxpars,
       boxall,
       file = file.path(atlantis_dir, atlantis_vers, paste0(atlantis_vers, '_TRUTH.Rdata')))
}else{
  load(file = file.path(atlantis_dir, atlantis_vers, 'Outrun_truth.RData'))
  stock_truth <- result
  rm(result)
}