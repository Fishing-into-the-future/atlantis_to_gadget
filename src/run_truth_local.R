
om_init <- function(config = configfile, d.name){
  
  # Where are the atlantis output files? Consider filling with shiny app in future
  source(config)
  # needs these files, for example config file CC3config.R is:
  
  # d.name <- here::here("atlantisoutput","CC_2063_OA_OFF_22")
  # functional.groups.file <- "CalCurrentV3Groups.csv"
  # biomass.pools.file <- "DIVCalCurrentV3_Biol.nc"
  # biol.prm.file <- "CalCurrentV3_Biol.prm"
  # box.file <- "CalCurrentV3_utm.bgm"
  # initial.conditions.file <- "DIVCalCurrentV3_Biol.nc"
  # run.prm.file <- "CalCurrentV3_run.xml"
  # scenario.name <- "CCV3"
  # bioind.file <- "outputCCV3BiomIndx.txt"
  # catch.file <- "outputCCV3Catch.txt"
  # annage <- FALSE
  # fisheries.file <- "CalCurrentV3Fisheries.csv"
  
  #Load functional groups
  funct.groups <- atlantisom::load_fgs(dir=d.name,
                                       file_fgs = functional.groups.file)
  #Get just the names of active functional groups
  funct.group.names <- 
    funct.groups %>% 
    filter(IsTurnedOn == 1, (Name %in% local(species_ss))) %>% # HACK FOR NOW GroupType == 'FISH') %>%
    select(Name) %>%
    .$Name
  
  # load true total biomass in tons
  truetotbio <- atlantisom::load_bioind(d.name, file_bioind = bioind.file, fgs = funct.groups)
  
  # load true catch in tons
  truecatchbio <- atlantisom::load_catch(d.name, file_catch = catch.file, fgs = funct.groups)
  
  # load YOY
  YOY <- atlantisom::load_yoy(d.name, paste0(scenario.name, "YOY.txt"))
  
  # load biol_prm
  biol <- load_biolprm(d.name, biol.prm.file)
  
  # load run_prm
  runpar <- atlantisom::load_runprm(d.name, run.prm.file)
  
  # load box info file
  boxpars <- atlantisom::load_box(d.name, box.file)
  boxall <- c(0:(boxpars$nbox - 1))
  
  # default run_truth setup will save the file, so check for that first
  if(!file.exists(file.path(d.name,
                            paste0(scenario.name, "run_truth.RData")))){
    #Store all loaded results into an R object
    truth <- run_truth(scenario = scenario.name,
                       dir = d.name,
                       file_fgs = functional.groups.file,
                       file_bgm = box.file,
                       select_groups = funct.group.names,
                       file_init = initial.conditions.file,
                       file_biolprm = biol.prm.file,
                       file_runprm = run.prm.file,
                       verbose = TRUE,
                       annage = annage,
                       file_fish = fisheries.file
                       )
  } else {
    truth <- get(load(file.path(d.name,
                                paste0(scenario.name, "run_truth.RData"))))
  }
  
  omlist <-list("funct.groups" = funct.groups,
                "funct.group.names" = funct.group.names,
                "truetotbio" = truetotbio,
                "truecatchbio" = truecatchbio,
                "YOY" = YOY,
                "biol" = biol,
                "runpar" = runpar,
                "boxpars" = boxpars,
                "boxall" = boxall, 
                "truth" = truth)
  
  return(omlist)
}


run_truth <- function(scenario, dir = getwd(),
  file_fgs, file_bgm, select_groups, file_init, file_biolprm, file_runprm,
  file_fish, verbose = FALSE, save = TRUE, annage = FALSE){

  # Read in information
  # Read in the functional groups csv since that is used by many functions
  fgs <- load_fgs(dir = dir, file_fgs = file_fgs)
  # Read in the biomass pools
  bps <- load_bps(dir = dir, fgs = file_fgs, file_init = file_init)
  # Read in the biological parameters
  biol <- load_biolprm(dir = dir, file_biolprm = file_biolprm)
  # Read in the run parameters
  runprm <- load_runprm(dir = dir, file_runprm = file_runprm)

  nc_catch <- paste0(scenario, 'CATCH.nc')
  dietcheck <- paste0(scenario, 'DietCheck.txt')
  nc_out <- paste0(scenario, ".nc")
  nc_prod <- paste0(scenario, "PROD.nc")
  file_catchfish <- file.path(dir,
    paste0(scenario, "CatchPerFishery.txt"))
  file_catch <- paste0(scenario, "Catch.txt")

  if(annage){
    if(!file.exists(paste0(file.path(dir,paste0(scenario, 'ANNAGEBIO.nc'))))){
      stop("ANNAGEBIO.nc file not found")
    }
    if(!file.exists(paste0(file.path(dir,paste0(scenario, 'ANNAGECATCH.nc'))))){
      stop("ANNAGECATCH.nc file not found")
    }
    nc_annagebio <- paste0(scenario, 'ANNAGEBIO.nc')
    nc_annagecatch <- paste0(scenario, 'ANNAGECATCH.nc')
  }

  # Get the boundary boxes
  allboxes <- load_box(dir = dir, file_bgm = file_bgm)
  boxes <- get_boundary(allboxes)

  #Extract from NetCDF files
  # Need: dir, file_nc, bps, fgs, select_groups, select_variable,
  # check_acronyms, bboxes
  nums <- load_nc_cohort(dir = dir,
                         file_nc = nc_out,
                         bps = bps,
                         fgs = fgs,
                         biolprm = biol,
                         select_groups = select_groups,
                         select_variable = "Nums",
                         check_acronyms = TRUE,
                         bboxes = boxes)
  if(verbose) message("Numbers read in.")

  resn <- load_nc_cohort(dir = dir,
                         file_nc = nc_out,
                         bps = bps,
                         fgs = fgs,
                         biolprm = biol,
                         select_groups = select_groups,
                         select_variable = "ResN",
                         check_acronyms = TRUE,
                         bboxes = boxes)
  if(verbose) message("Reserve nitrogen read in.")

  structn <- load_nc_cohort(dir = dir,
                            file_nc = nc_out,
                            bps = bps,
                            fgs = fgs,
                            biolprm = biol,
                            select_groups = select_groups,
                            select_variable = "StructN",
                            check_acronyms = TRUE,
                            bboxes = boxes)
  if(verbose) message("Structural nitrogen read in.")

  eat <- load_nc_cohort(dir = dir,
                     file_nc = nc_prod,
                     bps = bps,
                     fgs = fgs,
                     biolprm = biol,
                     select_groups = select_groups,
                     select_variable = "Eat",
                     check_acronyms = TRUE,
                     bboxes = boxes)
  if(verbose) message("Eaten read in.")

  grazing <- load_nc_cohort(dir = dir,
                 file_nc = nc_prod,
                 bps = bps,
                 fgs = fgs,
                 biolprm = biol,
                 select_groups = select_groups,
                 select_variable = "Grazing",
                 check_acronyms = TRUE,
                 bboxes = boxes)
  if(verbose) message("Grazing read in.")

  vol <- load_nc_physics(dir = dir,
                         file_nc = nc_out,
                         physic_variables = "volume",
                         aggregate_layers = FALSE,
                         bboxes = boxes)
  if(verbose) message("Volume read in.")

  catch <- load_nc_cohort(dir = dir,
                 file_nc = nc_catch,
                 bps = bps,
                 fgs = fgs,
                 biolprm = biol,
                 select_groups = select_groups,
                 select_variable = "Catch",
                 check_acronyms = TRUE,
                 bboxes = boxes)
  if(verbose) message("Catch read in.")
  
  if(annage){
    numsage <- load_nc_annage(dir = dir,
                              file_nc = nc_annagebio,
                              bps = bps,
                              fgs = fgs,
                              biolprm = biol,
                              select_groups = select_groups,
                              select_variable = "Nums",
                              check_acronyms = TRUE,
                              bboxes = boxes,
                              verbose = TRUE)
    if(verbose) message("Numbers read in from ANNAGEBIO.")

    # Weight output seems wrong compared with standard nc weights
    # Don't include until we can sort this out
    # weightage <- load_nc_annage(dir = dir,
    #                             file_nc = nc_annagebio,
    #                             bps = bps,
    #                             fgs = fgs,
    #                             biolprm = biol,
    #                             select_groups = select_groups,
    #                             select_variable = "Weight",
    #                             check_acronyms = TRUE,
    #                             bboxes = boxes,
    #                             verbose = TRUE)
    # if(verbose) message("Weight read in from ANNAGEBIO.")

    catchage <- load_nc_annage(dir = dir,
                              file_nc = nc_annagecatch,
                              file_fish = file_fish,
                              bps = bps,
                              fgs = fgs,
                              biolprm = biol,
                              select_groups = select_groups,
                              select_variable = "Catch",
                              check_acronyms = TRUE,
                              bboxes = boxes,
                              verbose = TRUE)
    if(verbose) message("Catch read in from ANNAGECATCH.")

    discage <- load_nc_annage(dir = dir,
                               file_nc = nc_annagecatch,
                               file_fish = file_fish,
                               bps = bps,
                               fgs = fgs,
                               biolprm = biol,
                               select_groups = select_groups,
                               select_variable = "Discard",
                               check_acronyms = TRUE,
                               bboxes = boxes,
                               verbose = TRUE)
    if(verbose) message("Discard read in from ANNAGECATCH.")

  }

  # May 2019 this is the catch in nums correction needed for legacy atlantis codebases

  # check for logfile, send warning if not found.
  if(file.exists(paste0(file.path(dir, "log.txt")))){
    #if found compare codedate and do catch numbers correction if necessary
    logfile <- paste0(file.path(dir, "log.txt"))
    codedate <- system(paste0("grep 'Atlantis SVN' ", logfile), intern = TRUE)
    codedate <- as.Date(stringr::str_extract(codedate, "\\d+[- \\/.]\\d+[- \\/.]\\d+"))
    if(codedate < "2015-12-15"){
      if(verbose) message("Catch numbers correction needed for this codebase, starting")
      # read in initial conditions NC file
      at_init <- RNetCDF::open.nc(con = file.path(dir, file_init))

      # Get info from netcdf file! (Filestructure and all variable names)
      var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_init)$nvars - 1),
                               function(x) RNetCDF::var.inq.nc(at_init, x)$name)
      numlayers <- RNetCDF::var.get.nc(ncfile = at_init, variable = "numlayers")

      RNetCDF::close.nc(at_init)

      # are these in box order??? if so make a box-numlayer lookup
      layerbox.lookup <- data.frame(polygon=c(0:(allboxes$nbox - 1)), numlayers)

      catch.tmp <- merge(catch, layerbox.lookup)

      # divide the numbers at age by (86400 * number_water_column_layers_in_the_box)
      # replace truth$catch atoutput with correction
      catch <- catch.tmp %>%
        mutate(atoutput = atoutput / (86400 * numlayers)) %>%
        select(species, agecl, polygon, time, atoutput)
      if(verbose) message("Catch numbers corrected")
    }else{
      message("Codebase later than December 2015, no correction needed")
    }
  }else{
    warning(strwrap(prefix = " ", initial = "",
                    "log.txt file not found; catch in numbers correction not done. For Atlantis SVN dates prior to December 2015, CATCH.nc output units were incorrect. Correction requires presence of log.txt file in the directory."))
  }
  catchfish <- read.table(file_catchfish, header = TRUE)
  over <- colnames(catchfish)[-(1:2)]
  catchfish <- reshape(catchfish, direction = "long",
    varying = over, v.names = "catch",
    timevar = "species", times = over)
  rownames(catchfish) <- 1:NROW(catchfish)
  catchfish <- catchfish[catchfish$catch > 0,
    -which(colnames(catchfish) == "id")]
  catchfish$species <- fgs$Name[match(catchfish$species, fgs$Code)]
  colnames(catchfish) <- tolower(colnames(catchfish))
  catchfish$time <- catchfish$time / runprm$toutfinc
  if(verbose) message("Catch per fishery read in.")

  # Get catch from txt. Sum per species and compare with values from nc-file!
  catch_all <- load_catch(dir = dir, file = file_catch, fgs = fgs)
  # over <- colnames(catch_all)[(colnames(catch_all) %in% fgs$Code)]
  # catch_all <- reshape(catch_all[, c("Time", over)], direction = "long",
  #   varying = over, v.names = "catch",
  #   timevar = "species", times = over)
  # rownames(catch_all) <- 1:NROW(catch_all)
  # catch_all <- catch_all[catch_all$catch > 0,
  #   -which(colnames(catch_all) == "id")]
  # catch_all$species <- fgs$Name[match(catch_all$species, fgs$Code)]
  # colnames(catch_all) <- tolower(colnames(catch_all))
  catch_all$time <- catch_all$time / runprm$toutfinc
  if(verbose) message("Catch for all fisheries in biomass read in.")

  diet <- load_diet_comp(dir = dir, file_diet = dietcheck, fgs = fgs)
  diet <- diet[diet$atoutput>0,]
  if(verbose) message("Global diet composition (proportion) read in.")

  # May 2019 let's not do the catch calcs until they are corrected
  # Sept 2020 nor the biomass_eaten

  if(verbose) message("Start calc_functions")
  # catchbio <- calc_biomass_age(nums = catch,
  #   resn = resn, structn = structn, biolprm = biol)
  biomass_eaten <- calc_pred_cons(eat = eat,
     grazing = grazing, vol = vol, biolprm = biol,
     runprm = runprm)
  biomass_ages <- calc_biomass_age(nums = nums,
    resn = resn, structn = structn, biolprm = biol)
  # bio_catch <- calc_biomass_age(nums = catch,
  #   resn = resn, structn = structn, biolprm = biol)
  #
  # bio_catch <- aggregate(atoutput ~ species + time,
  #   data = bio_catch, sum)

  # todo: check that the biomass of the catches are correct
  # also should catch in biomass be exported as well
  # as catch in numbers?
  # check <- merge(catch_all, bio_catch,
  #   by = c("species", "time"))
  # check$check <- with(check, atoutput / catch)

  # SKG May 2019, no export of catch in biomass for now
  # does not match catch.txt output file
  # read that in separately instead

  # SKG Sept 2020, no export of biomass_eaten
  # does not match DetailedDietCheck.txt output
  # polygon-specific consumption will be from DetailedDietCheck
  # files are too big to combine; do separately from run_truth
  # export model-wide true diet comp though

  # SKG June 2022, biomass_eaten is now total consumption
  # based on the new calc_pred_cons() which gives results
  # on a more reasonable scale, adding back to output object

  if(!annage){
    result <- list("biomass_ages" = biomass_ages,
                   "biomass_eaten" = biomass_eaten,
                   "catch" = catch,
                   "catch_all" = catch_all,
                   "nums" = nums,
                   "resn" = resn,
                   "structn" = structn,
                   "diet" = diet,
                   "biolprm" = biol,
                   "fgs" = fgs)
  }

  if(annage){
    result <- list("biomass_ages" = biomass_ages,
                   "biomass_eaten" = biomass_eaten,
                   "catch" = catch,
                   "catch_all" = catch_all,
                   "nums" = nums,
                   "numsage" = numsage,
                   "catchage" = catchage,
                   "discage" = discage,
                   "resn" = resn,
                   "structn" = structn,
                   "diet" = diet,
                   "biolprm" = biol,
                   "fgs" = fgs)
  }

  if(verbose) message("Start writing to HDD.")
  if(save) {
    save(result,
      file = file.path(dir, paste0(scenario, "run_truth.RData")))
  }

  invisible(result)
}
