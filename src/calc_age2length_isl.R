calc_age2length_isl <- function(structn, resn, nums,
                            biolprm, fgs, maxbin = 150,
                            CVlenage = 0.1, remove.zeroes = TRUE) {
  
  ### Inputs required
  ### Ages (hard-wired right now for 10 cohorts per group). This can be modified.
  # calculate this for standard age classes, not annage output. leave hard-wired
  ages = 1:10
  
  ## Get group codes to calculate size comps for
  #groups <- as.factor(fgs$Name)
  groups <- unique(as.factor(structn$species)) # use nums instead of structn?
  
  times <- unique(structn$time) # use nums instead of structn?
  
  ### Length structure - making very fine (and appropriate for fish),
  # but presumably this could be something
  # that is pre-specified in the call to the function, and would likely change by species
  # Perhaps solution is to make number of length bins fixed across groups, but
  # change the upper limits for each length bin
  # SG changing to allow input by species or (default) single value
  if(length(maxbin)==1){
    #set maxbin for all species to the single input value
    maxbin <- data.frame(species = groups,
                         maxlenbin = rep(maxbin, length(groups))
    )
  }
  
  # CV length at age for each species is Needed to create the age-length key.
  # This could conceivably be passed to the function with vals for each species.
  # SG changing to allow single CV or by species
  if(length(CVlenage)==1){
    #set CVlenage for all species to the single input value
    CVlenage <- data.frame(species = groups,
                           cvlenage = rep(CVlenage, length(groups))
    )
  }
  
  # extract rows from structn and resn that match the rows in nums, which are only non-zeroes
  resn.ind <- with(resn,paste(species,'.',agecl,'.',polygon,'.',layer,'.',time,sep=""))
  num.ind <- with(nums,paste(species,'.',agecl,'.',polygon,'.',layer,'.',time,sep=""))
  pick <- match(num.ind,resn.ind)
  SRN <- resn$atoutput[pick] + structn$atoutput[pick]
  
  # get weight-length parameters
  li_a_use <- biolprm$wl[match(fgs$Code[match(nums$species,fgs$Name)],biolprm$wl[, 1]), 2]
  li_b_use <- biolprm$wl[match(fgs$Code[match(nums$species,fgs$Name)],biolprm$wl[, 1]), 3]
  
  #calc mean length and weight at age
  nums$mulen <- ((biolprm$kgw2d*biolprm$redfieldcn*SRN)/(1000*li_a_use))^(1/li_b_use)
  nums$muweight <- li_a_use*nums$mulen^li_b_use
  
  #
  # #calculate length comps
  #(numbers at length at max resolution - can then be collapsed to appropriate
  #spatial/temporal resolution later)
  lenfreq <- NULL
  
  # small effective sample sizes may return 0 nums for oldest age classes, resulting in NA
  # I'd rather keep that in here then change the match function in line 67 to return 0
  spmax <- nums %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(maxmulen = max(mulen, na.rm = TRUE)) %>%
    dplyr::left_join(maxbin)
  
  if(any(spmax$maxlenbin<spmax$maxmulen)){
    print("Warning: maximum bin size is smaller than the longest fish in the sample. Fish above the maximum bin size will be removed from length compositions.")
  }
  
  tmp <- lapply(split(nums, 1:nrow(nums)), function(x, maxbin, CVlenage){
    
    group <- x$species
    group <- factor(group, levels=levels(groups))
    igroup <- which(groups==group)
    box <- x$polygon
    layer <- x$nums$layer
    time <- x$nums$time
    age <- x$nums$agecl
    
    upper.bins <- 1:maxbin$maxlenbin[which(maxbin$species==group)]
    lower.bins <- c(0,upper.bins[-length(upper.bins)])
    
    sigma <- sqrt(log((CVlenage$cvlenage[which(CVlenage$species==group)]^2)+1))
    muuse <- log(x$mulen) - 0.5*(sigma^2)
    CumFracperbin <- plnorm(upper.bins,muuse,sigma)
    Fracperbin <- c(CumFracperbin[1],diff(CumFracperbin))
    natlength <- Fracperbin*x$atoutput
    results <- cbind(x,lower.bins,upper.bins, row.names = NULL) # add row.names = NULL?
    results$atoutput <- round(natlength,0)
    return(results)
    
  }, maxbin = maxbin, CVlenage = CVlenage)
  
  lenfreq <- do.call('rbind', tmp)
  
  #output. natlength now has additional columns because of need to store length bin info
  lenout <- NULL
  lenout$mulen <- dplyr::select(nums, species, agecl, polygon, layer, time, atoutput = mulen) 
  lenout$muweight <- dplyr::select(nums, species, agecl, polygon, layer, time, atoutput = muweight)
  
  #get rid of zero rows.
  if (remove.zeroes) lenfreq <- lenfreq[lenfreq$atoutput>0,]
  lenout$natlength <- dplyr::select(lenfreq, species, agecl, polygon, layer, time, atoutput, lower.bins, upper.bins)
  
  return(lenout)
  # END THE SIZE COMP FUNCTION HERE.
}
