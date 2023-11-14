sample_ages <- function(dat,prop,ageErr=NULL) {
  
  #how will a max age be defined for each species. Is this just the max agecl, or will it be something different after appling stage2age
  
  #assumes that input is aggregated over the necessary columns (box, layer)
  
  #need to use output from sample_fish
  
  dat$numAtAgeSamp <- dat$ageComp <- NA
  
  out <- data.frame(species = NULL,
                    age = NULL,
                    time = NULL,
                    propAtAge = NULL)
  
  species <- unique(dat$species)
  for(sp in species) {
    #set up ageing error if defined as none
    if(!(sp%in%names(ageErr))) {
      maxAge <- max(dat$agecl[dat$species==sp])
      ageErr[[sp]] <- diag(x=1, nrow=maxAge, ncol=maxAge)
    }
    
    #do sampling for each species
    pp <- prop[prop$species==sp,"prop"]
    for(y in unique(dat$time)) {
      ind <- dat$species == sp & dat$time == y
      
      totalNums <- sum(dat[ind,]$atoutput)
      nn <- pp * totalNums
     
      #Ageing Error
      ageingError <- ageErr[[sp]]
      
      #add in missing ages
      dat2 <- merge(dat[ind,],data.frame(agecl=1:max(dat[ind,"agecl"])),by="agecl",all=T)
      dat2$species <- sp
      dat2$time <- y
      dat2$atoutput[is.na(dat2$atoutput)] <- 0
      
      #want to actually sample from this so that it returns the exact same vector when percentage = 100
      #this could be expanded in the future to actually provide a sample of individual fish
      sampVec <- rep(dat2$agecl,times=dat2$atoutput)
      samp <- sample(sampVec, nn, replace=FALSE)
      sampTable <- table(samp)
      
      dat2$numAtAgeSamp <- sampTable[match(dat2$agecl, names(sampTable))]
      dat2$numAtAgeSamp[is.na(dat2$numAtAgeSamp)] <- 0
      #probs <- matrix(dat2$atoutput,nrow=1)
      #dat2$numAtAgeSamp <- (rmultinom(1,nn,probs)[,1]%*%ageingError)[1,]   #this is a quick way to apply ageing error. Could think of each row (true age) as a multinomial sample
      dat2$ageComp <- dat2$numAtAgeSamp/sum(dat2$numAtAgeSamp)
      
      dat3 <- data.frame(species = dat2$species,
                         age = dat2$agecl,
                         time = dat2$time,
                         numAtAgeSamp = as.numeric(dat2$numAtAgeSamp),
                         propAtAge = as.numeric(dat2$ageComp))
      
      out <- rbind(out,dat3)
    }
  }
  
  return(out)
}

## Re-write of atlantisom::sample_ages that enables age distributions to be sampled
## from length distributions. Means we don't need to re-run calc_len2age on sub-samples of age
## Not included the age-reading error part yet
sample_ages_isl <- function(dat, prop){
  
  sampling_fun <- function(dat, prop){
    
    ind <- sample(1:sum(dat$atoutput), prop*sum(dat$atoutput), replace = FALSE)
    ages <- rep(dat$agecl, dat$atoutput)[ind]
    
    ## Age-length data?
    if ('lower.bins' %in% names(dat)){
      samps <- paste(ages, 
                     rep(dat$lower.bins, dat$atoutput)[ind],
                     rep(dat$upper.bins, dat$atoutput)[ind], sep = '-')
      dat$id <- paste(dat$agecl, dat$lower.bins, dat$upper.bins, sep = '-')
    }else{
      samps <- ages
      dat$id <- dat$agecl
    }
    
    sampTable <- as.data.frame(table(id = samps), stringsAsFactors = FALSE, responseName = 'numAtAgeSamp')
    
    ## Merge in numbers
    out <- merge(dat, sampTable, by = 'id', all = TRUE)
    out$numAtAgeSamp[is.na(out$numAtAgeSamp)] <- 0
    
    return(out[, names(out) != 'id'])
    
  }
  
  ## Loop over species
  by_species <- lapply(split(dat, dat$species), function(sdat, prop){
    sp <- prop[prop$species == unique(sdat$species), 'prop']
    ## And time
    return(do.call('rbind', lapply(split(sdat, sdat$time), sampling_fun, sp)))
  }, prop = prop)
  
  out <- do.call('rbind', by_species)
  if ('lower.bins' %in% names(dat)) out <- out[order(out$species, out$time, out$lower.bins),]
  else  out <- out[order(out$species, out$time),]
  rownames(out) <- 1:nrow(out)
  return(out)
  
}




