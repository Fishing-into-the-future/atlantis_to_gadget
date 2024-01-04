sample_fish_box <- function(dat, scale, sample = TRUE) {
  
  #TODO: parameterize effN to vary with time period
  if(sample){
    #sum over layers
    dat2 <- aggregate(list(numAtAge = dat$atoutput), list(species = dat$species,
                                                          agecl = dat$agecl,
                                                          polygon = dat$polygon,
                                                          time = dat$time), sum)
    dat2 <- merge(dat2, scale, by = c('species','polygon','time'))
    
    ## Create id for applying
    dat2$id <- paste(dat2$species, dat2$polygon, dat2$time)
    # unique(table(dat2$id)) == length(unique(dat2$agecl))
    
    ## Loop over ids
    out <- do.call('rbind',
                   lapply(split(dat2, dat2$id), function(x){
                     
                     nn <- unique(x$effN)
                     totalNums <- sum(x$numAtAge)
                     
                     if(nn > totalNums) {
                       nn <- totalNums
                       message("effN is greater than total numbers available, so nEff set equal to ", nn," for species ",sp," and time ",y,"\n")
                     }
                     probs <- matrix(x$numAtAge,nrow=1)
                     
                     if(nn > 0){
                       x$numAtAgeSamp <- rmultinom(1,nn,probs)[,1]
                     } else { # sample is 0 if probs vector all 0s, no fish that year
                       x$numAtAgeSamp <- rep(0, length(probs))
                       message("total numAtAge ", nn,", assigning 0 sample for species ",sp," and time ",y,"\n")
                     }
                     return(x[,c('species','agecl','polygon','time',"numAtAgeSamp")])
                     }))
  }else{
    out <- aggregate(dat$medatoutput,list(dat$species,dat$agecl,dat$polygon,dat$time),median)
    names(out) <- c("species","agecl","polygon","time","medatoutput")
    out$numAtAgeSamp <- out$medatoutput
  }
  
  return(data.frame(
    species = out$species,
    agecl = out$agecl,
    polygon = out$polygon,
    layer = NA,
    time = out$time,
    atoutput = out$numAtAgeSamp
  ))
}