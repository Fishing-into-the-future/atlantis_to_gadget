

yoy_numbers <- function(omlist_ss){
  ## Modified from calc_Z function in atlantisom package
  k_wetdry <- omlist_ss$biol$kgw2d  / 1000000000
  nitro <- omlist_ss$biol$kwsr[omlist_ss$biol$kwsr[,1] == 'FCD', 2] + 
    omlist_ss$biol$kwrr[omlist_ss$biol$kwrr[,1] == 'FCD', 2]
  
  out <- 
    omlist_ss$YOY_ss %>% 
    mutate(year = round(Time/365,0) + 1947,
           n = FCD.0/nitro,
           n = n/k_wetdry/omlist_ss$biol$redfieldcn) %>% 
    select(-c(Time, FCD.0))
  
  return(out)
  
}
 

## Function to convert atlantis time to year and step (taken from Atlantis truth object)
atl_time_convert <- function(dat, origin = 1948){
  return(
    dat %>% 
      mutate(simyear = ceiling(time*73/365),
             year = origin-1+simyear,
             doy = time*73-(simyear-1)*365,
             step = case_when(doy == 73 ~ 1,
                              doy == 73*2 ~ 2,
                              doy == 73*3 ~ 3,
                              doy == 73*4 ~ 4,
                              .default = 5)) #%>% 
    # select(-c(simyear, doy))
  )
}

