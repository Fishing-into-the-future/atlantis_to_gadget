create_polygon_scale <- function(omlist_ss, survey, effN, boxes){
  
  if (survey){
    out <- 
      omlist_ss$truenums_ss %>% 
      filter(polygon %in% boxes) %>% 
      group_by(species, polygon, time) %>% 
      summarise(numbers = sum(atoutput), .groups = 'drop') %>% 
      left_join(
        do.call('rbind', lapply(omlist_ss$boxpars$boxes, function(x){
          return(data.frame(polygon = as.numeric(gsub('Box', '', x$label)), area = x$area))
        }))
      ) %>% 
      mutate(density = numbers/area) %>% 
      group_by(time) %>% 
      mutate(scale = density/sum(density)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(effN, by = c('species')) %>% 
      dplyr::mutate(effN = round(effN * scale, 0)) %>% 
      dplyr::select(species, time, polygon, effN) %>% 
      dplyr::filter(time > 0)  
      
  }else{
    ## Trying to sample per polygon
    ## For commercial data, we'll split effN between polygons using total catch per polygon per timestep
    out <- 
      omlist_ss$truecatchtons_ss %>% 
      dplyr::filter(polygon %in% boxes) %>% 
      dplyr::group_by(species, time, polygon) %>% 
      dplyr::summarise(catch = sum(atoutput), .groups = 'drop') %>% 
      dplyr::group_by(species, time) %>% 
      dplyr::mutate(scale = catch/sum(catch)) %>% 
      dplyr::ungroup() %>% 
      dplyr::rename(code = species) %>% 
      dplyr::left_join(data.frame(species = omlist_ss$species_ss, code = omlist_ss$code_ss), by = 'code') %>% 
      dplyr::left_join(effN, by = 'species') %>% 
      dplyr::mutate(effN = round(effN * scale, 0)) %>% 
      dplyr::select(species, time, polygon, effN) %>% 
      dplyr::filter(time > 0)  
  }
  if (any(duplicated(out[,c('species','polygon','time')]))) stop('The scale is duplicated')
  
  return(out)
}
