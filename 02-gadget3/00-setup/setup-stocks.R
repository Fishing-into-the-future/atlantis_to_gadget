## -----------------------------------------------------------------------------
##
## Runner to set up stocks and associated actions
##
## -----------------------------------------------------------------------------

if (single_stock_model){
  single_stock <- 
    g3_stock(c(species = 'cod'), seq(4, 150, 1)) |>
    g3s_livesonareas(areas[c('1')]) |>
    g3s_age(minage = 0, maxage = 12)
  
  stocks <- list(single_stock)
    
}else{
  
  imm_stock <-
    g3_stock(c(species = 'cod', 'imm'), seq(4, 150, 1)) %>%
    g3s_livesonareas(areas[c('1')]) %>%
    g3s_age(minage = 1, maxage = 10)
  
  mat_stock <-
    g3_stock(c(species = 'cod', 'mat'), seq(4, 150, 1)) %>%
    g3s_livesonareas(areas[c('1')]) %>%
    g3s_age(minage = 3, maxage = 16)
  
  ## Stock list
  stocks <- list(imm_stock, mat_stock)
  
}

