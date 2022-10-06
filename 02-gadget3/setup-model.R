## -----------------------------------------------------------------------------
##
## Runner to set up stocks and associated actions
##
## -----------------------------------------------------------------------------

## Modelled stocks
imm_stock <-
  g3_stock(c(species = 'cod', 'imm'), seq(4, 150, 2)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 1, maxage = 10)

mat_stock <-
  g3_stock(c(species = 'cod', 'mat'), seq(4, 150, 2)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 3, maxage = 16)

## Stock list
stocks <- list(imm_stock, mat_stock)

## Maximum number of length groups a stock can group within a time step (maxlengthgroupgrowth)
mlgg <- 10

## How do set up the initial population
## Options:
## 0 - population is initialised at equilibrium
## 1 - parameter for each age group (across stocks)
## 2 - parameter for each age group of each stock
initial_abund_mode <- 2

## setup stock actions
imm_actions <- model_actions(imm = imm_stock,
                             mat = mat_stock,
                             mlgg = mlgg,
                             mature = FALSE,
                             comp_id = 'species',
                             init_mode = initial_abund_mode,
                             exp_params = c(),
                             tv_params = c())

## Mature stock actions
mat_actions <- model_actions(imm = imm_stock,
                             mat = mat_stock,
                             mlgg = mlgg,
                             mature = TRUE,
                             comp_id = 'species',
                             init_mode = initial_abund_mode,
                             exp_params = c(),
                             tv_params = c())

## Combine stock actions
stock_actions <- c(imm_actions, mat_actions)
