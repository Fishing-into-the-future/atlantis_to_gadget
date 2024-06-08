################################################################################
##
## RUNNER TO CALCULATE THE SKILL FOR A NUMBER OF MODELS
##
################################################################################

library(tidyverse)
library(gadgetutils)

theme_set(theme_bw())

source('src/functions_skill.R')

## LOAD ATLANTIS AND GADGET FITS
load('03-analyses/data/atlout.Rdata')
load('03-analyses/data/gadgout.Rdata')

## -----------------------------------------------------------------------------
## Calculate skill

out <- NULL
for (i in names(gadgout)){
  
  atl.tmp <- atlout[[i]] %>% filter(year < 2011)
  skl.tmp <- do.call('rbind', 
                     lapply(split(gadgout[[i]], gadgout[[i]]$idsc), function(x, atl){
                       return(calc_skill(atl$value, x$value, type = 'fit', id = unique(x$idsc)))
                      }, atl = atl.tmp)
                     )
  skl.tmp$var <- i
  skl.tmp <- skl.tmp %>% separate(col = id, into = c('id', 'Scenario'), sep = '-')
  out <- rbind(out, skl.tmp)
  
}

rmse_plot <- 
  out %>% 
  filter(measure %in% c('rmse')) %>% 
  ggplot(aes(var, value, fill = Scenario)) + 
  geom_boxplot() + 
  labs(x = 'Variable', y = 'RMSE (kt)') + 
  scale_x_discrete(labels = c('Reference biomass', 'Total biomass', 'Recruitment'))
  

cor_plot <- 
  out %>% 
  filter(measure == 'cor') %>% 
  ggplot(aes(var, value, fill = Scenario)) + 
  geom_boxplot() + 
  labs(x = 'Variable', y = 'Correlation') + 
  scale_x_discrete(labels = c('Reference biomass', 'Total biomass', 'Recruitment'))

cowplot::plot_grid(rmse_plot, cor_plot, nrow = 2)
