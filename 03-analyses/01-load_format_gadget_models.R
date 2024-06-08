################################################################################
##
## Runner to load and format Gadget models for skill assessment
##
################################################################################

library(tidyverse)
library(gadgetutils)

source('src/functions_gadg.R')
source('03-analyses/config.R')

## Model 1: Baseline
vers1 <- '37_baseline_lengroup_cv01'
load(file = file.path('02-gadget3', 'models', vers, 'fit.Rdata'))
load(file = file.path('02-gadget3', 'models', vers, 'BOOTSTRAP', 'boot_fit.Rdata'))
boot_fit <- boot_fit %>% discard(~class(.)[[1]]=='try-error') 
## Bootstraps to remove
badboots <- NULL
#fits <- lapply(c(list(baseline = fit), remove_nonconverged(boot_fit)), FUN = g3_fix_recruitment)
fits <- lapply(c(list(baseline = fit), boot_fit), FUN = g3_fix_recruitment) 
rm(fit, boot_fit)

## Model 2: no IGFS
vers2 <- '34_baseline_effic0.5_lengroup_cv0.2_noigfs'
load(file = file.path('02-gadget3', 'models', vers, 'fit.Rdata'))
load(file = file.path('02-gadget3', 'models', vers, 'BOOTSTRAP', 'boot_fit.Rdata'))
boot_fit <- boot_fit %>% discard(~class(.)[[1]]=='try-error') 
## Bootstraps to remove
badboots <- NULL
#fits <- lapply(c(list(baseline = fit), remove_nonconverged(boot_fit)), FUN = g3_fix_recruitment)
fits_noigfs <- lapply(c(list(baseline = fit), boot_fit), FUN = g3_fix_recruitment) 
rm(fit, boot_fit)

## Collate fits together
allbio <- 
  bind_fit_components(fits, 'stock.std') %>% 
  mutate(Scenario = vers1) %>% 
  bind_rows(
    bind_fit_components(fits_noigfs, 'stock.std') %>% 
      mutate(Scenario = vers2) 
    )

allrec <- 
  bind_fit_components(fits, 'stock.recruitment') %>% 
  mutate(Scenario = vers1) %>% 
  bind_rows(
    bind_fit_components(fits_noigfs, 'stock.recruitment') %>% 
      mutate(Scenario = vers2) 
  )



gadgout <- list(total.biomass = 
                  allbio %>% 
                  filter(age > 0, step == 1) %>% 
                  mutate(biomass = number * mean_weight) %>% 
                  group_by(year, id, Scenario) %>% 
                  summarise(value = sum(biomass), .groups = 'drop') %>% 
                  mutate(idsc = paste(id, Scenario, sep = '-')),
                ref.biomass = 
                  allbio %>% 
                  filter(age > refbio_minage, step == 1) %>% 
                  mutate(biomass = number * mean_weight) %>% 
                  group_by(year, id, Scenario) %>% 
                  summarise(value = sum(biomass), .groups = 'drop') %>% 
                  mutate(idsc = paste(id, Scenario, sep = '-')),
                recruitment = 
                  allrec %>%
                  rename(value = recruitment) %>% 
                  mutate(idsc = paste(id, Scenario, sep = '-')))

save(gadgout, file = '03-analyses/data/gadgout.Rdata')
