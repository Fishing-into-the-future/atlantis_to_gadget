################################################################################
##
## Runner to load Atlantis truth and format for skill assessment
##
################################################################################

source('03-analyses/config.R')
source("src/functions_atl.R")

load("~/Atlantis/AtlantisIceland/v6610/Out/Outrun_truth.RData")
iceom_ss <- read_rds(file = "01-atlantis_to_mfdb/data/v16_Scv02_reps100/omlist_ss.rds")

atlout <- list(stock.std = 
                 atl_time_convert(result$biomass_ages) %>% 
                 filter(step == 5) %>% 
                 mutate(year = year + 1,
                        age = case_when(agecl > 11 ~ 12,
                                        .default = agecl)) %>% 
                 filter(year > 1959) %>% 
                 group_by(year, age) %>% 
                 summarise(value = sum(atoutput)*1e3, .groups = 'drop') %>%
                 mutate(model = 'Atlantis'),
               stock.n = 
                 atl_time_convert(result$nums) %>% 
                 filter(step == 5) %>% 
                 mutate(year = year + 1,
                        age = case_when(agecl > 11 ~ 12,
                                        .default = agecl)) %>% 
                 filter(year > 1959) %>% 
                 group_by(year, age) %>% 
                 summarise(value = sum(atoutput), .groups = 'drop') %>%
                 mutate(model = 'Atlantis'),
               total.biomass = 
                 atl_time_convert(result$biomass_ages) %>% 
                 filter(step == 5) %>% 
                 mutate(year = year + 1) %>% 
                 filter(year > 1959) %>% 
                 group_by(year) %>% 
                 summarise(value = sum(atoutput)*1e3, .groups = 'drop') %>% 
                 mutate(Model = 'Atlantis'),
               ref.biomass = 
                 atl_time_convert(result$biomass_ages) %>%
                 filter(step == 5) %>% 
                 mutate(year = year + 1) %>% 
                 filter(year > 1959, agecl >= refbio_minage) %>% 
                 group_by(year) %>% 
                 summarise(value = sum(atoutput)*1e3, .groups = 'drop') %>% 
                 mutate(Model = 'Atlantis'),
               recruitment = 
                 yoy_numbers(iceom_ss) %>% 
                 filter(year > 1959) %>% 
                 rename(value = n) %>% 
                 mutate(Model = 'Atlantis')
)

save(atlout, file = '03-analyses/data/atlout.Rdata')

rm(iceom_ss, result)

## Check 
if (FALSE){
  cowplot::plot_grid(
    atlout$total.biomass %>% ggplot(aes(year, biomass/1e6)) + geom_line() + ylab('Total biomass (000t)'),
    atlout$ref.biomass %>% ggplot(aes(year, biomass/1e6)) + geom_line() + ylab('Ref. biomass (000t)'),
    atlout$recruitment %>% ggplot(aes(year, n/1e6)) + geom_bar(stat = 'identity') + ylab('Recruitment age 3 (millions)')
    )
}
  
