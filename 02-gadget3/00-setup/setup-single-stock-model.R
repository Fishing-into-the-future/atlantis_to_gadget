## -----------------------------------------------------------------------------
##
## Runner to set up stocks parameters and actions using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------
if (FALSE){
mdb <- mfdb::mfdb(dbname)

lw.constants <- list(a = simBiolPar$WLa, b = simBiolPar$WLb)

## Growth
tmp <- mfdb_sample_count(mdb, c('age','length','species'), list(
  #area          = areas,
  timestep      = defaults$timestep,
  year          = defaults$year,
  sampling_type = c('IGFS', 'AUT'),
  data_source   = 'atlantis_survey_aldists_rep1',
  species       = defaults$species,
  length        = mfdb_interval("len", seq(4, 150, 1)), 
  age           = mfdb_interval('age', seq(0, 12, 1))))[[1]] %>%
  left_join(sppListi %>% rename(species = mfdbSpp)) %>%
  mutate(len2 = as.numeric(substring(length,4,6)),
         age2 = as.numeric(substring(age,4,5)) + as.numeric(step)/4-1/(4*2)) # refine with the actual time of the survey and adj for ageCls
## age2 = as.numeric(substring(age,4,5)) + as.numeric(step)/12-1/(12*2)) # refine with the actual time of the survey and adj for ageCls

tmp <- tmp %>%
  filter(step == 4) %>% 
  group_by(year, area, species, age2) %>% 
  mutate(prop = number / sum(number))

library(nls.multstart)
grw.constants <- tmp %>%
  ungroup() %>%
  do(broom::tidy(nls_multstart(len2~Linf*(1-exp(-k*(age2-t0))),. ,
                               modelweights = prop, # needed because they are not individual data
                               start_lower=c(Linf=sppListi %>% .$maxLen * 0.8,
                                             k=0.1, t0=-2),
                               start_upper=c(Linf=sppListi %>% .$maxLen * 1.2,
                                             k=0.4, t0=0),
                               iter=500))) %>%
  .$estimate
grw.constants <- c(grw.constants, grw.constants[1]*(1-exp(-grw.constants[2] * ((sppListi$RecruitMonth/12-1/12/2)-grw.constants[3]))))
names(grw.constants) <- c("Linf","k","t0","recl")

ggplot() +
   geom_point(data=tmp %>% filter(year %in% c(1970, 1980, 1990)),
              aes(age2,len2,size=number)) +
   geom_line(data=data.frame(age = seq(1, 12, length.out=100)) %>%
                 mutate(len = grw.constants["Linf"]*(1-exp(-grw.constants["k"]*(age-grw.constants["t0"])))),
             aes(age,len), col=2) +
   geom_vline(xintercept = (sppListi$RecruitMonth/12-1/12/2), color = 3) +
   geom_hline(yintercept = grw.constants["recl"], color = 3) +
   facet_wrap(~year)


## initial num@age
init.num <- mfdb_sample_count(mdb, c('age'), list(
  area            = defaults$area,
  timestep        = defaults$timestep,
  year            = year_range,
  species         = defaults$species, 
  age             = mfdb_interval('age', seq(0, 16, 1)),
  sampling_type   = 'INIT',
  data_source     = 'atlantis_anumb_init'))[[1]] %>%
  mutate(age2 = as.numeric(substring(age,4,5))) %>%
  arrange(age2) %>%
  mutate(age2 = NULL)

## initial conditions
init.sigma <- mfdb_sample_meanlength_stddev(mdb, c('age','species'), list(
  area            = defaults$area,
  timestep        = defaults$timestep,
  year            = year_range,
  species         = defaults$species, 
  age             = mfdb_interval('age', seq(0, 16, 1)),
  sampling_type   = 'INIT',
  data_source     = 'atlantis_alnumb_init'))[[1]] %>%
  left_join(sppListi %>% rename(species = mfdbSpp)) %>%
  mutate(age2 = as.numeric(substring(age,4,5)) +
           (ifelse(ageGrpSize==1,0,
                   ifelse(ageGrpSize==2,1,
                          ifelse(ageGrpSize==4,2,NA))))) %>%
  arrange(age2) %>%
  mutate(age2 = NULL)


## initial recruitment
init.rec <- mfdb_sample_count(mdb, c('age'), list(
  area            = defaults$area,
  timestep        = defaults$timestep,
  year            = 1, # avgRec stored in year1
  species         = defaults$species, 
  age             = mfdb_interval('age', seq(0, 16, 1)),
  sampling_type   = 'INIT',
  data_source     = 'atlantis_logrec_avg'))[[1]]

## Z age0 ----> Z = (log(N0)-log(N1))/dt
z0 <- (log(exp(init.rec$number)*1e6) - log(init.num %>% filter(age=="age1") %>% .$number))/(1-(sppListi$SpawnMonth/12-1/12/2))

## M vector (based on an adaptation of Lorenzen eq, see Powers 2014 https://academic.oup.com/icesjms/article/71/7/1629/664136)
ageVec <- 2 : 16
Minf <- exp(1.46-1.01*log(ageVec[length(ageVec)])) # from Hoenig 1983 for fish ln(Z)=1.46-1.01*ln(tmax)
Ma <- data.frame(age = ageVec,
                 M = Minf*(1-exp(-grw.constants["k"]*(ageVec-grw.constants["t0"])))^(-lw.constants$b[1]*0.305),
                 Mc = grw.constants["k"]*(1-exp(-grw.constants["k"]*(ageVec-grw.constants["t0"])))^(-1.5)) %>%
  mutate(M = round(M,3), Mc = round(Mc, 3))
ggplot(Ma, aes(age,M)) + geom_point() + geom_line() + ylim(0,NA)

mfdb::mfdb_disconnect(mdb)

}
## Initial numbers
# One parameter per age group per stock
#initvonb <- gadget3::g3a_renewal_vonb(recage = gadget3::g3_stock_def(single_stock, 'minage'), by_stock = stocks)
initvonb <- g3a_renewal_vonb_t0(K = g3_parameterized("K", by_stock = TRUE),
                                t0 = g3_parameterized('t0', by_stock = TRUE))
# Use a CV for initial sds
initsd <- gadget3:::f_substitute(~x * y, list(x = gadget3::g3_parameterized('lencv', optimise = FALSE),
                                              y = initvonb))


naturalmortality <- g3_parameterized('M', by_stock = TRUE, by_age = TRUE, 
                                     optimise = FALSE, random = FALSE, 
                                     offset = g3_parameterized(paste0(single_stock$name, '_M_offset'), 
                                                               optimise = estimateM, 
                                                               random = random_M,
                                                               value = 0))


## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

stock_actions <- 
  list(
    ## Initial conditions
    g3a_initialconditions_normalparam(single_stock,
                                      factor_f = g3a_renewal_initabund(M = naturalmortality),
                                      mean_f = initvonb,
                                      stddev_f = initsd),
    ## Natural mortality
    gadget3::g3a_naturalmortality(single_stock, gadget3::g3a_naturalmortality_exp(param_f = naturalmortality)),
    ## Ageing
    gadget3::g3a_age(single_stock, run_f = ~cur_step == 2),
    ## Growth and maturity
    gadget3::g3a_growmature(single_stock,
                            ## Growth
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(by_stock = stocks),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(by_stock = stocks),
                              maxlengthgroupgrowth = maxlengthgroupgrowth,
                              by_stock = stocks 
                            )
                            ),
    # Renewal
    gadget3::g3a_renewal_normalparam(single_stock, mean_f = initvonb, run_step = 2),
    
    list()
    
  )
