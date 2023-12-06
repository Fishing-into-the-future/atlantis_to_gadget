## Useful constansts

## Weight-length parameters
lw.constants <- list(a = simBiolPar$WLa*1e-3, b = simBiolPar$WLb)

## Growth and mortality estimates
mdb <- mfdb::mfdb(dbname)

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

# age2 = as.numeric(substring(age,4,5)) + as.numeric(step)/12-1/(12*2)) # refine with the actual time of the survey and adj for ageCls
  
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
  year            = 1948, # avgRec stored in year1
  species         = defaults$species, 
  age             = mfdb_interval('age', seq(0, 16, 1)),
  sampling_type   = 'INIT',
  data_source     = 'atlantis_logrec_avg'))[[1]]

## Z age0 ----> Z = (log(N0)-log(N1))/dt
z0 <- (log(exp(init.rec$number)*1e6) - log(init.num %>% filter(age=="age1") %>% .$number))/(1-(sppListi$SpawnMonth/12-1/12/2))

## M vector (based on an adaptation of Lorenzen eq, see Powers 2014 https://academic.oup.com/icesjms/article/71/7/1629/664136)
ageVec <- 1 : 12
Minf <- exp(1.46-1.01*log(ageVec[length(ageVec)])) # from Hoenig 1983 for fish ln(Z)=1.46-1.01*ln(tmax)
Ma <- data.frame(age = ageVec,
                 M = Minf*(1-exp(-grw.constants["k"]*(ageVec-grw.constants["t0"])))^(-lw.constants$b[1]*0.305),
                 Mc = grw.constants["k"]*(1-exp(-grw.constants["k"]*(ageVec-grw.constants["t0"])))^(-1.5)) %>%
  mutate(M = round(M,3), Mc = round(Mc, 3))


ggplot(Ma, aes(age,M)) + geom_point() + geom_line() + ylim(0,NA)

mfdb::mfdb_disconnect(mdb)



