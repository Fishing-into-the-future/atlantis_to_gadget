## Useful constansts

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'IGFS',
         !is.na(weight),
         length>0,
         weight>0) %>% 
  select(length,weight) %>% 
  collect(n=Inf) %>% 
  lm(log(weight/1e3)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants$estimate[1] <- exp(lw.constants$estimate[1])

## initial conditions sigma
init.sigma <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == local(defaults$species),age >0,!is.na(length))  %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))

## Initial coefficients for sd
init.sigma.coef <- 
  init.sigma %>% 
  filter(age > gadget3:::stock_definition(imm_stock, 'minage') & 
           age < gadget3:::stock_definition(mat_stock, 'maxage')) %>% 
  lm(I(ms/ml)~I(1/age) + age, data = .) %>% 
  coefficients() %>% 
  setNames(c('alpha', 'beta', 'gamma'))

## initial guess for the maturity ogive:
mat.l50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'IGFS',
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  ungroup() %>% 
  filter(maturity_stage=='2',p>0.50,length>25) %>% 
  dplyr::summarise(l50=min(length)) %>% 
  collect(n=Inf)

mat.a50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'IGFS',
         !is.na(maturity_stage),
         year > 1998,
         !is.na(age)) %>% 
  select(age,maturity_stage) %>% 
  group_by(age,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  group_by(age) %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  ungroup() %>% 
  filter(maturity_stage=='2',p>0.50) %>% 
  dplyr::summarise(a50=min(age)) %>% 
  collect(n=Inf)

if (TRUE){
  save(lw.constants, mat.l50, init.sigma, init.sigma.coef, mat.a50,
       file = file.path(base_dir, 'data', 'init_param.Rdata'))
}
