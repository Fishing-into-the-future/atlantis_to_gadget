## To synchronise with Atlantis, recruitment happens at the beginning of the third step
## this means there is a 1 year lag in the fit obejct
g3_fix_recruitment <- function(fit){
  
  yrs <- sort(unique(fit$stock.recruitment$year))
  out <- NULL
  for (i in yrs){
    parname <- paste0('cod.rec.', i)
    tmp <- data.frame(year = i,
                      recruitment = fit$params$value$cod.rec.scalar * fit$params$value[[parname]] * 1e4)
    out <- rbind(out, tmp)
  }
  
  fit$stock.recruitment <- as_tibble(out)
  return(fit)
}

remove_nonconverged <- function(boot_fit){
  
  good_boots <- do.call('rbind', 
                       lapply(setNames(names(boot_fit), names(boot_fit)), function(x){ 
                         return(attr(boot_fit[[x]], 'summary') %>% mutate(id = x))
                        })) %>% 
    filter(convergence) %>% 
    pull(id)
  
  return(boot_fit[good_boots])
  
}

## -----------------------------------------------------------------------------
## Projection functions
## -----------------------------------------------------------------------------

proj_stock_actions <- function(num_project_years,
                               mat,
                               imm = NULL,
                               prop_ogive = NULL,
                               comp_id = 'species'){
  
  if (is.null(imm)) imm <- mat
  
  if(gadget3::g3_stock_def(imm, 'minage') == 0) {
    dummy_stock <- imm
  } else {
    ## Setup up a dummy stock
    dummy_stock <- 
      gadget3::g3_stock(c(species = gadgetutils::g3_stock_name(imm), 'dummy'),
                        lengthgroups = seq(min(gadget3::g3_stock_def(imm, 'minlen')),
                                           max(gadget3::g3_stock_def(imm, 'maxlen')[1:((length(gadget3::g3_stock_def(imm, 'maxlen')))-1)]),
                                           1)) %>% 
      gadget3::g3s_livesonareas(areas[c('1')]) %>% 
      gadget3::g3s_age(minage = 0, maxage = gadget3::g3_stock_def(imm, 'minage')-1)
    
  }
  
  dummy_actions <- 
    list(
      gadget3::g3a_age(dummy_stock, 
                       output_stocks = list(imm), 
                       run_f = ~cur_step == 2)
    )
  
  if (is.null(prop_ogive)){
    prop_ogive <- 1
  }
  
  spawning_actions <- 
    list(
      gadget3::g3a_spawn(
        stock = mat,
        output_stocks = list(dummy_stock),
        recruitment_f = 
          g3a_spawn_recruitment_hockeystick(
            r0 = gadget3:::f_substitute(~scale * g3_param_table('project_rec',
                                                                expand.grid(cur_year = seq(end_year - (minage - 1), 
                                                                                           end_year + py)), ifmissing = 0),
                                        list(py = num_project_years,
                                             minage = gadget3::g3_stock_def(imm, 'minage'),
                                             scale = 1e4)),
            blim = g3_parameterized('blim')), 
        proportion_f = prop_ogive,
        mean_f = g3a_renewal_vonb_t0(K = vonb_K, by_stock = list(mat)),
        stddev_f = gadget3:::f_substitute(~mu*cv,
                                          list(mu = g3a_renewal_vonb_t0(K = vonb_K, by_stock = list(mat)),
                                               cv = g3_parameterized(paste0("lencv.", g3_stock_def(mat, 'minage')),
                                                                     by_stock = list(mat)))),
        alpha_f = g3_parameterized('walpha', by_stock = comp_id),
        beta_f = g3_parameterized('wbeta', by_stock = comp_id),
        run_f = gadget3:::f_substitute(~cur_year_projection && cur_step == 3 || 
                                        cur_year >= (end_year - (minage)) && cur_step == 3,
                                       list(minage = gadget3::g3_stock_def(imm, 'minage'))),
        run_at = 2) 
      
    )
  
  return(c(dummy_actions, spawning_actions))
  
}

run_proj <- function(adfun, pars){
  
  reports <- adfun$report(gadget3::g3_tmb_par(pars))
  
  sbio <- apply(reports$detail_cod__num*reports$detail_cod__wgt, 4, sum)
  rbio <- apply((reports$detail_cod__num*reports$detail_cod__wgt)[,,paste0('age',4:12),], 3, sum)
  catch <- apply(reports$detail_cod__predby_comm + 
                   reports$detail_cod__predby_comm_proj, 4, sum) 
  rec <- apply(reports$detail_cod__num[,,1,],2,sum)
  
  fbar <-
    apply(((reports$detail_cod__predby_comm +
              reports$detail_cod__predby_comm_proj)/reports$detail_cod__wgt)[,,paste0('age',5:10),],
          3, sum, na.rm = TRUE)/apply(reports$detail_cod__num[,,paste0('age',5:10),],3,sum, na.rm = TRUE)
  
  fbar <- 5*(-log(1 - fbar))
  
  out <- 
    tibble(time = names(sbio),
           total.biomass = sbio) |> 
    left_join(
      tibble(time = names(rbio),
             ref.biomass = rbio),
      by = 'time') |>
    left_join(
      tibble(time = names(catch),
             catch = catch),
      by = 'time') |> 
    left_join(
      tibble(time = names(fbar),
             fbar = fbar), 
      by = 'time') |> 
    left_join(
      tibble(time = names(rec),
             rec = rec), 
      by = 'time') |> 
    gadgetutils::extract_year_step() |>
    select(year, step, catch, total.biomass, ref.biomass, fbar, rec)
  
  #out[out$step > 1, 'ssb'] <- 0
  #out[out$step != 2, 'rec'] <- 0
  
  return(out)
  
}


