## Function to calculate RMSE
rmse <- function(obs, preds){ return( sqrt(mean((obs - preds)^2)) ) }

calc_skill <- function(obs, preds, type = NULL, id = NULL){
  if (is.null(id)) id <- ''
  if (is.null(type)) type <- 'fit/prediction'
  out <- data.frame(measure = c('rmse', 'cor', 'var_obs', 'var_preds', 'lawofcos'),
                    value   = c(rmse(obs, preds)/1e6,
                                cor(obs, preds),
                                var(obs)/1e6,
                                var(preds)/1e6,
                                (var(obs) + var(preds) - 2*sd(obs)*sd(preds)*cor(obs, preds))/1e6)
  )
  out$type <- type
  out$id <- id
  return(out)
}