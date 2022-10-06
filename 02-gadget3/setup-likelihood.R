## -----------------------------------------------------------------------------
##
## Setup likelihood
##
## -----------------------------------------------------------------------------

## weird inconsistencies in Gadget
aldist.igfs[[1]]$step <- 2
aldist.aut[[1]]$step <- 4

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stocks, nll_breakdown = nll_breakdown, weight = 1e6),

  g3l_catchdistribution(
    'aldist_total',
    aldist.sea[[1]] %>% mutate(area = '1') ,
    fleets = list(total),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'aldist_igfs',
    (aldist.igfs[[1]] %>% mutate(area = '1')), ## otolith sampling stratified by length pre 1989
    fleets = list(igfs),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'aldist_aut',
    (aldist.aut[[1]] %>% mutate(area = '1')) ,
    fleets = list(aut),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'si_igfs_1',
    (indices_igfs[[1]] %>% mutate(area = '1')),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'si_aut_1',
    (indices_aut[[1]] %>% mutate(area = '1')),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),


  list()
)
