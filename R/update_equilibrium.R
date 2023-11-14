#' Update equilibrium
#'
#' @description
#' `update_equilibrium()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
#'
#' @return List of list of data.tables with initial_conditions, model_scenarios and equilibrium_new.
#'
#' @param initial_conditions Required list of prepared initial conditions, e.g. for GTAP 10.
#' @param tariff Optional data.table of alternative initial tariffs (\eqn{\tau}), dimensions: origin x destination x sector.
#' @param tariff_new  Optional data.table of new tariffs (\eqn{\tau'}), dimensions: origin x destination x sector.
#' @param ntb Optional data.table of alternative initial non-tariff barriers (\eqn{\kappa}), dimensions: origin x destination x sector.
#' @param ntb_new Optional data.table of new non-tariff barriers (\eqn{\kappa'}), dimensions: origin x destination x sector.
#' @param ntb_change Optional data.table of relative change to new non-tariff barriers (\eqn{\kappa' / \kappa}), dimensions: origin x destination x sector.
#' @param consumption_share Optional data.table of alternative elasticities of substitution or consumption shares (\eqn{\alpha}), dimensions: destination x sector.
#' @param factor_share Optional data.table of alternative input factor shares (\eqn{\beta}), dimensions: country x sector.
#' @param intermediate_share Optional data.table of alternative input-output coefficients (\eqn{\gamma}), dimensions: country x input_sector x output_sector.
#' @param trade_elasticity Optional data.table of alternative trade elasticities (\eqn{\theta}), dimensions: sector.
#' @param trade_share Optional data.table of alternative initial trade matrix (\eqn{\pi}), dimensions: origin x destination x sector.
#' @param value_added Optional data.table of alternative value added (\eqn{w * L}), dimensions: country.
#' @param trade_balance Optional data.table of alternative trade balances (\eqn{D}), dimensions: country.
#' @param max_iterations maximum number of iterations.
#' @param tolerance Tolerance value for iterations.
#' @param vfactor Regulates the "speed" of the adjustment of wages.
#' @param verbose How talkative should the function be?
#'
#' @import cli
#' @import data.table
#' @importFrom stringr str_c
#' @importFrom reshape2 acast
#' @importFrom abind abind
#'
#' @export

update_equilibrium = function (initial_conditions = NULL,
                               tariff = NULL,
                               tariff_new = NULL,
                               ntb = NULL,
                               ntb_new = NULL,
                               ntb_change = NULL,
                               consumption_share = NULL,
                               factor_share = NULL,
                               intermediate_share = NULL,
                               trade_elasticity = NULL,
                               trade_share = NULL,
                               value_added = NULL,
                               trade_balance = NULL,
                               max_iterations = 1000,
                               tolerance = 1e-4,
                               vfactor = 0.1,
                               verbose = T) {

  if (verbose >= 1L) cli_h1("Initializing variables")
  if (verbose == 3L) writeLines(readLines("misc/kite.txt"))

  # check if valid input ----
  if (is.null(initial_conditions)) {
    cli_alert_danger("'initial_conditions' has to be specified.")
    return()
  }

  # assign manual changes to initial conditions and model scenario ----
  if (!is.null(consumption_share)) {
    cli_alert_warning("'consumption_share' is manually changed.")
    initial_conditions$consumption_share = consumption_share
    rm(consumption_share)
  }
  if (!is.null(factor_share)) {
    cli_alert_warning("'factor_share' is manually changed.")
    initial_conditions$factor_share = factor_share
    rm(factor_share)
  }
  if (!is.null(trade_elasticity)) {
    cli_alert_warning("'trade_elasticity' is manually changed.")
    initial_conditions$trade_elasticity = trade_elasticity
    rm(trade_elasticity)
  }
  if (!is.null(value_added)) {
    cli_alert_info("'value_added' is manually changed.")
    initial_conditions$value_added = value_added
    rm(value_added)
  }
  if (!is.null(trade_balance)) {
    cli_alert_info("'trade_balance' is manually changed.")
    initial_conditions$trade_balance = trade_balance
    rm(trade_balance)
  }
  if (!is.null(intermediate_share)) {
    cli_alert_warning("'intermediate_share' is manually changed.")
    initial_conditions$intermediate_share = intermediate_share
    rm(intermediate_share)
  }
  if (!is.null(trade_share)) {
    cli_alert_info("'trade_share' is manually changed.")
    initial_conditions$trade_share = trade_share
    rm(trade_share)
  }
  if (!is.null(tariff)) {
    cli_alert_info("'tariff' is manually changed.")
    initial_conditions$tariff = tariff
    rm(tariff)
  }
  if (!is.null(ntb)) {
    cli_alert_info("'ntb' is manually changed.")
    initial_conditions$ntb = ntb
    rm(ntb)
  }

  # set model scenario ----
  model_scenario = list()
  if (!is.null(tariff_new)) {
    cli_alert_info("'tariff_new' is set.")
    model_scenario$tariff_new = tariff_new
    rm(tariff_new)
  }
  if (!is.null(ntb_new)) {
    cli_alert_info("'ntb_new' is set.")
    model_scenario$ntb_new = ntb_new
    rm(ntb_new)
  }
  if (!is.null(ntb_change)) {
    cli_alert_info("'ntb_change' is set.")
    model_scenario$ntb_change = ntb_change
    rm(ntb_change)
  }

  # fake bindings ----
  . = NULL
  sector = NULL
  input = NULL
  output = NULL
  country = NULL
  origin = NULL
  destination = NULL
  value = NULL

  # set model attributes ----
  model_attributes = list()
  model_attributes$countries = initial_conditions$factor_share[order(country), unique(country)]
  model_attributes$sectors = initial_conditions$factor_share[order(sector), unique(sector)]
  model_attributes$length_countries = length(model_attributes$countries)
  model_attributes$length_sectors = length(model_attributes$sectors)
  model_attributes$max_iterations = max_iterations
  model_attributes$tolerance = tolerance

  # reshape elasticities ----
  setorder(initial_conditions$consumption_share, country, sector) # reverse sort for correct array
  consumption_share = array(data = initial_conditions$consumption_share[, value],
                            dim = c(model_attributes$length_sectors, model_attributes$length_countries),
                            dimnames = list(sector = model_attributes$sectors, country = model_attributes$countries))

  setorder(initial_conditions$factor_share, country, sector) # reverse sort for correct array
  factor_share = array(data = initial_conditions$factor_share[, value],
                       dim = c(model_attributes$length_sectors, model_attributes$length_countries),
                       dimnames = list(sector = model_attributes$sectors, country = model_attributes$countries))

  setorder(initial_conditions$trade_elasticity, sector) # reverse sort for correct array
  trade_elasticity = array(data = initial_conditions$trade_elasticity[, value],
                           dim = model_attributes$length_sectors,
                           dimnames = list(sector = model_attributes$sectors))

  # reshape value added ----
  setorder(initial_conditions$value_added, country) # reverse sort for correct array
  value_added = array(data = initial_conditions$value_added[, value],
                      dim = model_attributes$length_countries,
                      dimnames = list(country = model_attributes$countries))

  # reshape trade deficits ----
  setorder(initial_conditions$trade_balance, country) # reverse sort for correct array
  trade_balance = array(data = initial_conditions$trade_balance[, value],
                        dim = model_attributes$length_countries,
                        dimnames = list(country = model_attributes$countries))

  # reshape input-output coefficients ----
  setorder(initial_conditions$intermediate_share, country, output, input) # reverse sort for correct array
  intermediate_share = array(data = initial_conditions$intermediate_share[, value],
                             dim = c(model_attributes$length_sectors, model_attributes$length_sectors, model_attributes$length_countries),
                             dimnames = list(input = model_attributes$sectors, output = model_attributes$sectors, country = model_attributes$countries))

  # create intermediate_share bar ----
  input_share = intermediate_share
  for (c in model_attributes$countries) input_share[,,c] = t((1 - factor_share[,c]) * t(intermediate_share[,,c]))

  # reshape trade shares ----
  setorder(initial_conditions$trade_share, sector, destination, origin) # reverse sort for correct array
  trade_share = array(data = initial_conditions$trade_share[, value],
                      dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                      dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))

  # reshape tariff ----
  if (is.null(initial_conditions$tariff) & is.null(model_scenario$tariff_new)) {
    cli_alert_warning("Neither initial nor new tariffs are provided. Will not abort automatically, but take in caution interpreting results.")
  }
  if (!is.null(initial_conditions$tariff)) {
    setorder(initial_conditions$tariff, sector, destination, origin) # reverse sort for correct array
    tariff = array(data = initial_conditions$tariff[, value],
                   dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                   dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  } else {
    cli_alert_warning("No initial tariffs provided. Will assume zero tariffs for all country pairs and sectors.")
    tariff = array(data = 1,
                   dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                   dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  }

  if (!is.null(model_scenario$tariff_new)) {
    setorder(model_scenario$tariff_new, sector, destination, origin) # reverse sort for correct array
    tariff_new = array(data = model_scenario$tariff_new[, value],
                       dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                       dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  } else {
    tariff_new = tariff
  }

  tariff_change = tariff_new
  for (s in model_attributes$sectors) tariff_change[,,s] = tariff_new[,,s] / tariff[,,s]

  # reshape non-tariff barrier arrays ----
  if (is.null(initial_conditions$ntb) & is.null(model_scenario$ntb_new) && is.null(model_scenario$ntb_change)) {
    if (verbose >= 1L) cli_alert_info("Neither initial nor new non-tariff barriers are provided.")
  }
  if (!is.null(initial_conditions$ntb)) {
    setorder(initial_conditions$ntb, sector, destination, origin) # reverse sort for correct array
    ntb = array(data = initial_conditions$ntb[, value],
                dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  } else {
    cli_alert_warning("No initial non-tariff barriers provided. Will assume zero NTBs for all country pairs and sectors.")
    ntb = array(data = 1,
                dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  }

  if (!is.null(model_scenario$ntb_new)) {
    setorder(model_scenario$ntb_new, sector, destination, origin) # reverse sort for correct array
    ntb_new = array(data = model_scenario$ntb_new[, value],
                    dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                    dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  } else {
    ntb_new = ntb
  }

  if (!is.null(model_scenario$ntb_change)) { # ntb_change can also directly be provided as it isn't relevant for budget
    setorder(model_scenario$ntb_change, sector, destination, origin) # reverse sort for correct array
    ntb_change = array(data = model_scenario$ntb_change[, value],
                       dim = c(model_attributes$length_countries, model_attributes$length_countries, model_attributes$length_sectors),
                       dimnames = list(origin = model_attributes$countries, destination = model_attributes$countries, sector = model_attributes$sectors))
  } else {
    ntb_change = ntb_new
    for (s in model_attributes$sectors) ntb_change[,,s] = ntb_new[,,s] / ntb[,,s]
  }


  # compute trade_cost ----
  trade_cost = tariff
  for (s in model_attributes$sectors) trade_cost[,,s] = tariff[,,s] * ntb[,,s]
  trade_cost_new = tariff_new
  for (s in model_attributes$sectors) trade_cost_new[,,s] = tariff_new[,,s] * ntb_new[,,s]
  trade_cost_change = tariff_change
  for (s in model_attributes$sectors) trade_cost_change[,,s] = tariff_change[,,s] * ntb_change[,,s]

  # initialize vectors of ex-post wage and price factors, expenditure matrix ----
  wage_change = rep(1, length.out = length(model_attributes$countries))
  names(wage_change) = model_attributes$countries

  price_change = matrix(1,
                        nrow = model_attributes$length_sectors, ncol = model_attributes$length_countries,
                        dimnames = list(sector = model_attributes$sectors, country = model_attributes$countries))
  input_cost_change = matrix(1,
                             nrow = model_attributes$length_sectors, ncol = model_attributes$length_countries,
                             dimnames = list(sector = model_attributes$sectors, country = model_attributes$countries))
  expenditure_new = matrix(1,
                           nrow = model_attributes$length_sectors, ncol = model_attributes$length_countries,
                           dimnames = list(sector = model_attributes$sectors, country = model_attributes$countries))

  if (verbose >= 1L) cli_alert_success("Successfully initialized all variables.")

  # start iteration ----
  if (verbose >= 1L) cli_h1("Iterating to new equilibrium")
  if (verbose == 1L) status1 = cli_status("{symbol$arrow_right} Starting iteration procedure.")
  if (verbose >= 2L) cat("\n")

  timer = Sys.time()
  change_list <- as.matrix(t(c(as.numeric(Sys.time()),0)))
  criterion = 1
  h = 1

  while (h <= model_attributes$max_iterations && criterion > model_attributes$tolerance) {

    if (verbose >= 2L) cat(format(Sys.time()), ":", "Iteration", h, "\n")

    wage_change0 = wage_change

    # update input cost ----
    if (verbose >= 2L) cat("\r \u2014 Input update")
    input_cost_change = update_input_cost(input_cost_change,
                                          wage_change,
                                          price_change,
                                          factor_share,
                                          intermediate_share,
                                          model_attributes)
    if (verbose >= 2L) cat(" \u2713\n")

    # update price_change ----
    if (verbose >= 2L) cat("\r \u2014 Price index update")
    price_change = update_price(price_change,
                                trade_share,
                                trade_cost_change,
                                input_cost_change,
                                trade_elasticity,
                                model_attributes)
    if (verbose >= 2L) cat(" \u2713\n")

    # update new trade shares ----
    if (verbose >= 2L) cat("\r \u2014 Trade share update")
    trade_share_new = update_trade_share(trade_share,
                                         trade_cost_change,
                                         input_cost_change,
                                         price_change,
                                         trade_elasticity,
                                         model_attributes)
    if (verbose >= 2L) cat(" \u2713\n")

    # update new expenditure, ensuring goods market clearing and balanced trade condition ----
    if (verbose >= 2L) cat("\r \u2014 Expenditure update")
    expenditure_new = update_expenditure(expenditure_new,
                                         consumption_share,
                                         input_share,
                                         trade_share_new,
                                         tariff_new,
                                         value_added,
                                         wage_change,
                                         trade_balance,
                                         model_attributes,
                                         verbose)
    if (verbose >= 2L) cat(" \u2713\n")

    # update trade flows ----
    trade_flow_new = trade_share_new
    for (c in model_attributes$countries) trade_flow_new[c,,] = trade_share_new[c,,] * t(expenditure_new)

    # compute trade flow fob ----
    trade_flow_new_fob = trade_flow_new
    for (c in model_attributes$countries) trade_flow_new_fob[c,,] = trade_flow_new[c,,] / (tariff_new[c,,])

    # compute exports and imports ----
    exports = apply(trade_flow_new_fob,1,sum)
    imports = apply(trade_flow_new_fob,2,sum)

    # compute excess function trade balance ----
    excess = (exports - imports - trade_balance) / value_added

    # compute adjusted wages ----
    wage_change = wage_change + vfactor * excess

    # prepare next iteration ----
    criterion = sum(abs(wage_change - wage_change0))
    change_list <- rbind(change_list, as.matrix(t(c(as.numeric(Sys.time()), criterion))))

    if (verbose == 1L && h > 5) cli_status_update(id = status1, "{symbol$arrow_right} Currently in iteration {h}, convergence in about {predict_convergence_eta(change_list[,1],change_list[,2], model_attributes$tolerance)} seconds.")
    if (verbose >= 2L) cat(" \u2014 Criterion = ", criterion, "\n")

    h = h + 1

  }

  if (verbose >= 1L) cli_alert_success("Convergence after {h-1} iterations and {round(Sys.time() - timer)} seconds.")

  # final clean up ----
  if (verbose >= 1L) cli_h1("Processing and returning results")

  # melt outcomes into data.table ----
  equilibrium_new = list(wage_change = setDT(as.data.frame.table(wage_change, responseName = "value")),
                         input_cost_change = setDT(as.data.frame.table(input_cost_change, responseName = "value")),
                         price_change = setDT(as.data.frame.table(price_change, responseName = "value")),
                         trade_share_new = setDT(as.data.frame.table(trade_share_new, responseName = "value")),
                         expenditure_new = setDT(as.data.frame.table(expenditure_new, responseName = "value")),
                         trade_flow_new = setDT(as.data.frame.table(trade_flow_new, responseName = "value")),
                         trade_flow_new_fob = setDT(as.data.frame.table(trade_flow_new_fob, responseName = "value")))

  if (verbose >= 1L) cli_alert_success("Processing and returning results.")

  # return results ----
  list(initial_conditions = initial_conditions,
       model_scenario = model_scenario,
       equilibrium_new = equilibrium_new)

}
