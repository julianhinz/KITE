#' Compute Caliendo and Parro (2015)-type model in changes
#'
#' @description
#' `caliendo_parro_2015()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
#'
#' @return List of list of new equilibrium `output`.
#'
#' @param input List of prepared initial and counterfactual conditions.
#' @param settings List of various settings and model dimensions
#'
#' @export

caliendo_parro_2015 = function (input, settings) {

  # initialize settings
  if (is.null(settings[['vfactor']])) settings[['vfactor']] = 0.1
  if (is.null(settings[['max_inner_iterations']])) settings[['max_inner_iterations']] = 5000L
  if (is.null(settings[['trade_balance_rule']])) settings[['trade_balance_rule']] = "fixed"
  if (is.null(settings[['tolerance_expenditure']])) settings[['tolerance_expenditure']] = settings[['tolerance']]
  if (is.null(settings[['tolerance_output']])) settings[['tolerance_output']] = settings[['tolerance']]
  if (is.null(settings[['convergence_method']])) settings[['convergence_method']] = "root_mean_square"
  if (is.null(settings[['require_inner_convergence']])) settings[['require_inner_convergence']] = TRUE

  # create input share arrays ----
  input[['input_share']] = input[['intermediate_share']]
  for (c in settings[['model_dimensions']]$destination) input[['input_share']][c,,] = t((1 - input[['factor_share']][c,]) * t(input[['intermediate_share']][c,,]))

  # create tariff arrays ----
  if (is.null(input[['tariff']])) input[['tariff']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  if (is.null(input[['tariff_new']])) input[['tariff_new']] = input[['tariff']]
  input[['tariff_change']] = input[['tariff_new']] / input[['tariff']]

  # create ntb arrays ----
  if (is.null(input[['ntb']])) input[['ntb']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  if (is.null(input[['ntb_new']])) input[['ntb_new']] = input[['ntb']]
  if (is.null(input[['ntb_change']])) input[['ntb_change']] = input[['ntb_new']] / input[['ntb']]

  # create export subsidies arrays ----
  if (is.null(input[['export_subsidy']])) input[['export_subsidy']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  if (is.null(input[['export_subsidy_new']])) input[['export_subsidy_new']] = input[['export_subsidy']]
  input[['export_subsidy_change']] = input[['export_subsidy_new']] / input[['export_subsidy']]

  # compute trade_cost ----
  input[['trade_cost']] = input[['tariff']] * input[['ntb']] * input[['export_subsidy']]
  input[['trade_cost_new']] = input[['tariff_new']] * input[['ntb_new']] * input[['export_subsidy_new']]
  input[['trade_cost_change']] = input[['tariff_change']] * input[['ntb_change']] * input[['export_subsidy_change']]

  # initialize vectors of ex-post wage and price factors, expenditure, income and output matrix ----
  input[['wage_change']] = initialize_variable(settings[['model_dimensions']][c("country")])
  input[['price_change']] = initialize_variable(settings[['model_dimensions']][c("country", "sector")])
  input[['input_cost_change']] = initialize_variable(settings[['model_dimensions']][c("country", "sector")])
  input[['trade_share_new']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")], value = 0)
  input[['trade_balance_new']] = copy(input[['trade_balance']])

  # check for exogenous productivity and population changes
  if (is.null(input[['productivity_change']])) input[['productivity_change']] = initialize_variable(settings[['model_dimensions']][c("country", "sector")], value = 1)
  if (is.null(input[['population_change']])) input[['population_change']] = initialize_variable(settings[['model_dimensions']][c("country")], value = 1)
  if (!is.null(input[['global_value_added_change']])) input[['value_added']] = input[['global_value_added_change']] * input[['value_added']]

  # initialize income ----
  # generate trade flows
  input[['trade_flow']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  for (c in settings[['model_dimensions']]$country) {
    input[['trade_flow']][c,,] = input[['trade_share']][c,,] * input[['expenditure']]
  }

  # generate income
  input[['income']] <-
    array_sum((input[['tariff']] - 1) * input[['trade_flow']] / input[['tariff']], 2) + # tariff_revenue
    array_sum((input[['export_subsidy']] - 1) * input[['trade_flow']] / (input[['tariff']] * input[['export_subsidy']]), 1) + # export_subsidy_cost
    input[['wage_change']] * input[['value_added']] -
    input[['trade_balance']]

  # generate output
  input[['output']] <- array_sum(input[['trade_flow']] / (input[['tariff']] * input[['export_subsidy']]), c(1, 3))

  # initialize new
  input[['income_new']] = input[['income']]
  input[['output_new']] = input[['output']]

  if (settings[['verbose']] >= 1L) cli_alert_success("Successfully initialized all variables.")

  # iteration ----
  if (settings[['verbose']] >= 1L) cli_h1("Iterating to new equilibrium")
  if (settings[['verbose']] == 1L) status1 = cli_status("{symbol[['arrow_right']]} Starting iteration procedure.")
  if (settings[['verbose']] >= 2L) cat("\n")

  timer_start = Sys.time()
  change_list <- as.matrix(t(c(as.numeric(Sys.time()),0)))
  
  inner_converged_flag = TRUE

  criterion = 1
  h = 1
  while (h <= settings[['max_iterations']] &&
         (criterion > settings[['tolerance']] ||
          (settings[['require_inner_convergence']] && !inner_converged_flag))) {

    if (settings[['verbose']] >= 2L) cat(format(Sys.time()), ":", "Iteration", h, "\n")

    input[['wage_change0']] = input[['wage_change']]

    # update input cost ----
    if (settings[['verbose']] >= 2L) cat("\r \u2014 Input update")
    input[['input_cost_change']] = update_input_cost_cp_2015(input[['input_cost_change']],
                                                             input[['wage_change']],
                                                             input[['productivity_change']],
                                                             input[['price_change']],
                                                             input[['factor_share']],
                                                             input[['intermediate_share']],
                                                             settings[['model_dimensions']])
    if (settings[['verbose']] >= 2L) cat(" \u2713\n")

    # update price_change ----
    if (settings[['verbose']] >= 2L) cat("\r \u2014 Price index update")
    input[['price_change']] = update_price_index_cp_2015(input[['price_change']],
                                                         input[['trade_share']],
                                                         input[['trade_cost_change']],
                                                         input[['input_cost_change']],
                                                         input[['elasticities']][['trade_elasticity']],
                                                         settings[['model_dimensions']])
    if (settings[['verbose']] >= 2L) cat(" \u2713\n")

    # update new trade shares ----
    if (settings[['verbose']] >= 2L) cat("\r \u2014 Trade share update")
    input[['trade_share_new']] = update_trade_share_cp_2015(input[['trade_share']],
                                                            input[['trade_cost_change']],
                                                            input[['input_cost_change']],
                                                            input[['price_change']],
                                                            input[['elasticities']][['trade_elasticity']],
                                                            settings[['model_dimensions']])
    if (settings[['verbose']] >= 2L) cat(" \u2713\n")

    # compute output, income, and expenditure update ----
    if (settings[['verbose']] >= 2L) cat("\r \u2014 Output and income update")
    input[['output_income_new']] = update_output_income_cp_2015(input[['output_new']],
                                                                input[['income_new']],
                                                                input[['consumption_share']],
                                                                input[['input_share']],
                                                                input[['trade_share_new']],
                                                                input[['tariff_new']],
                                                                input[['export_subsidy_new']],
                                                                input[['value_added']],
                                                                input[['wage_change']],
                                                                input[['population_change']],
                                                                input[['trade_balance_new']],
                                                                settings[['model_dimensions']],
                                                                settings[['tolerance_output']],
                                                                settings[['convergence_method']],
                                                                settings[['verbose']],
                                                                settings[['max_inner_iterations']])

    input[['output_new']] = input[['output_income_new']]$output_new
    input[['income_new']] = input[['output_income_new']]$income_new
    input[['expenditure_new']] = input[['output_income_new']]$expenditure_new
    inner_converged_flag = isTRUE(input[['output_income_new']]$inner_converged)
    input[['output_income_new']] = NULL

    if (settings[['verbose']] >= 2L) cat(" \u2713\n")

    # compute wage update, explicitly ----
    input[['value_added_new']] = array_sum(input[['factor_share']] * input[['output_new']], 1)
    dimnames(input[['value_added_new']]) = dimnames(input[['value_added']])

    # gentle update of the wage change
    input[['wage_change']] = exp((1 - settings[['vfactor']]) * log(input[['wage_change']]) + settings[['vfactor']] * log((input[['value_added_new']] / input[['value_added']]) * (1 / input[['population_change']])))

    # normalize wage change (to be made flexible in the future, e.g. alternative US wage)
    input[['wage_change']] = input[['wage_change']] * sum(input[['value_added']]) / sum(input[['value_added_new']])

    # update trade balance ----
    input[['trade_balance_new']] = update_trade_balance(input[['trade_balance']],
                                                        input[['value_added']],
                                                        input[['value_added_new']],
                                                        settings[['trade_balance_rule']])

    # prepare next iteration ----
    criterion = check_convergence(input[['wage_change']], input[['wage_change0']], method = settings[['convergence_method']])
    change_list <- rbind(change_list, as.matrix(t(c(as.numeric(Sys.time()), criterion))))

    if (settings[['verbose']] == 1L) cli_status_update(id = status1, "{symbol[['arrow_right']]} Currently in iteration {h}.")
    if (settings[['verbose']] >= 2L) cat(" \u2014 Criterion =", sprintf("%.1e", criterion), "\n")

    h = h + 1

  }

  if (settings[['verbose']] >= 1L) cli_alert_success("Convergence after {h-1} iterations and {format_time_diff(timer_start, Sys.time())}")

  input[['criterion']] = criterion
  input[['iterations']] = h - 1

  # return
  output_variables(input, c(c("wage_change",
                              "input_cost_change",
                              "price_change",
                              "trade_share_new",
                              "expenditure_new",
                              "value_added_new",
                              "trade_balance_new",
                              "criterion",
                              "iterations"),
                            settings[["additional_output_variables"]]))
}


#' Update input cost matrix for Caliendo and Parro (2015)
#'
#' @description
#' Updates cost of input bundle following Caliendo & Parro (2015) equations (10).
#'
#' @return Matrix of changes in input costs, dimensions: sector x country.
#'
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param productivity_change Vector of change in productivity, dimension: country x sector.
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param factor_share Matrix of input factor shares, dimensions: sector x country.
#' @param intermediate_share Array of input-output coefficients, dimensions: input_sector x output_sector x country.
#' @param model_dimensions List of model dimensions.
#'

update_input_cost_cp_2015 = function (input_cost_change,
                                      wage_change,
                                      productivity_change,
                                      price_change,
                                      factor_share,
                                      intermediate_share,
                                      model_dimensions) {

  for (c in model_dimensions[['destination']]) input_cost_change[c,] =  1 / productivity_change[c,] * exp(factor_share[c,] * log(wage_change[c]) + (1 - factor_share[c,]) * (t(intermediate_share[c,,]) %*% log(price_change[c,])))

  return (input_cost_change)
}


#' Update price index matrix for Caliendo and Parro (2015)
#'
#' @description
#' Update price index following Caliendo & Parro (2015) equation (11).
#'
#' @return Matrix of changes in price index, dimensions: sector x country.
#'
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param trade_share Array of initial trade flows, dimensions: origin x destination x sector.
#' @param trade_cost_change Array of change in trade costs, dimensions: origin x destination x sector.
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param trade_elasticity Vector of trade elasticities, dimensions: sector.
#' @param model_dimensions List of model dimensions.
#'

update_price_index_cp_2015 = function (price_change,
                                       trade_share,
                                       trade_cost_change,
                                       input_cost_change,
                                       trade_elasticity,
                                       model_dimensions) {

  for (s in model_dimensions[['sector']]) price_change[,s] = ((t(trade_share[,,s]) * (t(trade_cost_change[,,s])^(-1/trade_elasticity[s]))) %*% (input_cost_change[,s]^(-1/trade_elasticity[s])))^(-trade_elasticity[s])

  # correct for zeros (i.e. prices for non-traded)
  for (s in model_dimensions[['sector']]) price_change[,s][price_change[,s] == 0] = 1

  return (price_change)
}


#' Update trade share matrix for Caliendo and Parro (2015)
#'
#' @description
#' Compute new trade shares following Caliendo & Parro (2015) equation (12).
#'
#' @return Array of new trade shares, dimensions: origin x destination x sector.
#'
#' @param trade_share Array of initial trade flows, dimensions: origin x destination x sector.
#' @param trade_cost_change Array of change in trade costs, dimensions: origin x destination x sector.
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param trade_elasticity Vector of trade elasticities, dimension: sector.
#' @param model_dimensions List of model dimensions.
#'

update_trade_share_cp_2015 = function (trade_share,
                                       trade_cost_change,
                                       input_cost_change,
                                       price_change,
                                       trade_elasticity,
                                       model_dimensions) {

  trade_share_new = trade_share
  for (s in model_dimensions[['sector']]) trade_share_new[,,s] = t(t(trade_cost_change[,,s] * input_cost_change[,s]) / price_change[,s])^(-1/trade_elasticity[s]) * trade_share[,,s]

  return (trade_share_new)

}


#' Update output and income for Caliendo Parro (2015)
#'
#' @description
#' Updates output and income analogous to Caliendo & Parro (2015) equation (13) for the expenditure update.
#'
#' @return Matrix of new expenditures, dimensions: country x sector.
#'
#' @param output_new Matrix of new outputs, dimensions: country x sector.
#' @param income_new Vector of new incomes, dimension: country.
#' @param consumption_share Matrix of consumption shares, dimensions: country x sector.
#' @param input_share Array of beta-multiplied input-output coefficients, dimensions: country x input x output.
#' @param trade_share_new Array of new trade shares, dimensions: origin x destination x sector.
#' @param tariff_new Array of new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy_new Array of export subsidies, dimensions: origin x destination x sector.
#' @param value_added Vector of value added, dimension: country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param population_change Vector of change in population, dimension: country.
#' @param trade_balance_new Vector of new aggregate trade balance, dimension: country.
#' @param model_dimensions List of model dimensions.
#' @param tolerance Tolerance for inner convergence.
#' @param convergence_method Method for convergence checking.
#' @param verbose Verbosity level.
#' @param max_inner_iterations Maximum number of inner iterations before the loop bails out (default 5000).
#'
update_output_income_cp_2015 = function (output_new,
                                         income_new,
                                         consumption_share,
                                         input_share,
                                         trade_share_new,
                                         tariff_new,
                                         export_subsidy_new,
                                         value_added,
                                         wage_change,
                                         population_change,
                                         trade_balance_new,
                                         model_dimensions,
                                         tolerance,
                                         convergence_method,
                                         verbose,
                                         max_inner_iterations = 5000L) {

  # initialize variables
  input_purchases_new = initialize_variable(model_dimensions[c("country", "input", "output")])
  final_demand_new = initialize_variable(model_dimensions[c("country", "sector")])
  trade_flow_new = initialize_variable(model_dimensions[c("origin", "destination", "sector")])

  # iterate to new output
  crit = 1
  j = 1
  inner_converged = FALSE
  while (j <= max_inner_iterations) {
    if (verbose == 2L) cat("\r \u2014 Output update, criterion = ")

    out0 = output_new

    # update input purchases (country x input x output) and final demand (country x sector)
    input_purchases_new <- array_sweep(input_share, c("country", "output"), output_new, "*")
    final_demand_new = array_sweep(consumption_share, "country", income_new, "*")
    expenditure_new = final_demand_new + array_sum(input_purchases_new, c(1,2)) # sum over country x input

    # update trade flows
    trade_flow_new <- array_sweep(trade_share_new, c("destination", "sector"), expenditure_new, "*")

    # update income
    income_new <- array_sum((tariff_new - 1) * trade_flow_new / tariff_new, 2) + # tariff_revenue
      array_sum((export_subsidy_new - 1) * trade_flow_new / (tariff_new * export_subsidy_new), 1) + # export_subsidy_cost
      population_change * wage_change * value_added -
      trade_balance_new

    # update output
    output_new <- array_sum(trade_flow_new / (tariff_new * export_subsidy_new), c(1, 3))

    crit = check_convergence(output_new, out0, method = convergence_method)
    if (verbose == 2L) cat(sprintf("%.1e", crit), "after iteration", j)
    if (!is.finite(crit)) break
    if (crit < tolerance) { inner_converged = TRUE; break }
    j = j + 1
  }

  list(income_new = income_new,
       output_new = output_new,
       expenditure_new = expenditure_new,
       n_iterations_output_update = j,
       inner_converged = inner_converged)
}


#' Update expenditure for Caliendo Parro (2015)
#'
#' @description
#' Updates expenditures following Caliendo & Parro (2015) equation (13).
#'
#' @return Matrix of new expenditures, dimensions: sector x country.
#'
#' @param expenditure_new Matrix of new expenditures, dimensions: sector x country.
#' @param consumption_share Matrix of elasticities of substitution or consumption shares, dimensions: sector x country.
#' @param input_share Array of beta-multiplied input-output coefficients, dimensions: input_sector x output_sector x country.
#' @param trade_share_new Array of new trade flows (FOB), dimensions: origin x destination x sector.
#' @param tariff_new Array of new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy_new Array of export taxes, dimensions: origin x destination x sector.
#' @param value_added Matrix of value added, dimension: country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param trade_balance_new Vector of new aggregate trade balance, dimension: country.
#' @param model_dimensions List of model dimensions.
#' @param tolerance Tolerance for inner convergence.
#' @param verbose Verbosity level.
#' @param convergence_method Method for convergence checking.
#' @param max_inner_iterations Maximum number of inner iterations before the loop bails out (default 5000).
#'

update_expenditure_cp_2015 = function (expenditure_new,
                                       consumption_share,
                                       input_share,
                                       trade_share_new,
                                       tariff_new,
                                       export_subsidy_new,
                                       value_added,
                                       wage_change,
                                       trade_balance_new,
                                       model_dimensions,
                                       tolerance,
                                       verbose,
                                       convergence_method,
                                       max_inner_iterations = 5000L) {

  # iterate to new expenditure instead of inverting
  crit = 1
  j = 1
  inner_converged = FALSE
  while (j <= max_inner_iterations) {
    if (verbose == 2L) cat("\r \u2014 Expenditure update, criterion = ")

    exp0 = expenditure_new

    for (c in model_dimensions[['destination']]) {
      expenditure_new[c,] = input_share[c,,] %*% (t(expenditure_new) %diag% (trade_share_new[c,,] / (tariff_new[c,,] * export_subsidy_new[c,,]))) + # input
        consumption_share[c,] * (
          sum(((tariff_new[,c,] - 1) * trade_share_new[,c,] / tariff_new[,c,]) %*% expenditure_new[c,]) # new tariff revenue
          + sum(t(expenditure_new) %diag% ((export_subsidy_new[c,,] - 1) * trade_share_new[c,,] / (tariff_new[c,,] * export_subsidy_new[c,,]))) # new export subsidy costs
          + wage_change[c] * value_added[c] # new value added
          - trade_balance_new[c] # trade balance
        )
    }

    crit = check_convergence(expenditure_new, exp0, method = convergence_method)
    if (verbose == 2L) cat(crit, "after iteration", j)
    if (!is.finite(crit)) break
    if (crit < tolerance) { inner_converged = TRUE; break }
    j = j + 1
  }
  list(expenditure_new = expenditure_new,
       n_iterations_expenditure_update = j,
       inner_converged = inner_converged)
}
