#' Compute Chowdhry, Hinz, Kamin & Wanner (2022) model in changes
#'
#' @description
#' `chowdhry_hinz_kamin_wanner_2022()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
#'
#' @return List of list of data.tables with initial_conditions, model_scenarios and equilibrium_new.
#'
#' @param input List of prepared initial and counterfactual conditions.
#' @param settings List of various settings and model dimensions
#'
#' @export

chowdhry_hinz_kamin_wanner_2022 = function (input, settings) {

  # create intermediate_share_bar ----
  input$input_share = input$intermediate_share
  for (c in settings$model_dimensions$destination) input$input_share[,,c] = t((1 - input$factor_share[,c]) * t(input$intermediate_share[,,c]))

  # create tariff arrays ----
  if (is.null(input$tariff)) input$tariff = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(input$tariff_new)) input$tariff_new = input$tariff
  input$tariff_change = input$tariff_new / input$tariff

  # create ntb arrays ----
  if (is.null(input$ntb)) input$ntb = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(input$ntb_new)) input$ntb_new = input$ntb
  if (is.null(input$ntb_change)) input$ntb_change = input$ntb_new / input$ntb

  # create export subsidies arrays ----
  if (is.null(input$export_subsidy)) input$export_subsidy = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(input$export_subsidy_new)) input$export_subsidy_new = input$export_subsidy
  input$export_subsidy_change = input$export_subsidy_new / input$export_subsidy

  # compute trade_cost ----
  input$trade_cost = input$tariff * input$ntb * input$export_subsidy
  input$trade_cost_new = input$tariff_new * input$ntb_new * input$export_subsidy_new
  input$trade_cost_change = input$tariff_change * input$ntb_change * input$export_subsidy_change

  # initialize vectors of ex-post wage and price factors, expenditure matrix ----
  input$wage_change = initialize_variable(settings$model_dimensions[c("country")])
  input$price_change = initialize_variable(settings$model_dimensions[c("sector", "country")])
  input$input_cost_change = initialize_variable(settings$model_dimensions[c("sector", "country")])
  input$expenditure_new = initialize_variable(settings$model_dimensions[c("sector", "country")])
  input$trade_share_new = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  input$income_old = initialize_variable(settings$model_dimensions[c("country")], value = 0)
  input$income_new = initialize_variable(settings$model_dimensions[c("country")], value = 0)
  input$price_index_change = initialize_variable(settings$model_dimensions[c("country")])
  input$transfer = initialize_variable(settings$model_dimensions[c("country")], value = 0)

  # compute initial expenditure
  input$expenditure = update_expenditure_chkw_2022(input$expenditure_new,
                                                   input$consumption_share,
                                                   input$input_share,
                                                   input$trade_share,
                                                   input$tariff,
                                                   input$export_subsidy,
                                                   input$value_added,
                                                   input$wage_change,
                                                   input$trade_balance,
                                                   input$transfer,
                                                   settings$model_dimensions,
                                                   settings$tolerance,
                                                   verbose = F)

  if (settings$verbose >= 1L) cli_alert_success("Successfully initialized all variables.")

  # iteration ----
  if (settings$verbose >= 1L) cli_h1("Iterating to new equilibrium")
  if (settings$verbose == 1L) status1 = cli_status("{symbol$arrow_right} Starting iteration procedure.")
  if (settings$verbose >= 2L) cat("\n")

  timer = Sys.time()
  change_list <- as.matrix(t(c(as.numeric(Sys.time()),0)))
  criterion = 1
  h = 1

  while (h <= settings$max_iterations && criterion > settings$tolerance) {

    if (settings$verbose >= 2L) cat(format(Sys.time()), ":", "Iteration", h, "\n")

    input$wage_change0 = input$wage_change

    # update input cost ----
    if (settings$verbose >= 2L) cat("\r \u2014 Input update")
    input$input_cost_change = update_input_cost_chkw_2022(input$input_cost_change,
                                                          input$wage_change,
                                                          input$price_change,
                                                          input$factor_share,
                                                          input$intermediate_share,
                                                          settings$model_dimensions)
    if (settings$verbose >= 2L) cat(" \u2713\n")

    # update price_change ----
    if (settings$verbose >= 2L) cat("\r \u2014 Price index update")
    input$price_change = update_price_index_chkw_2022(input$price_change,
                                                      input$trade_share,
                                                      input$trade_cost_change,
                                                      input$input_cost_change,
                                                      input$trade_elasticity,
                                                      settings$model_dimensions)
    if (settings$verbose >= 2L) cat(" \u2713\n")

    # update new trade shares ----
    if (settings$verbose >= 2L) cat("\r \u2014 Trade share update")
    input$trade_share_new = update_trade_share_chkw_2022(input$trade_share,
                                                         input$trade_cost_change,
                                                         input$input_cost_change,
                                                         input$price_change,
                                                         input$trade_elasticity,
                                                         settings$model_dimensions)
    if (settings$verbose >= 2L) cat(" \u2713\n")

    # update new expenditure, ensuring goods market clearing and balanced trade condition ----
    if (settings$verbose >= 2L) cat("\r \u2014 Expenditure update")
    input$expenditure_new = update_expenditure_chkw_2022(input$expenditure_new,
                                                         input$consumption_share,
                                                         input$input_share,
                                                         input$trade_share_new,
                                                         input$tariff_new,
                                                         input$export_subsidy_new,
                                                         input$value_added,
                                                         input$wage_change,
                                                         input$trade_balance,
                                                         input$transfer,
                                                         settings$model_dimensions,
                                                         settings$tolerance,
                                                         settings$verbose)
    if (settings$verbose >= 2L) cat(" \u2713\n")

    # compute transfers ----
    if (settings$verbose >= 2L) cat("\r \u2014 Transfer update")
    input$transfer = update_transfer_chkw_2022(input$transfer,
                                               input$income_old,
                                               input$income_new,
                                               input$price_index_change,
                                               input$expenditure,
                                               input$expenditure_new,
                                               input$consumption_share,
                                               input$trade_share,
                                               input$trade_share_new,
                                               input$tariff,
                                               input$tariff_new,
                                               input$export_subsidy,
                                               input$export_subsidy_new,
                                               input$value_added,
                                               input$wage_change,
                                               input$trade_balance,
                                               input$price_change,
                                               input$coalition_member,
                                               settings$model_dimensions)
    if (settings$verbose >= 2L) cat(" \u2713\n")

    # update trade flows ----
    input$trade_flow_new = input$trade_share_new
    for (c in settings$model_dimensions$destination) input$trade_flow_new[c,,] = input$trade_share_new[c,,] * t(input$expenditure_new)

    # compute excess function trade balance ----
    input$excess = (apply(input$trade_flow_new / input$tariff_new,1,sum) # exports
                    - apply(input$trade_flow_new / input$tariff_new,2,sum) # imports
                    - input$trade_balance
                    + input$transfer) / input$value_added

    # compute adjusted wages ----
    input$wage_change = input$wage_change + settings$vfactor * input$excess

    # prepare next iteration ----
    criterion = sum(abs(input$wage_change - input$wage_change0))
    change_list <- rbind(change_list, as.matrix(t(c(as.numeric(Sys.time()), criterion))))

    if (settings$verbose == 1L && h > 5) cli_status_update(id = status1, "{symbol$arrow_right} Currently in iteration {h}, convergence in about {predict_convergence_eta(change_list[,1],change_list[,2], settings$model_dimensions$tolerance)} seconds.")
    if (settings$verbose >= 2L) cat(" \u2014 Criterion = ", criterion, "\n")

    h = h + 1

  }

  if (settings$verbose >= 1L) cli_alert_success("Convergence after {h-1} iterations and {round(Sys.time() - timer)} seconds.")

  output = list(wage_change = input$wage_change,
                input_cost_change = input$input_cost_change,
                price_change = input$price_change,
                trade_share_new = input$trade_share_new,
                expenditure_new = input$expenditure_new,
                transfer = input$transfer)

  return(output)

}


#' Update input cost matrix for Chowdhry, Hinz, Kamin & Wanner (2022)
#'
#' @description
#' Updates cost of input bundle following Chowdhry, Hinz, Kamin & Wanner (2022) equations (10).
#'
#' @return Matrix of changes in input costs, dimensions: sector x country.
#'
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param factor_share Matrix of input factor shares, dimensions: sector x country.
#' @param intermediate_share Array of input-output coefficients, dimensions: input_sector x output_sector x country.
#' @param model_dimensions List of model dimensions.
#'

update_input_cost_chkw_2022 = function (input_cost_change,
                                        wage_change,
                                        price_change,
                                        factor_share,
                                        intermediate_share,
                                        model_dimensions) {

  for (c in model_dimensions$destination) input_cost_change[,c] = exp(factor_share[,c] * log(wage_change[c]) + (1 - factor_share[,c]) * (t(intermediate_share[,,c]) %*% log(price_change[,c])))

  return (input_cost_change)
}


#' Update price index matrix for Chowdhry, Hinz, Kamin & Wanner (2022)
#'
#' @description
#' Update price index following Chowdhry, Hinz, Kamin & Wanner (2022) equation (11).
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

update_price_index_chkw_2022 = function (price_change,
                                         trade_share,
                                         trade_cost_change,
                                         input_cost_change,
                                         trade_elasticity,
                                         model_dimensions) {

  for (s in model_dimensions$sector) price_change[s,] = ((t(trade_share[,,s]) * (t(trade_cost_change[,,s])^(-1/trade_elasticity[s]))) %*% (input_cost_change[s,]^(-1/trade_elasticity[s])))^(-trade_elasticity[s])

  # correct for zeros (i.e. prices for non-traded)
  for (s in model_dimensions$sector) price_change[s,][price_change[s,] == 0] = 1

  return (price_change)
}


#' Update trade share matrix for Chowdhry, Hinz, Kamin & Wanner (2022)
#'
#' @description
#' Compute new trade shares following Chowdhry, Hinz, Kamin & Wanner (2022) equation (12).`
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

update_trade_share_chkw_2022 = function (trade_share,
                                         trade_cost_change,
                                         input_cost_change,
                                         price_change,
                                         trade_elasticity,
                                         model_dimensions) {

  trade_share_new = trade_share
  for (s in model_dimensions$sector) trade_share_new[,,s] = t(t(trade_cost_change[,,s] * input_cost_change[s,]) / price_change[s,])^(-1/trade_elasticity[s]) * trade_share[,,s]

  return (trade_share_new)

}


#' Update expenditure for Chowdhry, Hinz, Kamin & Wanner (2022)
#'
#' @description
#' Updates expenditures following Chowdhry, Hinz, Kamin & Wanner (2022) equation (13).
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
#' @param trade_balance Vector of aggregate trade balance, dimension: country.
#' @param transfer Vector of transfers, dimension: country.
#' @param model_dimensions List of model dimensions.
#' @param tolerance tolerance
#' @param verbose verbose
#'

update_expenditure_chkw_2022 = function (expenditure_new,
                                         consumption_share,
                                         input_share,
                                         trade_share_new,
                                         tariff_new,
                                         export_subsidy_new,
                                         value_added,
                                         wage_change,
                                         trade_balance,
                                         transfer,
                                         model_dimensions,
                                         tolerance,
                                         verbose) {

  # iterate to new expenditure instead of inverting
  crit = 1
  j = 1
  while(crit >= tolerance) {
    if (verbose == 2L) cat("\r \u2014 Expenditure update, criterion = ")
    sum_exp0 = sum(expenditure_new)
    for (c in model_dimensions$destination) {
      expenditure_new[,c] = input_share[,,c] %*% (expenditure_new %diag% (trade_share_new[c,,] / (tariff_new[c,,] * export_subsidy_new[c,,]))) + # input
        consumption_share[,c] * (
          sum(((tariff_new[,c,] - 1) * trade_share_new[,c,] / tariff_new[,c,]) %*% expenditure_new[,c]) # new tariff revenue
          + sum(expenditure_new %diag% ((export_subsidy_new[c,,] - 1) * trade_share_new[c,,] / (tariff_new[c,,] * export_subsidy_new[c,,]))) # new export subsidy costs
          + wage_change[c] * value_added[c] # new value added
          - trade_balance[c] # (unchanged) trade balance
          + transfer[c]
        )
    }
    crit = abs((sum(expenditure_new) - sum_exp0) / sum_exp0)
    j = j + 1
    if (verbose == 2L) cat(crit, "after iteration", j-1)
  }
  return (expenditure_new)
}


#' Update transfers for Chowdhry, Hinz, Kamin & Wanner (2022)
#'
#' @description
#' Updates transfers following Chowdhry, Hinz, Kamin & Wanner (2022) equation (13).
#'
#' @return Vector of new transfers, dimensions: country.
#'
#' @param transfer Vector of transfers, dimension: country.
#' @param income_old Matrix of old income, dimensions: country.
#' @param income_new Matrix of new income, dimensions: country.
#' @param price_index_change Matrix of change of price index, dimensions: country.
#' @param expenditure Matrix of expenditures, dimensions: sector x country.
#' @param expenditure_new Matrix of new expenditures, dimensions: sector x country.
#' @param consumption_share Matrix of elasticities of substitution or consumption shares, dimensions: sector x country.
#' @param trade_share Array of trade flows (FOB), dimensions: origin x destination x sector.
#' @param trade_share_new Array of new trade flows (FOB), dimensions: origin x destination x sector.
#' @param tariff Array of tariffs, dimensions: origin x destination x sector.
#' @param tariff_new Array of new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy Array of export taxes, dimensions: origin x destination x sector.
#' @param export_subsidy_new Array of new export taxes, dimensions: origin x destination x sector.
#' @param value_added Matrix of value added, dimension: country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param trade_balance Vector of aggregate trade balance, dimension: country.
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param coalition_member Vector of country codes with coalition members, dimension: country.
#' @param model_dimensions List of model dimensions.
#'

update_transfer_chkw_2022 = function (transfer,
                                      income_old,
                                      income_new,
                                      price_index_change,
                                      expenditure,
                                      expenditure_new,
                                      consumption_share,
                                      trade_share,
                                      trade_share_new,
                                      tariff,
                                      tariff_new,
                                      export_subsidy,
                                      export_subsidy_new,
                                      value_added,
                                      wage_change,
                                      trade_balance,
                                      price_change,
                                      coalition_member,
                                      model_dimensions) {

  # compute old and new income, discounted by change in price index
  for (c in coalition_member) {
    income_new[c] = (
      sum(((tariff_new[,c,] - 1) * trade_share_new[,c,] / tariff_new[,c,]) %*% expenditure_new[,c]) # new tariff revenue
      + sum(expenditure_new %diag% ((export_subsidy_new[c,,] - 1) * trade_share_new[c,,] / (tariff_new[c,,] * export_subsidy_new[c,,]))) # new export subsidy costs
      + wage_change[c] * value_added[c] # new value added
      - trade_balance[c] # (unchanged) trade balance
    )
    income_old[c] = (
      sum(((tariff[,c,] - 1) * trade_share[,c,] / tariff[,c,]) %*% expenditure[,c]) # old tariff revenue
      + sum(expenditure %diag% ((export_subsidy[c,,] - 1) * trade_share[c,,] / (tariff[c,,] * export_subsidy[c,,]))) # old export subsidy costs
      + value_added[c] # value added
      - trade_balance[c] # (unchanged) trade balance
    )
    price_index_change[c] = exp(sum(consumption_share[,c] * log(price_change)[,c]))
  }

  # compute common welfare change and transfer
  welfare_change_coalition = sum(income_new) / (sum(income_old * price_index_change))
  transfer = welfare_change_coalition * (income_old * price_index_change) - income_new

  return (transfer)
}
