#' Compute Mahlkow and Wanner (2021)-type model with carbon policy in changes
#'
#' @description
#' `mahlkow_wanner_2021()` updates the equilibrium to a counterfactual situation
#' with new trade costs, a carbon tax in a climate club and a carbon border
#' adjustment (CBAM). It extends [caliendo_parro_2015()]: without a carbon
#' policy it solves the same equilibrium with the same algorithm.
#'
#' @details
#' **Carbon tax.** Using one unit of sector-`s` goods emits
#' `carbon_intensity[s]` units of carbon, whether a firm uses the goods as an
#' intermediate input or a household consumes them. A member `n` of the climate
#' club levies the carbon tax per unit of carbon on all domestic use of these
#' goods, domestic and imported alike. The tax is a wedge `tax[n, s]` on the
#' price of sector-`s` goods in `n`:
#' `tax_new[n, s] = tax_new0[n, s] + carbon_tax * carbon_intensity[s] / price_new[n, s]`,
#' where `tax_new0` is the exogenous tax (scenario `tax_new`, default `tax`)
#' and `price_new = price * price_change` is the endogenous price level of
#' sector-`s` goods in `n`. The carbon tax is a specific tax, so the initial
#' price levels `price` set its ad-valorem equivalent. Non-members keep
#' `tax_new0`.
#'
#' Expenditure `expenditure` (`X[n, s]`) includes the tax. Firms pay
#' `tax * price` for their intermediate inputs, so the input-cost change is
#' `w^beta * prod_k (tax_change[n, k] * price_change[n, k])^((1 - beta) * gamma[k, s])`.
#' Suppliers receive `X[n, s] / tax[n, s]`, so trade flows are
#' `trade_share[i, n, s] * X[n, s] / tax[n, s]`, and the tax revenue
#' `sum_s (tax[n, s] - 1) / tax[n, s] * X[n, s]` is part of income.
#' Households also pay the tax, so [process_results()] reports the consumer
#' price index `prod_s (tax_change[n, s] * price_change[n, s])^alpha[n, s]`.
#'
#' **Border carbon adjustment.** Producing one unit of value of sector `s` in
#' country `i` embodies the carbon
#' `embodied[i, s] = sum_k carbon_intensity[k] / (tax_new[i, k] * price_new[i, k]) * (1 - beta[i, s]) * gamma[i, k, s]`
#' of its intermediate inputs. With `scenario_carbon_tariff = TRUE`, club
#' members levy the carbon tax on this embodied carbon on imports from
#' non-members in the sectors `cbam_sector`:
#' `tariff_new[i, n, s] = tariff_new0[i, n, s] + carbon_tax * embodied[i, s]`.
#' With `scenario_export_rebate = TRUE`, club members refund it on their
#' exports to non-members:
#' `export_subsidy_new[i, n, s] = export_subsidy_new0[i, n, s] - carbon_tax * embodied[i, s]`.
#' `tariff_new0` and `export_subsidy_new0` are the exogenous scenario values.
#' The tax, the border tariffs and the export rebates depend on prices, so the
#' solver updates them in every iteration and returns their equilibrium
#' values as `tax_new`, `tariff_new` and `export_subsidy_new`.
#'
#' **Inputs.** Besides the inputs of [caliendo_parro_2015()] (including
#' `productivity_change`, `population_change` and
#' `global_value_added_change`), the model reads these optional variables.
#' Put the policy variables into the `model_scenario`.
#' \describe{
#'   \item{`price`}{Initial price levels, dimensions: country x sector. Required for a carbon tax.}
#'   \item{`carbon_intensity`}{Carbon per unit of sector goods, dimension: sector. Required for a carbon tax.}
#'   \item{`tax`, `tax_new`}{Initial and exogenous new tax wedges, dimensions: country x sector (default 1).}
#'   \item{`carbon_tax`}{Carbon tax per unit of carbon, a number (default 0, no carbon policy).}
#'   \item{`countries_climate_club`}{Members of the climate club: a character
#'     vector of country codes, or a 0/1 (or logical) indicator by country, as
#'     a named vector or as a data.table with columns `country` and `value`
#'     (default: all countries).}
#'   \item{`cbam_sector`}{Sectors with border carbon adjustment, in the same
#'     forms with `sector` (default: all sectors).}
#'   \item{`scenario_carbon_tariff`}{`TRUE` for border carbon tariffs (default `FALSE`).}
#'   \item{`scenario_export_rebate`}{`TRUE` for export rebates (default `FALSE`).}
#' }
#' An unknown country or sector code in `countries_climate_club` or
#' `cbam_sector` is an error.
#'
#' The settings are those of [caliendo_parro_2015()]: `vfactor`,
#' `max_inner_iterations`, `trade_balance_rule` (`"fixed"`,
#' `"fixed_country_share"`, `"fixed_global_share"` or `"zero"`, see
#' [update_trade_balance()]), `tolerance_output`, `convergence_method`,
#' `require_inner_convergence` and `additional_output_variables`. As in
#' [caliendo_parro_2015()], world value added is the numeraire.
#'
#' @return List of list of new equilibrium `output`.
#'
#' @param input List of prepared initial and counterfactual conditions.
#' @param settings List of various settings and model dimensions
#'
#' @references
#' Mahlkow, H. and Wanner, J. (2021). The economic and environmental effects
#' of carbon border adjustments in a quantitative trade model. Unpublished
#' manuscript, Kiel Institute for the World Economy.
#'
#' @export

mahlkow_wanner_2021 = function (input, settings) {

  # initialize settings
  if (is.null(settings[['vfactor']])) settings[['vfactor']] = 0.1
  if (is.null(settings[['max_inner_iterations']])) settings[['max_inner_iterations']] = 5000L
  if (is.null(settings[['trade_balance_rule']])) settings[['trade_balance_rule']] = "fixed"
  if (is.null(settings[['tolerance_expenditure']])) settings[['tolerance_expenditure']] = settings[['tolerance']]
  if (is.null(settings[['tolerance_output']])) settings[['tolerance_output']] = settings[['tolerance']]
  if (is.null(settings[['convergence_method']])) settings[['convergence_method']] = "root_mean_square"
  if (is.null(settings[['require_inner_convergence']])) settings[['require_inner_convergence']] = TRUE

  # read carbon policy ----
  input = prepare_carbon_policy_mw_2021(input, settings[['model_dimensions']])

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

  # create tax arrays ----
  if (is.null(input[['tax']])) input[['tax']] = initialize_variable(settings[['model_dimensions']][c("country", "sector")])
  if (is.null(input[['tax_new']])) input[['tax_new']] = input[['tax']]
  input[['tax_change']] = input[['tax_new']] / input[['tax']]

  # exogenous instruments, before the carbon policy adds to them
  input[['tariff_new0']] = input[['tariff_new']]
  input[['export_subsidy_new0']] = input[['export_subsidy_new']]
  input[['tax_new0']] = input[['tax_new']]

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
  # generate trade flows; suppliers receive expenditure net of the tax
  input[['trade_flow']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  for (c in settings[['model_dimensions']]$country) {
    input[['trade_flow']][c,,] = input[['trade_share']][c,,] * (input[['expenditure']] / input[['tax']])
  }

  # generate income
  input[['income']] <-
    array_sum((input[['tariff']] - 1) * input[['trade_flow']] / input[['tariff']], 2) + # tariff_revenue
    array_sum((input[['export_subsidy']] - 1) * input[['trade_flow']] / (input[['tariff']] * input[['export_subsidy']]), 1) + # export_subsidy_cost
    input[['wage_change']] * input[['value_added']] +
    array_sum((input[['tax']] - 1) / input[['tax']] * input[['expenditure']], 1) - # tax revenue
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
  active_io = any(is.finite(input[['factor_share']]) & input[['factor_share']] < 1)

  criterion = 1
  h = 1
  while (h <= settings[['max_iterations']] &&
         (criterion > settings[['tolerance']] ||
          (settings[['require_inner_convergence']] && !inner_converged_flag))) {

    if (settings[['verbose']] >= 2L) cat(format(Sys.time()), ":", "Iteration", h, "\n")

    input[['wage_change0']] = input[['wage_change']]
    input[['price_change0']] = input[['price_change']]

    # update carbon tax, border carbon tariffs and export rebates ----
    if (input[['carbon_policy_active']]) {
      if (settings[['verbose']] >= 2L) cat("\r \u2014 Carbon policy update")
      policy = update_carbon_policy_mw_2021(input[['price_change']],
                                            input[['price']],
                                            input[['tax_new0']],
                                            input[['tariff_new0']],
                                            input[['export_subsidy_new0']],
                                            input[['carbon_tax']],
                                            input[['carbon_intensity']],
                                            input[['countries_climate_club']],
                                            input[['cbam_sector']],
                                            input[['scenario_carbon_tariff']],
                                            input[['scenario_export_rebate']],
                                            input[['factor_share']],
                                            input[['intermediate_share']],
                                            settings[['model_dimensions']])
      input[['tax_new']] = policy$tax_new
      input[['tariff_new']] = policy$tariff_new
      input[['export_subsidy_new']] = policy$export_subsidy_new
      input[['tax_change']] = input[['tax_new']] / input[['tax']]
      input[['tariff_change']] = input[['tariff_new']] / input[['tariff']]
      input[['export_subsidy_change']] = input[['export_subsidy_new']] / input[['export_subsidy']]
      input[['trade_cost_change']] = input[['tariff_change']] * input[['ntb_change']] * input[['export_subsidy_change']]
      if (settings[['verbose']] >= 2L) cat(" \u2713\n")
    }

    # update input cost ----
    if (settings[['verbose']] >= 2L) cat("\r \u2014 Input update")
    input[['input_cost_change']] = update_input_cost_mw_2021(input[['input_cost_change']],
                                                             input[['wage_change']],
                                                             input[['productivity_change']],
                                                             input[['price_change']],
                                                             input[['tax_change']],
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
    input[['output_income_new']] = update_output_income_mw_2021(input[['output_new']],
                                                                input[['income_new']],
                                                                input[['consumption_share']],
                                                                input[['input_share']],
                                                                input[['trade_share_new']],
                                                                input[['tariff_new']],
                                                                input[['export_subsidy_new']],
                                                                input[['tax_new']],
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

    # normalize wage change: world value added is the numeraire
    input[['wage_change']] = input[['wage_change']] * sum(input[['value_added']]) / sum(input[['value_added_new']])

    # update trade balance ----
    input[['trade_balance_new']] = update_trade_balance(input[['trade_balance']],
                                                        input[['value_added']],
                                                        input[['value_added_new']],
                                                        settings[['trade_balance_rule']])

    # prepare next iteration ----
    criterion_wage = check_convergence(input[['wage_change']], input[['wage_change0']], method = settings[['convergence_method']])
    if (active_io || input[['carbon_policy_active']]) {
      criterion_price = check_convergence(input[['price_change']], input[['price_change0']], method = settings[['convergence_method']])
      criterion = max(criterion_wage, criterion_price)
    } else {
      criterion = criterion_wage
    }
    change_list <- rbind(change_list, as.matrix(t(c(as.numeric(Sys.time()), criterion))))

    if (settings[['verbose']] == 1L) cli_status_update(id = status1, "{symbol[['arrow_right']]} Currently in iteration {h}.")
    if (settings[['verbose']] >= 2L) cat(" \u2014 Criterion =", sprintf("%.1e", criterion), "\n")

    h = h + 1

  }

  if (settings[['verbose']] >= 1L) cli_alert_success("Convergence after {h-1} iterations and {format_time_diff(timer_start, Sys.time())}")

  input[['criterion']] = criterion
  input[['iterations']] = h - 1

  # return
  result = output_variables(input, c(c("wage_change",
                                       "input_cost_change",
                                       "price_change",
                                       "trade_share_new",
                                       "expenditure_new",
                                       "value_added_new",
                                       "trade_balance_new",
                                       "tax_new",
                                       "tariff_new",
                                       "export_subsidy_new",
                                       "criterion",
                                       "iterations"),
                                     settings[["additional_output_variables"]]))
  attr(result, "inner_converged") = inner_converged_flag
  result
}


#' Read the carbon policy of Mahlkow and Wanner (2021)
#'
#' @description
#' Checks the carbon-policy inputs of [mahlkow_wanner_2021()] and stores them
#' in standard forms: `carbon_tax` as a number, `carbon_intensity` as a
#' vector by sector, `countries_climate_club` and `cbam_sector` as character
#' vectors of codes, the scenario switches as `TRUE` or `FALSE`, and
#' `carbon_policy_active`.
#'
#' @return List `input` with the standardized carbon-policy inputs.
#'
#' @param input List of prepared initial and counterfactual conditions.
#' @param model_dimensions List of model dimensions.
#'

prepare_carbon_policy_mw_2021 = function (input, model_dimensions) {

  countries = model_dimensions[['country']]
  sectors = model_dimensions[['sector']]

  # carbon tax: a single number
  carbon_tax = input[['carbon_tax']]
  if (is.null(carbon_tax)) carbon_tax = 0
  if (is.data.frame(carbon_tax)) carbon_tax = carbon_tax[['value']]
  if (!is.numeric(carbon_tax) || length(carbon_tax) != 1L || !is.finite(carbon_tax)) {
    cli_abort("{.var carbon_tax} must be a single finite number.")
  }
  input[['carbon_tax']] = as.numeric(carbon_tax)

  input[['countries_climate_club']] = normalize_selection_mw_2021(input[['countries_climate_club']],
                                                                  countries, "countries_climate_club", "country")
  input[['cbam_sector']] = normalize_selection_mw_2021(input[['cbam_sector']],
                                                       sectors, "cbam_sector", "sector")

  for (v in c("scenario_carbon_tariff", "scenario_export_rebate")) {
    x = input[[v]]
    if (is.null(x)) x = FALSE
    if (is.data.frame(x)) x = x[['value']]
    if (length(x) != 1L || is.na(x) || !(is.logical(x) || (is.numeric(x) && x %in% c(0, 1)))) {
      cli_abort("{.var {v}} must be TRUE or FALSE.")
    }
    input[[v]] = as.logical(x)
  }

  input[['carbon_policy_active']] = input[['carbon_tax']] != 0
  if (!input[['carbon_policy_active']]) return(input)

  # a carbon tax needs carbon intensities and initial price levels
  if (is.null(input[['carbon_intensity']])) {
    cli_abort("A {.var carbon_tax} needs {.var carbon_intensity} (dimension: sector).")
  }
  carbon_intensity = input[['carbon_intensity']]
  if (length(carbon_intensity) != length(sectors) || any(!is.finite(carbon_intensity))) {
    cli_abort("{.var carbon_intensity} must have one finite value for each of the {length(sectors)} sector{?s}.")
  }
  if (!is.null(names(carbon_intensity)) && !all(names(carbon_intensity) == sectors)) {
    carbon_intensity = carbon_intensity[sectors]
  }
  input[['carbon_intensity']] = array(as.numeric(carbon_intensity), dim = length(sectors),
                                      dimnames = list(sector = sectors))

  if (is.null(input[['price']])) {
    cli_abort("A {.var carbon_tax} needs the initial price levels {.var price} (dimensions: country x sector).")
  }
  if (!identical(dim(input[['price']]), c(length(countries), length(sectors))) ||
      any(!is.finite(input[['price']])) || any(input[['price']] <= 0)) {
    cli_abort("{.var price} must be positive and finite for each country x sector.")
  }

  input
}


#' Read a selection of countries or sectors for Mahlkow and Wanner (2021)
#'
#' @description
#' Converts a selection to a character vector of codes. A selection is a
#' character vector of codes, or a 0/1 (or logical) indicator, either as a
#' named vector or array or as a data.table with columns `<dimension>` and
#' `value`. `NULL` selects all codes.
#'
#' @return Character vector of the selected codes, in the order of `universe`.
#'
#' @param x Selection.
#' @param universe Character vector of all codes.
#' @param name Name of the input, for error messages.
#' @param dimension Name of the dimension, for error messages and data.tables.
#'

normalize_selection_mw_2021 = function (x, universe, name, dimension) {

  if (is.null(x)) return(universe)

  if (is.data.frame(x)) {
    if (!all(c(dimension, "value") %in% names(x))) {
      cli_abort("{.var {name}} as a table needs the columns {.val {dimension}} and {.val value}.")
    }
    values = x[['value']]
    names(values) = as.character(x[[dimension]])
    x = values
  }
  if (is.factor(x)) x = as.character(x)

  if (is.character(x)) {
    codes = unique(x)
  } else if (is.logical(x) || is.numeric(x)) {
    codes = names(x)
    if (is.null(codes) && is.array(x)) codes = dimnames(x)[[1]]
    if (is.null(codes)) {
      cli_abort("{.var {name}} as an indicator must be named by {dimension}.")
    }
    values = as.numeric(x)
    if (any(is.na(values)) || !all(values %in% c(0, 1))) {
      cli_abort("{.var {name}} as an indicator must be 0/1 or TRUE/FALSE.")
    }
    unknown = setdiff(codes, universe)
    if (length(unknown) > 0) {
      cli_abort("{.var {name}} contains the unknown {dimension} code{?s} {.val {unknown}}.")
    }
    codes = codes[values == 1]
  } else {
    cli_abort("{.var {name}} must be a character vector of codes or a 0/1 indicator by {dimension}.")
  }

  codes = codes[!is.na(codes)]
  unknown = setdiff(codes, universe)
  if (length(unknown) > 0) {
    cli_abort("{.var {name}} contains the unknown {dimension} code{?s} {.val {unknown}}.")
  }
  universe[universe %in% codes]
}


#' Update carbon tax, border carbon tariffs and export rebates for Mahlkow and Wanner (2021)
#'
#' @description
#' Computes the tax wedge of the carbon tax in the climate club, the border
#' carbon tariffs on imports of the club from non-members, and the export
#' rebates on exports of the club to non-members, at the current prices.
#' See [mahlkow_wanner_2021()] for the equations.
#'
#' @return List with `tax_new` (country x sector), `tariff_new` and
#'   `export_subsidy_new` (origin x destination x sector).
#'
#' @param price_change Matrix of price index changes, dimensions: country x sector.
#' @param price Matrix of initial price levels, dimensions: country x sector.
#' @param tax_new0 Matrix of exogenous new tax wedges, dimensions: country x sector.
#' @param tariff_new0 Array of exogenous new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy_new0 Array of exogenous new export subsidies, dimensions: origin x destination x sector.
#' @param carbon_tax Carbon tax per unit of carbon.
#' @param carbon_intensity Vector of carbon per unit of sector goods, dimension: sector.
#' @param countries_climate_club Character vector of club members.
#' @param cbam_sector Character vector of sectors with border carbon adjustment.
#' @param scenario_carbon_tariff `TRUE` for border carbon tariffs.
#' @param scenario_export_rebate `TRUE` for export rebates.
#' @param factor_share Matrix of factor shares, dimensions: country x sector.
#' @param intermediate_share Array of input-output coefficients, dimensions: country x input x output.
#' @param model_dimensions List of model dimensions.
#'

update_carbon_policy_mw_2021 = function (price_change,
                                         price,
                                         tax_new0,
                                         tariff_new0,
                                         export_subsidy_new0,
                                         carbon_tax,
                                         carbon_intensity,
                                         countries_climate_club,
                                         cbam_sector,
                                         scenario_carbon_tariff,
                                         scenario_export_rebate,
                                         factor_share,
                                         intermediate_share,
                                         model_dimensions) {

  countries = model_dimensions[['country']]
  members = countries[countries %in% countries_climate_club]
  non_members = countries[!countries %in% countries_climate_club]

  # carbon tax on the use of goods in the club
  price_new = pmax(price * price_change, .Machine$double.eps)
  tax_new = tax_new0
  for (c in members) tax_new[c,] = tax_new0[c,] + carbon_tax * carbon_intensity / price_new[c,]

  tariff_new = tariff_new0
  export_subsidy_new = export_subsidy_new0
  if ((scenario_carbon_tariff || scenario_export_rebate) &&
      length(members) > 0 && length(non_members) > 0 && length(cbam_sector) > 0) {

    # carbon embodied in the intermediate inputs per unit of output value
    embodied = initialize_variable(model_dimensions[c("country", "sector")], value = 0)
    for (c in countries) {
      embodied[c,] = (1 - factor_share[c,]) *
        as.numeric(crossprod(slice_matrix(intermediate_share, c), carbon_intensity / (tax_new[c,] * price_new[c,])))
    }

    for (s in cbam_sector) {
      # border carbon tariff on imports of members from non-members
      if (scenario_carbon_tariff) {
        for (d in members) tariff_new[non_members, d, s] = tariff_new0[non_members, d, s] + carbon_tax * embodied[non_members, s]
      }
      # export rebate on exports of members to non-members
      if (scenario_export_rebate) {
        for (d in non_members) export_subsidy_new[members, d, s] = export_subsidy_new0[members, d, s] - carbon_tax * embodied[members, s]
      }
    }
  }

  tax_new[tax_new <= 0] = .Machine$double.eps
  tariff_new[tariff_new <= 0] = .Machine$double.eps
  export_subsidy_new[export_subsidy_new <= 0] = .Machine$double.eps

  list(tax_new = tax_new,
       tariff_new = tariff_new,
       export_subsidy_new = export_subsidy_new)
}


#' Update input cost matrix for Mahlkow and Wanner (2021)
#'
#' @description
#' Updates cost of input bundle as in Caliendo & Parro (2015) equation (10),
#' with intermediate inputs at tax-inclusive prices.
#'
#' @return Matrix of changes in input costs, dimensions: country x sector.
#'
#' @param input_cost_change Matrix of input cost changes, dimensions: country x sector.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param productivity_change Matrix of change in productivity, dimensions: country x sector.
#' @param price_change Matrix of price index changes, dimensions: country x sector.
#' @param tax_change Matrix of tax wedge changes, dimensions: country x sector.
#' @param factor_share Matrix of input factor shares, dimensions: country x sector.
#' @param intermediate_share Array of input-output coefficients, dimensions: country x input x output.
#' @param model_dimensions List of model dimensions.
#'

update_input_cost_mw_2021 = function (input_cost_change,
                                      wage_change,
                                      productivity_change,
                                      price_change,
                                      tax_change,
                                      factor_share,
                                      intermediate_share,
                                      model_dimensions) {

  for (c in model_dimensions[['destination']]) input_cost_change[c,] = 1 / productivity_change[c,] * exp(factor_share[c,] * log(wage_change[c]) + (1 - factor_share[c,]) * (t(slice_matrix(intermediate_share, c)) %*% log(tax_change[c,] * price_change[c,])))

  return (input_cost_change)
}


#' Update output and income for Mahlkow and Wanner (2021)
#'
#' @description
#' Updates output and income as [update_output_income_cp_2015()], with
#' tax-inclusive expenditure: suppliers receive expenditure net of the tax
#' wedge, and the tax revenue is part of income.
#'
#' @return List with `income_new`, `output_new`, `expenditure_new`,
#'   `n_iterations_output_update` and `inner_converged`.
#'
#' @param output_new Matrix of new outputs, dimensions: country x sector.
#' @param income_new Vector of new incomes, dimension: country.
#' @param consumption_share Matrix of consumption shares, dimensions: country x sector.
#' @param input_share Array of beta-multiplied input-output coefficients, dimensions: country x input x output.
#' @param trade_share_new Array of new trade shares, dimensions: origin x destination x sector.
#' @param tariff_new Array of new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy_new Array of export subsidies, dimensions: origin x destination x sector.
#' @param tax_new Matrix of new tax wedges, dimensions: country x sector.
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

update_output_income_mw_2021 = function (output_new,
                                         income_new,
                                         consumption_share,
                                         input_share,
                                         trade_share_new,
                                         tariff_new,
                                         export_subsidy_new,
                                         tax_new,
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

    # update input purchases (country x input x output) and final demand (country x sector), both tax-inclusive
    input_purchases_new <- array_sweep(input_share, c("country", "output"), output_new, "*")
    final_demand_new = array_sweep(consumption_share, "country", income_new, "*")
    expenditure_new = final_demand_new + array_sum(input_purchases_new, c(1,2)) # sum over country x input

    # update trade flows; suppliers receive expenditure net of the tax
    trade_flow_new <- array_sweep(trade_share_new, c("destination", "sector"), expenditure_new / tax_new, "*")

    # update income
    income_new <- array_sum((tariff_new - 1) * trade_flow_new / tariff_new, 2) + # tariff_revenue
      array_sum((export_subsidy_new - 1) * trade_flow_new / (tariff_new * export_subsidy_new), 1) + # export_subsidy_cost
      population_change * wage_change * value_added +
      array_sum((tax_new - 1) / tax_new * expenditure_new, 1) - # tax revenue
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


#' Update expenditure for Mahlkow and Wanner (2021)
#'
#' @description
#' Solves for tax-inclusive expenditure as [update_expenditure_cp_2015()],
#' with suppliers receiving expenditure net of the tax wedge and the tax
#' revenue as part of income. [process_results()] uses it for the initial
#' expenditure.
#'
#' @return List with `expenditure_new` (country x sector),
#'   `n_iterations_expenditure_update` and `inner_converged`.
#'
#' @param expenditure_new Matrix of new expenditures, dimensions: country x sector.
#' @param consumption_share Matrix of consumption shares, dimensions: country x sector.
#' @param input_share Array of beta-multiplied input-output coefficients, dimensions: country x input x output.
#' @param trade_share_new Array of new trade shares, dimensions: origin x destination x sector.
#' @param tariff_new Array of new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy_new Array of export subsidies, dimensions: origin x destination x sector.
#' @param tax_new Matrix of new tax wedges, dimensions: country x sector.
#' @param value_added Vector of value added, dimension: country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param trade_balance_new Vector of new aggregate trade balance, dimension: country.
#' @param model_dimensions List of model dimensions.
#' @param tolerance Tolerance for inner convergence.
#' @param verbose Verbosity level.
#' @param convergence_method Method for convergence checking.
#' @param max_inner_iterations Maximum number of inner iterations before the loop bails out (default 5000).
#'

update_expenditure_mw_2021 = function (expenditure_new,
                                       consumption_share,
                                       input_share,
                                       trade_share_new,
                                       tariff_new,
                                       export_subsidy_new,
                                       tax_new,
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
      # slices as matrices, so that one-sector models keep their dimensions
      input_share_c = slice_matrix(input_share, c)
      trade_share_out = slice_matrix(trade_share_new, c)
      trade_share_in = slice_matrix(trade_share_new, c, 2)
      tariff_out = slice_matrix(tariff_new, c)
      tariff_in = slice_matrix(tariff_new, c, 2)
      export_subsidy_out = slice_matrix(export_subsidy_new, c)
      supplier_receipts = expenditure_new / tax_new # destination x sector
      expenditure_new[c,] = input_share_c %*% (t(supplier_receipts) %diag% (trade_share_out / (tariff_out * export_subsidy_out))) + # input
        consumption_share[c,] * (
          sum(((tariff_in - 1) * trade_share_in / tariff_in) %*% supplier_receipts[c,]) # new tariff revenue
          + sum(t(supplier_receipts) %diag% ((export_subsidy_out - 1) * trade_share_out / (tariff_out * export_subsidy_out))) # new export subsidy costs
          + sum((tax_new[c,] - 1) / tax_new[c,] * expenditure_new[c,]) # new tax revenue
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


#' Carbon policy outputs of Mahlkow and Wanner (2021)
#'
#' @description
#' Computes the outputs of [process_results()] that are specific to
#' [mahlkow_wanner_2021()]: tax revenue, the parts of tariff revenue and export
#' subsidy costs that are due to border carbon tariffs and export rebates,
#' carbon emissions and price levels.
#'
#' Emissions are `sum_s carbon_intensity[s] * X[n, s] / (tax[n, s] * price[n, s])`,
#' the carbon of all goods used in country `n`: expenditure net of the tax,
#' divided by the price level, is the quantity used. In a club member without
#' other taxes, the tax revenue is the carbon tax times the emissions.
#'
#' @return List `output` with the added variables.
#'
#' @param initial_conditions List of initial conditions as arrays.
#' @param model_scenario List of scenario variables as arrays, with the solved
#'   `tariff_new`, `export_subsidy_new` and `tax_new`.
#' @param output List of outputs as arrays.
#' @param tariff_new0 Array of exogenous new tariffs, dimensions: origin x destination x sector.
#' @param export_subsidy_new0 Array of exogenous new export subsidies, dimensions: origin x destination x sector.
#' @param supplier_expenditure_new Matrix of new expenditure net of the tax, dimensions: country x sector.
#' @param model_dimensions List of model dimensions.
#'

carbon_policy_outputs_mw_2021 = function (initial_conditions,
                                          model_scenario,
                                          output,
                                          tariff_new0,
                                          export_subsidy_new0,
                                          supplier_expenditure_new,
                                          model_dimensions) {

  countries = model_dimensions[['country']]
  sectors = model_dimensions[['sector']]

  # tax revenue ----
  output[['tax_revenue']] = array_sum((initial_conditions[['tax']] - 1) / initial_conditions[['tax']] * initial_conditions[['expenditure']], 1)
  output[['tax_revenue_new']] = array_sum((model_scenario[['tax_new']] - 1) / model_scenario[['tax_new']] * output[['expenditure_new']], 1)
  output[['tax_revenue_change']] = output[['tax_revenue_new']] / output[['tax_revenue']]
  output[['tax_revenue_change']][is.nan(output[['tax_revenue_change']])] = 1

  # border carbon tariff revenue (part of tariff_revenue_new) and export rebate costs (part of export_subsidy_costs_new) ----
  tariff_new = model_scenario[['tariff_new']]
  export_subsidy_new = model_scenario[['export_subsidy_new']]
  output[['cbam_revenue_new']] = array_sum((tariff_new - tariff_new0) / tariff_new * output[['trade_flow_new']], 2)
  output[['export_rebate_costs_new']] = array_sum((export_subsidy_new - export_subsidy_new0) * output[['trade_flow_new']] / (tariff_new * export_subsidy_new), 1)
  names(dimnames(output[['cbam_revenue_new']])) = "country"
  names(dimnames(output[['export_rebate_costs_new']])) = "country"

  # price levels and emissions ----
  value_of = function (name) {
    x = model_scenario[[name]]
    if (is.null(x)) x = initial_conditions[[name]]
    x
  }
  price = value_of("price")
  if (!is.null(price)) output[['price_new']] = price * output[['price_change']]

  carbon_intensity = value_of("carbon_intensity")
  if (!is.null(price) && !is.null(carbon_intensity) && length(carbon_intensity) == length(sectors)) {
    if (!is.null(names(carbon_intensity))) carbon_intensity = carbon_intensity[sectors]
    carbon_intensity = as.numeric(carbon_intensity)

    quantity = initial_conditions[['expenditure']] / (initial_conditions[['tax']] * price)
    quantity_new = supplier_expenditure_new / output[['price_new']]
    output[['emissions']] = array_sum(sweep(quantity, 2, carbon_intensity, "*"), 1)
    output[['emissions_new']] = array_sum(sweep(quantity_new, 2, carbon_intensity, "*"), 1)
    names(dimnames(output[['emissions']])) = "country"
    names(dimnames(output[['emissions_new']])) = "country"
    output[['emissions_change']] = output[['emissions_new']] / output[['emissions']]
    output[['emissions_change']][is.nan(output[['emissions_change']])] = 1
  }

  output
}
