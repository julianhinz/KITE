#' Process results for new equilibrium
#'
#' @description
#' Takes the results from `update_equilibrium()` and computes additional variables.
#' Uses S3 dispatch on the model-specific result class.
#'
#' @return List with `initial_conditions`, `model_scenario`, `output`, and `settings`.
#'
#' @param results List of results from `update_equilibrium()`.
#' @param ... Currently unused.
#'
#' @export
process_results = function(results, ...) {
  UseMethod("process_results")
}

#' @export
process_results.default = function(results, ...) {
  if (!is.list(results)) {
    stop("'results' must be a list returned by update_equilibrium().")
  }

  model_id = as.character(results[['model']])[1]
  if (is.na(model_id) || is.null(model_id) || model_id == "") model_id = "kite_result"

  class(results) = unique(c(model_id, "kite_result", class(results)))
  process_results(results, ...)
}

#' @export
process_results.kite_result = function(results, ...) {
  # Default fallback: try standard processing for CP2015-type models,
  # otherwise passthrough. Adding new model files with their own
  # process_results.<model> S3 method will take precedence over this.
  model_id = as.character(results[['model']])[1]
  # Models that share the standard result structure can use the standard processor.
  # If a new model file provides its own process_results.<model> S3 method,
  # it will take precedence over this fallback.
  standard_compatible = c("caliendo_parro_2015", "chowdhry_hinz_kamin_wanner_2022")
  if (model_id %in% standard_compatible) {
    return(.process_results_standard(results))
  }
  .process_results_passthrough(results)
}


#' @export
process_results.caliendo_parro_2015 = function(results, ...) .process_results_standard(results)

#' @export
process_results.chowdhry_hinz_kamin_wanner_2022 = function(results, ...) .process_results_standard(results)

.process_results_standard = function(results) {

  # setup
  model = results[['model']]
  settings = results[['settings']]

  # reshape data
  initial_conditions = lapply(results[['initial_conditions']], cast_variable)
  model_scenario = lapply(results[['model_scenario']], cast_variable)
  output = lapply(results[['output']], cast_variable)

  # create input share arrays ----
  initial_conditions[['input_share']] = initial_conditions[['intermediate_share']]
  for (c in settings[['model_dimensions']]$destination) {
    initial_conditions[['input_share']][c,,] = t((1 - initial_conditions[['factor_share']][c,]) * t(initial_conditions[['intermediate_share']][c,,]))
  }

  # create tariff arrays ----
  if (is.null(initial_conditions[['tariff']])) initial_conditions[['tariff']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  if (is.null(model_scenario[['tariff_new']])) model_scenario[['tariff_new']] = initial_conditions[['tariff']]
  model_scenario[['tariff_change']] = model_scenario[['tariff_new']] / initial_conditions[['tariff']]

  # create ntb arrays ----
  if (is.null(initial_conditions[['ntb']])) initial_conditions[['ntb']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  if (is.null(model_scenario[['ntb_new']])) model_scenario[['ntb_new']] = initial_conditions[['ntb']]
  if (is.null(model_scenario[['ntb_change']])) model_scenario[['ntb_change']] = model_scenario[['ntb_new']] / initial_conditions[['ntb']]

  # create export subsidies arrays ----
  if (is.null(initial_conditions[['export_subsidy']])) initial_conditions[['export_subsidy']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  if (is.null(model_scenario[['export_subsidy_new']])) model_scenario[['export_subsidy_new']] = initial_conditions[['export_subsidy']]
  model_scenario[['export_subsidy_change']] = model_scenario[['export_subsidy_new']] / initial_conditions[['export_subsidy']]

  # create trade balance arrays ----
  if (is.null(initial_conditions[['trade_balance']])) initial_conditions[['trade_balance']] = initialize_variable(settings[['model_dimensions']][c("country")])
  if (is.null(model_scenario[['trade_balance_new']])) model_scenario[['trade_balance_new']] = initial_conditions[['trade_balance']]

  # compute initial expenditure ----
  if (model == "chowdhry_hinz_kamin_wanner_2022" && is.null(initial_conditions[['expenditure']])) {
    initial_conditions[['transfer']] = initialize_variable(settings[['model_dimensions']][c("country")], value = 0)
    initial_conditions[['expenditure_result']] = update_expenditure_chkw_2022(
      initialize_variable(settings[['model_dimensions']][c("sector", "country")]),
      initial_conditions[['consumption_share']],
      initial_conditions[['input_share']],
      initial_conditions[['trade_share']],
      initial_conditions[['tariff']],
      initial_conditions[['export_subsidy']],
      initial_conditions[['value_added']],
      initialize_variable(settings[['model_dimensions']][c("country")]),
      initial_conditions[['trade_balance']],
      initial_conditions[['transfer']],
      settings[['model_dimensions']],
      settings[['tolerance']],
      verbose = FALSE
    )
    initial_conditions[['expenditure']] = t(initial_conditions[['expenditure_result']][['expenditure_new']])
    initial_conditions[['transfer']] = NULL
    initial_conditions[['expenditure_result']] = NULL
  } else {
    initial_conditions[['expenditure_result']] = update_expenditure_cp_2015(
      initialize_variable(settings[['model_dimensions']][c("country", "sector")]),
      initial_conditions[['consumption_share']],
      initial_conditions[['input_share']],
      initial_conditions[['trade_share']],
      initial_conditions[['tariff']],
      initial_conditions[['export_subsidy']],
      initial_conditions[['value_added']],
      initialize_variable(settings[['model_dimensions']][c("country")]),
      initial_conditions[['trade_balance']],
      settings[['model_dimensions']],
      settings[['tolerance']],
      verbose = FALSE,
      convergence_method = "root_mean_square"
    )
    initial_conditions[['expenditure']] = initial_conditions[['expenditure_result']][['expenditure_new']]
    initial_conditions[['expenditure_result']] = NULL
  }

  # compute new trade flows ----
  output[['trade_flow']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  for (c in settings[['model_dimensions']]$country) {
    output[['trade_flow']][c,,] = initial_conditions[['trade_share']][c,,] * initial_conditions[['expenditure']]
  }

  output[['trade_flow_new']] = initialize_variable(settings[['model_dimensions']][c("origin", "destination", "sector")])
  for (c in settings[['model_dimensions']]$country) output[['trade_flow_new']][c,,] = output[['trade_share_new']][c,,] * output[['expenditure_new']]

  output[['trade_flow_change']] = output[['trade_flow_new']] / output[['trade_flow']]

  # compute trade flow fob ----
  output[['trade_flow_fob']] = output[['trade_flow']] / initial_conditions[['tariff']]
  output[['trade_flow_fob_new']] = output[['trade_flow_new']] / model_scenario[['tariff_new']]
  output[['trade_flow_fob_change']] = output[['trade_flow_fob_new']] / output[['trade_flow_fob']]

  # compute exports and imports ----
  output[['exports']] = vector_to_array(apply(output[['trade_flow_fob']], 1, sum), "country")
  output[['exports_new']] = vector_to_array(apply(output[['trade_flow_fob_new']], 1, sum), "country")
  output[['exports_change']] = output[['exports_new']] / output[['exports']]

  output[['imports']] = vector_to_array(apply(output[['trade_flow_fob']], 2, sum), "country")
  output[['imports_new']] = vector_to_array(apply(output[['trade_flow_fob_new']], 2, sum), "country")
  output[['imports_change']] = output[['imports_new']] / output[['imports']]

  get_country_sector_vector <- function(x, country) {
    if (!is.array(x) || length(dim(x)) != 2) return(x[country, ])
    rn <- rownames(x)
    cn <- colnames(x)
    if (!is.null(rn) && country %in% rn) return(x[country, ])
    if (!is.null(cn) && country %in% cn) return(x[, country])
    x[country, ]
  }

  # compute price index change ----
  output[['price_index_change']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) {
    output[['price_index_change']][[c]] = exp(sum(initial_conditions[['consumption_share']][c,] * log(get_country_sector_vector(output[['price_change']], c))))
  }

  # compute trade share change ----
  output[['trade_share_change']] = output[['trade_share_new']] / initial_conditions[['trade_share']]

  # compute tariff revenue ----
  output[['tariff_revenue']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country)  output[['tariff_revenue']][[c]] =
    sum(((initial_conditions[['tariff']][,c,] - 1) * initial_conditions[['trade_share']][,c,] / initial_conditions[['tariff']][,c,]) %*% initial_conditions[['expenditure']][c,])

  output[['tariff_revenue_new']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country)  output[['tariff_revenue_new']][[c]] =
    sum(((model_scenario[['tariff_new']][,c,] - 1) * output[['trade_share_new']][,c,] / model_scenario[['tariff_new']][,c,]) %*% output[['expenditure_new']][c,])

  # compute export subsidy costs ----
  output[['export_subsidy_costs']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['export_subsidy_costs']][[c]] =
    sum(initial_conditions[['expenditure']] * ((initial_conditions[['export_subsidy']][c,,] - 1) * initial_conditions[['trade_share']][c,,] / (initial_conditions[['tariff']][c,,] * initial_conditions[['export_subsidy']][c,,])))

  output[['export_subsidy_costs_new']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['export_subsidy_costs_new']][[c]] =
    sum(output[['expenditure_new']] * ((model_scenario[['export_subsidy_new']][c,,] - 1) * output[['trade_share_new']][c,,] / (model_scenario[['tariff_new']][c,,] * model_scenario[['export_subsidy_new']][c,,])))

  output[['export_subsidy_costs_change']] = output[['export_subsidy_costs_new']] / output[['export_subsidy_costs']]
  output[['export_subsidy_costs_change']][is.nan(output[['export_subsidy_costs_change']])] = 1

  # compute income ----
  output[['income']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['income']][[c]] =
    initial_conditions[['value_added']][c] + output[['tariff_revenue']][c] + output[['export_subsidy_costs']][c] - initial_conditions[['trade_balance']][c]

  output[['income_new']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['income_new']][[c]] =
    initial_conditions[['value_added']][c] * output[['wage_change']][c] + output[['tariff_revenue_new']][c] + output[['export_subsidy_costs_new']][c] - model_scenario[['trade_balance_new']][c]

  output[['income_change']] = output[['income_new']] / output[['income']]

  # compute production ----
  output[['production']] = initialize_variable(settings[['model_dimensions']][c("country", "sector")])
  for (c in settings[['model_dimensions']]$country) output[['production']][c,] =
    t(initial_conditions[['expenditure']]) %diag% (initial_conditions[['trade_share']][c,,] / (initial_conditions[['tariff']][c,,] * initial_conditions[['export_subsidy']][c,,]))

  output[['production_new']] = initialize_variable(settings[['model_dimensions']][c("country", "sector")])
  for (c in settings[['model_dimensions']]$country) output[['production_new']][c,] =
    t(output[['expenditure_new']]) %diag% (output[['trade_share_new']][c,,] / (model_scenario[['tariff_new']][c,,] * model_scenario[['export_subsidy_new']][c,,]))

  output[['production_change']] = output[['production_new']] / output[['production']]

  output[['production_total']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['production_total']][[c]] = sum(output[['production']][c,])

  output[['production_total_new']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['production_total_new']][[c]] = sum(output[['production_new']][c,])

  output[['production_total_change']] = output[['production_total_new']] / output[['production_total']]

  output[['production_real_new']] = output[['production_new']] / output[['price_change']]
  output[['production_real_change']] = output[['production_real_new']] / output[['production']]

  output[['production_total_real_new']] = output[['production_total_new']] / output[['price_index_change']]
  output[['production_total_real_change']] = output[['production_total_real_new']] / output[['production_total']]

  # compute value added prime & hat ----
  output[['value_added_new']] = output[['wage_change']] * initial_conditions[['value_added']]

  # compute wage ----
  output[['wage_change_real']] = initialize_variable(settings[['model_dimensions']][c("country")])
  for (c in settings[['model_dimensions']]$country) output[['wage_change_real']][[c]] =
    exp(sum(initial_conditions[['consumption_share']][c,] * log(output[['wage_change']][c] / output[['price_index_change']][c])))

  # compute welfare ----
  output[['welfare_change']] = output[['income_change']] / output[['price_index_change']]

  # melt output ----
  output = lapply(output, melt_variable)

  # keep backward-compatible welfare table schema used by tests and examples
  if (!is.null(output[['welfare_change']]) &&
      data.table::is.data.table(output[['welfare_change']]) &&
      "value" %in% names(output[['welfare_change']])) {
    output[['welfare_change']][, welfare_change := value]
    if (!"weight" %in% names(output[['welfare_change']])) {
      weight = as.numeric(initial_conditions[['value_added']])
      names(weight) = settings[['model_dimensions']][['country']]
      weight = weight / sum(weight)
      output[['welfare_change']][, weight := weight[country]]
    }
  }

  # expects transpose dimensionality, temporary fix for #2
  initial_conditions[['expenditure']] <- t(initial_conditions[['expenditure']])

  # return results ----
  list(initial_conditions = initial_conditions,
       model_scenario = model_scenario,
       output = output,
       settings = settings)
}

.process_results_passthrough = function(results) {
  list(initial_conditions = results[['initial_conditions']],
       model_scenario = results[['model_scenario']],
       output = results[['output']],
       settings = results[['settings']])
}
