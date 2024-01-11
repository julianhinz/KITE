#' Process results for new equilibrium
#'
#' @description
#' Takes the results from the updated equilibrium and computes additional variables.
#'
#' @return List of data.tables with additional variables
#'
#' @param results List of results from update_equilibrium.
#'
#' @export

process_results = function(results){

  # reshape results ----
  model = results$model
  initial_conditions = lapply(results$initial_conditions, cast_variable)
  model_scenario = lapply(results$model_scenario, cast_variable)
  output = lapply(results$output, cast_variable)
  settings = results$settings

  # create tariff arrays ----
  if (is.null(initial_conditions$tariff)) initial_conditions$tariff = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(model_scenario$tariff_new)) model_scenario$tariff_new = initial_conditions$tariff
  model_scenario$tariff_change = model_scenario$tariff_new / initial_conditions$tariff

  # create ntb arrays ----
  if (is.null(initial_conditions$ntb)) initial_conditions$ntb = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(model_scenario$ntb_new)) model_scenario$ntb_new = initial_conditions$ntb
  if (is.null(model_scenario$ntb_change)) model_scenario$ntb_change = model_scenario$ntb_new / initial_conditions$ntb

  # create export subsidies arrays ----
  if (is.null(initial_conditions$export_subsidy)) initial_conditions$export_subsidy = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(model_scenario$export_subsidy_new)) model_scenario$export_subsidy_new = initial_conditions$export_subsidy
  model_scenario$export_subsidy_change = model_scenario$export_subsidy_new / initial_conditions$export_subsidy

  # create trade balance arrays ----
  if (is.null(initial_conditions$trade_balance)) initial_conditions$trade_balance = initialize_variable(settings$model_dimensions[c("country")])
  if (is.null(model_scenario$trade_balance_new)) model_scenario$trade_balance_new = initial_conditions$trade_balance

  # compute initial expenditure if necessary ----
  if (model == "caliendo_parro_2015") {
    if (is.null(initial_conditions$expenditure)) {
      initial_conditions$expenditure = update_expenditure_cp_2015(initialize_variable(settings$model_dimensions[c("sector", "country")]),
                                                                  initial_conditions$consumption_share,
                                                                  initial_conditions$input_share,
                                                                  initial_conditions$trade_share,
                                                                  initial_conditions$tariff,
                                                                  initial_conditions$export_subsidy,
                                                                  initial_conditions$value_added,
                                                                  initialize_variable(settings$model_dimensions[c("country")]),
                                                                  initial_conditions$trade_balance,
                                                                  settings$model_dimensions,
                                                                  settings$tolerance,
                                                                  verbose = F)
    }
  }

  # compute new trade flows ----
  output$trade_flow = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  for (c in settings$model_dimensions$country) output$trade_flow[c,,] = initial_conditions$trade_share[c,,] * t(initial_conditions$expenditure)

  output$trade_flow_new = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  for (c in settings$model_dimensions$country) output$trade_flow_new[c,,] = output$trade_share_new[c,,] * t(output$expenditure_new)

  output$trade_flow_change = output$trade_flow_new / output$trade_flow

  # compute trade flow fob ----
  output$trade_flow_fob = output$trade_flow / initial_conditions$tariff

  output$trade_flow_fob_new = output$trade_flow_new / model_scenario$tariff_new

  output$trade_flow_fob_change = output$trade_flow_fob_new / output$trade_flow_fob

  # compute exports and imports ----
  output$exports = vector_to_array(apply(output$trade_flow_fob,1,sum), "country")

  output$exports_new = vector_to_array(apply(output$trade_flow_fob_new,1,sum), "country")

  output$exports_change = output$exports_new / output$exports

  output$imports = vector_to_array(apply(output$trade_flow_fob,2,sum), "country")

  output$imports_new = vector_to_array(apply(output$trade_flow_fob_new,2,sum), "country")

  output$imports_change = output$imports_new / output$imports

  # compute price index change ----
  output$price_index_change = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$price_index_change[[c]] =  exp(sum(initial_conditions$consumption_share[,c] * log(output$price_change)[,c]))

  # compute trade share / flow change ----
  output$trade_share_change = output$trade_share_new / initial_conditions$trade_share

  output$trade_flow = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  for (c in settings$model_dimensions$country) output$trade_flow[c,,] = initial_conditions$trade_share[c,,] * t(initial_conditions$expenditure)

  output$trade_flow_change = output$trade_flow_new / output$trade_flow

  # compute tariff revenue ----
  output$tariff_revenue = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country)  output$tariff_revenue[[c]] =
    sum(((initial_conditions$tariff[,c,] - 1) * initial_conditions$trade_share[,c,] / initial_conditions$tariff[,c,]) %*% initial_conditions$expenditure[,c])

  output$tariff_revenue_new = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country)  output$tariff_revenue_new[[c]] =
    sum(((model_scenario$tariff_new[,c,] - 1) * output$trade_share_new[,c,] / model_scenario$tariff_new[,c,]) %*% output$expenditure_new[,c])

  output$tariff_revenue_change = output$tariff_revenue_new / output$tariff_revenue
  output$tariff_revenue_change[is.nan(output$tariff_revenue_change)] = 1

  # compute export subsidy costs ----
  output$export_subsidy_costs = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$export_subsidy_costs[[c]] =
    sum(initial_conditions$expenditure %diag% ((initial_conditions$export_subsidy[c,,] - 1) * initial_conditions$trade_share[c,,] / (initial_conditions$tariff[c,,] * initial_conditions$export_subsidy[c,,])))

  output$export_subsidy_costs_new = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$export_subsidy_costs_new[[c]] =
    sum(output$expenditure_new %diag% ((model_scenario$export_subsidy_new[c,,] - 1) * output$trade_share_new[c,,] / (model_scenario$tariff_new[c,,] * model_scenario$export_subsidy_new[c,,])))

  output$export_subsidy_costs_change = output$export_subsidy_costs_new / output$export_subsidy_costs
  output$export_subsidy_costs_change[is.nan(output$export_subsidy_costs_change)] = 1

  # compute income ----
  output$income = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$income[[c]] =
    initial_conditions$value_added[c] + output$tariff_revenue[c] + output$export_subsidy_costs[c] - initial_conditions$trade_balance[c]

  output$income_new = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$income_new[[c]] =
    initial_conditions$value_added[c] * output$wage_change[c] + output$tariff_revenue_new[c] + output$export_subsidy_costs_new[c] - model_scenario$trade_balance_new[c]

  output$income_change = output$income_new / output$income

  # compute production ----
  # domestic production values (fob - what producers actually get)
  output$production = initialize_variable(settings$model_dimensions[c("sector", "country")])
  for (c in settings$model_dimensions$country) output$production[,c] =
    initial_conditions$expenditure %diag% (initial_conditions$trade_share[c,,] / (initial_conditions$tariff[c,,] * initial_conditions$export_subsidy[c,,])) # Note: is this correct?

  output$production_new = initialize_variable(settings$model_dimensions[c("sector", "country")])
  for (c in settings$model_dimensions$country) output$production_new[,c] =
    output$expenditure_new %diag% (output$trade_share_new[c,,] / (model_scenario$tariff_new[c,,] * model_scenario$export_subsidy_new[c,,])) # Note: is this correct?

  output$production_change = output$production_new / output$production

  output$production_total = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$production_total[[c]] = sum(output$production[,c])

  output$production_total_new = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$production_total_new[[c]] = sum(output$production_new[,c])

  output$production_total_change = output$production_total_new / output$production_total

  output$production_real_new = output$production_new / output$price_change

  output$production_real_change = output$production_real_new / output$production

  output$production_total_real_new = output$production_total_new / output$price_index_change

  output$production_total_real_change = output$production_total_real_new / output$production_total

  # compute value added prime & hat ----
  output$value_added_new = output$wage_change * initial_conditions$value_added

  # compute wage ----
  output$wage_change_real = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$wage_change_real[[c]] =
    exp(sum(initial_conditions$consumption_share[,c] * log(output$wage_change[c] / output$price_index_change[c])))

  # w_hat = value_added # Note: how to name this variable?
  # for (c in model_attributes$countries) w_hat[c] = exp(sum(consumption_share[,c] * log(wage_change[c])))

  # compute welfare ----
  output$welfare_change = output$income_change / output$price_index_change

  # # compute import flows ----
  # import_flow_change = trade_flow_new
  # for (c in model_attributes$countries){
  #   import_flow_change[,c,] =
  #     ((trade_flow_new[,c,] / price_index_change[c]) / trade_flow[,c,] - 1) * 100
  # }
  #
  # import_flow_difference = trade_flow_new
  # for (c in model_attributes$countries){
  #   import_flow_difference[,c,] = (
  #     ((trade_flow_new[,c,] / price_index_change[c])
  #      / sum(trade_flow_new[,c,] / price_index_change[c]))
  #     - ((trade_flow[,c,] / price_index_change[c])
  #        / sum(trade_flow[,c,] / price_index_change[c]))) * 100
  # }
  #
  # import_flow_difference_absolut = trade_flow_new
  # for (c in model_attributes$countries){
  #   import_flow_difference_absolut[,c,] =
  #     (trade_flow_new[,c,] / price_index_change[c]) - (trade_flow[,c,] / price_index_change[c])
  # }
  #
  # # compute export flows ----
  # export_flow_change = trade_flow_new_fob
  # for (c in model_attributes$countries){
  #   export_flow_change[,c,] =
  #     ((trade_flow_new_fob[,c,] / price_index_change[c]) / (trade_flow[,c,] / tariff[,c,]) - 1) * 100
  # }
  #
  # export_flow_difference = trade_flow_new
  # for (c in model_attributes$countries){
  #   export_flow_difference[,c,] =
  #     ((trade_flow_new_fob[,c,] / sum(trade_flow_new_fob[,c,]))
  #      - ((trade_flow[,c,] / tariff[,c,]) / sum(trade_flow[,c,] / tariff[,c,]))) * 100
  # }
  #
  # export_flow_difference_absolut = trade_flow_new
  # for (c in model_attributes$countries){
  #   export_flow_difference_absolut[,c,] = (trade_flow_new_fob[,c,] / price_index_change[c])
  #   - trade_flow[,c,]
  # }

  # melt output ----
  output = lapply(output, melt_variable)

  # return results ----
  list(initial_conditions = initial_conditions,
       model_scenario = model_scenario,
       output = output,
       settings = settings)
}
