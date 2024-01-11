#' Update equilibrium
#'
#' @description
#' `update_equilibrium()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
#'
#' @return List of list of data.tables with model, initial_conditions, model_scenario and output
#'
#' @param model Model specification to run, e.g. caliendo_parro_2015()
#' @param initial_conditions List of initial conditions, e.g. for GTAP 10.
#' @param model_scenario List of counterfactual conditions
#' @param settings List of settings
#'
#' @import cli
#' @import data.table
#'
#' @export

update_equilibrium = function (model = NULL,
                               initial_conditions = NULL,
                               model_scenario = NULL,
                               settings = NULL) {

  # check if valid input ----
  if (is.null(model)) {
    cli_alert_danger("'model' has to be specified.")
    return()
  }
  if (is.null(initial_conditions)) {
    cli_alert_danger("'initial_conditions' has to be specified.")
    return()
  }
  if (is.null(settings$max_iterations)) settings$max_iterations = 1000
  if (is.null(settings$tolerance)) settings$tolerance = 1e-4
  if (is.null(settings$vfactor)) settings$vfactor = 0.1
  if (is.null(settings$verbose)) settings$verbose = T

  # initializing variables ----
  if (settings$verbose >= 1L) cli_h1("Initializing variables")

  # model dimensions
  settings$model_dimensions = get_model_dimensions(initial_conditions)

  # generate inputs
  input = generate_input(initial_conditions, model_scenario, settings)

  # reshape inputs
  input = lapply(input, cast_variable)

  # run model ----
  output = model(input, settings)

  # polishing results ----
  if (settings$verbose >= 1L) cli_h1("Reshaping and returning results")

  # melt outcomes into data.table
  output = lapply(output, melt_variable)

  # return results
  if (settings$verbose >= 1L) cli_alert_success("Reshaping and returning results.")

  list(model = deparse(substitute(model)),
       initial_conditions = initial_conditions,
       model_scenario = model_scenario,
       output = output,
       settings = settings)
}
