#' Update equilibrium
#'
#' @description
#' `update_equilibrium()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
#'
#' @return An S3-classed `kite_result` (also inheriting the model class, e.g.
#'   `caliendo_parro_2015` or `chowdhry_hinz_kamin_wanner_2022`) with elements:
#'   `model` (character id), `model_function` (the model function used),
#'   `initial_conditions`, `model_scenario`, `output` (named list of result
#'   tables produced by the model), `settings`, and an `info` block with
#'   `convergence` (TRUE / FALSE / NA), `criterion`, `iterations`, and
#'   `elapsed_seconds`. Use [process_results()] for downstream formatting.
#'
#' @param model Model specification to run, e.g. caliendo_parro_2015()
#' @param initial_conditions List of initial conditions.
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

  timer_start = Sys.time()

  # check if valid input ----
  if (is.null(model)) {
    cli_alert_danger("'model' has to be specified.")
    return()
  }
  if (is.null(initial_conditions)) {
    cli_alert_danger("'initial_conditions' has to be specified.")
    return()
  }
  if (is.null(model_scenario)) model_scenario = list()
  if (is.null(settings)) settings = list()
  if (is.null(settings[['max_iterations']])) settings[['max_iterations']] = 1000
  if (is.null(settings[['tolerance']])) settings[['tolerance']] = 1e-4
  if (is.null(settings[['vfactor']])) settings[['vfactor']] = 0.1
  if (is.null(settings[['verbose']])) settings[['verbose']] = 1L

  # move elasticity variables into nested list if provided at top-level
  initial_conditions <- nest_elasticity_variables(initial_conditions)
  model_scenario <- nest_elasticity_variables(model_scenario)

  # initializing variables ----
  if (settings[['verbose']] >= 1L) cli_h1("Initializing variables")

  # model dimensions
  settings[['model_dimensions']] = get_model_dimensions(initial_conditions)

  # generate inputs
  input = generate_input(initial_conditions, model_scenario, settings)

  # reshape inputs
  input = lapply(input, cast_variable)

  # resolve canonical model id for result classing and dispatch
  resolve_model_id = function(model, model_expr) {
    known_models = c("caliendo_parro_2015", "chowdhry_hinz_kamin_wanner_2022")

    for (nm in known_models) {
      if (exists(nm, mode = "function", inherits = TRUE) &&
          identical(model, get(nm, mode = "function", inherits = TRUE))) {
        return(nm)
      }
    }

    model_label = deparse(model_expr)
    if (length(model_label) > 1) model_label = model_label[1]
    model_label
  }

  model_id = resolve_model_id(model, substitute(model))

  # run model ----
  raw_output = model(input, settings)

  # polishing results ----
  if (settings[['verbose']] >= 1L) cli_h1("Reshaping and returning results")

  # melt outcomes into data.table
  output = lapply(raw_output, melt_variable)

  # convergence metadata (available for all models, detailed fields optional)
  extract_scalar = function(x) {
    if (is.null(x)) return(NA_real_)
    as.numeric(x)[1]
  }

  criterion = extract_scalar(raw_output[['criterion']])
  iterations = as.integer(extract_scalar(raw_output[['iterations']]))
  if (is.na(iterations)) {
    iterations = as.integer(extract_scalar(raw_output[['n_iterations']]))
  }
  elapsed_seconds = as.numeric(difftime(Sys.time(), timer_start, units = "secs"))

  # Convergence is TRUE only when the model returned a finite criterion below
  # tolerance. Without a finite criterion we can't claim convergence, so we
  # report NA rather than silently saying TRUE.
  convergence = NA
  if (is.finite(criterion) && !is.null(settings[['tolerance']])) {
    convergence = criterion <= settings[['tolerance']]
  }

  # return results
  if (settings[['verbose']] >= 1L) cli_alert_success("Reshaping and returning results.")

  results = list(model = model_id,
                 model_function = model,
                 initial_conditions = initial_conditions,
                 model_scenario = model_scenario,
                 output = output,
                 settings = settings,
                 info = list(convergence = convergence,
                             criterion = criterion,
                             iterations = iterations,
                             elapsed_seconds = elapsed_seconds))

  class(results) = c(model_id, "kite_result", "list")
  results
}
