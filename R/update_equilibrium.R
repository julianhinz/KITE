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
#' @param settings List of settings. `elasticity_convention` may be `"auto"`
#'   (the default, which converts legacy inverse/sign conventions with a
#'   warning), `"silent"`, or `"strict"`.
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
  if (is.null(settings[['require_inner_convergence']])) settings[['require_inner_convergence']] = TRUE
  if (is.null(settings[['elasticity_convention']])) settings[['elasticity_convention']] = "auto"

  # move elasticity variables into nested list if provided at top-level
  initial_conditions <- nest_elasticity_variables(initial_conditions)
  model_scenario <- nest_elasticity_variables(model_scenario)

  if (!is.null(initial_conditions[['elasticities']][['trade_elasticity']])) {
    initial_conditions[['elasticities']][['trade_elasticity']] =
      normalize_trade_elasticity(
        initial_conditions[['elasticities']][['trade_elasticity']],
        settings[['elasticity_convention']],
        "initial_conditions"
      )
  }
  if (!is.null(model_scenario[['elasticities']][['trade_elasticity']])) {
    model_scenario[['elasticities']][['trade_elasticity']] =
      normalize_trade_elasticity(
        model_scenario[['elasticities']][['trade_elasticity']],
        settings[['elasticity_convention']],
        "model_scenario"
      )
  }

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

  # a scenario variable that the model does not read has no effect; say so
  warn_unknown_scenario_variables(model_id, model_scenario)

  # run model ----
  raw_output = model(input, settings)

  # polishing results ----
  if (settings[['verbose']] >= 1L) cli_h1("Reshaping and returning results")

  # melt outcomes into data.table
  output_payload = raw_output
  output_payload[['inner_converged']] = NULL
  output = lapply(output_payload, melt_variable)

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

  metadata_fields = c("criterion", "iterations", "n_iterations", "inner_converged")
  economic_output = raw_output[setdiff(names(raw_output), metadata_fields)]
  output_finite = all(vapply(economic_output, function(v) {
    if (data.table::is.data.table(v) && "value" %in% names(v)) {
      return(all(is.finite(v[['value']])))
    }
    if (is.numeric(v)) return(all(is.finite(v)))
    TRUE
  }, logical(1)))

  # Claim convergence only when the outer criterion establishes it. Preserve
  # NA when that cannot be established, reject a required failed/unknown inner
  # solve, and always reject non-finite economic output.
  convergence = NA
  if (is.finite(criterion) && !is.null(settings[['tolerance']])) {
    convergence = criterion <= settings[['tolerance']]
  }
  if (isTRUE(settings[['require_inner_convergence']])) {
    inner_converged = raw_output[['inner_converged']]
    if (is.null(inner_converged)) inner_converged = attr(raw_output, "inner_converged")
    if (identical(inner_converged, FALSE)) {
      convergence = FALSE
    } else if (!isTRUE(inner_converged) && isTRUE(convergence)) {
      convergence = NA
    }
  }
  if (!output_finite) convergence = FALSE

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


# Scenario variables that a model reads. Each solver reads these from its
# input before it assigns them; process_results() reads no others from the
# scenario. A solver computes `tariff_change` and `export_subsidy_change`
# itself, so a scenario must set `tariff_new` and `export_subsidy_new`.
# `ntb_change` is read when it is set. Elasticities are nested under
# `elasticities` before the check. Returns NULL for a model outside the
# package.
kite_scenario_variables = function (model_id) {
  common = c("trade_share", "intermediate_share", "factor_share",
             "consumption_share", "value_added", "trade_balance",
             "elasticities",
             "tariff", "tariff_new",
             "ntb", "ntb_new", "ntb_change",
             "export_subsidy", "export_subsidy_new")
  switch(model_id,
         caliendo_parro_2015 = c(common, "expenditure",
                                 "productivity_change", "population_change",
                                 "global_value_added_change"),
         chowdhry_hinz_kamin_wanner_2022 = c(common, "coalition_member"),
         NULL)
}

# Warn (class `kite_unknown_scenario_variable`, fields `model` and
# `variables`) when `model_scenario` sets a variable that the model does not
# read. Such a variable has no effect on the run.
warn_unknown_scenario_variables = function (model_id, model_scenario) {
  if (!is.character(model_id) || length(model_id) != 1L || is.na(model_id)) return(invisible(NULL))
  known = kite_scenario_variables(model_id)
  if (is.null(known)) return(invisible(NULL))
  unknown = setdiff(names(model_scenario), c(known, ""))
  if (length(unknown) == 0L) return(invisible(NULL))

  hints = vapply(unknown, function (v) {
    # a scenario trade_balance replaces the baseline balance; the
    # counterfactual balance is not an input, so do not point to it
    if (v == "trade_balance_new") {
      return(paste0("`trade_balance_new`: not a scenario variable; the counterfactual ",
                    "trade balance follows `settings$trade_balance_rule`."))
    }
    base = sub("_(new|change)$", "", v)
    candidates = if (grepl("_change$", v)) paste0(base, c("_new", "")) else base
    candidates = candidates[candidates != v & candidates %in% known]
    if (length(candidates) > 0L) {
      sprintf("`%s`: set `%s` instead.", v, candidates[1])
    } else {
      sprintf("`%s`: not a scenario variable of this model.", v)
    }
  }, character(1))

  message = paste0(
    "Model \"", model_id, "\" does not use ",
    if (length(unknown) == 1L) "the scenario variable " else "the scenario variables ",
    paste0("`", unknown, "`", collapse = ", "),
    if (length(unknown) == 1L) "; it has no effect." else "; they have no effect.",
    paste0("\n* ", hints, collapse = "")
  )
  # base warning() with a condition object: cli_warn() with class and fields needs rlang, which is not in Imports
  warning(structure(class = c("kite_unknown_scenario_variable", "warning", "condition"),
                    list(message = message, call = NULL,
                         model = model_id, variables = unknown)))
  invisible(NULL)
}
