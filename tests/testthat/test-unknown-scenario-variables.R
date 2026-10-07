library(testthat)
library(data.table)

# A model_scenario entry that the model does not read has no effect. Before,
# update_equilibrium() accepted it silently (e.g. coalition_member_new gave
# zero coalition transfers). It now warns with class
# kite_unknown_scenario_variable.

unknown_settings <- list(verbose = 0L, tolerance = 1e-8, vfactor = 0.1,
                         max_iterations = 2000L)

unknown_tariff <- function(ic) {
  tariff_new <- copy(ic$tariff)
  tariff_new[origin == "c2" & destination != "c2", value := 1.3]
  tariff_new
}

catch_unknown <- function(expr) {
  caught <- NULL
  value <- withCallingHandlers(expr, kite_unknown_scenario_variable = function(w) {
    caught <<- w
    invokeRestart("muffleWarning")
  })
  list(value = value, warning = caught)
}

test_that("CHKW warns on coalition_member_new and points to coalition_member", {
  ic <- make_fixture(seed = 401L)
  coalition <- copy(ic$coalition_member)[, value := as.integer(country %in% c("c1", "c3"))]
  scenario <- list(tariff_new = unknown_tariff(ic), coalition_member_new = coalition)

  expect_warning(
    update_equilibrium(chowdhry_hinz_kamin_wanner_2022, ic, scenario, unknown_settings),
    class = "kite_unknown_scenario_variable"
  )
  run <- catch_unknown(update_equilibrium(chowdhry_hinz_kamin_wanner_2022, ic, scenario,
                                          unknown_settings))
  w <- run$warning
  expect_identical(w$model, "chowdhry_hinz_kamin_wanner_2022")
  expect_identical(w$variables, "coalition_member_new")
  expect_match(conditionMessage(w), "chowdhry_hinz_kamin_wanner_2022", fixed = TRUE)
  expect_match(conditionMessage(w), "set `coalition_member` instead", fixed = TRUE)

  # the run itself is unchanged: the variable has no effect
  plain <- update_equilibrium(chowdhry_hinz_kamin_wanner_2022, ic,
                              list(tariff_new = scenario$tariff_new), unknown_settings)
  expect_identical(run$value$output, plain$output)
  expect_true(all(run$value$output$transfer$value == 0))
})

test_that("tariff_change and export_subsidy_change warn and point to the _new form", {
  ic <- make_fixture(seed = 402L)
  change <- copy(ic$tariff)[origin != destination, value := 1.2]
  for (model in c("caliendo_parro_2015", "chowdhry_hinz_kamin_wanner_2022")) {
    run <- catch_unknown(update_equilibrium(get(model), ic,
                                            list(tariff_change = change,
                                                 export_subsidy_change = change),
                                            unknown_settings))
    expect_false(is.null(run$warning), info = model)
    expect_identical(run$warning$model, model)
    expect_setequal(run$warning$variables, c("tariff_change", "export_subsidy_change"))
    expect_match(conditionMessage(run$warning), "set `tariff_new` instead", fixed = TRUE)
    expect_match(conditionMessage(run$warning), "set `export_subsidy_new` instead", fixed = TRUE)
  }
})

test_that("a variable without a base name is reported as not a scenario variable", {
  ic <- make_fixture(seed = 403L)
  run <- catch_unknown(update_equilibrium(chowdhry_hinz_kamin_wanner_2022, ic,
                                          list(population_change = ic$value_added),
                                          unknown_settings))
  expect_identical(run$warning$variables, "population_change")
  expect_match(conditionMessage(run$warning), "not a scenario variable of this model",
               fixed = TRUE)
})

test_that("trade_balance_new points to settings$trade_balance_rule, not trade_balance", {
  ic <- make_fixture(seed = 406L)
  for (model in c("caliendo_parro_2015", "chowdhry_hinz_kamin_wanner_2022")) {
    run <- catch_unknown(update_equilibrium(get(model), ic,
                                            list(trade_balance_new = ic$trade_balance),
                                            unknown_settings))
    expect_identical(run$warning$variables, "trade_balance_new", info = model)
    expect_match(conditionMessage(run$warning), "settings$trade_balance_rule", fixed = TRUE)
    expect_no_match(conditionMessage(run$warning), "set `trade_balance` instead", fixed = TRUE)
  }
})

test_that("variables that the models read do not warn", {
  ic <- make_fixture(seed = 404L, coalition_members = c("c1", "c3"))
  ones_cs <- copy(ic$value_added)[, value := 1]
  common <- c(ic[setdiff(names(ic), c("coalition_member", "expenditure"))],
              list(tariff_new = unknown_tariff(ic), ntb_new = ic$ntb,
                   ntb_change = ic$ntb, export_subsidy_new = ic$export_subsidy))
  scenarios <- list(
    caliendo_parro_2015 = c(common, list(
      expenditure = ic$expenditure,
      productivity_change = copy(ic$factor_share)[, value := 1],
      population_change = ones_cs,
      global_value_added_change = ones_cs)),
    chowdhry_hinz_kamin_wanner_2022 = c(common, list(
      coalition_member = ic$coalition_member)),
    mahlkow_wanner_2021 = c(common, list(
      expenditure = ic$expenditure,
      productivity_change = copy(ic$factor_share)[, value := 1],
      population_change = ones_cs,
      global_value_added_change = ones_cs,
      tax = copy(ic$factor_share)[, value := 1],
      tax_new = copy(ic$factor_share)[, value := 1],
      price = copy(ic$factor_share)[, value := 1],
      carbon_tax = 0.5,
      carbon_intensity = data.table(sector = unique(ic$factor_share$sector), value = 0.1),
      countries_climate_club = "c1",
      cbam_sector = unique(ic$factor_share$sector)[1],
      scenario_carbon_tariff = TRUE,
      scenario_export_rebate = TRUE))
  )
  for (model in names(scenarios)) {
    expect_no_warning(
      update_equilibrium(get(model), ic, scenarios[[model]], unknown_settings),
      class = "kite_unknown_scenario_variable"
    )
  }
  # a legacy top-level trade_elasticity is nested under elasticities first
  expect_no_warning(
    update_equilibrium(caliendo_parro_2015, ic,
                       list(trade_elasticity = ic$elasticities$trade_elasticity),
                       unknown_settings),
    class = "kite_unknown_scenario_variable"
  )
})

test_that("the known scenario variables are the inputs a solver reads before it assigns them", {
  first_use_is_read <- function(lines, variable) {
    pattern <- paste0('input[["', variable, '"]]')
    line <- lines[grepl(pattern, lines, fixed = TRUE)][1]
    assignment <- paste0("^\\s*input\\[\\[\"", variable, "\"\\]\\]\\s*(=|<-)")
    if (!grepl(assignment, line)) return(TRUE)
    rhs <- sub(assignment, "", line)
    grepl(pattern, rhs, fixed = TRUE)
  }
  for (model in c("caliendo_parro_2015", "chowdhry_hinz_kamin_wanner_2022",
                   "mahlkow_wanner_2021")) {
    lines <- deparse(body(get(model)), width.cutoff = 500L)
    # mahlkow_wanner_2021 reads its carbon policy in a helper before its body
    if (model == "mahlkow_wanner_2021") {
      lines <- c(deparse(body(KITE:::prepare_carbon_policy_mw_2021), width.cutoff = 500L), lines)
    }
    used <- unique(regmatches(lines, gregexpr('input\\[\\["[A-Za-z0-9_.]+"\\]\\]', lines)))
    used <- unique(gsub('^input\\[\\["|"\\]\\]$', "", unlist(used)))
    read <- used[vapply(used, function(v) first_use_is_read(lines, v), logical(1))]
    expect_true(length(read) > 5L, info = model)
    expect_setequal(read, KITE:::kite_scenario_variables(model))
  }
})

test_that("models outside the package are not checked", {
  ic <- make_fixture(seed = 405L)
  custom_model <- function(input, settings) caliendo_parro_2015(input, settings)
  expect_no_warning(
    update_equilibrium(custom_model, ic, list(anything_new = ic$value_added),
                       unknown_settings),
    class = "kite_unknown_scenario_variable"
  )
})
