library(testthat)
library(data.table)

named_values <- function(x, key = "country", labels = NULL) {
  if (data.table::is.data.table(x)) {
    out <- x$value
    names(out) <- x[[key]]
    return(out)
  }
  out <- as.numeric(x)
  output_names <- names(x)
  if (is.null(output_names) && is.array(x)) output_names <- dimnames(x)[[1]]
  if (is.null(output_names)) output_names <- labels
  names(out) <- output_names
  out
}

run_cp_for_results <- function(initial_conditions, model_scenario = list(), settings = list()) {
  update_equilibrium(
    model = caliendo_parro_2015,
    initial_conditions = initial_conditions,
    model_scenario = model_scenario,
    settings = utils::modifyList(
      list(verbose = 0L, tolerance = 1e-6, vfactor = 0.1,
           max_iterations = 500L),
      settings
    )
  )
}

test_that("process_results uses the solver-returned trade balance", {
  ic <- make_fixture(seed = 101L)
  expect_true(any(abs(ic$trade_balance$value) > 1e-8))

  result <- run_cp_for_results(
    ic,
    settings = list(trade_balance_rule = "zero")
  )
  processed <- process_results(result)

  solved_balance <- named_values(result$output$trade_balance_new,
                                 labels = sort(attr(ic, "countries"), method = "radix"))
  baseline_balance <- named_values(ic$trade_balance)
  expect_true(any(abs(solved_balance - baseline_balance[names(solved_balance)]) > 1e-8))

  expected_income <-
    named_values(ic$value_added) * named_values(result$output$wage_change) +
    named_values(processed$output$tariff_revenue_new) +
    named_values(processed$output$export_subsidy_costs_new) -
    solved_balance

  expect_equal(named_values(processed$output$income_new), expected_income,
               tolerance = 1e-10)
})

test_that("tariff_revenue_change is retained and normalizes zero over zero", {
  processed <- process_results(run_cp_for_results(make_fixture(seed = 102L)))

  expect_true("tariff_revenue_change" %in% names(processed$output))
  expect_equal(unname(named_values(processed$output$tariff_revenue_change)),
               rep(1, length(named_values(processed$output$tariff_revenue_change))))
})

test_that("process_results preserves solver value added and reports per-capita welfare", {
  ic <- make_fixture(seed = 103L)
  countries <- attr(ic, "countries")
  population_hat <- setNames(c(1.20, 0.90, 1.05), countries)
  population_change <- data.table(country = countries,
                                  value = unname(population_hat[countries]))

  result <- run_cp_for_results(
    ic,
    model_scenario = list(population_change = population_change)
  )
  processed <- process_results(result)

  expect_equal(named_values(processed$output$value_added_new),
               named_values(result$output$value_added_new), tolerance = 1e-12)

  expected_income <-
    named_values(ic$value_added) * named_values(result$output$wage_change) * population_hat +
    named_values(processed$output$tariff_revenue_new) +
    named_values(processed$output$export_subsidy_costs_new) -
    named_values(result$output$trade_balance_new,
                 labels = sort(countries, method = "radix"))
  expect_equal(named_values(processed$output$income_new), expected_income,
               tolerance = 1e-10)

  expected_welfare <-
    named_values(processed$output$income_change) /
    named_values(processed$output$price_index_change) /
    population_hat[names(named_values(processed$output$income_change))]
  expect_equal(named_values(processed$output$welfare_change), expected_welfare,
               tolerance = 1e-12)
})

test_that("absent population changes default safely to one", {
  processed <- process_results(run_cp_for_results(make_fixture(seed = 104L)))

  expect_equal(named_values(processed$output$welfare_change),
               named_values(processed$output$income_change) /
                 named_values(processed$output$price_index_change),
               tolerance = 1e-12)
})

test_that("zero-baseline production hats are reported as one", {
  ic <- make_fixture(seed = 105L)
  ic$trade_share[origin == "c1" & sector == "s1", value := 0]
  ic$trade_share[sector == "s1", value := value / sum(value), by = destination]

  tariff_new <- copy(ic$tariff)
  tariff_new[origin == "c2" & destination == "c1", value := 1.25]
  processed <- process_results(run_cp_for_results(
    ic,
    model_scenario = list(tariff_new = tariff_new)
  ))

  production <- processed$output$production[country == "c1" & sector == "s1", value]
  production_hat <- processed$output$production_change[country == "c1" & sector == "s1", value]
  production_real_hat <- processed$output$production_real_change[country == "c1" & sector == "s1", value]

  expect_lt(abs(production), .Machine$double.eps)
  expect_identical(production_hat, 1)
  expect_identical(production_real_hat, 1)
})
