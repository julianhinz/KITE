library(testthat)
library(data.table)

# caliendo_parro_2015 sums trade flows over origin or destination to get
# income and output. The sums kept the summed-over array's dimension name, so
# income and income_new carried `destination` and output and output_new
# carried `origin`. Requested through additional_output_variables, income_new
# then replaced the processed income in process_results() and failed with
# "object 'country' not found". A requested variable must not change the
# processed results.

cp_request_settings <- list(verbose = 0L, tolerance = 1e-10, vfactor = 0.1,
                            max_iterations = 5000L)

cp_request_scenario <- function(ic, export_subsidy = FALSE) {
  tariff_new <- copy(ic$tariff)
  tariff_new[origin == "c2" & destination != "c2", value := 1.3]
  scenario <- list(tariff_new = tariff_new)
  if (export_subsidy) {
    export_subsidy_new <- copy(ic$tariff)
    export_subsidy_new[, value := 1]
    export_subsidy_new[origin == "c1" & destination == "c3", value := 1.1]
    scenario$export_subsidy_new <- export_subsidy_new
  }
  scenario
}

cp_request_pair <- function(ic, scenario, requested, extra = list()) {
  run <- function(more) {
    result <- update_equilibrium(caliendo_parro_2015, ic, scenario,
                                 c(cp_request_settings, extra, more))
    expect_true(isTRUE(result$info$convergence))
    list(result = result, processed = process_results(result))
  }
  list(plain = run(list()),
       requested = run(list(additional_output_variables = requested)))
}

expect_cp_request_neutral <- function(pair) {
  plain <- pair$plain$processed$output
  requested <- pair$requested$processed$output
  for (variable in names(plain)) {
    expect_identical(requested[[variable]], plain[[variable]], info = variable)
  }
  # the default solver outputs do not change
  for (variable in names(pair$plain$result$output)) {
    expect_identical(pair$requested$result$output[[variable]],
                     pair$plain$result$output[[variable]], info = variable)
  }
}

test_that("CP requested income_new processes and matches the processed income", {
  ic <- make_fixture(seed = 401L)
  pair <- cp_request_pair(ic, cp_request_scenario(ic), "income_new")
  expect_cp_request_neutral(pair)

  income_new <- pair$requested$result$output$income_new
  expect_identical(names(income_new), c("country", "value"))
  processed <- pair$plain$processed$output$income_new
  expect_equal(income_new$value[match(processed$country, income_new$country)],
               processed$value, tolerance = 1e-6)
})

test_that("CP requested income_new is neutral with export subsidies and a moving trade balance", {
  ic <- make_fixture(seed = 402L)
  pair <- cp_request_pair(ic, cp_request_scenario(ic, export_subsidy = TRUE),
                          "income_new",
                          list(trade_balance_rule = "fixed_country_share"))
  expect_cp_request_neutral(pair)
})

test_that("CP requested income_new is neutral in a one-sector model", {
  ic <- make_one_sector_fixture(seed = 403L)
  pair <- cp_request_pair(ic, cp_request_scenario(ic), "income_new")
  expect_cp_request_neutral(pair)
})

test_that("CP income and output carry the country dimension", {
  ic <- make_fixture(seed = 404L)
  requested <- c("income", "income_new", "output", "output_new")
  pair <- cp_request_pair(ic, cp_request_scenario(ic), requested)
  expect_cp_request_neutral(pair)
  output <- pair$requested$result$output
  expect_identical(names(output$income), c("country", "value"))
  expect_identical(names(output$income_new), c("country", "value"))
  expect_identical(names(output$output), c("country", "sector", "value"))
  expect_identical(names(output$output_new), c("country", "sector", "value"))
})
