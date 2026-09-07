library(testthat)
library(data.table)

test_that("caliendo_parro_2015 runs to a classed kite_result", {
  ic <- make_fixture(seed = 11L)
  res <- update_equilibrium(
    model = caliendo_parro_2015,
    initial_conditions = ic,
    model_scenario = list(),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )
  expect_s3_class(res, "kite_result")
  expect_s3_class(res, "caliendo_parro_2015")
  expect_true(res$info$convergence)
  expect_true("wage_change" %in% names(res$output))
  expect_true("price_change" %in% names(res$output))
  expect_true("trade_share_new" %in% names(res$output))
})

test_that("caliendo_parro_2015 tariff scenario moves trade shares away from baseline", {
  ic <- make_fixture(seed = 22L)
  # destination = "c2" raises a 20% tariff on imports from all origins
  tariff_war <- data.table::copy(ic$tariff)
  tariff_war[destination == "c2", value := 1.2]

  res_baseline <- update_equilibrium(
    model = caliendo_parro_2015,
    initial_conditions = ic,
    model_scenario = list(),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )
  res_shock <- update_equilibrium(
    model = caliendo_parro_2015,
    initial_conditions = ic,
    model_scenario = list(tariff_new = tariff_war),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )

  base_dt <- res_baseline$output$trade_share_new
  shock_dt <- res_shock$output$trade_share_new
  merged <- merge(base_dt, shock_dt,
                  by = c("origin", "destination", "sector"),
                  suffixes = c("_base", "_shock"))
  expect_gt(max(abs(merged$value_shock - merged$value_base)), 1e-4)
})

test_that("process_results dispatches the caliendo_parro_2015 method", {
  ic <- make_fixture(seed = 33L)
  res <- update_equilibrium(
    model = caliendo_parro_2015,
    initial_conditions = ic,
    model_scenario = list(),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )
  processed <- process_results(res)
  expect_type(processed, "list")
})
