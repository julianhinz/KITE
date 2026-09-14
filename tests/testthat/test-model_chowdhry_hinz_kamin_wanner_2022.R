library(testthat)
library(data.table)

test_that("chowdhry_hinz_kamin_wanner_2022 runs to a classed kite_result", {
  ic <- make_fixture(seed = 41L, coalition_members = c("c1", "c3"))
  res <- update_equilibrium(
    model = chowdhry_hinz_kamin_wanner_2022,
    initial_conditions = ic,
    model_scenario = list(),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )
  expect_s3_class(res, "kite_result")
  expect_s3_class(res, "chowdhry_hinz_kamin_wanner_2022")
  expect_true(res$info$convergence)
  expect_true("wage_change" %in% names(res$output))
  expect_true("trade_share_new" %in% names(res$output))
})

test_that("chowdhry_hinz_kamin_wanner_2022 coalition-applied tariff reduces target's foreign trade", {
  ic <- make_fixture(seed = 42L, coalition_members = c("c1", "c3"))
  tariff_sanction <- data.table::copy(ic$tariff)
  # coalition (c1, c3) tariffs the target (c2), bidirectional
  tariff_sanction[origin %in% c("c1", "c3") & destination == "c2", value := 1.5]
  tariff_sanction[origin == "c2" & destination %in% c("c1", "c3"), value := 1.5]

  res_baseline <- update_equilibrium(
    model = chowdhry_hinz_kamin_wanner_2022,
    initial_conditions = ic,
    model_scenario = list(),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )
  res_shock <- update_equilibrium(
    model = chowdhry_hinz_kamin_wanner_2022,
    initial_conditions = ic,
    model_scenario = list(tariff_new = tariff_sanction),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )

  base_dt <- res_baseline$output$trade_share_new
  shock_dt <- res_shock$output$trade_share_new

  # target (c2) trade with coalition members should fall in aggregate
  ts_base <- base_dt[(destination == "c2" & origin %in% c("c1", "c3")) |
                     (origin == "c2" & destination %in% c("c1", "c3")), sum(value)]
  ts_shock <- shock_dt[(destination == "c2" & origin %in% c("c1", "c3")) |
                      (origin == "c2" & destination %in% c("c1", "c3")), sum(value)]
  expect_lt(ts_shock, ts_base)
})

test_that("process_results dispatches the chowdhry_hinz_kamin_wanner_2022 method", {
  ic <- make_fixture(seed = 43L, coalition_members = c("c1"))
  res <- update_equilibrium(
    model = chowdhry_hinz_kamin_wanner_2022,
    initial_conditions = ic,
    model_scenario = list(),
    settings = list(verbose = 0L, tolerance = 1e-4, vfactor = 0.1, max_iterations = 200)
  )
  processed <- process_results(res)
  expect_type(processed, "list")
})
