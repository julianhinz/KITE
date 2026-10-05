library(testthat)
library(data.table)

# A variable requested through additional_output_variables must never change
# a processed output. Request every variable that a solver can return and
# compare the processed results with those of a run that requests nothing.

returnable_variables <- function(model) {
  code <- paste(deparse(body(model)), collapse = "\n")
  found <- regmatches(code, gregexpr('input\\[\\["[A-Za-z0-9_.]+"\\]\\]', code))[[1]]
  unique(sub('input\\[\\["([A-Za-z0-9_.]+)"\\]\\]', "\\1", found))
}

neutral_settings <- list(verbose = 0L, tolerance = 1e-10, vfactor = 0.1,
                         max_iterations = 5000L)

neutral_scenario <- function(ic) {
  tariff_new <- copy(ic$tariff)
  tariff_new[origin == "c2" & destination != "c2", value := 1.3]
  export_subsidy_new <- copy(ic$tariff)
  export_subsidy_new[, value := 1]
  export_subsidy_new[origin == "c1" & destination == "c3", value := 1.1]
  list(tariff_new = tariff_new, export_subsidy_new = export_subsidy_new)
}

expect_all_requests_neutral <- function(model, ic, extra = list()) {
  variables <- returnable_variables(model)
  expect_true("value_added_new" %in% variables)
  scenario <- neutral_scenario(ic)
  plain <- update_equilibrium(model, ic, scenario, c(neutral_settings, extra))
  requested <- update_equilibrium(model, ic, scenario,
                                  c(neutral_settings, extra,
                                    list(additional_output_variables = variables)))
  expect_true(isTRUE(plain$info$convergence))
  expect_true(isTRUE(requested$info$convergence))
  plain_output <- process_results(plain)$output
  requested_output <- process_results(requested)$output
  for (variable in names(plain_output)) {
    expect_identical(requested_output[[variable]], plain_output[[variable]],
                     info = variable)
  }
}

test_that("requested CP outputs do not change processed results", {
  ic <- make_fixture(seed = 501L)
  expect_all_requests_neutral(caliendo_parro_2015, ic)
  expect_all_requests_neutral(caliendo_parro_2015, ic,
                              list(trade_balance_rule = "fixed_country_share"))
  expect_all_requests_neutral(caliendo_parro_2015, make_one_sector_fixture(seed = 502L))
})

test_that("requested CHKW outputs do not change processed results", {
  ic <- make_fixture(seed = 503L, coalition_members = c("c1", "c3"))
  expect_all_requests_neutral(chowdhry_hinz_kamin_wanner_2022, ic)
  expect_all_requests_neutral(chowdhry_hinz_kamin_wanner_2022, ic,
                              list(trade_balance_rule = "fixed_country_share"))
  expect_all_requests_neutral(chowdhry_hinz_kamin_wanner_2022,
                              make_one_sector_fixture(seed = 504L,
                                                      coalition_members = c("c1", "c3")))
})
