library(testthat)
library(data.table)

# chowdhry_hinz_kamin_wanner_2022 keeps income_new and income_old as zero work
# vectors and price_index_change at its initial ones: the transfer update fills
# local copies only. Requested through additional_output_variables, these
# placeholders replaced the processed income (income_new = 0), so income_change
# and welfare_change were 0 for every country. A requested variable must be a
# real value or absent.

chkw_placeholders <- c("income_new", "income_old", "price_index_change")

chkw_placeholder_settings <- list(verbose = 0L, tolerance = 1e-10, vfactor = 0.1,
                                  max_iterations = 5000L)

chkw_placeholder_pair <- function(ic, scenario, requested) {
  run <- function(extra) {
    result <- update_equilibrium(chowdhry_hinz_kamin_wanner_2022, ic, scenario,
                                 c(chkw_placeholder_settings, extra))
    expect_true(isTRUE(result$info$convergence))
    list(result = result, processed = process_results(result))
  }
  list(plain = run(list()),
       requested = run(list(additional_output_variables = requested)))
}

expect_placeholder_request_neutral <- function(pair) {
  plain <- pair$plain$processed$output
  requested <- pair$requested$processed$output
  for (variable in c("income_new", "income_change", "welfare_change", "price_index_change")) {
    expect_identical(requested[[variable]], plain[[variable]], info = variable)
  }
  expect_true(all(plain$income_new$value > 0))
  # placeholders are absent from the solver output, requested or not
  for (variable in chkw_placeholders) {
    expect_null(pair$requested$result$output[[variable]], info = variable)
  }
  # the solver outputs themselves do not change
  for (variable in names(pair$plain$result$output)) {
    expect_identical(pair$requested$result$output[[variable]],
                     pair$plain$result$output[[variable]], info = variable)
  }
}

chkw_placeholder_tariff <- function(ic) {
  tariff_new <- copy(ic$tariff)
  tariff_new[origin == "c2" & destination != "c2", value := 1.3]
  tariff_new
}

test_that("CHKW requested income_new does not replace the processed income (no coalition)", {
  ic <- make_fixture(seed = 301L)
  pair <- chkw_placeholder_pair(ic, list(tariff_new = chkw_placeholder_tariff(ic)), "income_new")
  expect_true(all(pair$plain$result$output$transfer$value == 0))
  expect_placeholder_request_neutral(pair)
})

test_that("CHKW requested income_new does not replace the processed income (coalition)", {
  ic <- make_fixture(seed = 302L, coalition_members = c("c1", "c3"))
  pair <- chkw_placeholder_pair(ic, list(tariff_new = chkw_placeholder_tariff(ic)), "income_new")
  expect_gt(max(abs(pair$plain$result$output$transfer$value)), 0)
  expect_placeholder_request_neutral(pair)
})

test_that("CHKW requested income_old and price_index_change are absent, not placeholders", {
  ic <- make_fixture(seed = 303L, coalition_members = c("c1", "c3"))
  pair <- chkw_placeholder_pair(ic, list(tariff_new = chkw_placeholder_tariff(ic)),
                                chkw_placeholders)
  expect_placeholder_request_neutral(pair)
})

test_that("CHKW requested income_new does not replace the processed income (one sector)", {
  ic <- make_one_sector_fixture(seed = 304L, coalition_members = c("c1", "c3"))
  pair <- chkw_placeholder_pair(ic, list(tariff_new = chkw_placeholder_tariff(ic)), "income_new")
  expect_placeholder_request_neutral(pair)
})
