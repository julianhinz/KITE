library(testthat)
library(data.table)

one_sector_settings <- list(verbose = 0L, tolerance = 1e-10, vfactor = 0.1,
                            max_iterations = 5000L)

one_sector_tariff <- function(ic, rate = 1.2) {
  tariff_new <- copy(ic$tariff)
  tariff_new[origin != destination, value := rate]
  tariff_new
}

country_values <- function(x) {
  x <- as.data.table(x)
  setNames(x$value, x$country)
}

test_that("slice_matrix keeps length-one dimensions and their order", {
  x <- array(seq_len(12), c(2, 3, 2),
             list(origin = c("a", "b"), destination = c("a", "b", "c"),
                  sector = c("s1", "s2")))
  expect_identical(KITE:::slice_matrix(x, "a", 1), x["a", , ])
  expect_identical(KITE:::slice_matrix(x, "b", 2), x[, "b", ])
  expect_identical(KITE:::slice_matrix(x, "s2", 3), x[, , "s2"])

  one <- x[, , "s1", drop = FALSE]
  sliced <- KITE:::slice_matrix(one, "a", 1)
  expect_identical(dim(sliced), c(3L, 1L))
  expect_identical(dimnames(sliced), dimnames(one)[-1])
  expect_identical(as.numeric(sliced), as.numeric(one["a", , ]))
  expect_identical(dim(KITE:::slice_matrix(one, "b", 2)), c(2L, 1L))
})

for (model_name in c("caliendo_parro_2015", "chowdhry_hinz_kamin_wanner_2022")) {

  test_that(paste(model_name, "with one sector leaves an unchanged policy at one"), {
    ic <- make_one_sector_fixture(seed = 201L)
    result <- update_equilibrium(get(model_name), ic, list(), one_sector_settings)

    expect_true(result$info$convergence)
    expect_equal(result$output$wage_change$value, rep(1, 3), tolerance = 1e-8)
    expect_equal(result$output$price_change$value, rep(1, 3), tolerance = 1e-8)

    processed <- process_results(result)
    expect_equal(unname(country_values(processed$output$welfare_change)),
                 rep(1, 3), tolerance = 1e-8)
  })

  test_that(paste(model_name, "with one sector solves a tariff shock and processes it"), {
    ic <- make_one_sector_fixture(seed = 202L)
    result <- update_equilibrium(get(model_name), ic,
                                 list(tariff_new = one_sector_tariff(ic)),
                                 one_sector_settings)
    expect_true(result$info$convergence)
    expect_true(all(abs(result$output$wage_change$value - 1) > 1e-6))
    expect_true("sector" %in% names(result$output$price_change))

    processed <- process_results(result)
    expect_true(all(is.finite(processed$output$welfare_change$value)))
    expect_true(all(is.finite(processed$output$production_new$value)))
    expect_true("sector" %in% names(processed$output$production_new))
  })

  test_that(paste(model_name, "with one sector matches the roundabout Eaton-Kortum solution"), {
    ic <- make_one_sector_fixture(n_countries = 4L, seed = 203L)
    tariff_new <- one_sector_tariff(ic, 1.25)
    tariff_new[origin == "c1" & destination == "c2", value := 1.6]
    reference <- solve_one_sector_reference(ic, tariff_new)

    result <- update_equilibrium(get(model_name), ic,
                                 list(tariff_new = tariff_new),
                                 utils::modifyList(one_sector_settings,
                                                   list(tolerance = 1e-12)))
    expect_true(result$info$convergence)
    processed <- process_results(result)
    countries <- names(reference$wage_change)

    expect_equal(country_values(result$output$wage_change)[countries],
                 reference$wage_change, tolerance = 1e-7)
    expect_equal(country_values(result$output$price_change)[countries],
                 reference$price_change, tolerance = 1e-7)
    expect_equal(country_values(processed$output$welfare_change)[countries],
                 reference$welfare_change, tolerance = 1e-7)
  })
}

test_that("chowdhry_hinz_kamin_wanner_2022 with one sector solves a coalition scenario", {
  ic <- make_one_sector_fixture(seed = 204L, coalition_members = c("c1", "c2"))
  result <- update_equilibrium(chowdhry_hinz_kamin_wanner_2022, ic,
                               list(tariff_new = one_sector_tariff(ic)),
                               one_sector_settings)
  expect_true(result$info$convergence)
  expect_true(any(abs(result$output$transfer$value) > 1e-8))
  expect_true(all(is.finite(process_results(result)$output$welfare_change$value)))
})
