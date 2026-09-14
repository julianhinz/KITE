library(testthat)
library(data.table)

test_that("positive trade elasticity is the Frechet theta in both public models", {
  countries <- c("A", "B")
  dims <- list(country = countries, sector = "s1")
  dn3 <- list(origin = countries, destination = countries, sector = "s1")
  trade_share <- array(0.5, dim = c(2, 2, 1), dimnames = dn3)
  trade_cost_change <- array(1, dim = c(2, 2, 1), dimnames = dn3)
  trade_cost_change["A", "B", "s1"] <- 1.1
  theta <- c(s1 = 4)

  cp_cost <- matrix(1, 2, 1, dimnames = list(countries, "s1"))
  cp_price <- matrix(1, 2, 1, dimnames = list(countries, "s1"))
  cp_share <- KITE:::update_trade_share_cp_2015(
    trade_share, trade_cost_change, cp_cost, cp_price, theta, dims
  )
  expect_equal(cp_share["A", "B", "s1"] / trade_share["A", "B", "s1"],
               1.1^(-4), tolerance = 1e-12)

  chkw_cost <- matrix(1, 1, 2, dimnames = list("s1", countries))
  chkw_price <- matrix(1, 1, 2, dimnames = list("s1", countries))
  chkw_share <- KITE:::update_trade_share_chkw_2022(
    trade_share, trade_cost_change, chkw_cost, chkw_price, theta, dims
  )
  expect_equal(chkw_share["A", "B", "s1"] / trade_share["A", "B", "s1"],
               1.1^(-4), tolerance = 1e-12)
})

test_that("price indices use the negative inverse of positive theta", {
  countries <- c("A", "B")
  dims <- list(country = countries, sector = "s1")
  dn3 <- list(origin = countries, destination = countries, sector = "s1")
  trade_share <- array(0.5, dim = c(2, 2, 1), dimnames = dn3)
  trade_cost_change <- array(1, dim = c(2, 2, 1), dimnames = dn3)
  theta <- c(s1 = 4)
  expected <- (0.5 * 1.2^(-4) + 0.5)^(-1 / 4)

  cp_cost <- matrix(c(1.2, 1), 2, 1, dimnames = list(countries, "s1"))
  cp_price <- matrix(1, 2, 1, dimnames = list(countries, "s1"))
  cp_out <- KITE:::update_price_index_cp_2015(
    cp_price, trade_share, trade_cost_change, cp_cost, theta, dims
  )
  expect_equal(unname(cp_out[, "s1"]), rep(expected, 2), tolerance = 1e-12)

  chkw_cost <- matrix(c(1.2, 1), 1, 2, dimnames = list("s1", countries))
  chkw_price <- matrix(1, 1, 2, dimnames = list("s1", countries))
  chkw_out <- KITE:::update_price_index_chkw_2022(
    chkw_price, trade_share, trade_cost_change, chkw_cost, theta, dims
  )
  expect_equal(unname(chkw_out["s1", ]), rep(expected, 2), tolerance = 1e-12)
})

test_that("legacy elasticity conventions convert with a warning", {
  inverse <- data.table(sector = c("s1", "s2"), value = c(0.25, 0.20))
  expect_warning(
    converted_inverse <- KITE:::normalize_trade_elasticity(inverse),
    class = "kite_legacy_elasticity"
  )
  expect_equal(converted_inverse$value, c(4, 5))
  expect_equal(inverse$value, c(0.25, 0.20))

  expect_warning(
    converted_negative <- KITE:::normalize_trade_elasticity(c(s1 = -4, s2 = -5)),
    class = "kite_legacy_elasticity"
  )
  expect_equal(converted_negative, c(s1 = 4, s2 = 5))

  expect_warning(
    converted_negative_inverse <- KITE:::normalize_trade_elasticity(
      c(s1 = -0.25, s2 = -0.20)
    ),
    class = "kite_legacy_elasticity"
  )
  expect_equal(converted_negative_inverse, c(s1 = 4, s2 = 5))

  expect_no_warning(
    silent <- KITE:::normalize_trade_elasticity(inverse, convention = "silent")
  )
  expect_equal(silent$value, c(4, 5))
  expect_error(
    KITE:::normalize_trade_elasticity(inverse, convention = "strict"),
    "legacy"
  )
})

test_that("legacy inverse inputs reproduce standard-theta equilibrium", {
  ic <- make_fixture(seed = 111L)
  legacy <- ic
  legacy$elasticities <- list(trade_elasticity = copy(ic$elasticities$trade_elasticity))
  legacy$elasticities$trade_elasticity[, value := 1 / value]
  settings <- list(verbose = 0L, tolerance = 1e-5, vfactor = 0.1,
                   max_iterations = 500L)

  standard_result <- update_equilibrium(
    caliendo_parro_2015, ic, list(), settings
  )
  expect_warning(
    legacy_result <- update_equilibrium(
      caliendo_parro_2015, legacy, list(), settings
    ),
    class = "kite_legacy_elasticity"
  )

  for (field in c("wage_change", "price_change", "trade_share_new", "expenditure_new")) {
    expect_equal(legacy_result$output[[field]], standard_result$output[[field]],
                 tolerance = 1e-10)
  }
})

test_that("model dimensions and cast arrays share deterministic mixed-label ordering", {
  countries <- c("Central_Europe", "CHN", "BRA")
  sectors <- c("z_services", "A_goods")
  grid <- CJ(country = countries, sector = sectors, sorted = FALSE)
  set.seed(112L)
  grid <- grid[sample(.N)]
  grid[, country := factor(country, levels = rev(countries))]
  grid[, value := seq_len(.N)]

  dims <- KITE:::get_model_dimensions(list(example = grid))
  cast <- KITE:::cast_variable(copy(grid))

  expect_identical(dims$country, sort(countries, method = "radix"))
  expect_identical(dims$sector, sort(sectors, method = "radix"))
  expect_identical(dimnames(cast)$country, dims$country)
  expect_identical(dimnames(cast)$sector, dims$sector)
})

test_that("cast_variable preserves atomic mixed-label vectors", {
  labels <- c("USA", "Central_Europe", "CHN")
  expect_identical(KITE:::cast_variable(labels), labels)
  expect_identical(KITE:::cast_variable(list(labels = labels))$labels, labels)
})
