library(testthat)
library(data.table)

test_that("required inner convergence is reflected in result metadata", {
  ic <- list(marker = data.table(country = "A", value = 1))
  unconverged_inner <- function(input, settings) {
    list(metric = 1, criterion = 0, iterations = 1L, inner_converged = FALSE)
  }

  result <- update_equilibrium(
    unconverged_inner, ic, list(),
    list(verbose = 0L, tolerance = 1e-6, require_inner_convergence = TRUE)
  )
  expect_false(result$info$convergence)
})

test_that("unknown convergence stays NA and non-finite economic output is unsafe", {
  ic <- list(marker = data.table(country = "A", value = 1))
  unknown <- function(input, settings) {
    list(metric = 1, criterion = NA_real_, iterations = NA_integer_,
         inner_converged = NA)
  }
  broken <- function(input, settings) {
    list(metric = NaN, criterion = 0, iterations = 1L, inner_converged = TRUE)
  }

  unknown_result <- update_equilibrium(
    unknown, ic, list(), list(verbose = 0L, tolerance = 1e-6)
  )
  broken_result <- update_equilibrium(
    broken, ic, list(), list(verbose = 0L, tolerance = 1e-6)
  )

  expect_true(is.na(unknown_result$info$convergence))
  expect_false(broken_result$info$convergence)
})

test_that("CHKW transfers use baseline and counterfactual trade balances separately", {
  countries <- c("A", "B")
  sectors <- c("s1", "s2")
  dims <- list(country = countries, sector = sectors)
  dn3 <- list(origin = countries, destination = countries, sector = sectors)
  trade_share <- array(0.5, c(2, 2, 2), dn3)
  policy <- array(1, c(2, 2, 2), dn3)
  expenditure <- matrix(c(50, 50, 50, 50), 2, 2,
                        dimnames = list(sector = sectors, country = countries))
  consumption_share <- matrix(0.5, 2, 2,
                              dimnames = list(country = countries, sector = sectors))
  price_change <- matrix(1, 2, 2,
                         dimnames = list(sector = sectors, country = countries))
  baseline_balance <- array(c(10, -10), 2, list(country = countries))
  counterfactual_balance <- array(c(0, 0), 2, list(country = countries))

  transfer <- KITE:::update_transfer_chkw_2022(
    transfer = array(0, 2, list(country = countries)),
    income_old = array(0, 2, list(country = countries)),
    income_new = array(0, 2, list(country = countries)),
    price_index_change = array(1, 2, list(country = countries)),
    expenditure = expenditure,
    expenditure_new = expenditure,
    consumption_share = consumption_share,
    trade_share = trade_share,
    trade_share_new = trade_share,
    tariff = policy,
    tariff_new = policy,
    export_subsidy = policy,
    export_subsidy_new = policy,
    value_added = array(c(100, 100), 2, list(country = countries)),
    wage_change = array(1, 2, list(country = countries)),
    trade_balance = baseline_balance,
    trade_balance_new = counterfactual_balance,
    price_change = price_change,
    coalition_member = array(c(1, 1), 2, list(country = countries)),
    model_dimensions = dims
  )

  expect_equal(as.numeric(transfer), c(-10, 10), tolerance = 1e-12)
})

make_symmetric_cp_fixture <- function() {
  countries <- c("A", "B")
  sectors <- c("s1", "s2")
  beta <- c(s1 = 0.80, s2 = 0.65)
  alpha <- c(s1 = 0.60, s2 = 0.40)
  gamma <- matrix(c(0.70, 0.30,
                    0.40, 0.60), 2, 2,
                  dimnames = list(input = sectors, output = sectors))
  input_share <- sweep(gamma, 2, 1 - beta, "*")
  expenditure_by_sector <- as.numeric(solve(diag(2) - input_share, alpha) * 100)

  trade_share <- CJ(origin = countries, destination = countries,
                    sector = sectors, sorted = FALSE)
  trade_share[, value := ifelse(origin == destination, 0.8, 0.2)]

  intermediate_share <- CJ(country = countries, input = sectors,
                           output = sectors, sorted = FALSE)
  intermediate_share[, value := gamma[cbind(match(input, sectors), match(output, sectors))]]

  factor_share <- CJ(country = countries, sector = sectors, sorted = FALSE)
  factor_share[, value := beta[sector]]
  consumption_share <- CJ(country = countries, sector = sectors, sorted = FALSE)
  consumption_share[, value := alpha[sector]]
  expenditure <- CJ(destination = countries, sector = sectors, sorted = FALSE)
  expenditure[, value := expenditure_by_sector[match(sector, sectors)]]
  identity_policy <- copy(trade_share)[, value := 1]

  list(
    trade_share = trade_share,
    intermediate_share = intermediate_share,
    factor_share = factor_share,
    consumption_share = consumption_share,
    expenditure = expenditure,
    value_added = data.table(country = countries, value = 100),
    trade_balance = data.table(country = countries, value = 0),
    elasticities = list(trade_elasticity =
                          data.table(sector = sectors, value = c(4, 6))),
    tariff = copy(identity_policy),
    ntb = copy(identity_policy),
    export_subsidy = copy(identity_policy),
    coalition_member = data.table(country = countries, value = 0L)
  )
}

test_that("CP2015 near autarky converges the active IO price fixed point", {
  ic <- make_symmetric_cp_fixture()
  ntb_new <- copy(ic$ntb)
  ntb_new[origin != destination, value := 1e4]

  result <- update_equilibrium(
    caliendo_parro_2015,
    ic,
    list(ntb_new = ntb_new),
    list(verbose = 0L, tolerance = 1e-10, vfactor = 0.2,
         max_iterations = 1000L, require_inner_convergence = FALSE)
  )

  expect_gt(result$info$iterations, 1L)
  expect_true(result$info$convergence)

  dims <- result$settings$model_dimensions
  output <- lapply(result$output, KITE:::cast_variable)
  initial <- lapply(result$initial_conditions, KITE:::cast_variable)
  productivity <- KITE:::initialize_variable(dims[c("country", "sector")], 1)
  recomputed_input_cost <- KITE:::update_input_cost_cp_2015(
    output$input_cost_change,
    output$wage_change,
    productivity,
    output$price_change,
    initial$factor_share,
    initial$intermediate_share,
    dims
  )
  expect_lt(check_convergence(recomputed_input_cost, output$input_cost_change), 1e-8)
})
