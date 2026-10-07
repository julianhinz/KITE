library(testthat)
library(data.table)

# caliendo_parro_2015 rescales the wage change in every iteration so that
# world value added stays at its baseline level (the numeraire).
# chowdhry_hinz_kamin_wanner_2022 had no such rescale: its excess-function
# wage update keeps the wage level only while the world trade balance sums to
# zero. Under fixed_country_share after a shock, or on a baseline whose trade
# balances do not sum to zero, wages drifted and the run did not converge.
# Without a coalition, CHKW nests CP, so on an equilibrium baseline with
# balanced trade it must give CP's solution. Under fixed_country_share after a
# shock, or with trade balances that do not sum to zero, the trade balance
# rule cannot hold for the world as a whole; both solvers then stop at a fixed
# point that depends on their update rule and report no convergence; CHKW
# must still stay on CP's scale.

numeraire_settings <- list(verbose = 0L, tolerance = 1e-12, vfactor = 0.2,
                           max_iterations = 5000L)

numeraire_values <- function(x) setNames(x$value, x$country)

numeraire_shock <- function(ic) {
  list(tariff_new = copy(ic$tariff)[origin == "c1" & destination == "c3", value := 1.25])
}

# one-sector equilibrium baseline; its trade balances sum to zero unless
# `unbalanced`
numeraire_ic <- function(unbalanced = FALSE) {
  ic <- make_one_sector_fixture(seed = 501L)
  if (unbalanced) ic$trade_balance <- copy(ic$trade_balance)[country == "c1", value := value + 2]
  ic
}

# `feasible = FALSE`: the rule cannot hold for the world, so both runs warn
# and report no convergence
expect_chkw_equal_cp <- function(ic, scenario, rule, label, tolerance = 1e-8,
                                 feasible = TRUE) {
  settings <- c(numeraire_settings, list(trade_balance_rule = rule,
                                         additional_output_variables = "value_added_new"))
  run <- function(model) {
    if (feasible) return(update_equilibrium(model, ic, scenario, settings))
    expect_warning(result <- update_equilibrium(model, ic, scenario, settings),
                   class = "kite_world_trade_balance")
    result
  }
  cp <- run(caliendo_parro_2015)
  chkw <- run(chowdhry_hinz_kamin_wanner_2022)

  # same reported convergence, and the solver stops at a fixed point
  expect_identical(isTRUE(chkw$info$convergence), isTRUE(cp$info$convergence), info = label)
  expect_identical(isTRUE(cp$info$convergence), feasible, info = label)
  expect_lt(chkw$info$iterations, settings$max_iterations)
  expect_lte(chkw$info$criterion, settings$tolerance)
  # same scale: world value added as in CP
  expect_equal(sum(chkw$output$value_added_new$value), sum(cp$output$value_added_new$value),
               tolerance = tolerance, info = label)
  for (variable in c("wage_change", "value_added_new", "trade_balance_new")) {
    expect_equal(numeraire_values(chkw$output[[variable]]),
                 numeraire_values(cp$output[[variable]]),
                 tolerance = tolerance, info = paste(label, variable))
  }
}

test_that("CHKW converges on CP's scale under fixed_country_share after a shock", {
  for (unbalanced in c(FALSE, TRUE)) {
    ic <- numeraire_ic(unbalanced)
    expect_chkw_equal_cp(ic, numeraire_shock(ic), "fixed_country_share",
                         paste("fixed_country_share, unbalanced =", unbalanced),
                         tolerance = 1e-3, feasible = FALSE)
  }
})

test_that("CHKW converges on CP's scale when trade balances do not sum to zero", {
  ic <- numeraire_ic(unbalanced = TRUE)
  for (rule in c("fixed", "fixed_global_share")) {
    expect_chkw_equal_cp(ic, numeraire_shock(ic), rule, paste("unbalanced", rule),
                         tolerance = 1e-3, feasible = FALSE)
  }
})

test_that("CHKW still equals CP on a balanced baseline under every rule", {
  ic <- numeraire_ic()
  for (rule in c("fixed", "zero", "fixed_global_share")) {
    expect_chkw_equal_cp(ic, numeraire_shock(ic), rule, paste("balanced", rule))
  }
})
