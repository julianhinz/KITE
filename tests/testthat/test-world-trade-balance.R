library(testthat)
library(data.table)

# In the shipped models world exports equal world imports, so the solved
# trade balances must sum to zero for the world. If the trade balance rule
# cannot give that, the fixed point that the solver reaches is not an
# equilibrium: labour markets do not clear and the solution depends on
# `vfactor`. Such runs must report convergence = FALSE and warn.

wtb_models <- list(caliendo_parro_2015 = caliendo_parro_2015,
                   chowdhry_hinz_kamin_wanner_2022 = chowdhry_hinz_kamin_wanner_2022)

wtb_settings <- function(rule, vfactor = 0.2) {
  list(verbose = 0L, tolerance = 1e-12, vfactor = vfactor,
       max_iterations = 5000L, trade_balance_rule = rule)
}

wtb_shock <- function(ic) {
  list(tariff_new = copy(ic$tariff)[origin == "c1" & destination == "c3", value := 1.25])
}

# one-sector equilibrium baseline; trade balances sum to zero unless `unbalanced`
wtb_ic <- function(unbalanced = FALSE) {
  ic <- make_one_sector_fixture(seed = 12L)
  if (unbalanced) ic$trade_balance <- copy(ic$trade_balance)[country == "c1", value := value + 2]
  ic
}

expect_infeasible <- function(model, ic, scenario, rule, label) {
  expect_warning(result <- update_equilibrium(model, ic, scenario, wtb_settings(rule)),
                 class = "kite_world_trade_balance")
  expect_false(result$info$convergence, label = label)
  expect_false(result$info$accounting$ok, label = label)
  expect_identical(result$info$accounting$rule, rule)
  expect_gt(abs(result$info$accounting$world_trade_balance),
            result$info$accounting$tolerance * result$info$accounting$world_value_added)
  # the solver itself stopped at a fixed point; only the accounting fails
  expect_lte(result$info$criterion, 1e-12)
  invisible(result)
}

expect_feasible <- function(model, ic, scenario, rule, label) {
  expect_no_warning(result <- update_equilibrium(model, ic, scenario, wtb_settings(rule)))
  expect_true(result$info$convergence, label = label)
  expect_true(result$info$accounting$ok, label = label)
  invisible(result)
}

test_that("fixed_country_share after a shock reports no convergence", {
  ic <- wtb_ic()
  for (m in names(wtb_models)) {
    expect_infeasible(wtb_models[[m]], ic, wtb_shock(ic), "fixed_country_share", m)
  }
})

test_that("unbalanced baselines report no convergence unless the rule is zero", {
  ic <- wtb_ic(unbalanced = TRUE)
  for (m in names(wtb_models)) {
    for (rule in c("fixed", "fixed_global_share", "fixed_country_share")) {
      result <- expect_infeasible(wtb_models[[m]], ic, list(), rule, paste(m, rule))
      expect_equal(result$info$accounting$baseline_world_trade_balance, 2, tolerance = 1e-10)
    }
    expect_feasible(wtb_models[[m]], ic, wtb_shock(ic), "zero", paste(m, "zero"))
  }
})

test_that("infeasible runs depend on vfactor, feasible runs do not", {
  # the reason for the check: the reported fixed point is not an equilibrium
  ic <- wtb_ic(unbalanced = TRUE)
  wage <- function(rule, vfactor) {
    result <- suppressWarnings(update_equilibrium(caliendo_parro_2015, ic, wtb_shock(ic),
                                                  wtb_settings(rule, vfactor)))
    result$output$wage_change$value
  }
  expect_gt(max(abs(wage("fixed", 0.1) - wage("fixed", 0.3))), 1e-4)
  expect_lt(max(abs(wage("zero", 0.1) - wage("zero", 0.3))), 1e-9)
})

test_that("feasible runs report convergence under every rule", {
  ic <- wtb_ic()
  for (m in names(wtb_models)) {
    for (rule in c("fixed", "fixed_global_share", "zero")) {
      expect_feasible(wtb_models[[m]], ic, wtb_shock(ic), rule, paste(m, rule))
    }
    # without a shock, value added does not move and the rule holds
    expect_feasible(wtb_models[[m]], ic, list(), "fixed_country_share",
                    paste(m, "fixed_country_share, no shock"))
  }
})

test_that("CHKW with a coalition passes the check", {
  ic <- make_one_sector_fixture(seed = 12L, coalition_members = c("c1", "c2"))
  expect_feasible(chowdhry_hinz_kamin_wanner_2022, ic, wtb_shock(ic), "fixed", "coalition")
})

test_that("multi-sector fixtures pass the check", {
  ic <- make_fixture(n_countries = 3L, n_sectors = 2L, seed = 7L)
  scenario <- list(tariff_new = copy(ic$tariff)[origin == "c1" & destination == "c2", value := 1.1])
  for (m in names(wtb_models)) {
    result <- expect_feasible(wtb_models[[m]], ic, scenario, "fixed", m)
    expect_lt(abs(result$info$accounting$world_trade_balance), 1e-8)
  }
})

test_that("the default tolerance lets rounding residuals pass", {
  # a baseline whose trade balances sum to `share` of world value added
  residual_ic <- function(share) {
    ic <- wtb_ic()
    world_value_added <- sum(ic$value_added$value)
    ic$trade_balance <- copy(ic$trade_balance)[country == "c1", value := value + share * world_value_added]
    ic
  }
  for (m in names(wtb_models)) {
    ic <- residual_ic(1e-8)
    result <- expect_feasible(wtb_models[[m]], ic, wtb_shock(ic), "fixed", paste(m, "1e-8"))
    expect_identical(result$info$accounting$tolerance, 1e-6)
    ic <- residual_ic(1.5e-6)
    expect_infeasible(wtb_models[[m]], ic, wtb_shock(ic), "fixed", paste(m, "1.5e-6"))
  }
})

test_that("tolerance_accounting sets the tolerance", {
  ic <- wtb_ic(unbalanced = TRUE)
  settings <- c(wtb_settings("fixed"), list(tolerance_accounting = 0.1))
  result <- update_equilibrium(caliendo_parro_2015, ic, list(), settings)
  expect_true(result$info$convergence)
  expect_identical(result$info$accounting$tolerance, 0.1)
})

test_that("check_world_trade_balance refuses degenerate solutions", {
  va <- c(a = 100, b = 100)
  ok <- check_world_trade_balance(c(1, -1), c(2, -2), va, c(150, 50))
  expect_true(ok$ok)
  expect_false(check_world_trade_balance(c(1, -1), c(2, -2), va, c(200, 0))$ok)
  expect_match(check_world_trade_balance(c(1, -1), c(2, -2), va, c(250, -50))$reason,
               "degenerate")
  expect_false(check_world_trade_balance(c(1, -1), c(NaN, -2), va, c(150, 50))$ok)
  expect_false(check_world_trade_balance(c(1, -1), c(2, -2), va, c(NA, 50))$ok)
  # a model without trade_balance_new is not checked
  expect_true(is.na(check_world_trade_balance(c(1, -1), NULL, va)$ok))
})
