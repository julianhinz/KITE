library(testthat)
library(data.table)

mw_settings <- list(verbose = 0L, tolerance = 1e-10, vfactor = 0.1,
                    max_iterations = 5000L)

values_by <- function(x, key = "country") {
  x <- as.data.table(x)
  setNames(x$value, x[[key]])
}

values_by_keys <- function(x, keys) {
  x <- as.data.table(x)
  setorderv(x, keys)
  setNames(x$value, do.call(paste, x[, keys, with = FALSE]))
}

carbon_scenario <- function(ic, carbon_tax = 2, club = c("c1", "c2"),
                            carbon_tariff = TRUE, export_rebate = TRUE, ...) {
  c(list(carbon_tax = carbon_tax,
         countries_climate_club = club,
         scenario_carbon_tariff = carbon_tariff,
         scenario_export_rebate = export_rebate),
    list(...))
}

# arrays from long tables, independent of the package casting
as_array <- function(dt, keys, levels) {
  dt <- as.data.table(dt)
  a <- array(NA_real_, lengths(levels[keys]), levels[keys])
  a[as.matrix(dt[, keys, with = FALSE])] <- dt$value
  a
}

# income and trade from raw solver outputs: tax-inclusive absorption X minus
# intermediate purchases (1 - beta) Y is final demand, i.e. income
independent_accounts <- function(res, ic) {
  lv <- list(origin = attr(ic, "countries"), destination = attr(ic, "countries"),
             country = attr(ic, "countries"), sector = attr(ic, "sectors"))
  pi1 <- as_array(res$output$trade_share_new, c("origin", "destination", "sector"), lv)
  tariff <- as_array(res$output$tariff_new, c("origin", "destination", "sector"), lv)
  subsidy <- as_array(res$output$export_subsidy_new, c("origin", "destination", "sector"), lv)
  X <- as_array(res$output$expenditure_new, c("country", "sector"), lv)
  tax <- as_array(res$output$tax_new, c("country", "sector"), lv)
  beta <- as_array(ic$factor_share, c("country", "sector"), lv)
  flow <- sweep(pi1, 2:3, X / tax, "*")           # tariff-inclusive, net of tax
  Y <- apply(flow / (tariff * subsidy), c(1, 3), sum)
  list(income = rowSums(X) - rowSums((1 - beta) * Y),
       tax_revenue = rowSums((tax - 1) / tax * X),
       value_added = rowSums(beta * Y),
       net_exports = apply(flow / tariff, 1, sum) - apply(flow / tariff, 2, sum))
}

run_mw <- function(ic, scenario = list(), settings = list()) {
  update_equilibrium(mahlkow_wanner_2021, ic, scenario,
                     utils::modifyList(mw_settings, settings))
}

test_that("mahlkow_wanner_2021 runs to a classed kite_result", {
  ic <- make_mw_fixture(seed = 11L)
  res <- run_mw(ic, carbon_scenario(ic))
  expect_s3_class(res, "kite_result")
  expect_s3_class(res, "mahlkow_wanner_2021")
  expect_identical(res$model, "mahlkow_wanner_2021")
  expect_true(res$info$convergence)
  expect_true(all(c("wage_change", "price_change", "trade_share_new",
                    "expenditure_new", "value_added_new", "trade_balance_new",
                    "tax_new", "tariff_new", "export_subsidy_new") %in% names(res$output)))

  processed <- process_results(res)
  expect_true(all(c("welfare_change", "tax_revenue_new", "cbam_revenue_new",
                    "export_rebate_costs_new", "emissions", "emissions_new",
                    "emissions_change", "price_new") %in% names(processed$output)))
  expect_true(all(is.finite(processed$output$welfare_change$value)))
})

# reduction to caliendo_parro_2015 ----

cp_shock <- function(ic) {
  tariff_new <- copy(ic$tariff)
  tariff_new[origin == "c1" & destination != "c1", value := 1.25]
  export_subsidy_new <- copy(ic$export_subsidy)
  export_subsidy_new[origin == "c3" & destination == "c2", value := 0.9]
  productivity_change <- copy(ic$factor_share)[, value := 1]
  productivity_change[country == "c2" & sector == "s1", value := 1.05]
  population_change <- copy(ic$value_added)[, value := 1]
  population_change[country == "c3", value := 1.02]
  list(tariff_new = tariff_new, export_subsidy_new = export_subsidy_new,
       productivity_change = productivity_change,
       population_change = population_change)
}

for (rule in c("fixed", "fixed_country_share", "fixed_global_share", "zero")) {
  test_that(paste("without a carbon policy mahlkow_wanner_2021 reproduces caliendo_parro_2015, rule", rule), {
    ic <- make_mw_fixture(n_countries = 4L, n_sectors = 3L, seed = 31L)
    scenario <- cp_shock(ic)
    settings <- utils::modifyList(mw_settings, list(trade_balance_rule = rule))

    cp <- update_equilibrium(caliendo_parro_2015, ic, scenario, settings)
    # carbon inputs without a carbon tax are inert
    mw <- update_equilibrium(mahlkow_wanner_2021, ic,
                             c(scenario, list(countries_climate_club = c("c1", "c4"),
                                              scenario_carbon_tariff = TRUE,
                                              scenario_export_rebate = TRUE)),
                             settings)

    expect_true(cp$info$convergence)
    expect_true(mw$info$convergence)
    expect_identical(mw$info$iterations, cp$info$iterations)
    for (v in names(cp$output)) {
      expect_equal(mw$output[[v]], cp$output[[v]], tolerance = 1e-12, info = v)
    }

    cp_processed <- process_results(cp)$output
    mw_processed <- process_results(mw)$output
    for (v in names(cp_processed)) {
      expect_equal(mw_processed[[v]], cp_processed[[v]], tolerance = 1e-10, info = v)
    }
    expect_equal(unname(values_by(mw_processed$tax_revenue_new)), rep(0, 4))
    expect_equal(unname(values_by(mw_processed$cbam_revenue_new)), rep(0, 4))
  })
}

# exact equilibrium fixtures ----

test_that("an unchanged policy leaves an exact equilibrium at one, also with taxes", {
  for (with_tax in c(FALSE, TRUE)) {
    ic <- make_mw_equilibrium_fixture(n_countries = 4L, n_sectors = 3L, seed = 41L,
                                      with_tax = with_tax)
    res <- run_mw(ic)
    expect_true(res$info$convergence)
    expect_equal(res$output$wage_change$value, rep(1, 4), tolerance = 1e-8)
    expect_equal(res$output$price_change$value, rep(1, 12), tolerance = 1e-8)
    expect_equal(res$output$trade_share_new$value, ic$trade_share[order(sector, destination, origin)]$value,
                 tolerance = 1e-8)

    processed <- process_results(res)
    expect_equal(unname(values_by(processed$output$welfare_change)), rep(1, 4), tolerance = 1e-8)
    expect_equal(processed$output$production_change$value, rep(1, 12), tolerance = 1e-8)
    expect_equal(processed$output$emissions_change$value, rep(1, 4), tolerance = 1e-8)
    if (with_tax) {
      expect_true(all(values_by(processed$output$tax_revenue) > 0))
      expect_equal(processed$output$tax_revenue_change$value, rep(1, 4), tolerance = 1e-8)
    }
  }
})

test_that("a global carbon tax moves the equilibrium away from one", {
  ic <- make_mw_equilibrium_fixture(n_countries = 3L, n_sectors = 3L, seed = 42L)
  res <- run_mw(ic, list(carbon_tax = 1))
  expect_true(res$info$convergence)
  processed <- process_results(res)
  expect_true(all(values_by(processed$output$emissions_change) < 1))
  expect_true(all(values_by(processed$output$tax_revenue_new) > 0))
})

# accounting identities ----

test_that("carbon tax, border tariffs and export rebates satisfy their accounting identities", {
  ic <- make_mw_equilibrium_fixture(n_countries = 4L, n_sectors = 3L, seed = 51L)
  club <- c("c1", "c3")
  res <- run_mw(ic, carbon_scenario(ic, carbon_tax = 1.5, club = club,
                                    cbam_sector = c("s1", "s3")))
  expect_true(res$info$convergence)
  processed <- process_results(res)
  out <- processed$output

  countries <- attr(ic, "countries")
  sectors <- attr(ic, "sectors")
  member <- countries %in% club
  intensity <- setNames(ic$carbon_intensity$value, ic$carbon_intensity$sector)
  price_new <- values_by_keys(out$price_new, c("country", "sector"))
  price0 <- values_by_keys(ic$price, c("country", "sector"))
  price_change <- values_by_keys(res$output$price_change, c("country", "sector"))
  expect_equal(price_new, price0 * price_change)

  # tax wedge: 1 + carbon_tax * intensity / price level in the club, 1 outside
  tax_new <- res$output$tax_new
  expected_tax <- ifelse(tax_new$country %in% club,
                         1 + 1.5 * intensity[tax_new$sector] /
                           price_new[paste(tax_new$country, tax_new$sector)],
                         1)
  # the policy is updated once per iteration, so it is exact to the solver tolerance
  expect_equal(tax_new$value, unname(expected_tax), tolerance = 1e-8)

  # tax revenue is the carbon tax times the emissions in the club, zero outside
  expect_equal(values_by(out$tax_revenue_new)[club], 1.5 * values_by(out$emissions_new)[club],
               tolerance = 1e-8)
  expect_equal(unname(values_by(out$tax_revenue_new)[!member]), c(0, 0))

  # border tariffs only on imports of members from non-members in CBAM sectors
  tariff <- res$output$tariff_new
  cbam_cells <- tariff[!origin %in% club & destination %in% club & sector %in% c("s1", "s3")]
  expect_true(all(cbam_cells$value > 1))
  expect_true(all(tariff[!(!origin %in% club & destination %in% club & sector %in% c("s1", "s3"))]$value == 1))
  rebate <- res$output$export_subsidy_new
  rebate_cells <- rebate[origin %in% club & !destination %in% club & sector %in% c("s1", "s3")]
  expect_true(all(rebate_cells$value < 1))
  expect_true(all(rebate[!(origin %in% club & !destination %in% club & sector %in% c("s1", "s3"))]$value == 1))

  # border tariff rate is the carbon tax on the carbon embodied in intermediate inputs
  beta <- values_by_keys(ic$factor_share, c("country", "sector"))
  gamma <- ic$intermediate_share
  tax_cs <- values_by_keys(tax_new, c("country", "sector"))
  embodied <- function(o, s) {
    g <- gamma[country == o & output == s]
    sum(intensity[g$input] / (tax_cs[paste(o, g$input)] * price_new[paste(o, g$input)]) * g$value) *
      (1 - beta[paste(o, s)])
  }
  for (i in seq_len(nrow(cbam_cells))) {
    expect_equal(cbam_cells$value[i] - 1, unname(1.5 * embodied(cbam_cells$origin[i], cbam_cells$sector[i])),
                 tolerance = 1e-8)
  }
  for (i in seq_len(nrow(rebate_cells))) {
    expect_equal(1 - rebate_cells$value[i], unname(1.5 * embodied(rebate_cells$origin[i], rebate_cells$sector[i])),
                 tolerance = 1e-8)
  }

  # border tariff revenue and rebates: only members collect, only members pay
  expect_true(all(values_by(out$cbam_revenue_new)[club] > 0))
  expect_equal(unname(values_by(out$cbam_revenue_new)[!member]), c(0, 0))
  expect_true(all(values_by(out$export_rebate_costs_new)[club] < 0))
  expect_equal(unname(values_by(out$export_rebate_costs_new)[!member]), c(0, 0))
  # without baseline tariffs, all tariff revenue is border carbon tariff revenue
  expect_equal(values_by(out$cbam_revenue_new)[countries], values_by(out$tariff_revenue_new)[countries], tolerance = 1e-10)
  expect_equal(values_by(out$export_rebate_costs_new)[countries], values_by(out$export_subsidy_costs_new)[countries], tolerance = 1e-10)

  # income: processed income equals final demand from the raw solver outputs
  accounts <- independent_accounts(res, ic)
  expect_equal(values_by(out$income_new)[countries], accounts$income[countries], tolerance = 1e-8)
  expect_equal(values_by(out$tax_revenue_new)[countries], accounts$tax_revenue[countries], tolerance = 1e-10)

  # factor markets clear: value added from gross output equals wage bill
  va_new <- values_by(res$output$value_added_new)
  production_va <- merge(out$production_new, ic$factor_share, by = c("country", "sector"))
  production_va <- production_va[, .(va = sum(value.x * value.y)), by = country]
  expect_equal(setNames(production_va$va, production_va$country)[countries], va_new[countries],
               tolerance = 1e-8)
  expect_equal(va_new[countries],
               values_by(ic$value_added)[countries] * values_by(res$output$wage_change)[countries],
               tolerance = 1e-8)

  # tax-inclusive expenditure: suppliers receive expenditure net of the tax
  flows <- out$trade_flow_new[, .(value = sum(value)), by = .(destination, sector)]
  expenditure <- merge(res$output$expenditure_new, tax_new, by = c("country", "sector"))
  expenditure[, received := value.x / value.y]
  check <- merge(flows, expenditure, by.x = c("destination", "sector"), by.y = c("country", "sector"))
  expect_equal(check$value, check$received, tolerance = 1e-12)
})

# one sector ----

test_that("mahlkow_wanner_2021 with one sector leaves an unchanged policy at one", {
  ic <- add_carbon_inputs(make_one_sector_fixture(seed = 61L), 61L)
  res <- run_mw(ic)
  expect_true(res$info$convergence)
  expect_equal(res$output$wage_change$value, rep(1, 3), tolerance = 1e-8)
  expect_equal(res$output$price_change$value, rep(1, 3), tolerance = 1e-8)
  expect_true("sector" %in% names(res$output$price_change))
  processed <- process_results(res)
  expect_equal(unname(values_by(processed$output$welfare_change)), rep(1, 3), tolerance = 1e-8)
})

for (case in list(list(name = "a global carbon tax", club = c("c1", "c2", "c3", "c4"), cbam = FALSE, rebate = FALSE),
                  list(name = "a club carbon tax", club = c("c1", "c2"), cbam = FALSE, rebate = FALSE),
                  list(name = "a club carbon tax with border tariffs and export rebates", club = c("c1", "c2"), cbam = TRUE, rebate = TRUE))) {
  test_that(paste("one-sector", case$name, "matches an independent solution"), {
    ic <- add_carbon_inputs(make_one_sector_fixture(n_countries = 4L, seed = 62L), 62L)
    reference <- solve_one_sector_carbon_reference(ic, carbon_tax = 3, club = case$club,
                                                   carbon_tariff = case$cbam,
                                                   export_rebate = case$rebate)
    res <- run_mw(ic, carbon_scenario(ic, carbon_tax = 3, club = case$club,
                                      carbon_tariff = case$cbam, export_rebate = case$rebate),
                  list(tolerance = 1e-12))
    expect_true(res$info$convergence)
    processed <- process_results(res)
    countries <- names(reference$wage_change)

    expect_equal(values_by(res$output$wage_change)[countries], reference$wage_change, tolerance = 1e-7)
    expect_equal(values_by(res$output$price_change)[countries], reference$price_change, tolerance = 1e-7)
    expect_equal(values_by(res$output$tax_new)[countries], reference$tax_new, tolerance = 1e-7)
    expect_equal(values_by(processed$output$welfare_change)[countries], reference$welfare_change, tolerance = 1e-7)
    expect_equal(values_by(processed$output$emissions_new)[countries], reference$emissions_new, tolerance = 1e-7)
    expect_equal(values_by(processed$output$tax_revenue_new)[countries], reference$tax_revenue_new, tolerance = 1e-7)
  })
}

# trade-balance rules ----

for (rule in c("fixed", "fixed_country_share", "fixed_global_share", "zero")) {
  test_that(paste("a carbon policy converges under the trade-balance rule", rule), {
    ic <- make_mw_fixture(n_countries = 4L, n_sectors = 3L, seed = 71L)
    res <- run_mw(ic, carbon_scenario(ic, carbon_tax = 1),
                  list(trade_balance_rule = rule, tolerance = 1e-12))
    expect_true(res$info$convergence)
    processed <- process_results(res)
    expect_true(all(is.finite(processed$output$welfare_change$value)))

    countries <- attr(ic, "countries")
    accounts <- independent_accounts(res, ic)
    balance <- values_by(res$output$trade_balance_new)[countries]
    balance0 <- values_by(ic$trade_balance)[countries]
    va0 <- values_by(ic$value_added)[countries]
    va_new <- values_by(res$output$value_added_new)[countries]

    # the trade balance follows its rule
    expected <- switch(rule,
                       fixed = balance0,
                       fixed_country_share = balance0 / va0 * va_new,
                       fixed_global_share = balance0 / sum(va0) * sum(va_new),
                       zero = 0 * balance0)
    expect_equal(balance, expected, tolerance = 1e-10)

    # fixed_country_share does not keep the world trade balance at zero, as in
    # caliendo_parro_2015, so world accounts cannot close under that rule
    if (rule != "fixed_country_share") {
      expect_equal(sum(balance), 0, tolerance = 1e-10)
      # net exports at prices net of tariffs equal the solved trade balance
      expect_equal(accounts$net_exports[countries], balance, tolerance = 1e-8)
      # value added equals the wage bill; world value added is the numeraire
      expect_equal(accounts$value_added[countries], va0 * values_by(res$output$wage_change)[countries],
                   tolerance = 1e-10)
      expect_equal(sum(va_new), sum(va0), tolerance = 1e-12)
    }
  })
}

# inputs ----

test_that("the climate club accepts codes, named indicators and tables alike", {
  ic <- make_mw_fixture(n_countries = 4L, n_sectors = 2L, seed = 81L)
  base <- run_mw(ic, carbon_scenario(ic, club = c("c2", "c4")), list(tolerance = 1e-8))
  forms <- list(
    c(c4 = 1, c1 = 0, c2 = 1, c3 = 0),
    c(c2 = TRUE, c4 = TRUE),
    data.table(country = c("c1", "c2", "c3", "c4"), value = c(0L, 1L, 0L, 1L)),
    factor(c("c4", "c2"))
  )
  for (club in forms) {
    res <- run_mw(ic, carbon_scenario(ic, club = club), list(tolerance = 1e-8))
    expect_identical(res$output$wage_change, base$output$wage_change)
    expect_identical(res$output$tax_new, base$output$tax_new)
  }
})

test_that("unknown codes and invalid policy inputs are errors", {
  ic <- make_mw_fixture(seed = 82L)
  expect_error(run_mw(ic, carbon_scenario(ic, club = c("c1", "xx"))), "unknown country code")
  expect_error(run_mw(ic, carbon_scenario(ic, club = c(c1 = 1, zz = 0))), "unknown country code")
  expect_error(run_mw(ic, carbon_scenario(ic, club = c(c1 = 2))), "0/1")
  expect_error(run_mw(ic, carbon_scenario(ic, club = c(1, 0, 1))), "named")
  expect_error(run_mw(ic, carbon_scenario(ic, cbam_sector = "steel")), "unknown sector code")
  expect_error(run_mw(ic, carbon_scenario(ic, carbon_tax = c(1, 2))), "single finite number")
  expect_error(run_mw(ic, carbon_scenario(ic, carbon_tariff = NA)), "TRUE or FALSE")

  ic_no_price <- ic
  ic_no_price$price <- NULL
  expect_error(run_mw(ic_no_price, carbon_scenario(ic)), "price")
  ic_no_intensity <- ic
  ic_no_intensity$carbon_intensity <- NULL
  expect_error(run_mw(ic_no_intensity, carbon_scenario(ic)), "carbon_intensity")
})

test_that("policy variables in the initial conditions do not become model dimensions", {
  ic <- make_mw_fixture(seed = 83L)
  ic_policy <- c(ic, carbon_scenario(ic))
  res_ic <- run_mw(ic_policy, list(), list(tolerance = 1e-8))
  res_scenario <- run_mw(ic, carbon_scenario(ic), list(tolerance = 1e-8))
  expect_identical(names(res_ic$settings$model_dimensions), names(res_scenario$settings$model_dimensions))
  expect_equal(res_ic$output$wage_change, res_scenario$output$wage_change)
})

test_that("a named carbon_intensity must name each sector once", {
  ic <- make_mw_fixture(n_countries = 3L, n_sectors = 3L, seed = 85L)
  scenario <- carbon_scenario(ic)
  scenario$carbon_intensity <- c(s1 = 0.1, s2 = 0.2, typo = 0.3)
  expect_error(run_mw(ic, scenario), "typo")
  scenario$carbon_intensity <- c(s1 = 0.1, s2 = 0.2, s2 = 0.3)
  expect_error(run_mw(ic, scenario), "each sector once")
  # a named vector in any order equals the table
  scenario$carbon_intensity <- setNames(rev(ic$carbon_intensity$value), rev(ic$carbon_intensity$sector))
  expect_equal(run_mw(ic, scenario, list(tolerance = 1e-8))$output$wage_change,
               run_mw(ic, carbon_scenario(ic), list(tolerance = 1e-8))$output$wage_change)
})

test_that("carbon_tax is a single number and the switches are TRUE/FALSE or 1/0", {
  ic <- make_mw_fixture(seed = 86L)
  # a value-only table is not a valid input
  expect_error(suppressWarnings(run_mw(ic, carbon_scenario(ic, carbon_tax = data.table(value = 1)))))
  expect_error(run_mw(ic, carbon_scenario(ic, carbon_tax = "1")), "single finite number")
  as_logical <- run_mw(ic, carbon_scenario(ic), list(tolerance = 1e-8))
  as_number <- run_mw(ic, carbon_scenario(ic, carbon_tariff = 1, export_rebate = 1), list(tolerance = 1e-8))
  expect_identical(as_number$output$wage_change, as_logical$output$wage_change)
  expect_error(run_mw(ic, carbon_scenario(ic, carbon_tariff = 2)), "TRUE or FALSE")
})

test_that("the carbon tax adds to the exogenous tax wedge", {
  ic <- make_mw_equilibrium_fixture(n_countries = 3L, n_sectors = 3L, seed = 87L)
  tax_new <- dt_country_sector(1.1, attr(ic, "countries"), attr(ic, "sectors"))
  res <- run_mw(ic, list(carbon_tax = 0.5, countries_climate_club = "c2", tax_new = tax_new))
  expect_true(res$info$convergence)
  price_new <- values_by_keys(process_results(res)$output$price_new, c("country", "sector"))
  intensity <- setNames(ic$carbon_intensity$value, ic$carbon_intensity$sector)
  tax <- res$output$tax_new
  expected <- 1.1 + ifelse(tax$country == "c2",
                           0.5 * intensity[tax$sector] / price_new[paste(tax$country, tax$sector)], 0)
  expect_equal(tax$value, unname(expected), tolerance = 1e-8)
})

test_that("a baseline carbon price is replaced with tax_new = 1 and carbon_tax", {
  # the baseline wedge is a carbon price of 0.4 on all use, in every country
  ic <- make_mw_equilibrium_fixture(n_countries = 3L, n_sectors = 3L, seed = 88L,
                                    carbon_price = 0.4)
  ones <- dt_country_sector(1, attr(ic, "countries"), attr(ic, "sectors"))

  # the same carbon price again: nothing changes
  same <- run_mw(ic, list(carbon_tax = 0.4, tax_new = ones))
  expect_true(same$info$convergence)
  expect_equal(same$output$wage_change$value, rep(1, 3), tolerance = 1e-8)
  expect_equal(same$output$price_change$value, rep(1, 9), tolerance = 1e-8)
  expect_equal(same$output$tax_new$value, ic$tax[order(sector, country)]$value, tolerance = 1e-8)
  expect_equal(process_results(same)$output$welfare_change$value, rep(1, 3), tolerance = 1e-8)

  # a higher carbon price lowers emissions
  higher <- run_mw(ic, list(carbon_tax = 0.8, tax_new = ones))
  expect_true(all(process_results(higher)$output$emissions_change$value < 1))
  # without tax_new = 1 the new price comes on top of the old wedge
  on_top <- run_mw(ic, list(carbon_tax = 0.4))
  expect_true(all(on_top$output$tax_new$value > ic$tax[order(sector, country)]$value))
})

test_that("additional_output_variables returns further solver variables", {
  ic <- make_mw_fixture(seed = 84L)
  res <- run_mw(ic, carbon_scenario(ic),
                list(tolerance = 1e-8, additional_output_variables = c("income_new", "output_new")))
  expect_true(all(c("income_new", "output_new") %in% names(res$output)))
  # the solver's income is the income that process_results() computes
  processed <- process_results(run_mw(ic, carbon_scenario(ic), list(tolerance = 1e-8)))
  solver_income <- values_by(res$output$income_new, key = "destination")
  expect_equal(values_by(processed$output$income_new)[names(solver_income)], solver_income, tolerance = 1e-6)
})
