# Test-only helpers for mahlkow_wanner_2021: fixtures with price levels and
# carbon intensities, a multi-sector fixture whose baseline is an exact
# equilibrium, and an independent solver of the one-sector model with a
# carbon policy.

dt_country_sector <- function(values, countries, sectors) {
  grid <- data.table::CJ(country = countries, sector = sectors, sorted = FALSE)
  grid[, value := as.numeric(values)]
  grid[]
}

#' Add initial price levels and carbon intensities to a fixture
add_carbon_inputs <- function(ic, seed = 1L) {
  set.seed(seed + 1000L)
  countries <- attr(ic, "countries")
  sectors <- attr(ic, "sectors")
  ic$price <- dt_country_sector(runif(length(countries) * length(sectors), 0.5, 1.5),
                                countries, sectors)
  ic$carbon_intensity <- data.table::data.table(
    sector = sectors, value = runif(length(sectors), 0.05, 0.3))
  ic
}

make_mw_fixture <- function(n_countries = 3L, n_sectors = 2L, seed = 1L) {
  add_carbon_inputs(make_fixture(n_countries, n_sectors, seed), seed)
}

#' Build a multi-sector fixture whose baseline is an exact equilibrium
#'
#' Draws income I, solves tax-inclusive expenditure
#' X[n, k] = sum_s (1 - beta[n, s]) gamma[n, k, s] Y[n, s] + alpha[n, k] I[n]
#' with Y[i, s] = sum_n pi[i, n, s] X[n, s] / tax[n, s], and sets
#' value added VA = sum_s beta Y and the trade balance
#' D = VA + tax revenue - I. An unchanged policy then leaves every change
#' variable at one.
make_mw_equilibrium_fixture <- function(n_countries = 3L, n_sectors = 3L,
                                        seed = 1L, with_tax = FALSE) {
  ic <- make_mw_fixture(n_countries, n_sectors, seed)
  countries <- attr(ic, "countries")
  sectors <- attr(ic, "sectors")
  n <- n_countries; S <- n_sectors

  pi_arr <- array(0, c(n, n, S), list(countries, countries, sectors))
  pi_arr[cbind(ic$trade_share$origin, ic$trade_share$destination, ic$trade_share$sector)] <- ic$trade_share$value
  gamma <- array(0, c(n, S, S), list(countries, sectors, sectors))
  gamma[cbind(ic$intermediate_share$country, ic$intermediate_share$input, ic$intermediate_share$output)] <- ic$intermediate_share$value
  cs_matrix <- function(dt) {
    m <- matrix(0, n, S, dimnames = list(countries, sectors))
    m[cbind(dt$country, dt$sector)] <- dt$value
    m
  }
  beta <- cs_matrix(ic$factor_share)
  alpha <- cs_matrix(ic$consumption_share)
  set.seed(seed + 2000L)
  tax <- matrix(1, n, S, dimnames = list(countries, sectors))
  if (with_tax) tax[] <- runif(n * S, 1, 1.3)
  income <- setNames(runif(n, 100, 200), countries)

  X <- matrix(100, n, S, dimnames = list(countries, sectors))
  for (k in 1:20000) {
    Y <- matrix(0, n, S, dimnames = list(countries, sectors))
    for (s in sectors) Y[, s] <- pi_arr[, , s] %*% (X[, s] / tax[, s])
    X_next <- X
    for (c in countries) {
      X_next[c, ] <- as.numeric(matrix(gamma[c, , ], S, S) %*% ((1 - beta[c, ]) * Y[c, ])) + alpha[c, ] * income[c]
    }
    done <- max(abs(X_next / X - 1)) < 1e-15
    X <- X_next
    if (done) break
  }
  Y <- matrix(0, n, S, dimnames = list(countries, sectors))
  for (s in sectors) Y[, s] <- pi_arr[, , s] %*% (X[, s] / tax[, s])
  value_added <- rowSums(beta * Y)
  tax_revenue <- rowSums((tax - 1) / tax * X)

  ic$expenditure <- data.table::data.table(
    destination = rep(countries, each = S), sector = rep(sectors, n),
    value = as.numeric(t(X)))
  ic$value_added <- data.table::data.table(country = countries, value = value_added)
  ic$trade_balance <- data.table::data.table(country = countries,
                                             value = value_added + tax_revenue - income)
  if (with_tax) ic$tax <- dt_country_sector(t(tax), countries, sectors)
  ic
}

#' Solve the one-sector model with a carbon policy in changes
#'
#' Independent of the package solvers: nested fixed points for the price
#' index (with the carbon tax, border carbon tariffs and export rebates at
#' the current prices), a direct linear solve for tax-inclusive expenditure,
#' and damped wage updates with world value added as the numeraire.
#' Baseline tariffs, export subsidies and taxes are one.
solve_one_sector_carbon_reference <- function(ic, carbon_tax, club,
                                              carbon_tariff = FALSE,
                                              export_rebate = FALSE,
                                              tolerance = 1e-13,
                                              damping = 0.2,
                                              max_iterations = 100000L) {
  countries <- attr(ic, "countries")
  n <- length(countries)
  to_matrix <- function(dt) {
    m <- matrix(0, n, n, dimnames = list(countries, countries))
    m[cbind(dt$origin, dt$destination)] <- dt$value
    m
  }
  to_vector <- function(dt) setNames(dt$value, dt$country)[countries]

  pi0 <- to_matrix(ic$trade_share)
  stopifnot(all(to_matrix(ic$tariff) == 1), all(to_matrix(ic$export_subsidy) == 1))
  beta <- to_vector(ic$factor_share)
  value_added <- to_vector(ic$value_added)
  trade_balance <- to_vector(ic$trade_balance)
  price0 <- to_vector(ic$price)
  intensity <- ic$carbon_intensity$value
  theta <- ic$elasticities$trade_elasticity$value
  member <- countries %in% club

  instruments <- function(p) {
    tax <- ifelse(member, 1 + carbon_tax * intensity / (price0 * p), 1)
    embodied <- intensity * (1 - beta) / (tax * price0 * p)
    tariff <- matrix(1, n, n); subsidy <- matrix(1, n, n)
    if (carbon_tariff) tariff[!member, member] <- 1 + carbon_tax * embodied[!member]
    if (export_rebate) subsidy[member, !member] <- 1 - carbon_tax * embodied[member]
    list(tax = tax, tariff = tariff, subsidy = subsidy)
  }

  solve_prices <- function(w) {
    p <- rep(1, n)
    for (k in seq_len(max_iterations)) {
      ins <- instruments(p)
      cost <- w^beta * (ins$tax * p)^(1 - beta)
      p_next <- colSums(pi0 * (ins$tariff * ins$subsidy * cost)^(-theta))^(-1 / theta)
      if (max(abs(p_next / p - 1)) < tolerance) return(p_next)
      p <- p_next
    }
    stop("reference price index did not converge")
  }

  evaluate <- function(w) {
    p <- solve_prices(w)
    ins <- instruments(p)
    cost <- w^beta * (ins$tax * p)^(1 - beta)
    kappa <- ins$tariff * ins$subsidy
    pi1 <- pi0 * sweep((kappa * cost)^(-theta), 2, p^(-theta), "/")
    cif_share <- sweep(pi1, 2, ins$tax, "/")   # flows per unit of expenditure X_j
    fob_share <- cif_share / kappa
    tariff_rate <- colSums((ins$tariff - 1) / ins$tariff * cif_share)
    subsidy_cost <- (ins$subsidy - 1) * fob_share  # origin x destination
    tax_rate <- (ins$tax - 1) / ins$tax
    a <- diag(n) - diag(1 - beta) %*% fob_share - diag(tariff_rate) - subsidy_cost - diag(tax_rate)
    expenditure <- as.numeric(solve(a, w * value_added - trade_balance))
    list(price_change = p, tax = ins$tax,
         output = as.numeric(fob_share %*% expenditure),
         income = w * value_added + tariff_rate * expenditure +
           as.numeric(subsidy_cost %*% expenditure) + tax_rate * expenditure - trade_balance,
         emissions = intensity * expenditure / (ins$tax * price0 * p),
         tax_revenue = tax_rate * expenditure)
  }

  w <- rep(1, n)
  converged <- FALSE
  for (k in seq_len(max_iterations)) {
    eq <- evaluate(w)
    w_next <- w^(1 - damping) * (beta * eq$output / value_added)^damping
    w_next <- w_next * sum(value_added) / sum(w_next * value_added)
    converged <- max(abs(w_next / w - 1)) < tolerance
    w <- w_next
    if (converged) break
  }
  if (!converged) stop("reference wages did not converge")

  eq <- evaluate(w)
  income <- value_added - trade_balance # baseline taxes and tariffs are one
  list(wage_change = setNames(w, countries),
       price_change = setNames(eq$price_change, countries),
       tax_new = setNames(eq$tax, countries),
       welfare_change = setNames(eq$income / income / (eq$tax * eq$price_change), countries),
       emissions_new = setNames(eq$emissions, countries),
       tax_revenue_new = setNames(eq$tax_revenue, countries))
}
