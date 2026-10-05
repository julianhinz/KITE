# Test-only helpers for one-sector models. A one-sector Caliendo-Parro
# economy whose only intermediate input is its own sector is the
# roundabout one-sector Eaton-Kortum model. `solve_one_sector_reference()`
# solves that model in changes with a direct linear solve for expenditure,
# independently of the package solvers.

#' Build a one-sector fixture whose baseline is an equilibrium
#'
#' Starts from `make_fixture(n_countries, 1)` and sets expenditure X,
#' trade balance D and value added VA so that, with gross output
#' Y_i = sum_j pi_ij X_j, D = Y - X and VA = beta * Y hold exactly. An
#' unchanged policy then leaves every change variable at one.
make_one_sector_fixture <- function(n_countries = 3L,
                                    seed = 1L,
                                    coalition_members = character(0)) {
  ic <- make_fixture(n_countries = n_countries, n_sectors = 1L, seed = seed,
                     coalition_members = coalition_members)
  countries <- attr(ic, "countries")

  pi_mat <- matrix(0, n_countries, n_countries,
                   dimnames = list(countries, countries))
  pi_mat[cbind(ic$trade_share$origin, ic$trade_share$destination)] <- ic$trade_share$value
  beta <- setNames(ic$factor_share$value, ic$factor_share$country)[countries]
  expenditure <- setNames(ic$expenditure$value, ic$expenditure$destination)[countries]

  output <- as.numeric(pi_mat %*% expenditure)
  ic$trade_balance <- data.table::data.table(country = countries,
                                             value = output - expenditure)
  ic$value_added <- data.table::data.table(country = countries,
                                           value = beta * output)
  ic
}

#' Solve the one-sector roundabout Eaton-Kortum model in changes
#'
#' Unknowns are the wage changes w. Given w, the price index changes P
#' solve P_j^(-theta) = sum_i pi_ij (kappa_ij w_i^beta_i P_i^(1 - beta_i))^(-theta),
#' new trade shares follow, and expenditure X solves the linear system
#' X_j = (1 - beta_j) Y_j + w_j VA_j + tariff revenue_j - D_j with
#' Y_i = sum_j pi'_ij X_j / t'_ij. Wages then move towards
#' w_i = beta_i Y_i / VA_i, keeping world value added at its baseline.
solve_one_sector_reference <- function(ic, tariff_new = NULL,
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
  tariff0 <- to_matrix(ic$tariff)
  stopifnot(all(tariff0 == 1))
  tariff1 <- if (is.null(tariff_new)) tariff0 else to_matrix(tariff_new)
  kappa <- tariff1 / tariff0
  beta <- to_vector(ic$factor_share)
  value_added <- to_vector(ic$value_added)
  trade_balance <- to_vector(ic$trade_balance)
  theta <- ic$elasticities$trade_elasticity$value

  solve_prices <- function(w) {
    p <- rep(1, n)
    for (k in seq_len(max_iterations)) {
      cost <- w^beta * p^(1 - beta)
      p_next <- colSums(pi0 * (kappa * cost)^(-theta))^(-1 / theta)
      if (max(abs(p_next / p - 1)) < tolerance) return(p_next)
      p <- p_next
    }
    stop("reference price index did not converge")
  }

  evaluate <- function(w) {
    p <- solve_prices(w)
    cost <- w^beta * p^(1 - beta)
    pi1 <- pi0 * sweep((kappa * cost)^(-theta), 2, p^(-theta), "/")
    fob_share <- pi1 / tariff1
    revenue_share <- colSums((tariff1 - 1) / tariff1 * pi1)
    a <- diag(n) - diag(1 - beta) %*% fob_share - diag(revenue_share)
    expenditure <- as.numeric(solve(a, w * value_added - trade_balance))
    list(price_change = p,
         output = as.numeric(fob_share %*% expenditure),
         tariff_revenue = revenue_share * expenditure)
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
  income <- value_added - trade_balance # baseline tariffs are one
  income_new <- w * value_added + eq$tariff_revenue - trade_balance
  list(wage_change = setNames(w, countries),
       price_change = setNames(eq$price_change, countries),
       welfare_change = setNames(income_new / income / eq$price_change, countries))
}
