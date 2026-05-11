# Test-only fixture helpers. Construct small synthetic initial conditions
# inline so the test suite has no on-disk dependencies.

#' Build minimal initial conditions for testing
#'
#' Produces a list of initial conditions satisfying every economic constraint
#' the shipped models read: trade-share row sums to 1 over destination x
#' sector, intermediate-share row sums to 1 over country x output, factor
#' shares in (0, 1), positive value added, balanced trade, sector trade
#' elasticities > 1. The optional `coalition_members` argument flags
#' chowdhry_hinz_kamin_wanner_2022 coalition participants.
make_fixture <- function(n_countries = 3L,
                         n_sectors = 2L,
                         seed = 1L,
                         coalition_members = character(0)) {
  set.seed(seed)
  countries <- paste0("c", seq_len(n_countries))
  sectors <- paste0("s", seq_len(n_sectors))

  # trade_share: rows (destination x sector) sum to 1 across origins
  trade_share_raw <- array(
    runif(n_countries * n_countries * n_sectors, 0.3, 1),
    dim = c(n_countries, n_countries, n_sectors),
    dimnames = list(origin = countries,
                    destination = countries,
                    sector = sectors)
  )
  trade_share <- sweep(trade_share_raw, 2:3,
                       apply(trade_share_raw, 2:3, sum), "/")

  # intermediate_share: rows (country x output) sum to 1 across inputs
  intermediate_share_raw <- array(
    runif(n_countries * n_sectors * n_sectors, 0.3, 1),
    dim = c(n_countries, n_sectors, n_sectors),
    dimnames = list(country = countries, input = sectors, output = sectors)
  )
  intermediate_share <- sweep(intermediate_share_raw, c(1, 3),
                              apply(intermediate_share_raw, c(1, 3), sum), "/")

  # factor_share in (0, 1)
  factor_share <- matrix(
    runif(n_countries * n_sectors, 0.25, 0.55),
    nrow = n_countries, ncol = n_sectors,
    dimnames = list(country = countries, sector = sectors)
  )

  # value_added > 0
  value_added <- matrix(
    runif(n_countries * n_sectors, 80, 200),
    nrow = n_countries, ncol = n_sectors,
    dimnames = list(country = countries, sector = sectors)
  )

  # expenditure > 0 (country x sector)
  expenditure <- matrix(
    runif(n_countries * n_sectors, 80, 200),
    nrow = n_countries, ncol = n_sectors,
    dimnames = list(country = countries, sector = sectors)
  )

  # trade_balance sums to zero
  tb_raw <- runif(n_countries, -5, 5)
  trade_balance <- tb_raw - mean(tb_raw)
  names(trade_balance) <- countries

  # trade_elasticity > 1 per sector
  trade_elasticity <- setNames(runif(n_sectors, 4, 6), sectors)

  # tariffs / ntb / export_subsidy default to 1 (no policy)
  identity_3d <- function() {
    array(1, dim = c(n_countries, n_countries, n_sectors),
          dimnames = list(origin = countries,
                          destination = countries,
                          sector = sectors))
  }

  ic <- list(
    trade_share = trade_share,
    intermediate_share = intermediate_share,
    factor_share = factor_share,
    value_added = value_added,
    expenditure = expenditure,
    trade_balance = trade_balance,
    trade_elasticity = trade_elasticity,
    tariff = identity_3d(),
    ntb = identity_3d(),
    export_subsidy = identity_3d()
  )

  if (length(coalition_members) > 0) {
    ic$coalition_member <- countries %in% coalition_members
  } else {
    ic$coalition_member <- rep(FALSE, n_countries)
  }
  names(ic$coalition_member) <- countries

  attr(ic, "countries") <- countries
  attr(ic, "sectors") <- sectors
  ic
}

#' Build a model_dimensions list matching a fixture
make_model_dimensions <- function(fixture) {
  countries <- attr(fixture, "countries")
  sectors <- attr(fixture, "sectors")
  list(
    country = countries,
    origin = countries,
    destination = countries,
    sector = sectors,
    input = sectors,
    output = sectors
  )
}
