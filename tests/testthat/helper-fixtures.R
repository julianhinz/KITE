# Test-only fixture helpers. Construct small synthetic initial conditions
# in long-format data.tables so the test suite has no on-disk dependencies.

#' Build minimal initial conditions for testing
#'
#' Returns a list of long-format data.tables that the public pipeline
#' (`update_equilibrium`) will cast to arrays via `cast_variable`. All
#' generated values satisfy the economic constraints the shipped models
#' read: trade-share row sums (over destination x sector) = 1,
#' intermediate-share row sums (over country x output) = 1, factor share
#' in (0, 1), consumption share row sums (per country) = 1, value added
#' positive, expenditure positive, trade balance sums to 0, trade
#' elasticity > 1 per sector. The optional `coalition_members` argument
#' flags chowdhry_hinz_kamin_wanner_2022 participants.
make_fixture <- function(n_countries = 3L,
                         n_sectors = 2L,
                         seed = 1L,
                         coalition_members = character(0)) {
  set.seed(seed)
  countries <- paste0("c", seq_len(n_countries))
  sectors <- paste0("s", seq_len(n_sectors))

  dt_3d <- function(values, dim_names) {
    # values: numeric vector matching prod(lengths)
    grid <- data.table::CJ(d1 = dim_names[[1]], d2 = dim_names[[2]], d3 = dim_names[[3]],
                           sorted = FALSE)
    data.table::setnames(grid, c("d1", "d2", "d3"), names(dim_names))
    grid[, value := values]
    grid[]
  }

  dt_2d <- function(values, dim_names) {
    grid <- data.table::CJ(d1 = dim_names[[1]], d2 = dim_names[[2]], sorted = FALSE)
    data.table::setnames(grid, c("d1", "d2"), names(dim_names))
    grid[, value := values]
    grid[]
  }

  dt_1d <- function(values, dim_name, dim_levels) {
    dt <- data.table::data.table(d1 = dim_levels, value = values)
    data.table::setnames(dt, "d1", dim_name)
    dt[]
  }

  # trade_share: origin x destination x sector, normalised so each
  # (destination, sector) row sums to 1 across origins
  ts_raw <- runif(n_countries * n_countries * n_sectors, 0.3, 1)
  ts_arr <- array(ts_raw, dim = c(n_countries, n_countries, n_sectors),
                  dimnames = list(origin = countries,
                                  destination = countries,
                                  sector = sectors))
  ts_norm <- sweep(ts_arr, 2:3, apply(ts_arr, 2:3, sum), "/")
  trade_share <- dt_3d(as.numeric(aperm(ts_norm, c(3, 2, 1))),
                       list(origin = countries,
                            destination = countries,
                            sector = sectors))

  # intermediate_share: country x input x output, sums to 1 over country x output
  is_raw <- runif(n_countries * n_sectors * n_sectors, 0.3, 1)
  is_arr <- array(is_raw, dim = c(n_countries, n_sectors, n_sectors),
                  dimnames = list(country = countries,
                                  input = sectors,
                                  output = sectors))
  is_norm <- sweep(is_arr, c(1, 3), apply(is_arr, c(1, 3), sum), "/")
  intermediate_share <- dt_3d(as.numeric(aperm(is_norm, c(3, 2, 1))),
                              list(country = countries,
                                   input = sectors,
                                   output = sectors))

  # factor_share in (0, 1)
  factor_share <- dt_2d(
    as.numeric(t(matrix(runif(n_countries * n_sectors, 0.25, 0.55), n_countries, n_sectors))),
    list(country = countries, sector = sectors))

  # consumption_share row sums to 1 per country
  cs_raw <- matrix(runif(n_countries * n_sectors, 0.3, 1), n_countries, n_sectors)
  cs_norm <- cs_raw / rowSums(cs_raw)
  consumption_share <- dt_2d(as.numeric(t(cs_norm)),
                              list(country = countries, sector = sectors))

  # expenditure > 0, destination x sector
  expenditure <- dt_2d(
    as.numeric(t(matrix(runif(n_countries * n_sectors, 100, 200), n_countries, n_sectors))),
    list(destination = countries, sector = sectors))

  # value_added > 0, per country
  value_added <- dt_1d(runif(n_countries, 100, 200), "country", countries)

  # trade_balance sums to 0
  tb_raw <- runif(n_countries, -5, 5)
  trade_balance <- dt_1d(tb_raw - mean(tb_raw), "country", countries)

  # trade_elasticity > 1 per sector (nested under elasticities)
  trade_elasticity <- dt_1d(runif(n_sectors, 4, 6), "sector", sectors)

  # tariff / ntb / export_subsidy default to 1 (no policy)
  identity_3d_dt <- function() {
    dt_3d(rep(1, n_countries * n_countries * n_sectors),
          list(origin = countries, destination = countries, sector = sectors))
  }

  ic <- list(
    trade_share = trade_share,
    intermediate_share = intermediate_share,
    factor_share = factor_share,
    consumption_share = consumption_share,
    expenditure = expenditure,
    value_added = value_added,
    trade_balance = trade_balance,
    elasticities = list(trade_elasticity = trade_elasticity),
    tariff = identity_3d_dt(),
    ntb = identity_3d_dt(),
    export_subsidy = identity_3d_dt()
  )

  if (length(coalition_members) > 0) {
    cm <- as.integer(countries %in% coalition_members)
  } else {
    cm <- rep(0L, n_countries)
  }
  ic$coalition_member <- dt_1d(cm, "country", countries)

  attr(ic, "countries") <- countries
  attr(ic, "sectors") <- sectors
  ic
}
