# KITE -- end-to-end tutorial
#
# Demonstrates running both shipped models on a synthetic 3-country,
# 2-sector economy. For calibrated initial conditions matching real
# trade and IO data, contact KITE@kielinstitut.de.

library(KITE)
library(data.table)

run_kite_example <- function(seed = 1L) {
  set.seed(seed)
  countries <- c("c1", "c2", "c3")
  sectors <- c("s1", "s2")
  n_c <- length(countries); n_s <- length(sectors)

  # ---- 1. Build minimal synthetic initial conditions ------------------------

  dt_3d <- function(values, names_list) {
    grid <- data.table::CJ(d1 = names_list[[1]],
                           d2 = names_list[[2]],
                           d3 = names_list[[3]],
                           sorted = FALSE)
    data.table::setnames(grid, c("d1", "d2", "d3"), names(names_list))
    grid[, value := values][]
  }
  dt_2d <- function(values, names_list) {
    grid <- data.table::CJ(d1 = names_list[[1]], d2 = names_list[[2]], sorted = FALSE)
    data.table::setnames(grid, c("d1", "d2"), names(names_list))
    grid[, value := values][]
  }
  dt_1d <- function(values, name, levels) {
    dt <- data.table::data.table(d1 = levels, value = values)
    data.table::setnames(dt, "d1", name); dt[]
  }

  # trade_share normalised so each (destination, sector) row sums to 1
  ts_arr <- array(runif(n_c * n_c * n_s, 0.3, 1), c(n_c, n_c, n_s),
                  dimnames = list(origin = countries,
                                  destination = countries,
                                  sector = sectors))
  ts_norm <- sweep(ts_arr, 2:3, apply(ts_arr, 2:3, sum), "/")
  trade_share <- dt_3d(as.numeric(aperm(ts_norm, c(3, 2, 1))),
                       list(origin = countries,
                            destination = countries,
                            sector = sectors))

  is_arr <- array(runif(n_c * n_s * n_s, 0.3, 1), c(n_c, n_s, n_s),
                  dimnames = list(country = countries,
                                  input = sectors, output = sectors))
  is_norm <- sweep(is_arr, c(1, 3), apply(is_arr, c(1, 3), sum), "/")
  intermediate_share <- dt_3d(as.numeric(aperm(is_norm, c(3, 2, 1))),
                              list(country = countries,
                                   input = sectors, output = sectors))

  factor_share <- dt_2d(
    as.numeric(t(matrix(runif(n_c * n_s, 0.25, 0.55), n_c, n_s))),
    list(country = countries, sector = sectors))

  cs_raw <- matrix(runif(n_c * n_s, 0.3, 1), n_c, n_s)
  consumption_share <- dt_2d(as.numeric(t(cs_raw / rowSums(cs_raw))),
                              list(country = countries, sector = sectors))

  expenditure <- dt_2d(
    as.numeric(t(matrix(runif(n_c * n_s, 100, 200), n_c, n_s))),
    list(destination = countries, sector = sectors))

  value_added <- dt_1d(runif(n_c, 100, 200), "country", countries)

  tb_raw <- runif(n_c, -5, 5)
  trade_balance <- dt_1d(tb_raw - mean(tb_raw), "country", countries)

  trade_elasticity <- dt_1d(runif(n_s, 4, 6), "sector", sectors)

  identity_tariff <- dt_3d(rep(1, n_c * n_c * n_s),
                           list(origin = countries,
                                destination = countries,
                                sector = sectors))

  ic <- list(
    trade_share = trade_share,
    intermediate_share = intermediate_share,
    factor_share = factor_share,
    consumption_share = consumption_share,
    expenditure = expenditure,
    value_added = value_added,
    trade_balance = trade_balance,
    elasticities = list(trade_elasticity = trade_elasticity),
    tariff = identity_tariff,
    ntb = data.table::copy(identity_tariff),
    export_subsidy = data.table::copy(identity_tariff),
    coalition_member = dt_1d(c(1L, 0L, 1L), "country", countries)
  )

  # ---- 2. Scenario: 20% tariff war between c1 and c2 ------------------------

  tariff_war <- data.table::copy(identity_tariff)
  tariff_war[origin == "c1" & destination == "c2", value := 1.2]
  tariff_war[origin == "c2" & destination == "c1", value := 1.2]

  # ---- 3. Run caliendo_parro_2015 -------------------------------------------

  result_cp <- update_equilibrium(
    model = caliendo_parro_2015,
    initial_conditions = ic,
    model_scenario = list(tariff_new = tariff_war),
    settings = list(verbose = 0L, tolerance = 1e-4,
                    vfactor = 0.1, max_iterations = 200)
  )
  cp_summary <- process_results(result_cp)

  # ---- 4. Run chowdhry_hinz_kamin_wanner_2022 with the coalition ------------

  result_chkw <- update_equilibrium(
    model = chowdhry_hinz_kamin_wanner_2022,
    initial_conditions = ic,
    model_scenario = list(tariff_new = tariff_war),
    settings = list(verbose = 0L, tolerance = 1e-4,
                    vfactor = 0.1, max_iterations = 200)
  )
  chkw_summary <- process_results(result_chkw)

  invisible(list(cp = result_cp, chkw = result_chkw,
                 cp_summary = cp_summary, chkw_summary = chkw_summary))
}

if (interactive() || nzchar(Sys.getenv("KITE_RUN_EXAMPLE", unset = ""))) {
  res <- run_kite_example()
  cat("\nCP2015 converged in", res$cp$info$iterations,
      "iterations (criterion =", res$cp$info$criterion, ").\n")
  cat("CHKW2022 converged in", res$chkw$info$iterations,
      "iterations (criterion =", res$chkw$info$criterion, ").\n")
}
