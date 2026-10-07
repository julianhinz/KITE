#' Update trade balance according to different adjustment rules
#'
#' @description
#' Updates the trade balance based on different rules:
#' (1) "fixed" - Unchanged trade balance,
#' (2) "fixed_country_share" - Constant relative to country's value added,
#' (3) "fixed_global_share" - Constant share in global value added,
#' (4) "zero" - Set all trade balances to zero.
#'
#' The solved trade balances must sum to zero for the world. Under
#' `"fixed_country_share"` they do so only if value added changes by the same
#' factor in every country, which in practice means no shock. Under `"fixed"`
#' and `"fixed_global_share"` they do so only if the baseline trade balances
#' sum to zero. [update_equilibrium()] reports `convergence = FALSE` with a
#' warning of class `kite_world_trade_balance` for runs where the rule cannot
#' hold for the world.
#'
#' @return Vector of updated trade balance values, dimension: country.
#'
#' @param trade_balance Vector of initial trade balances, dimension: country.
#' @param value_added Vector of initial value added, dimension: country.
#' @param value_added_new Vector of new value added, dimension: country.
#' @param trade_balance_rule Character string specifying adjustment rule: `"fixed"`, `"fixed_country_share"`, `"fixed_global_share"`, or `"zero"`.
#'

update_trade_balance = function (trade_balance,
                                 value_added,
                                 value_added_new,
                                 trade_balance_rule = "fixed") {

  # Check if trade balance is all zeros and warn if using scaling rules
  if (all(trade_balance == 0) && trade_balance_rule %in% c("fixed_country_share", "fixed_global_share")) {
    warning("Trade balance is all zeros. Automatically setting trade_balance_rule to 'zero'.")
    trade_balance_rule = "zero"
  }

  # Apply the selected trade balance adjustment rule
  trade_balance_new = switch(trade_balance_rule,

                             "fixed" = {
                               # No change
                               copy(trade_balance)
                             },

                             "fixed_country_share" = {
                               # Trade balance remains constant relative to each country's value added
                               # Protect against division by zero for individual countries
                               ifelse(value_added == 0, 0, trade_balance / value_added * value_added_new)
                             },

                             "fixed_global_share" = {
                               # Trade balance shares in global value added are constant
                               trade_balance / sum(value_added) * sum(value_added_new)
                             },

                             "zero" = {
                               # Set all trade balances to zero
                               trade_balance_zero = copy(trade_balance)
                               trade_balance_zero[] = 0
                               trade_balance_zero
                             },

                             stop("Invalid trade_balance_rule specified. Choose 'fixed', 'fixed_country_share', 'fixed_global_share', or 'zero'.")
  )

  return(trade_balance_new)
}


#' Check that the solved trade balances can hold for the world
#'
#' @description
#' In every shipped model, world exports equal world imports, so the solved
#' trade balances of all countries must sum to zero. The trade balance rule
#' sets the world sum of `trade_balance_new`: `"fixed"` keeps the baseline
#' sum, `"fixed_global_share"` scales it with world value added, `"zero"`
#' sets it to zero, and `"fixed_country_share"` gives each country's baseline
#' share of its own value added. When this world sum is not zero, the rule
#' cannot hold for the world: labour markets do not clear and the fixed point
#' that the solver reaches depends on `vfactor`. This happens with
#' `"fixed_country_share"` after a shock that does not move value added
#' proportionally in every country, and under every rule except `"zero"`
#' when the baseline trade balances do not sum to zero.
#'
#' The check passes when the absolute world sum is at most `tolerance` times
#' world value added. It also refuses a solution with non-finite trade
#' balances or with value added that is not positive.
#'
#' @return List with `ok` (TRUE, FALSE, or NA when the model returns no
#'   `trade_balance_new`), `rule`, `world_trade_balance`,
#'   `baseline_world_trade_balance`, `world_value_added`, `tolerance` and
#'   `reason`.
#'
#' @param trade_balance Vector of baseline trade balances, dimension: country.
#' @param trade_balance_new Vector of solved trade balances, dimension: country.
#'   Each of the four inputs may also be a long table with a `value` column.
#' @param value_added Vector of baseline value added, dimension: country.
#' @param value_added_new Vector of solved value added, dimension: country, or
#'   NULL when the model does not return it.
#' @param trade_balance_rule Character string, the trade balance rule.
#' @param tolerance Tolerance relative to world value added.
#'
#' @keywords internal

check_world_trade_balance = function (trade_balance,
                                      trade_balance_new,
                                      value_added,
                                      value_added_new = NULL,
                                      trade_balance_rule = "fixed",
                                      tolerance = 1e-6) {

  out = list(ok = NA, rule = trade_balance_rule,
             world_trade_balance = NA_real_,
             baseline_world_trade_balance = NA_real_,
             world_value_added = NA_real_,
             tolerance = tolerance,
             reason = NA_character_)
  fail = function (reason) { out$ok = FALSE; out$reason = reason; out }

  # arrays and vectors as they are; long tables (e.g. from a model outside the
  # package) through their `value` column; anything else as missing
  values = function (x) {
    if (is.data.frame(x)) {
      if (!"value" %in% names(x)) return(NULL)
      x = x[["value"]]
    }
    if (!is.numeric(x)) return(NULL)
    as.numeric(x)
  }
  trade_balance = values(trade_balance)
  trade_balance_new = values(trade_balance_new)
  value_added = values(value_added)
  if (!is.null(value_added_new)) {
    value_added_new = values(value_added_new)
    if (is.null(value_added_new)) {
      out$reason = "value_added_new is not numeric"
      return(out)
    }
  }

  if (is.null(trade_balance_new) || is.null(trade_balance)) {
    out$reason = "the model returns no numeric trade_balance_new"
    return(out)
  }

  out$baseline_world_trade_balance = sum(trade_balance)
  if (!all(is.finite(trade_balance_new))) return(fail("trade_balance_new is not finite"))

  # degenerate solutions: value added must stay positive
  if (!is.null(value_added_new)) {
    if (!all(is.finite(value_added_new))) return(fail("value_added_new is not finite"))
    if (any(value_added_new <= 0)) {
      return(fail(sprintf("value_added_new is not positive for %d of %d countries (degenerate solution)",
                          sum(value_added_new <= 0), length(value_added_new))))
    }
    world_value_added = sum(value_added_new)
  } else {
    world_value_added = if (is.null(value_added)) NA_real_ else sum(abs(value_added))
  }
  if (!is.finite(world_value_added) || world_value_added <= 0) {
    return(fail("world value added is not positive"))
  }

  out$world_trade_balance = sum(trade_balance_new)
  out$world_value_added = world_value_added
  out$ok = abs(out$world_trade_balance) <= tolerance * world_value_added
  out$reason = if (out$ok) {
    "the solved trade balances sum to zero for the world"
  } else if (abs(out$baseline_world_trade_balance) > tolerance * world_value_added &&
             !identical(trade_balance_rule, "zero")) {
    "the baseline trade balances do not sum to zero"
  } else {
    sprintf("trade_balance_rule = '%s' cannot hold for the world after this scenario", trade_balance_rule)
  }
  out
}
