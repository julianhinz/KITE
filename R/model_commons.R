#' Update trade balance according to different adjustment rules
#'
#' @description
#' Updates the trade balance based on different rules:
#' (1) "fixed" - Unchanged trade balance,
#' (2) "fixed_country_share" - Constant relative to country's value added,
#' (3) "fixed_global_share" - Constant share in global value added,
#' (4) "zero" - Set all trade balances to zero.
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
                               rep(0, length(trade_balance))
                             },

                             stop("Invalid trade_balance_rule specified. Choose 'fixed', 'fixed_country_share', 'fixed_global_share', or 'zero'.")
  )

  return(trade_balance_new)
}
