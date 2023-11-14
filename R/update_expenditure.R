#' Update expenditure
#'
#' @description
#' Updates expenditures following Caliendo & Parro (2015) equation (13).
#'
#' @return Matrix of new expenditures, dimensions: sector x country.
#'
#' @param expenditure_new Matrix of new expenditures, dimensions: sector x country.
#' @param consumption_share Matrix of elasticities of substitution or consumption shares, dimensions: sector x country.
#' @param input_share Array of beta-multiplied input-output coefficients, dimensions: input_sector x output_sector x country.
#' @param trade_share_new Array of new trade flows (FOB), dimensions: origin x destination x sector.
#' @param tariff_new Array of new tariffs, dimensions: origin x destination x sector.
#' @param value_added Matrix of value added, dimension: country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param trade_balance Vector of aggregate trade balance, dimension: country.
#' @param model_attributes List of model attributes.
#' @param verbose verbose
#'

update_expenditure = function (expenditure_new,
                               consumption_share,
                               input_share,
                               trade_share_new,
                               tariff_new,
                               value_added,
                               wage_change,
                               trade_balance,
                               model_attributes,
                               verbose) {

  # iterate to new expenditure instead of inverting
  crit = 1
  j = 1
  while(crit >= model_attributes$tolerance) {
    if (verbose == 2L) cat("\r \u2014 Expenditure update, criterion = ")
    sum_exp0 = sum(expenditure_new)
    for (c in model_attributes$countries) {
      expenditure_new[,c] = input_share[,,c] %*% (expenditure_new %diag% (trade_share_new[c,,] / (tariff_new[c,,]))) + # input
        consumption_share[,c] * (
          sum(((tariff_new[,c,] - 1) * trade_share_new[,c,] / tariff_new[,c,]) %*% expenditure_new[,c]) # new tariff revenue
          + wage_change[c] * value_added[c] # new value added
          - trade_balance[c] # (unchanged) trade balance
        )
    }
    crit = abs((sum(expenditure_new) - sum_exp0) / sum_exp0)
    j = j + 1
    if (verbose == 2L) cat(crit, "after iteration", j-1)
  }
  return (expenditure_new)
}
