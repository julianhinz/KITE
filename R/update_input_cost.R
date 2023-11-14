#' Update input cost matrix
#'
#' @description
#' Updates cost of input bundle following Caliendo & Parro (2015) equations (10).
#'
#' @return Matrix of changes in input costs, dimensions: sector x country.
#'
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param wage_change Vector of change in wages, dimension: country.
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param factor_share Matrix of input factor shares, dimensions: sector x country.
#' @param intermediate_share Array of input-output coefficients, dimensions: input_sector x output_sector x country.
#' @param model_attributes List of model attributes.
#'

update_input_cost = function (input_cost_change,
                              wage_change,
                              price_change,
                              factor_share,
                              intermediate_share,
                              model_attributes) {

  for (c in model_attributes$countries) input_cost_change[,c] = exp(factor_share[,c] * log(wage_change[c]) + (1 - factor_share[,c]) * (t(intermediate_share[,,c]) %*% log(price_change[,c])))

  return (input_cost_change)
}
