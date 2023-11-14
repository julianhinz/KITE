#' Update price index matrix
#'
#' @description
#' Update price index following Caliendo & Parro (2015) equation (11).
#'
#' @return Matrix of changes in price index, dimensions: sector x country.
#'
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param trade_share Array of initial trade flows, dimensions: origin x destination x sector.
#' @param trade_cost_change Array of change in trade costs, dimensions: origin x destination x sector.
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param trade_elasticity Vector of trade elasticities, dimensions: sector.
#' @param model_attributes List of model attributes.
#'

update_price = function (price_change,
                         trade_share,
                         trade_cost_change,
                         input_cost_change,
                         trade_elasticity,
                         model_attributes) {

  for (s in model_attributes$sectors) price_change[s,] = ((t(trade_share[,,s]) * (t(trade_cost_change[,,s])^(-1/trade_elasticity[s]))) %*% (input_cost_change[s,]^(-1/trade_elasticity[s])))^(-trade_elasticity[s])

  # correct for zeros (i.e. prices for non-traded)
  for (s in model_attributes$sectors) price_change[s,][price_change[s,] == 0] = 1

  return (price_change)
}
