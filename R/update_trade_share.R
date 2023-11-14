#' Update trade share matrix
#'
#' @description
#' Compute new trade shares following Caliendo & Parro (2015) equation (12).`
#'
#' @return Array of new trade shares, dimensions: origin x destination x sector.
#'
#' @param trade_share Array of initial trade flows, dimensions: origin x destination x sector.
#' @param trade_cost_change Array of change in trade costs, dimensions: origin x destination x sector.
#' @param input_cost_change Matrix of input cost changes, dimensions: sector x country.
#' @param price_change Matrix of price index changes, dimensions: sector x country.
#' @param trade_elasticity Vector of trade elasticities, dimension: sector.
#' @param model_attributes List of model attributes.
#'

update_trade_share = function (trade_share,
                               trade_cost_change,
                               input_cost_change,
                               price_change,
                               trade_elasticity,
                               model_attributes) {

  trade_share_new = trade_share
  for (s in model_attributes$sectors) trade_share_new[,,s] = t(t(trade_cost_change[,,s] * input_cost_change[s,]) / price_change[s,])^(-1/trade_elasticity[s]) * trade_share[,,s]

  return (trade_share_new)

}
