#' KITE Model Suite
#'
#' A quantitative framework for international trade policy analysis.
#' Multi-sector Ricardian models with intra- and international input-output
#' linkages; supports simulation of trade policy changes via tariff and
#' non-tariff measure scenarios.
#'
#' @importFrom stats coef lm setNames
"_PACKAGE"

# Silence R CMD check NOTEs about data.table NSE column references.
utils::globalVariables(c(
  "value", "country", "origin", "destination", "sector",
  "input", "output", "welfare_change"
))
