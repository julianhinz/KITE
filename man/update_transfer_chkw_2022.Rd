% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_chowdhry_hinz_kamin_wanner_2022.R
\name{update_transfer_chkw_2022}
\alias{update_transfer_chkw_2022}
\title{Update transfers for Chowdhry, Hinz, Kamin & Wanner (2022)}
\usage{
update_transfer_chkw_2022(
  transfer,
  income_old,
  income_new,
  price_index_change,
  expenditure,
  expenditure_new,
  consumption_share,
  trade_share,
  trade_share_new,
  tariff,
  tariff_new,
  export_subsidy,
  export_subsidy_new,
  value_added,
  wage_change,
  trade_balance,
  price_change,
  coalition_member,
  model_dimensions
)
}
\arguments{
\item{transfer}{Vector of transfers, dimension: country.}

\item{income_old}{Matrix of old income, dimensions: country.}

\item{income_new}{Matrix of new income, dimensions: country.}

\item{price_index_change}{Matrix of change of price index, dimensions: country.}

\item{expenditure}{Matrix of expenditures, dimensions: sector x country.}

\item{expenditure_new}{Matrix of new expenditures, dimensions: sector x country.}

\item{consumption_share}{Matrix of elasticities of substitution or consumption shares, dimensions: sector x country.}

\item{trade_share}{Array of trade flows (FOB), dimensions: origin x destination x sector.}

\item{trade_share_new}{Array of new trade flows (FOB), dimensions: origin x destination x sector.}

\item{tariff}{Array of tariffs, dimensions: origin x destination x sector.}

\item{tariff_new}{Array of new tariffs, dimensions: origin x destination x sector.}

\item{export_subsidy}{Array of export taxes, dimensions: origin x destination x sector.}

\item{export_subsidy_new}{Array of new export taxes, dimensions: origin x destination x sector.}

\item{value_added}{Matrix of value added, dimension: country.}

\item{wage_change}{Vector of change in wages, dimension: country.}

\item{trade_balance}{Vector of aggregate trade balance, dimension: country.}

\item{price_change}{Matrix of price index changes, dimensions: sector x country.}

\item{coalition_member}{Vector of country codes with coalition members, dimension: country.}

\item{model_dimensions}{List of model dimensions.}
}
\value{
Vector of new transfers, dimensions: country.
}
\description{
Updates transfers following Chowdhry, Hinz, Kamin & Wanner (2022) equation (13).
}
