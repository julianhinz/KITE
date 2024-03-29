% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_caliendo_parro_2015.R
\name{update_expenditure_cp_2015}
\alias{update_expenditure_cp_2015}
\title{Update expenditure for Caliendo Parro (2015)}
\usage{
update_expenditure_cp_2015(
  expenditure_new,
  consumption_share,
  input_share,
  trade_share_new,
  tariff_new,
  export_subsidy_new,
  value_added,
  wage_change,
  trade_balance,
  model_dimensions,
  tolerance,
  verbose
)
}
\arguments{
\item{expenditure_new}{Matrix of new expenditures, dimensions: sector x country.}

\item{consumption_share}{Matrix of elasticities of substitution or consumption shares, dimensions: sector x country.}

\item{input_share}{Array of beta-multiplied input-output coefficients, dimensions: input_sector x output_sector x country.}

\item{trade_share_new}{Array of new trade flows (FOB), dimensions: origin x destination x sector.}

\item{tariff_new}{Array of new tariffs, dimensions: origin x destination x sector.}

\item{export_subsidy_new}{Array of export taxes, dimensions: origin x destination x sector.}

\item{value_added}{Matrix of value added, dimension: country.}

\item{wage_change}{Vector of change in wages, dimension: country.}

\item{trade_balance}{Vector of aggregate trade balance, dimension: country.}

\item{model_dimensions}{List of model dimensions.}

\item{tolerance}{tolerance}

\item{verbose}{verbose}
}
\value{
Matrix of new expenditures, dimensions: sector x country.
}
\description{
Updates expenditures following Caliendo & Parro (2015) equation (13).
}
