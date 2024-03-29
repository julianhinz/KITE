% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_chowdhry_hinz_kamin_wanner_2022.R
\name{chowdhry_hinz_kamin_wanner_2022}
\alias{chowdhry_hinz_kamin_wanner_2022}
\title{Compute Chowdhry, Hinz, Kamin & Wanner (2022) model in changes}
\usage{
chowdhry_hinz_kamin_wanner_2022(input, settings)
}
\arguments{
\item{input}{List of prepared initial and counterfactual conditions.}

\item{settings}{List of various settings and model dimensions}
}
\value{
List of list of data.tables with initial_conditions, model_scenarios and equilibrium_new.
}
\description{
`chowdhry_hinz_kamin_wanner_2022()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
}
